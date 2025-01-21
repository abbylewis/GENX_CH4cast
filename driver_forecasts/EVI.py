import ee

# Initialize the Earth Engine API
ee.Initialize()

pip install earthengine-api

# Set parameters
site_name = 'Louisiana Universities Marine Consortium'
start_year = '2019'
end_year = '2024'

# Define lat/lon coordinates and a buffer radius (if needed)
latitude = 38.87477  # Replace with your latitude
longitude = -76.54977  # Replace with your longitude
buffer_radius = 100  # Buffer radius in meters (optional)

# Create a point geometry
point = ee.Geometry.Point([longitude, latitude])

# Optionally create a buffer around the point
area_of_interest = point.buffer(buffer_radius)

# Prepare functions for Sentinel-2 processing
def add_cloud_bands(img):
    cld_prb = ee.Image(img.get('s2cloudless')).select('probability')
    is_cloud = cld_prb.gt(75).rename('clouds')
    return img.addBands([cld_prb, is_cloud]) 

def add_shadow_bands(img):
    not_water = img.select('SCL').neq(6)
    dark_pixels = img.select('B8').lt(0.15 * 1e4).multiply(not_water).rename('dark_pixels')
    shadow_azimuth = ee.Number(90).subtract(ee.Number(img.get('MEAN_SOLAR_AZIMUTH_ANGLE')))
    cld_proj = img.select('clouds').directionalDistanceTransform(shadow_azimuth, 35) \
        .reproject({'crs': img.select(0).projection(), 'scale': 100}) \
        .rename('cloud_transform')
    shadows = cld_proj.multiply(dark_pixels).rename('shadows')
    return img.addBands([dark_pixels, cld_proj, shadows])


def add_cld_shdw_mask(img):
    img = add_cloud_bands(img)
    img = add_shadow_bands(img)
    is_cld_shdw = img.select('clouds').add(img.select('shadows')).gt(0)
    mask = is_cld_shdw.focalMin(2).focalMax(100).Not().rename('cloudmask')
    return img.addBands(mask)

def apply_cld_shdw_mask(img):
    return img.updateMask(img.select('cloudmask'))

# Function to process and download EVI time series
def download_evi_series(site_name, start_year, end_year):
    start_date = f"{start_year}-01-01"
    end_date = f"{end_year}-12-31"

    # Landsat 8 collection
    l8 = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2") \
        .filterBounds(area_of_interest) \
        .filterDate(start_date, end_date)

    # Sentinel-2 collections
    s2_sr_col = ee.ImageCollection('COPERNICUS/S2_SR') \
        .filterBounds(area_of_interest) \
        .filterDate(start_date, end_date) \
        .filter(ee.Filter.lte('CLOUDY_PIXEL_PERCENTAGE', 99))

    s2_cloudless_col = ee.ImageCollection('COPERNICUS/S2_CLOUD_PROBABILITY') \
        .filterBounds(area_of_interest) \
        .filterDate(start_date, end_date)

    # Join Sentinel-2 SR with cloud probability
    s2_joined = ee.ImageCollection(ee.Join.saveFirst('s2cloudless').apply(
        primary=s2_sr_col,
        secondary=s2_cloudless_col,
        condition=ee.Filter.equals(
            leftField='system:index',
            rightField='system:index'
        )
    ))

    s2_processed = s2_joined \
        .map(add_cld_shdw_mask) \
        .map(apply_cld_shdw_mask)

    # Merge Landsat 8 and Sentinel-2 collections
    hls = l8.merge(s2_processed)

    # Apply masks and calculate EVI
    hls = hls.map(lambda img: img.addBands(img.expression(
        '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))', {
            'NIR': img.select('nir'),
            'RED': img.select('red'),
            'BLUE': img.select('blue')
        }).rename('evi')))

    # Sample regions and export data
    hls_sampled = hls.select(['evi']).map(lambda img: img.sampleRegions(area_of_interest)).flatten()
    file_name = site_name.replace(' ', '').replace(',', '').replace('.', '').replace("'", '').replace('-', '').lower()
    task = ee.batch.Export.table.toDrive(
        collection=hls_sampled,
        description=f'{file_name}_evi_series',
        folder='phenology_mapping',
        fileNamePrefix=f'{file_name}_evi_series',
        fileFormat='CSV'
    )
    task.start()

# Execute function
download_evi_series(site_name, start_year, end_year)
