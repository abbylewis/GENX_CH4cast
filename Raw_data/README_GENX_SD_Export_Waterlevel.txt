Methods used to generate GENX data files (GENX_SD_Export_Waterlevel):

These data describe environmental conditions at GENX, located at the Smithsonian Environmental Research Center Global Change Research Wetlands (GCREW).

Instrumentation and sensors:
CO2 was measured using K30 sensors. 
Air temperature, air pressure, relative humidity, and volatile organic compounds were measured using Adafruit BME680s. 
Soil temperature was measured using thermistors at 10 cm depth near heating cable pins, 10 cm depth within experimental chambers, 25 cm depth within experimental chambers, and 90 cm depth outside of experimental chambers.
Water level and associated variables (e.g., conductivity, resistivity, salinity) were collected using In-Situ AquaTROLL 200.
Day/night variables were calculated using the suncalc package in R.
Data is collected on SD cards using Arduino Mega 2560.
Data is also automatically logged on a CR1000 and sent to LoggerNet.

Brief description of dataset creation and code:
The dataset was assembled using R v4.2.1.
The dataset consists of 4 parts: 
1) CO2, BME, and thermistor data from SD cards, collected every ~10s (aggregated to 15 min data)
2) CO2 data, BME data, thermistor data, and heating treatment checks from LoggerNet, which collects data automatically every minute (aggregated to 15 min data)
3) Water level data from LoggerNet, collected every 15 min
4) Derived variables describing temperature across treatments, difference from ambient, etc.

Range limitations were applied to CO2, BME, and soil temperature data to eliminate erroneous observations. 
We also cleaned soil temperature data using rolling standard deviations. These variables were also manually cleaned and visualized in JMP 16 afterwards.
We joined all datasets and created derived variables. Joins between SD data and LoggerNet data were full joins, so there may be NAs in one dataset for some timestamps.
Some water level data was missing from GENX, so we took water level data from other sites at GCREW.

The code can be found at the "GENX_share" GitHub repository: https://github.com/Smithsonian/GENX_share.git