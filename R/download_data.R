
#Source
source("./R/drop_dir.R")
source("./R/get_dropbox_token.R")

#Load data
files <- drop_dir(path = "GCREW_LOGGERNET_DATA")
relevant_files <- files %>%
  filter(grepl("GENX_INSTRUMENT_FLUX_COMB", name))

all_data <- relevant_files 

url <- "https://content.dropboxapi.com/2/files/download"

req <- httr::POST(
  url = url,
  httr::config(token = get_dropbox_token()),
  httr::add_headers("Dropbox-API-Arg" = jsonlite::toJSON(
    list(
      path = "/GCREW_LOGGERNET_DATA/GENX_INSTRUMENT_FLUX_COMB_20241009025358.dat"
    ),
    auto_unbox = TRUE
  )),
  httr::write_disk("./Raw_data/test.dat", overwrite = T)
)

data <- read_csv("https://www.dropbox.com/scl/fi/ce53uimlnducpgx4cflvh/GENX_INSTRUMENT_FLUX_COMB.dat?rlkey=543byau1gcwwfus8ylinrgsfb&st=2ppyowpl&dl=1", skip = 1) %>%
  filter(!is.na(TIMESTAMP),
         TIMESTAMP != "TS") %>%
  mutate(TIMESTAMP = as_datetime(TIMESTAMP))

data2 <- read_csv("https://www.dropbox.com/scl/fi/64vrg3pbwzgr0fae9amel/GENX_INSTRUMENT_FLUX_COMB_20241009025358.dat?rlkey=hcnkcghvblke4nbj8b8v8kaud&st=uidx2v07&dl=1", skip = 1) %>%
  filter(!is.na(TIMESTAMP),
         TIMESTAMP != "TS") %>%
  mutate(TIMESTAMP = as_datetime(TIMESTAMP))

#file in current folder
"https://www.dropbox.com/scl/fi/ce53uimlnducpgx4cflvh/GENX_INSTRUMENT_FLUX_COMB.dat?rlkey=543byau1gcwwfus8ylinrgsfb&st=n258tg8x&dl=0"
#Larger folder
"https://www.dropbox.com/scl/fo/9vn1x2dqpqxcndpv1jbu0/ANiYNxLB2mE28AIvGIChkFw?rlkey=da4ul7yzmndjfzlz20tdgbjmm&st=8teiwstw&dl=0"
#Example file in larger folder
"https://www.dropbox.com/scl/fi/64vrg3pbwzgr0fae9amel/GENX_INSTRUMENT_FLUX_COMB_20241009025358.dat?rlkey=hcnkcghvblke4nbj8b8v8kaud&st=uidx2v07&dl=0"

#data <- read_csv("Raw_data/GENX_INSTRUMENT_FLUX_LGR1.dat", skip = 1) %>%
#  filter(!is.na(TIMESTAMP),
#         TIMESTAMP != "TS") %>%
#  mutate(TIMESTAMP = as_datetime(TIMESTAMP))

#Create function to assign groups for separate readings
group_fun <- function(MIU_VALVE) {
  group <- rep(1, length(MIU_VALVE))
  for (i in 2:length(MIU_VALVE)) {
    if(MIU_VALVE[i] == MIU_VALVE[i - 1]) {
      group[i] <- group[i - 1]
    } else {
      group[i] <- group[i - 1] + 1
    }
  }
  return(group)
}

#Calculate slopes
slopes <- data %>%
  filter(!MIU_VALVE == 16) %>%
  mutate(group = group_fun(MIU_VALVE)) %>%
  group_by(group, MIU_VALVE) %>%
  mutate(start = min(TIMESTAMP),
         change = as.numeric(difftime(TIMESTAMP, start, units = "days")),
         index = row_number()) %>%
  filter(index > 35) %>%
  summarize(CH4_slope_ppm_per_day = lm(CH4_ppm ~ change)$coefficients[[2]],
            air_temp = mean(GasT_C, na.rm = T),
            CH4_slope_umol_per_day = CH4_slope_ppm_per_day * 265.8 / (0.08206*(air_temp + 273)),
            CH4_R2 = summary(lm(CH4_ppm ~ change))$r.squared,
            #CO2_slope = lm(CO2_ppm ~ change)$coefficients[[2]],
            #CO2_R2 = summary(lm(CO2_ppm ~ change))$r.squared,
            n = n(),
            CH4_init = first(CH4_ppm))

write.csv(slopes, "L0.csv", row.names = FALSE)


#Optimize removed data
calc_fits <- function(i){
  data3 <- data %>%
    filter(!MIU_VALVE == 16) %>%
    mutate(group = group_fun(MIU_VALVE)) %>%
    group_by(group, MIU_VALVE) %>%
    mutate(start = min(TIMESTAMP),
           change = as.numeric(difftime(TIMESTAMP, start, units = "days")),
           index = row_number()) %>%
    filter(index > i) %>%
    summarize(CH4_R2 = summary(lm(CH4_ppm ~ change))$r.squared,
              CH4_mse = mean(lm(CH4_ppm ~ change)$residuals^2)/n(),
              #CO2_R2 = summary(lm(CO2_ppm ~ change))$r.squared
              )
  
  out <- data3 %>%
    group_by(MIU_VALVE) %>%
    summarize(CH4_R2 = mean(CH4_R2),
              CH4_mse = mean(CH4_mse),
              #CO2_R2 = median(CO2_R2)
              ) %>%
    mutate(index = i)
}

fits <- map(15:35, calc_fits) |> 
  bind_rows() 

best_fits <- fits %>%
  ungroup() %>%
  filter(!is.na(CH4_mse)) %>%
  pivot_longer(cols = c(CH4_mse
                        #, CO2_R2
                        ),
               names_to = "variable", values_to = "value") %>%
  group_by(MIU_VALVE, variable) %>%
  filter(value == min(value))
#Most of these look okay, but chamber 5 is optimizing to include the peak. 
#Might get better with more than 3 years of data

#Look at data.
data %>%
  filter(!MIU_VALVE == 16) %>%
  mutate(group = group_fun(MIU_VALVE)) %>%
  group_by(group, MIU_VALVE) %>%
  mutate(start = min(TIMESTAMP),
         change = as.numeric(difftime(TIMESTAMP, start, units = "days")),
         index = row_number()) %>%
  ggplot(aes(x = index, y = CH4_ppm, group = start)) +
  geom_line(alpha = 0.2)+
  facet_wrap(~MIU_VALVE)+
  theme(legend.position = "none")

data2 %>%
  filter(CH4_init < 2.5) %>%
  ggplot(aes(x = CH4_init, y = CH4_slope, color = CH4_R2)) +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept = 2.3)

data2 %>%
  ggplot(aes(x = CH4_init))+
  geom_density()+
  geom_vline(xintercept = 2.3)

data_grouped <- data %>%
  filter(!MIU_VALVE == 16) %>%
  mutate(group = group_fun(MIU_VALVE)) %>%
  group_by(group, MIU_VALVE) %>%
  mutate(start = min(TIMESTAMP),
         change = as.numeric(difftime(TIMESTAMP, start, units = "days")),
         index = row_number())

data_check <- data_grouped %>%
  filter(group == 218)

data_check %>%
  ggplot(aes(x = index, y = CH4_ppm, group = start)) +
  geom_point()

p$sigma
