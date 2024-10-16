
#' calculate_flux
#'
#' @description
#' This function calculates the raw CH4 fluxes for all files in the dropbox_downloads folder
#'
#' @param start_date earliest file to process (based on file name)
#' @param end_date latest file to process
#' @param modif_start_date only run files that have been modified/created since this date
#'
#' @return L0 slopes

calculate_flux <- function(start_date = NULL,
                           end_date = NULL,
                           modif_start_date = file.info("L0.csv")$mtime){
  ### Load files ###
  files <- list.files("./Raw_data/dropbox_downloads", full.names = T)
  #By default, only calculate slopes for files that have been modified/created since the last time we ran the script
  if(!is.null(modif_start_date)){
    files <- files[file.info(files)$mtime > modif_start_date]
  }
  #If a start and end date are provided, look for files that match these dates
  if(!is.null(start_date) & !is.null(end_date)){
    possible_file_names <- seq(as.Date(start_date), 
                               as.Date(end_date), 
                               by = "1 day") %>%
      format("%Y%m%d")
    files <- files[grepl(paste0(possible_file_names, collapse = "|"), files)]
  } else if (!is.null(start_date) | !is.null(end_date)){
    stop("If you provide a start or end date, you must provide both")
  } 
  
  if(length(files) == 0){
    message("No files to process")
    return(read_csv("L0.csv", show_col_types = F))
  }
  
  message(paste0("Calculating fluxes for ", length(files), " files"))
  
  #Load data
  data_raw <- files %>%
    map(read_csv, col_types = cols(.default = "c"), skip = 1)  %>%
    bind_rows() %>%
    filter(!TIMESTAMP == "TS") %>%
    mutate(TIMESTAMP = as_datetime(TIMESTAMP)) %>%
    filter(!is.na(TIMESTAMP)) %>%
    distinct() 
  
  #Account for different formatting among files
  if("LGR_Time" %in% colnames(data_raw)){
    data_small <- data_raw %>%
      filter(is.na(LGR_Time) |
               !duplicated(LGR_Time) | 
               !duplicated(CH4_ppm)) %>% #I've spent some time looking into this and there are some duplicated LGR rows
      select(TIMESTAMP, CH4_ppm, CO2_ppm, MIU_VALVE, GasT_C)
  } else {
    data_small <- data_raw %>%
      mutate(CO2_ppm = NA) %>%
      select(TIMESTAMP, CH4_ppm, CO2_ppm, MIU_VALVE, GasT_C)
  }
  rm(data_raw) #Save memory
  
  ### Calculate slopes ###
  #Prep data
  index_cutoff <- 30 #Cutoff to use when we have plenty of data
  summer_index_cutoff <- 15 #Cutoff to use when there are < 35 measurements
  filtered_data <- data_small %>%
    mutate(CH4_ppm = as.numeric(CH4_ppm),
           CO2_ppm = as.numeric(CO2_ppm),
           GasT_C = as.numeric(GasT_C),
           MIU_VALVE = as.numeric(MIU_VALVE)) %>%
    filter(!MIU_VALVE == 16,
           !is.na(MIU_VALVE)) %>%
    arrange(TIMESTAMP) %>%
    mutate(group = group_fun(MIU_VALVE)) %>%
    group_by(group, MIU_VALVE) %>%
    mutate(start = min(TIMESTAMP),
           change = as.numeric(difftime(TIMESTAMP, start, units = "days")),
           index = row_number(),
           cutoff = ifelse(max(index) < 35, 
                           summer_index_cutoff,
                           index_cutoff),
           n = sum(index > cutoff & !is.na(CH4_ppm))) %>%
    filter(index > cutoff,
           n >= 5, #need at least 5 data points to calculate slope
           n < 200 #probably some issue if this many measurements are taken
    ) %>%
    #Issues during this window (concs > 100000 ppm)
    filter(TIMESTAMP > "2024-07-09 15:00:00" | TIMESTAMP < "2024-07-09 6:00:00")
  
  #Look at data to confirm index is okay
  filtered_data %>%
    ggplot(aes(x = index, y = CH4_ppm, group = start)) +
    geom_line(alpha = 0.2)+
    facet_wrap(~MIU_VALVE)+
    theme(legend.position = "none")
  
  #Run lm
  slopes <- filtered_data %>%
    summarize(CH4_slope_ppm_per_day = lm(CH4_ppm ~ change)$coefficients[[2]],
              air_temp = mean(GasT_C, na.rm = T),
              CH4_slope_umol_per_day = CH4_slope_ppm_per_day * 265.8 / (0.08206*(air_temp + 273)),
              CH4_R2 = summary(lm(CH4_ppm ~ change))$r.squared,
              CH4_p = summary(lm(CH4_ppm ~ change))$coefficients[,4][2],
              CH4_rmse = sqrt(mean(lm(CH4_ppm ~ change)$residuals^2)/n()),
              #CO2_slope = lm(CO2_ppm ~ change)$coefficients[[2]],
              #CO2_R2 = summary(lm(CO2_ppm ~ change))$r.squared,
              CH4_init = first(CH4_ppm),
              TIMESTAMP = unique(start),
              n = unique(n),
              .groups = "drop")
  
  #Load previously calculated slopes
  old_slopes <- read_csv("L0.csv", show_col_types = F) %>%
    filter(!TIMESTAMP %in% slopes$TIMESTAMP)
  slopes_comb <- bind_rows(old_slopes, slopes)
  
  #Output
  write.csv(slopes_comb, "L0.csv", row.names = FALSE)
  return(slopes_comb)
}

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
