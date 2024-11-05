
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
                           modif_start_date = file.info(here::here("L0.csv"))$mtime,
                           reprocess = F){
  ### Load files ###
  files <- list.files(here::here("Raw_data","dropbox_downloads"), full.names = T)
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
    return(read_csv(here::here("L0.csv"), show_col_types = F))
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
      filter(is.na(LGR_Time) | !duplicated(LGR_Time) | 
               !duplicated(CH4d_ppm)) %>% #I've spent some time looking into this and there are some duplicated LGR rows
      select(TIMESTAMP, CH4d_ppm, CO2d_ppm, MIU_VALVE, GasT_C)
  } else {
    data_small2 <- data_raw %>%
      select(TIMESTAMP, CH4d_ppm, CO2d_ppm, MIU_VALVE, GasT_C)
  }
  rm(data_raw) #Save memory
  
  ### Calculate slopes ###
  #Prep data
  time_cutoff <- 300 #Cutoff to use when we have plenty of data (seconds)
  summer_time_cutoff <- 200 #Cutoff to use when there are < 35 measurements (seconds)
  filtered_data <- data_small %>%
    mutate(CH4d_ppm = as.numeric(CH4d_ppm),
           CO2d_ppm = as.numeric(CO2d_ppm),
           GasT_C = as.numeric(GasT_C),
           MIU_VALVE = as.numeric(MIU_VALVE)) %>%
    filter(!MIU_VALVE == 16,
           !is.na(MIU_VALVE)) %>%
    arrange(TIMESTAMP) %>%
    mutate(group = group_fun(MIU_VALVE)) %>%
    group_by(group, MIU_VALVE) %>%
    filter(sum(!is.na(CH4d_ppm)) > 0) %>%
    mutate(start = min(TIMESTAMP),
           change = as.numeric(difftime(TIMESTAMP, start, units = "days")),
           change_s = as.numeric(difftime(TIMESTAMP, start, units = "secs")),
           cutoff = ifelse(max(change_s) < 350, 
                           summer_time_cutoff,
                           time_cutoff),
           n = sum(change_s > cutoff & !is.na(CH4d_ppm)),
           max_s = change_s[which.max(CH4d_ppm)]) %>%
    filter(change_s > cutoff,
           change_s < 1000, #After ~15 min there is probably a problem
           n >= 5, #need at least 5 data points to calculate slope
           n < 200 #probably some issue if this many measurements are taken
    ) %>%
    #Issues during this window (concs > 100000 ppm)
    filter(TIMESTAMP > "2024-07-09 15:00:00" | TIMESTAMP < "2024-07-09 6:00:00")
  
  #Look at data to confirm index is okay
  data_small %>%
    head(20000) %>%
    mutate(group = group_fun(MIU_VALVE)) %>%
    group_by(group, MIU_VALVE) %>%
    mutate(start = min(TIMESTAMP),
           change_s = as.numeric(difftime(TIMESTAMP, start, units = "secs"))) %>%
    ggplot(aes(x = change_s, y = as.numeric(GasT_C), group = start)) +
    geom_line(alpha = 0.2)+
    facet_wrap(~MIU_VALVE)+
    theme(legend.position = "none")
  
  #Run lm
  slopes <- filtered_data %>%
    pivot_longer(c(CH4d_ppm, CO2d_ppm), names_to = "gas", values_to = "conc") %>%
    group_by(gas, group, MIU_VALVE) %>%
    filter(!is.na(conc)) %>%
    summarize(slope_ppm_per_day = lm(conc ~ change)$coefficients[[2]],
              Temp_init = first(GasT_C),
              slope_umol_per_day = slope_ppm_per_day * 265.8 / (0.08206*(Temp_init + 273)),
              R2 = summary(lm(conc ~ change))$r.squared,
              p = summary(lm(conc ~ change))$coefficients[,4][2],
              rmse = sqrt(mean(lm(conc ~ change)$residuals^2)/n()),
              init = first(conc),
              max_s = unique(max_s),
              TIMESTAMP = unique(start),
              n = unique(n),
              .groups = "drop") %>%
    mutate(gas = ifelse(gas == "CH4d_ppm", "CH4", "CO2")) %>%
    pivot_wider(names_from = gas, 
                values_from = c(slope_ppm_per_day, slope_umol_per_day, R2, p, rmse, init),
                names_glue = "{gas}_{.value}")
  
  for(year_i in unique(year(slopes$TIMESTAMP))){
    p <- slopes %>%
      mutate(MIU_VALVE = factor(MIU_VALVE, 
                                levels = c(1,4,7,10,
                                           3,6,9,12,
                                           2,5,8,11))) %>%
      filter(year(TIMESTAMP) == year_i) %>%
      ggplot(aes(x = TIMESTAMP, y = max_s)) +
      geom_point(alpha = 0.02)+
      facet_wrap(~MIU_VALVE)+
      ggtitle(year_i)+
      xlab("Date")+
      ylab("Time to peak (s)")+
      theme_bw()
    jpeg(here::here("figures", paste0("TimeToPeak_", year_i, ".jpeg")), width = 6, height = 5, units = "in", res = 300)
    print(p)
    dev.off()
  }
  
  #Load previously calculated slopes
  if(!reprocess){
    old_slopes <- read_csv(here::here("L0.csv"), show_col_types = F) %>%
      filter(!TIMESTAMP %in% slopes$TIMESTAMP)
    slopes_comb <- bind_rows(old_slopes, slopes)
  } else {
    slopes_comb <- slopes
  }
  
  #Output
  write.csv(slopes_comb, here::here("L0.csv"), row.names = FALSE)
  round_comb <- function(x){round(as.numeric(x), 2)}
  write.csv(data_small %>%
              mutate(across(c(CO2d_ppm, GasT_C), round_comb)),
            here::here("processed_data","raw_small.csv"), row.names = FALSE)
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
