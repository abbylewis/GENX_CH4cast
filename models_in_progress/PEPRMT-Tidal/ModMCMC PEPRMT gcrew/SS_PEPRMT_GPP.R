##########################################
##        PEPRMT CH4 SS script          ##
##########################################
# March 24th,2021
#Written by Michael Najarro

# Introduction:
# This is a translation of a script written in
# Matlab by Patty Oikawa in 2019, for operation
# in R

# Goal:
# to assess the model sensitivity of PEPRMT Ch4
# component by calculating log likelihood estimates

#note that the negative log likelihood function is
# based on Gaussian prob distribution.

#~~Begin~~
ss_GPP <- function(theta,
               y){
 
  #1. call upon libraries
  library(tidyverse)
  
  #2. Load your ydata in then calculate vectors
  # of error CH4 exchange obs daily integral.
  # Note that y represents the path directory
  # to the file of interest.
  #y_core <- read.csv(y)
  #ydata <-  c(y_core[,1])
  ydata <- y[,1]
  #ydata <-  c(y2[,1])
  
  #daily integral gap-filling error pre-loaded
  gapfill_error <- y[,2]
  #gapfill_error <- c(y2[,2])
  
  #daily integral random error pre-loaded umol CH4 m-2 d-1
  random_error <- y[,3]
  #random_error <- c(y2[,3])
  
  #3. import observed eddy covariance tower data 
  #data2 <- read.csv(file = "./data_sets/EL_dataprepared_CH4_SI.csv")
  #data2 <- read.csv(file = "./data_sets/revised_EL_CH4_prepared_data.csv")
  #data2 <- read.csv(file = "./data_sets/revised_EL_CH4_prepared_data2.csv")
  #data2<- readRDS("./data_sets/EL4-20-21.rds") 
  #data2 <- data2 %>%
  #  select(-X1)
  data2<- readRDS("EL_RR_De_data.rds")
  
  
  #4. load PEPRMT from file
  source('PEPRMT_final_sys_CO2_GPP_param.R')
  #source('./PEPRMT_Scripts/PEPRMT_CH4_sulfate_inhibition.R')
  
  #5. execute PEPRMT CH4
  y_model <- PEPRMT_final_sys_CO2_GPP_param(theta,
                            data2)
  
  #6. calculate sample size of non nan observations
  nan_obs_m <- sum(c(as.numeric(is.na(y_model))))
  n <- length(y_model) - nan_obs_m
  
  #7. apply simple least squares optimization 
  # according to Keenan 2011 and 2012
  ss1 <- ((ydata - y_model)/(random_error + gapfill_error))^2

  #Put extra weight on pulses
  ss2<- ss1
  ydata_ave <- mean(ydata, na.rm = TRUE)

  for(i in 1:length(ydata)){
    if(ydata[i] > 4*ydata_ave){ss2[i] = ss1[i]*50}
    else {ss2[i] = ss1[i]}
    }
  
  ss = (sum(ss2,na.rm = TRUE))/n
  return(ss)
}