PEPRMT_final_sys_CO2_GPP_param <-  function(theta,data) {
#This model predicts GPP using a light use efficiency equation
#GPP can be predicted using LAI or a greeness index from Phenocam data or remote sensing such as NDVI
  
#Note: some exogenous variables are not used however it is easier to keep this same input variable stucture for all PEPRMT codes

#data <- RR_2016_master$data[[1]]
  if (  !is.data.frame(data)) { #If it's not a data frame make it data frame
    data  <-as.data.frame(data) 
    }
    
#Exogenous Variables
Time_2 <- data[,1] #day of year variable continuous
DOY_disc_2 <- data[,2] #day of year variable discontinuous, starts over again at beginning of ea year
TA_2 <- data[,3] #daily ave (C)
#WT_2 <- data[,4] #water table height (cm)
PAR_2 <- data[,5] #photosynthetically active radiation (umol m-2 d-1)
LAI_2 <- data[,6] #Leaf area index
#GPP_2 <- data[,7] #gpp modeled--g C m-2 d-1 (negative for uptake)
GI_2 <- data[,8] #greeness index from Phenocam or Landsat etc
#Season_drop_2 <- data[,9] 
  #Season variable that is set to 1 in winter (DOY 1-88, 336-365),
  #                               2 pre-spring (DOY 89-175),
  #                               3 spring (DOY 176-205),
  #                               4 summer (DOY 206-265),
  #                               5 fall (DOY 266-335)
#wc_90CI_2 <- data[,10] # 90% confidence interval around NEE determined by gapfilling error and random error
#wetland_age_2 <- data[,11] #age in years of wetland; only important if wetland is >4yrs old, otherwise just set to 10
FPAR <- data[,12] #If using LAI data, set FPAR variable to 1's, if using a greeness index set FPAR to 0's
LUE<- data[,19] #growing season LUE computed for each site using measured GPP in gC per umol

#FIRST COMPUTE GPP################################
#PARAMETERS
Ha <- theta[3]+30 #default=30;#activation energy for general crop plant (KJ mol-1)
Hd <- theta[4]+100 #default=100;(KJ mol-1)
#CONSTANTS
#LUE_mean <- theta[5] #0.9 #computed a mean across each growing season (g C MJ-1)
#Running 2000-closed shrubland epsilon=0.9; crop=0.6; deci broadleaf forest=1.0
vcopt <- 1.0
R_t <- 0.00831 #KJ mol-1 K-1
T_opt <- 25 + 274.15 #(K); our Temp opt for Ps is 25C
  
#EQUATIONS
#PAR_2_MJ <- (PAR_2/4.57) #convert PAR umol m-2 d-1 to MJ m-2 d-1 (Thimijan & Heins, 1983)

#Decide how to compute fPAR
#If Fpar = 1 then calculate FPAR using LAI if FPAR =0 then calculate using GI 
if (sum(FPAR)>0) {
  k <- 0 +0.8 #0.8 range=0-1
  fPAR_2 <- 0.95*(1-exp(-k*LAI_2)) #for an LAI=4.9, fpar=0.87--Yuan 2007
  fPAR_2 <- (fPAR_2/2)*10^4
} else {
  fPAR_2 <- theta[1]+theta[2]*GI_2 #for an LAI=4.9, fpar=0.87--Yuan 2007
 # fPAR_2 <- theta[1]+ GI_2 #for an LAI=4.9, fpar=0.87--Yuan 2007
}

#b/c we have so much dead veg at WP that is not included in LAI, our k is
#high b/c not much light reaches the floor (k=0.8)

APAR_2 <- fPAR_2*PAR_2 #umol m-2 daily average
  
AirT_K <- TA_2 + 274.15 #C to Kelvin

max_time <- length(TA_2)
vct <- vector("numeric",length(Time_2))
NPP_FPAR_T <- vector("numeric",length(Time_2))
#wetland_age_corr <- vector("numeric",length(Time_2))

for (t in 1:max_time) {
  #wetland_age_corr(t)=age_param/wetland_age_2(t)*wetland_age_2(t);
  
  exponent1 <- (Ha*(AirT_K[t]-T_opt))/(AirT_K[t]*R_t*T_opt)
  exponent2 <- (Hd*(AirT_K[t]-T_opt))/(AirT_K[t]*R_t*T_opt)
  top <- Hd*exp(exponent1)  
  bottom <- Hd-(Ha*(1-exp(exponent2)))
  vct[t] = vcopt*(top/bottom)
  #   if (Season[t] < 2%fPAR_2[t]<0.43% of winter threshold, when GI drops to winter levels turn off Ps)
  #       vct(t)=0.15
  #   }
  NPP_FPAR_T[t] <- ((vct[t]*(APAR_2[t]*LUE[t]))) #umol m-2 d-1* gC/umol == g C m-2 d-1


}

#GPP <- (NPP_FPAR_T/12)*10^6*-1 #go back to umol m-2 s-1
GPP <- (NPP_FPAR_T)*-1 #stay as g C m-2 d-1 where negative # mean uptake
# GPP <- NPP_FPAR_T #stay as g C m-2 d-1 where negative # mean uptake
# return(GPP)
}

