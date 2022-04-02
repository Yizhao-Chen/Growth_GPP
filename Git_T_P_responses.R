#The script is to process and plot the T&P response for GPP&growth outputs for Global synthesis
library("dplyr")
library("ggplot2")

Response_GPP_TRENDY <- read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_TRENDY_S2S3_mean_temp_prep_toshp.csv")

Response_growth <- read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_per_area_nbr_ndvi_mean_temp_prep_pcorr_toshp.csv")
#per tree
Response_growth_pt <- read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_per_tree_temp_prep_pcorr_tras_processed_toshp.csv")

#Get the z-score
#Calculate mean
T_mean <- mean(Response_GPP$Temp_4_9)
P_mean <- mean(Response_GPP$Prep_4_9)

#for per tree
T_mean1 <- mean(Response_growth_pt$Temp_4_9)
P_mean1 <- mean(Response_growth_pt$Prep_4_9)

#Calculate sd
T_sd <- sd(Response_GPP$Temp_4_9)
P_sd <- sd(Response_GPP$Prep_4_9)

#for per tree
T_sd1 <- sd(Response_growth_pt$Temp_4_9)
P_sd1 <- sd(Response_growth_pt$Prep_4_9)

#Calculate z-score

Response_GPP$Temp_zscore <- (Response_GPP$Temp_4_9 - T_mean) / T_sd
Response_GPP$Prep_zscore <- (Response_GPP$Prep_4_9 - P_mean) / P_sd 

Response_growth$Temp_zscore <- (Response_growth$Temp_4_9 - T_mean) / T_sd
Response_growth$Prep_zscore <- (Response_growth$Prep_4_9 - P_mean) / P_sd 

Response_growth_pt$Temp_zscore <- (Response_growth_pt$Temp_4_9 - T_mean1) / T_sd1
Response_growth_pt$Prep_zscore <- (Response_growth_pt$Prep_4_9 - P_mean1) / P_sd1 

#Group data using Temp&Prep zscore 4 groups
{
#Warm & Wet
Response_GPP$Zone[Response_GPP$Temp_zscore >= 0 & Response_GPP$Prep_zscore >= 0] <- "WW"
Response_growth$Zone[Response_growth$Temp_zscore >= 0 & Response_growth$Prep_zscore >= 0] <- "WW"
Response_growth_pt$Zone[Response_growth_pt$Temp_zscore >= 0 & Response_growth_pt$Prep_zscore >= 0] <- "WW"
#Warm & Dry
Response_GPP$Zone[Response_GPP$Temp_zscore >= 0 & Response_GPP$Prep_zscore < 0] <- "WD"
Response_growth$Zone[Response_growth$Temp_zscore >= 0 & Response_growth$Prep_zscore < 0] <- "WD"
Response_growth_pt$Zone[Response_growth_pt$Temp_zscore >= 0 & Response_growth_pt$Prep_zscore < 0] <- "WD"
#Cool & Wet
Response_GPP$Zone[Response_GPP$Temp_zscore < 0 & Response_GPP$Prep_zscore >= 0] <- "CW"
Response_growth$Zone[Response_growth$Temp_zscore < 0 & Response_growth$Prep_zscore >= 0] <- "CW"
Response_growth_pt$Zone[Response_growth_pt$Temp_zscore < 0 & Response_growth_pt$Prep_zscore >= 0] <- "CW"
#Cool & Dry
Response_GPP$Zone[Response_GPP$Temp_zscore < 0 & Response_GPP$Prep_zscore < 0] <- "CD"
Response_growth$Zone[Response_growth$Temp_zscore < 0 & Response_growth$Prep_zscore < 0] <- "CD"
Response_growth_pt$Zone[Response_growth_pt$Temp_zscore < 0 & Response_growth_pt$Prep_zscore < 0] <- "CD"
}

#Group data using Temp&Prep zscore 16 groups
{
  #Warm & Wet
  #GPP
  Response_GPP$Zone1[Response_GPP$Temp_zscore >= 1 & Response_GPP$Prep_zscore >= 1] <- "WW4"
  Response_GPP$Zone1[Response_GPP$Temp_zscore >= 0 & Response_GPP$Temp_zscore < 1 & Response_GPP$Prep_zscore >= 1] <- "WW3"
  Response_GPP$Zone1[Response_GPP$Temp_zscore >= 1 & Response_GPP$Prep_zscore >= 0  & Response_GPP$Prep_zscore <1] <- "WW2"
  Response_GPP$Zone1[Response_GPP$Temp_zscore >= 0 & Response_GPP$Temp_zscore < 1 & Response_GPP$Prep_zscore >= 0  & Response_GPP$Prep_zscore <1] <- "WW1"
  
  #growth
  Response_growth$Zone1[Response_growth$Temp_zscore >= 1 & Response_growth$Prep_zscore >= 1] <- "WW4"
  Response_growth$Zone1[Response_growth$Temp_zscore >= 0 & Response_growth$Temp_zscore < 1 & Response_growth$Prep_zscore >= 1] <- "WW3"
  Response_growth$Zone1[Response_growth$Temp_zscore >= 1 & Response_growth$Prep_zscore >= 0  & Response_growth$Prep_zscore <1] <- "WW2"
  Response_growth$Zone1[Response_growth$Temp_zscore >= 0 & Response_growth$Temp_zscore < 1 & Response_growth$Prep_zscore >= 0  & Response_growth$Prep_zscore <1] <- "WW1" 
  #growth_pt
  Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore >= 1 & Response_growth_pt$Prep_zscore >= 1] <- "WW4"
  Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore >= 0 & Response_growth_pt$Temp_zscore < 1 & Response_growth_pt$Prep_zscore >= 1] <- "WW3"
  Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore >= 1 & Response_growth_pt$Prep_zscore >= 0  & Response_growth_pt$Prep_zscore <1] <- "WW2"
  Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore >= 0 & Response_growth_pt$Temp_zscore < 1 & Response_growth_pt$Prep_zscore >= 0  & Response_growth_pt$Prep_zscore <1] <- "WW1"
  
  #Warm & Dry
  #GPP
  Response_GPP$Zone1[Response_GPP$Temp_zscore >= 1 & Response_GPP$Prep_zscore < -1] <- "WD4"
  Response_GPP$Zone1[Response_GPP$Temp_zscore >= 0 & Response_GPP$Temp_zscore < 1 & Response_GPP$Prep_zscore < -1] <- "WD3"
  Response_GPP$Zone1[Response_GPP$Temp_zscore >= 1 & Response_GPP$Prep_zscore >= -1 & Response_GPP$Prep_zscore < 0] <- "WD2"
  Response_GPP$Zone1[Response_GPP$Temp_zscore >= 0 & Response_GPP$Temp_zscore < 1 & Response_GPP$Prep_zscore >= -1 & Response_GPP$Prep_zscore < 0] <- "WD1"
  #growth
  Response_growth$Zone1[Response_growth$Temp_zscore >= 1 & Response_growth$Prep_zscore < -1] <- "WD4"
  Response_growth$Zone1[Response_growth$Temp_zscore >= 0 & Response_growth$Temp_zscore < 1 & Response_growth$Prep_zscore < -1] <- "WD3"
  Response_growth$Zone1[Response_growth$Temp_zscore >= 1 & Response_growth$Prep_zscore >= -1 & Response_growth$Prep_zscore < 0] <- "WD2"
  Response_growth$Zone1[Response_growth$Temp_zscore >= 0 & Response_growth$Temp_zscore < 1 & Response_growth$Prep_zscore >= -1 & Response_growth$Prep_zscore < 0] <- "WD1"
  #growth_pt
  Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore >= 1 & Response_growth_pt$Prep_zscore < -1] <- "WD4"
  Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore >= 0 & Response_growth_pt$Temp_zscore < 1 & Response_growth_pt$Prep_zscore < -1] <- "WD3"
  Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore >= 1 & Response_growth_pt$Prep_zscore >= -1 & Response_growth_pt$Prep_zscore < 0] <- "WD2"
  Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore >= 0 & Response_growth_pt$Temp_zscore < 1 & Response_growth_pt$Prep_zscore >= -1 & Response_growth_pt$Prep_zscore < 0] <- "WD1"
  
  #Cool & Wet
  Response_GPP$Zone1[Response_GPP$Temp_zscore < -1 & Response_GPP$Prep_zscore >= 1] <- "CW4"
  Response_GPP$Zone1[Response_GPP$Temp_zscore < 0 & Response_GPP$Temp_zscore >= -1 & Response_GPP$Prep_zscore >= 1] <- "CW3"
  Response_GPP$Zone1[Response_GPP$Temp_zscore < -1 & Response_GPP$Prep_zscore < 1 & Response_GPP$Prep_zscore >= 0] <- "CW2"
  Response_GPP$Zone1[Response_GPP$Temp_zscore < 0 & Response_GPP$Temp_zscore >= -1 & Response_GPP$Prep_zscore < 1 & Response_GPP$Prep_zscore >= 0] <- "CW1"
  #growth
  Response_growth$Zone1[Response_growth$Temp_zscore < -1 & Response_growth$Prep_zscore >= 1] <- "CW4"
  Response_growth$Zone1[Response_growth$Temp_zscore < 0 & Response_growth$Temp_zscore >= -1 & Response_growth$Prep_zscore >= 1] <- "CW3"
  Response_growth$Zone1[Response_growth$Temp_zscore < -1 & Response_growth$Prep_zscore < 1 & Response_growth$Prep_zscore >= 0] <- "CW2"
  Response_growth$Zone1[Response_growth$Temp_zscore < 0 & Response_growth$Temp_zscore >= -1 & Response_growth$Prep_zscore < 1 & Response_growth$Prep_zscore >= 0] <- "CW1"
  #growth_pt
  Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore < -1 & Response_growth_pt$Prep_zscore >= 1] <- "CW4"
  Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore < 0 & Response_growth_pt$Temp_zscore >= -1 & Response_growth_pt$Prep_zscore >= 1] <- "CW3"
  Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore < -1 & Response_growth_pt$Prep_zscore < 1 & Response_growth_pt$Prep_zscore >= 0] <- "CW2"
  Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore < 0 & Response_growth_pt$Temp_zscore >= -1 & Response_growth_pt$Prep_zscore < 1 & Response_growth_pt$Prep_zscore >= 0] <- "CW1"
  
  #Cool & Dry
  Response_GPP$Zone1[Response_GPP$Temp_zscore < -1 & Response_GPP$Prep_zscore < -1] <- "CD4"
  Response_GPP$Zone1[Response_GPP$Temp_zscore < 0 & Response_GPP$Temp_zscore >= -1 & Response_GPP$Prep_zscore < -1] <- "CD3" 
  Response_GPP$Zone1[Response_GPP$Temp_zscore < -1 & Response_GPP$Prep_zscore < 0 & Response_GPP$Prep_zscore >= -1] <- "CD2"
  Response_GPP$Zone1[Response_GPP$Temp_zscore < 0 & Response_GPP$Temp_zscore >= -1 & Response_GPP$Prep_zscore < 0 & Response_GPP$Prep_zscore >= -1] <- "CD1" 
  #growth
  Response_growth$Zone1[Response_growth$Temp_zscore < -1 & Response_growth$Prep_zscore < -1] <- "CD4"
  Response_growth$Zone1[Response_growth$Temp_zscore < 0 & Response_growth$Temp_zscore >= -1 & Response_growth$Prep_zscore < -1] <- "CD3" 
  Response_growth$Zone1[Response_growth$Temp_zscore < -1 & Response_growth$Prep_zscore < 0 & Response_growth$Prep_zscore >= -1] <- "CD2"
  Response_growth$Zone1[Response_growth$Temp_zscore < 0 & Response_growth$Temp_zscore >= -1 & Response_growth$Prep_zscore < 0 & Response_growth$Prep_zscore >= -1] <- "CD1" 
  #growth_pt
  Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore < -1 & Response_growth_pt$Prep_zscore < -1] <- "CD4"
  Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore < 0 & Response_growth_pt$Temp_zscore >= -1 & Response_growth_pt$Prep_zscore < -1] <- "CD3" 
  Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore < -1 & Response_growth_pt$Prep_zscore < 0 & Response_growth_pt$Prep_zscore >= -1] <- "CD2"
  Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore < 0 & Response_growth_pt$Temp_zscore >= -1 & Response_growth_pt$Prep_zscore < 0 & Response_growth_pt$Prep_zscore >= -1] <- "CD1" 
  }

#Group data based on zone 4 groups
{
  WW_GPP_df = filter(Response_GPP,Zone == "WW") 
  WW_growth_df = filter(Response_growth,Zone == "WW") 
  WW_growth_df = filter(Response_growth_pt,Zone == "WW") 
  
  WD_GPP_df = filter(Response_GPP,Zone == "WD") 
  WD_growth_df = filter(Response_growth,Zone == "WD") 
  WD_growth_df = filter(Response_growth_pt,Zone == "WD")   
  
  CW_GPP_df = filter(Response_GPP,Zone == "CW") 
  CW_growth_df = filter(Response_growth,Zone == "CW") 
  CW_growth_df = filter(Response_growth_pt,Zone == "CW") 
  
  CD_GPP_df = filter(Response_GPP,Zone == "CD") 
  CD_growth_df = filter(Response_growth,Zone == "CD") 
  CD_growth_df = filter(Response_growth_pt,Zone == "CD")
}
#16 groups
{
  WW4_GPP_df = filter(Response_GPP,Zone1 == "WW4") 
  WW3_GPP_df = filter(Response_GPP,Zone1 == "WW3")
  WW2_GPP_df = filter(Response_GPP,Zone1 == "WW2")
  WW1_GPP_df = filter(Response_GPP,Zone1 == "WW1")
  WW4_growth_df = filter(Response_growth,Zone1 == "WW4") 
  WW3_growth_df = filter(Response_growth,Zone1 == "WW3")
  WW2_growth_df = filter(Response_growth,Zone1 == "WW2")
  WW1_growth_df = filter(Response_growth,Zone1 == "WW1")
  WW4_growth_pt_df = filter(Response_growth_pt,Zone1 == "WW4") 
  WW3_growth_pt_df = filter(Response_growth_pt,Zone1 == "WW3")
  WW2_growth_pt_df = filter(Response_growth_pt,Zone1 == "WW2")
  WW1_growth_pt_df = filter(Response_growth_pt,Zone1 == "WW1")
  
  WD4_GPP_df = filter(Response_GPP,Zone1 == "WD4") 
  WD3_GPP_df = filter(Response_GPP,Zone1 == "WD3") 
  WD2_GPP_df = filter(Response_GPP,Zone1 == "WD2") 
  WD1_GPP_df = filter(Response_GPP,Zone1 == "WD1") 
  WD4_growth_df = filter(Response_growth,Zone1 == "WD4") 
  WD3_growth_df = filter(Response_growth,Zone1 == "WD3")
  WD2_growth_df = filter(Response_growth,Zone1 == "WD2")
  WD1_growth_df = filter(Response_growth,Zone1 == "WD1")
  WD4_growth_pt_df = filter(Response_growth_pt,Zone1 == "WD4")
  WD3_growth_pt_df = filter(Response_growth_pt,Zone1 == "WD3")
  WD2_growth_pt_df = filter(Response_growth_pt,Zone1 == "WD2")
  WD1_growth_pt_df = filter(Response_growth_pt,Zone1 == "WD1")
  
  CW4_GPP_df = filter(Response_GPP,Zone1 == "CW4") 
  CW3_GPP_df = filter(Response_GPP,Zone1 == "CW3") 
  CW2_GPP_df = filter(Response_GPP,Zone1 == "CW2") 
  CW1_GPP_df = filter(Response_GPP,Zone1 == "CW1") 
  CW4_growth_df = filter(Response_growth,Zone1 == "CW4")
  CW3_growth_df = filter(Response_growth,Zone1 == "CW3")
  CW2_growth_df = filter(Response_growth,Zone1 == "CW2")
  CW1_growth_df = filter(Response_growth,Zone1 == "CW1")
  CW4_growth_pt_df = filter(Response_growth_pt,Zone1 == "CW4")
  CW3_growth_pt_df = filter(Response_growth_pt,Zone1 == "CW3")
  CW2_growth_pt_df = filter(Response_growth_pt,Zone1 == "CW2")
  CW1_growth_pt_df = filter(Response_growth_pt,Zone1 == "CW1")
  
  CD4_GPP_df = filter(Response_GPP,Zone1 == "CD4") 
  CD3_GPP_df = filter(Response_GPP,Zone1 == "CD3") 
  CD2_GPP_df = filter(Response_GPP,Zone1 == "CD2") 
  CD1_GPP_df = filter(Response_GPP,Zone1 == "CD1") 
  CD4_growth_df = filter(Response_growth,Zone1 == "CD4")
  CD3_growth_df = filter(Response_growth,Zone1 == "CD3")
  CD2_growth_df = filter(Response_growth,Zone1 == "CD2")
  CD1_growth_df = filter(Response_growth,Zone1 == "CD1")
  CD4_growth_pt_df = filter(Response_growth_pt,Zone1 == "CD4")
  CD3_growth_pt_df = filter(Response_growth_pt,Zone1 == "CD3")
  CD2_growth_pt_df = filter(Response_growth_pt,Zone1 == "CD2")
  CD1_growth_pt_df = filter(Response_growth_pt,Zone1 == "CD1")
}

#Get mean and sd of responses for each group
{
#4 groups
#GPP
  {
#WW
T_response_GPP_WW_mean <- mean(WW_GPP_df$Corr_Temp,na.rm = T)
P_response_GPP_WW_mean <- mean(WW_GPP_df$Corr_Prep,na.rm = T)

T_response_GPP_WW_sd <- sd(WW_GPP_df$Corr_Temp,na.rm = T)
P_response_GPP_WW_sd <- sd(WW_GPP_df$Corr_Prep,na.rm = T)

#WD
T_response_GPP_WD_mean <- mean(WD_GPP_df$Corr_Temp,na.rm = T)
P_response_GPP_WD_mean <- mean(WD_GPP_df$Corr_Prep,na.rm = T)

T_response_GPP_WD_sd <- sd(WD_GPP_df$Corr_Temp,na.rm = T)
P_response_GPP_WD_sd <- sd(WD_GPP_df$Corr_Prep,na.rm = T)

#CW
T_response_GPP_CW_mean <- mean(CW_GPP_df$Corr_Temp,na.rm = T)
P_response_GPP_CW_mean <- mean(CW_GPP_df$Corr_Prep,na.rm = T)

T_response_GPP_CW_sd <- sd(CW_GPP_df$Corr_Temp,na.rm = T)
P_response_GPP_CW_sd <- sd(CW_GPP_df$Corr_Prep,na.rm = T)

#CD
T_response_GPP_CD_mean <- mean(CD_GPP_df$Corr_Temp,na.rm = T)
P_response_GPP_CD_mean <- mean(CD_GPP_df$Corr_Prep,na.rm = T)

T_response_GPP_CD_sd <- sd(CD_GPP_df$Corr_Temp,na.rm = T)
P_response_GPP_CD_sd <- sd(CD_GPP_df$Corr_Prep,na.rm = T)
}
#growth
  {
    #WW
    T_response_growth_WW_mean <- mean(WW_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WW_mean <- mean(WW_growth_df$Corr_Prep_mean,na.rm = T)
    
    T_response_growth_WW_sd <- sd(WW_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WW_sd <- sd(WW_growth_df$Corr_Prep_mean,na.rm = T)
    
    #WD
    T_response_growth_WD_mean <- mean(WD_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WD_mean <- mean(WD_growth_df$Corr_Prep_mean,na.rm = T)
    
    T_response_growth_WD_sd <- sd(WD_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WD_sd <- sd(WD_growth_df$Corr_Prep_mean,na.rm = T)
    
    #CW
    T_response_growth_CW_mean <- mean(CW_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CW_mean <- mean(CW_growth_df$Corr_Prep_mean,na.rm = T)
    
    T_response_growth_CW_sd <- sd(CW_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CW_sd <- sd(CW_growth_df$Corr_Prep_mean,na.rm = T)
    
    #CD
    T_response_growth_CD_mean <- mean(CD_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CD_mean <- mean(CD_growth_df$Corr_Prep_mean,na.rm = T)
    
    T_response_growth_CD_sd <- sd(CD_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CD_sd <- sd(CD_growth_df$Corr_Prep_mean,na.rm = T)
  }  
#growth_pt
  {
    #WW
    T_response_growth_pt_WW_mean <- mean(WW_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WW_mean <- mean(WW_growth_pt_df$Corr_Prep,na.rm = T)
    
    T_response_growth_pt_WW_sd <- sd(WW_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WW_sd <- sd(WW_growth_pt_df$Corr_Prep,na.rm = T)
    
    #WD
    T_response_growth_pt_WD_mean <- mean(WD_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WD_mean <- mean(WD_growth_pt_df$Corr_Prep,na.rm = T)
    
    T_response_growth_pt_WD_sd <- sd(WD_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WD_sd <- sd(WD_growth_pt_df$Corr_Prep,na.rm = T)
    
    #CW
    T_response_growth_pt_CW_mean <- mean(CW_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CW_mean <- mean(CW_growth_pt_df$Corr_Prep,na.rm = T)
    
    T_response_growth_pt_CW_sd <- sd(CW_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CW_sd <- sd(CW_growth_pt_df$Corr_Prep,na.rm = T)
    
    #CD
    T_response_growth_pt_CD_mean <- mean(CD_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CD_mean <- mean(CD_growth_pt_df$Corr_Prep,na.rm = T)
    
    T_response_growth_pt_CD_sd <- sd(CD_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CD_sd <- sd(CD_growth_pt_df$Corr_Prep,na.rm = T)
  } 
  
#16 groups
  #GPP
  {
    #WW
    T_response_GPP_WW4_mean <- mean(WW4_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_WW4_mean <- mean(WW4_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_WW3_mean <- mean(WW3_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_WW3_mean <- mean(WW3_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_WW2_mean <- mean(WW2_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_WW2_mean <- mean(WW2_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_WW1_mean <- mean(WW1_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_WW1_mean <- mean(WW1_GPP_df$Corr_Prep,na.rm = T)
    
    T_response_GPP_WW4_sd <- sd(WW4_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_WW4_sd <- sd(WW4_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_WW3_sd <- sd(WW3_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_WW3_sd <- sd(WW3_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_WW2_sd <- sd(WW2_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_WW2_sd <- sd(WW2_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_WW1_sd <- sd(WW1_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_WW1_sd <- sd(WW1_GPP_df$Corr_Prep,na.rm = T)
    
    #WD
    T_response_GPP_WD4_mean <- mean(WD4_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_WD4_mean <- mean(WD4_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_WD3_mean <- mean(WD3_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_WD3_mean <- mean(WD3_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_WD2_mean <- mean(WD2_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_WD2_mean <- mean(WD2_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_WD1_mean <- mean(WD1_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_WD1_mean <- mean(WD1_GPP_df$Corr_Prep,na.rm = T)
    
    T_response_GPP_WD4_sd <- sd(WD4_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_WD4_sd <- sd(WD4_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_WD3_sd <- sd(WD3_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_WD3_sd <- sd(WD3_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_WD2_sd <- sd(WD2_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_WD2_sd <- sd(WD2_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_WD1_sd <- sd(WD1_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_WD1_sd <- sd(WD1_GPP_df$Corr_Prep,na.rm = T)
    
    #CW
    T_response_GPP_CW4_mean <- mean(CW4_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_CW4_mean <- mean(CW4_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_CW3_mean <- mean(CW3_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_CW3_mean <- mean(CW3_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_CW2_mean <- mean(CW2_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_CW2_mean <- mean(CW2_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_CW1_mean <- mean(CW1_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_CW1_mean <- mean(CW1_GPP_df$Corr_Prep,na.rm = T)
    
    T_response_GPP_CW4_sd <- sd(CW4_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_CW4_sd <- sd(CW4_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_CW3_sd <- sd(CW3_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_CW3_sd <- sd(CW3_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_CW2_sd <- sd(CW2_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_CW2_sd <- sd(CW2_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_CW1_sd <- sd(CW1_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_CW1_sd <- sd(CW1_GPP_df$Corr_Prep,na.rm = T)
    
    #CD
    T_response_GPP_CD4_mean <- mean(CD4_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_CD4_mean <- mean(CD4_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_CD3_mean <- mean(CD3_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_CD3_mean <- mean(CD3_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_CD2_mean <- mean(CD2_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_CD2_mean <- mean(CD2_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_CD1_mean <- mean(CD1_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_CD1_mean <- mean(CD1_GPP_df$Corr_Prep,na.rm = T)
    
    T_response_GPP_CD4_sd <- sd(CD4_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_CD4_sd <- sd(CD4_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_CD3_sd <- sd(CD3_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_CD3_sd <- sd(CD3_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_CD2_sd <- sd(CD2_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_CD2_sd <- sd(CD2_GPP_df$Corr_Prep,na.rm = T)
    T_response_GPP_CD1_sd <- sd(CD1_GPP_df$Corr_Temp,na.rm = T)
    P_response_GPP_CD1_sd <- sd(CD1_GPP_df$Corr_Prep,na.rm = T)
  }
  #growth
  {
    #WW
    T_response_growth_WW4_mean <- mean(WW4_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WW4_mean <- mean(WW4_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_WW3_mean <- mean(WW3_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WW3_mean <- mean(WW3_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_WW2_mean <- mean(WW2_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WW2_mean <- mean(WW2_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_WW1_mean <- mean(WW1_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WW1_mean <- mean(WW1_growth_df$Corr_Prep_mean,na.rm = T)
    
    T_response_growth_WW4_sd <- sd(WW4_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WW4_sd <- sd(WW4_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_WW3_sd <- sd(WW3_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WW3_sd <- sd(WW3_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_WW2_sd <- sd(WW2_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WW2_sd <- sd(WW2_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_WW1_sd <- sd(WW1_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WW1_sd <- sd(WW1_growth_df$Corr_Prep_mean,na.rm = T)
    
    #WD
    T_response_growth_WD4_mean <- mean(WD4_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WD4_mean <- mean(WD4_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_WD3_mean <- mean(WD3_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WD3_mean <- mean(WD3_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_WD2_mean <- mean(WD2_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WD2_mean <- mean(WD2_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_WD1_mean <- mean(WD1_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WD1_mean <- mean(WD1_growth_df$Corr_Prep_mean,na.rm = T)
    
    T_response_growth_WD4_sd <- sd(WD4_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WD4_sd <- sd(WD4_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_WD3_sd <- sd(WD3_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WD3_sd <- sd(WD3_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_WD2_sd <- sd(WD2_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WD2_sd <- sd(WD2_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_WD1_sd <- sd(WD1_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_WD1_sd <- sd(WD1_growth_df$Corr_Prep_mean,na.rm = T)
    
    #CW
    T_response_growth_CW4_mean <- mean(CW4_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CW4_mean <- mean(CW4_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_CW3_mean <- mean(CW3_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CW3_mean <- mean(CW3_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_CW2_mean <- mean(CW2_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CW2_mean <- mean(CW2_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_CW1_mean <- mean(CW1_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CW1_mean <- mean(CW1_growth_df$Corr_Prep_mean,na.rm = T)
    
    T_response_growth_CW4_sd <- sd(CW4_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CW4_sd <- sd(CW4_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_CW3_sd <- sd(CW3_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CW3_sd <- sd(CW3_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_CW2_sd <- sd(CW2_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CW2_sd <- sd(CW2_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_CW1_sd <- sd(CW1_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CW1_sd <- sd(CW1_growth_df$Corr_Prep_mean,na.rm = T)
    
    #CD
    T_response_growth_CD4_mean <- mean(CD4_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CD4_mean <- mean(CD4_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_CD3_mean <- mean(CD3_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CD3_mean <- mean(CD3_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_CD2_mean <- mean(CD2_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CD2_mean <- mean(CD2_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_CD1_mean <- mean(CD1_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CD1_mean <- mean(CD1_growth_df$Corr_Prep_mean,na.rm = T)
    
    T_response_growth_CD4_sd <- sd(CD4_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CD4_sd <- sd(CD4_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_CD3_sd <- sd(CD3_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CD3_sd <- sd(CD3_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_CD2_sd <- sd(CD2_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CD2_sd <- sd(CD2_growth_df$Corr_Prep_mean,na.rm = T)
    T_response_growth_CD1_sd <- sd(CD1_growth_df$Corr_Temp_mean,na.rm = T)
    P_response_growth_CD1_sd <- sd(CD1_growth_df$Corr_Prep_mean,na.rm = T)
  }  
  #growth_pt
  {
    #WW
    T_response_growth_pt_WW4_mean <- mean(WW4_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WW4_mean <- mean(WW4_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_WW3_mean <- mean(WW3_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WW3_mean <- mean(WW3_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_WW2_mean <- mean(WW2_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WW2_mean <- mean(WW2_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_WW1_mean <- mean(WW1_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WW1_mean <- mean(WW1_growth_pt_df$Corr_Prep,na.rm = T)
    
    T_response_growth_pt_WW4_sd <- sd(WW4_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WW4_sd <- sd(WW4_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_WW3_sd <- sd(WW3_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WW3_sd <- sd(WW3_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_WW2_sd <- sd(WW2_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WW2_sd <- sd(WW2_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_WW1_sd <- sd(WW1_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WW1_sd <- sd(WW1_growth_pt_df$Corr_Prep,na.rm = T)
    
    #WD
    T_response_growth_pt_WD4_mean <- mean(WD4_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WD4_mean <- mean(WD4_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_WD3_mean <- mean(WD3_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WD3_mean <- mean(WD3_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_WD2_mean <- mean(WD2_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WD2_mean <- mean(WD2_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_WD1_mean <- mean(WD1_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WD1_mean <- mean(WD1_growth_pt_df$Corr_Prep,na.rm = T)
    
    T_response_growth_pt_WD4_sd <- sd(WD4_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WD4_sd <- sd(WD4_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_WD3_sd <- sd(WD3_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WD3_sd <- sd(WD3_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_WD2_sd <- sd(WD2_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WD2_sd <- sd(WD2_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_WD1_sd <- sd(WD1_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_WD1_sd <- sd(WD1_growth_pt_df$Corr_Prep,na.rm = T)
    
    #CW
    T_response_growth_pt_CW4_mean <- mean(CW4_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CW4_mean <- mean(CW4_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_CW3_mean <- mean(CW3_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CW3_mean <- mean(CW3_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_CW2_mean <- mean(CW2_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CW2_mean <- mean(CW2_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_CW1_mean <- mean(CW1_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CW1_mean <- mean(CW1_growth_pt_df$Corr_Prep,na.rm = T)
    
    T_response_growth_pt_CW4_sd <- sd(CW4_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CW4_sd <- sd(CW4_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_CW3_sd <- sd(CW3_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CW3_sd <- sd(CW3_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_CW2_sd <- sd(CW2_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CW2_sd <- sd(CW2_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_CW1_sd <- sd(CW1_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CW1_sd <- sd(CW1_growth_pt_df$Corr_Prep,na.rm = T)
    
    #CD
    T_response_growth_pt_CD4_mean <- mean(CD4_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CD4_mean <- mean(CD4_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_CD3_mean <- mean(CD3_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CD3_mean <- mean(CD3_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_CD2_mean <- mean(CD2_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CD2_mean <- mean(CD2_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_CD1_mean <- mean(CD1_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CD1_mean <- mean(CD1_growth_pt_df$Corr_Prep,na.rm = T)
    
    T_response_growth_pt_CD4_sd <- sd(CD4_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CD4_sd <- sd(CD4_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_CD3_sd <- sd(CD3_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CD3_sd <- sd(CD3_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_CD2_sd <- sd(CD2_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CD2_sd <- sd(CD2_growth_pt_df$Corr_Prep,na.rm = T)
    T_response_growth_pt_CD1_sd <- sd(CD1_growth_pt_df$Corr_Temp,na.rm = T)
    P_response_growth_pt_CD1_sd <- sd(CD1_growth_pt_df$Corr_Prep,na.rm = T)
  }  
}

#creat output dfs
#4 groups
{
Out_GPP_df <- data.frame(
  Zone <- c("WW","WD","CW","CD"),
  T_Mean <- c(T_response_GPP_WW_mean,T_response_GPP_WD_mean,T_response_GPP_CW_mean,T_response_GPP_CD_mean),
  P_Mean <- c(P_response_GPP_WW_mean,P_response_GPP_WD_mean,P_response_GPP_CW_mean,P_response_GPP_CD_mean),
  T_sd <- c(T_response_GPP_WW_sd,T_response_GPP_WD_sd,T_response_GPP_CW_sd,T_response_GPP_CD_sd),
  P_sd <- c(P_response_GPP_WW_sd,P_response_GPP_WD_sd,P_response_GPP_CW_sd,P_response_GPP_CD_sd))
names(Out_GPP_df) <- c("Zone","T_mean","P_mean","T_sd","P_sd")

Out_growth_df <- data.frame(
  Zone <- c("WW","WD","CW","CD"),
  T_Mean <- c(T_response_growth_WW_mean,T_response_growth_WD_mean,T_response_growth_CW_mean,T_response_growth_CD_mean),
  P_Mean <- c(P_response_growth_WW_mean,P_response_growth_WD_mean,P_response_growth_CW_mean,P_response_growth_CD_mean),
  T_sd <- c(T_response_growth_WW_sd,T_response_growth_WD_sd,T_response_growth_CW_sd,T_response_growth_CD_sd),
  P_sd <- c(P_response_growth_WW_sd,P_response_growth_WD_sd,P_response_growth_CW_sd,P_response_growth_CD_sd))
names(Out_growth_df) <- c("Zone","T_mean","P_mean","T_sd","P_sd")

Out_growth_pt_df <- data.frame(
  Zone <- c("WW","WD","CW","CD"),
  T_Mean <- c(T_response_growth_pt_WW_mean,T_response_growth_pt_WD_mean,T_response_growth_pt_CW_mean,T_response_growth_pt_CD_mean),
  P_Mean <- c(P_response_growth_pt_WW_mean,P_response_growth_pt_WD_mean,P_response_growth_pt_CW_mean,P_response_growth_pt_CD_mean),
  T_sd <- c(T_response_growth_pt_WW_sd,T_response_growth_pt_WD_sd,T_response_growth_pt_CW_sd,T_response_growth_pt_CD_sd),
  P_sd <- c(P_response_growth_pt_WW_sd,P_response_growth_pt_WD_sd,P_response_growth_pt_CW_sd,P_response_growth_pt_CD_sd))
names(Out_growth_pt_df) <- c("Zone","T_mean","P_mean","T_sd","P_sd")

#Combine the out dfs
Out_GPP_df$Source <- "GPP"
Out_growth_df$Source <-"growth"
Out_growth_pt_df$Source <- "growth_pt"

Out_df <- rbind(Out_GPP_df,Out_growth_df,Out_growth_pt_df)
Out_df1 <- rbind(Out_GPP_df,Out_growth_df)

Out_df1$T_max <- Out_df1$T_mean + Out_df1$T_sd
Out_df1$T_min <- Out_df1$T_mean - Out_df1$T_sd

Out_df1$P_max <- Out_df1$P_mean + Out_df1$P_sd
Out_df1$P_min <- Out_df1$P_mean - Out_df1$P_sd
}
#16 groups
{
  Out_GPP_df1 <- data.frame(
    Zone <- c("WW1","WW2","WW3","WW4","WD1","WD2","WD3","WD4","CW1","CW2","CW3","CW4","CD1","CD2","CD3","CD4"),
    T_Mean <- c(T_response_GPP_WW1_mean,T_response_GPP_WW2_mean,T_response_GPP_WW3_mean,T_response_GPP_WW4_mean,T_response_GPP_WD1_mean,T_response_GPP_WD2_mean,T_response_GPP_WD3_mean,T_response_GPP_WD4_mean,T_response_GPP_CW1_mean,T_response_GPP_CW2_mean,T_response_GPP_CW3_mean,T_response_GPP_CW4_mean,T_response_GPP_CD1_mean,T_response_GPP_CD2_mean,T_response_GPP_CD3_mean,T_response_GPP_CD4_mean),
    P_Mean <- c(P_response_GPP_WW1_mean,P_response_GPP_WW2_mean,P_response_GPP_WW3_mean,P_response_GPP_WW4_mean,P_response_GPP_WD1_mean,P_response_GPP_WD2_mean,P_response_GPP_WD3_mean,P_response_GPP_WD4_mean,P_response_GPP_CW1_mean,P_response_GPP_CW2_mean,P_response_GPP_CW3_mean,P_response_GPP_CW4_mean,P_response_GPP_CD1_mean,P_response_GPP_CD2_mean,P_response_GPP_CD3_mean,P_response_GPP_CD4_mean),
    T_sd <- c(T_response_GPP_WW1_sd,T_response_GPP_WW2_sd,T_response_GPP_WW3_sd,T_response_GPP_WW4_sd,T_response_GPP_WD1_sd,T_response_GPP_WD2_sd,T_response_GPP_WD3_sd,T_response_GPP_WD4_sd,T_response_GPP_CW1_sd,T_response_GPP_CW2_sd,T_response_GPP_CW3_sd,T_response_GPP_CW4_sd,T_response_GPP_CD1_sd,T_response_GPP_CD2_sd,T_response_GPP_CD3_sd,T_response_GPP_CD4_sd),
    P_sd <- c(P_response_GPP_WW1_sd,P_response_GPP_WW2_sd,P_response_GPP_WW3_sd,P_response_GPP_WW4_sd,P_response_GPP_WD1_sd,P_response_GPP_WD2_sd,P_response_GPP_WD3_sd,P_response_GPP_WD4_sd,P_response_GPP_CW1_sd,P_response_GPP_CW2_sd,P_response_GPP_CW3_sd,P_response_GPP_CW4_sd,P_response_GPP_CD1_sd,P_response_GPP_CD2_sd,P_response_GPP_CD3_sd,P_response_GPP_CD4_sd))
  names(Out_GPP_df1) <- c("Zone","T_mean","P_mean","T_sd","P_sd")
  
  Out_growth_df1 <- data.frame(
    Zone <- c("WW1","WW2","WW3","WW4","WD1","WD2","WD3","WD4","CW1","CW2","CW3","CW4","CD1","CD2","CD3","CD4"),
    T_Mean <- c(T_response_growth_WW1_mean,T_response_growth_WW2_mean,T_response_growth_WW3_mean,T_response_growth_WW4_mean,T_response_growth_WD1_mean,T_response_growth_WD2_mean,T_response_growth_WD3_mean,T_response_growth_WD4_mean,T_response_growth_CW1_mean,T_response_growth_CW2_mean,T_response_growth_CW3_mean,T_response_growth_CW4_mean,T_response_growth_CD1_mean,T_response_growth_CD2_mean,T_response_growth_CD3_mean,T_response_growth_CD4_mean),
    P_Mean <- c(P_response_growth_WW1_mean,P_response_growth_WW2_mean,P_response_growth_WW3_mean,P_response_growth_WW4_mean,P_response_growth_WD1_mean,P_response_growth_WD2_mean,P_response_growth_WD3_mean,P_response_growth_WD4_mean,P_response_growth_CW1_mean,P_response_growth_CW2_mean,P_response_growth_CW3_mean,P_response_growth_CW4_mean,P_response_growth_CD1_mean,P_response_growth_CD2_mean,P_response_growth_CD3_mean,P_response_growth_CD4_mean),
    T_sd <- c(T_response_growth_WW1_sd,T_response_growth_WW2_sd,T_response_growth_WW3_sd,T_response_growth_WW4_sd,T_response_growth_WD1_sd,T_response_growth_WD2_sd,T_response_growth_WD3_sd,T_response_growth_WD4_sd,T_response_growth_CW1_sd,T_response_growth_CW2_sd,T_response_growth_CW3_sd,T_response_growth_CW4_sd,T_response_growth_CD1_sd,T_response_growth_CD2_sd,T_response_growth_CD3_sd,T_response_growth_CD4_sd),
    P_sd <- c(P_response_growth_WW1_sd,P_response_growth_WW2_sd,P_response_growth_WW3_sd,P_response_growth_WW4_sd,P_response_growth_WD1_sd,P_response_growth_WD2_sd,P_response_growth_WD3_sd,P_response_growth_WD4_sd,P_response_growth_CW1_sd,P_response_growth_CW2_sd,P_response_growth_CW3_sd,P_response_growth_CW4_sd,P_response_growth_CD1_sd,P_response_growth_CD2_sd,P_response_growth_CD3_sd,P_response_growth_CD4_sd))
  names(Out_growth_df1) <- c("Zone","T_mean","P_mean","T_sd","P_sd")
  
  Out_growth_pt_df1 <- data.frame(
    Zone <- c("WW1","WW2","WW3","WW4","WD1","WD2","WD3","WD4","CW1","CW2","CW3","CW4","CD1","CD2","CD3","CD4"),
    T_Mean <- c(T_response_growth_pt_WW1_mean,T_response_growth_pt_WW2_mean,T_response_growth_pt_WW3_mean,T_response_growth_pt_WW4_mean,T_response_growth_pt_WD1_mean,T_response_growth_pt_WD2_mean,T_response_growth_pt_WD3_mean,T_response_growth_pt_WD4_mean,T_response_growth_pt_CW1_mean,T_response_growth_pt_CW2_mean,T_response_growth_pt_CW3_mean,T_response_growth_pt_CW4_mean,T_response_growth_pt_CD1_mean,T_response_growth_pt_CD2_mean,T_response_growth_pt_CD3_mean,T_response_growth_pt_CD4_mean),
    P_Mean <- c(P_response_growth_pt_WW1_mean,P_response_growth_pt_WW2_mean,P_response_growth_pt_WW3_mean,P_response_growth_pt_WW4_mean,P_response_growth_pt_WD1_mean,P_response_growth_pt_WD2_mean,P_response_growth_pt_WD3_mean,P_response_growth_pt_WD4_mean,P_response_growth_pt_CW1_mean,P_response_growth_pt_CW2_mean,P_response_growth_pt_CW3_mean,P_response_growth_pt_CW4_mean,P_response_growth_pt_CD1_mean,P_response_growth_pt_CD2_mean,P_response_growth_pt_CD3_mean,P_response_growth_pt_CD4_mean),
    T_sd <- c(T_response_growth_pt_WW1_sd,T_response_growth_pt_WW2_sd,T_response_growth_pt_WW3_sd,T_response_growth_pt_WW4_sd,T_response_growth_pt_WD1_sd,T_response_growth_pt_WD2_sd,T_response_growth_pt_WD3_sd,T_response_growth_pt_WD4_sd,T_response_growth_pt_CW1_sd,T_response_growth_pt_CW2_sd,T_response_growth_pt_CW3_sd,T_response_growth_pt_CW4_sd,T_response_growth_pt_CD1_sd,T_response_growth_pt_CD2_sd,T_response_growth_pt_CD3_sd,T_response_growth_pt_CD4_sd),
    P_sd <- c(P_response_growth_pt_WW1_sd,P_response_growth_pt_WW2_sd,P_response_growth_pt_WW3_sd,P_response_growth_pt_WW4_sd,P_response_growth_pt_WD1_sd,P_response_growth_pt_WD2_sd,P_response_growth_pt_WD3_sd,P_response_growth_pt_WD4_sd,P_response_growth_pt_CW1_sd,P_response_growth_pt_CW2_sd,P_response_growth_pt_CW3_sd,P_response_growth_pt_CW4_sd,P_response_growth_pt_CD1_sd,P_response_growth_pt_CD2_sd,P_response_growth_pt_CD3_sd,P_response_growth_pt_CD4_sd))
  names(Out_growth_pt_df1) <- c("Zone","T_mean","P_mean","T_sd","P_sd")
  
  #Combine the out dfs
  Out_GPP_df1$Source <- "GPP"
  Out_growth_df1$Source <-"growth"
  Out_growth_pt_df1$Source <- "growth_pt"
  
  Out_df_16 <- rbind(Out_GPP_df1,Out_growth_df1,Out_growth_pt_df1)
  Out_df1_16 <- rbind(Out_GPP_df1,Out_growth_df1)
  
  Out_df1_16$T_max <- Out_df1_16$T_mean + Out_df1_16$T_sd
  Out_df1_16$T_min <- Out_df1_16$T_mean - Out_df1_16$T_sd
  
  Out_df1_16$P_max <- Out_df1_16$P_mean + Out_df1_16$P_sd
  Out_df1_16$P_min <- Out_df1_16$P_mean - Out_df1_16$P_sd  
  
}

#boxplot
  test = ggplot()+
    geom_point(Out_df1,mapping = aes(x = T_mean, y = P_mean,color = Source,shape = Zone)) +
    geom_errorbar(Out_df1,mapping = aes(x = T_mean,ymin = P_min, ymax = P_max,color = Source))+
    geom_errorbarh(Out_df1,mapping = aes(y = P_mean,xmin = T_min, xmax = T_max,color = Source))+
    xlim(-1.1,1.1)+
    ylim(-1.1,1.1)
    
  test1 = ggplot(Out_GPP_df,aes(x = T_mean, y = P_mean,color = Zone))+
    geom_point() +
    geom_errorbar(aes(ymin = (P_mean - P_sd), ymax = (P_mean + P_sd)))+
    geom_errorbarh(aes(xmin = (T_mean - T_sd), xmax = (T_mean + T_sd)))+
    xlim(-1.1,1.1)+
    ylim(-1.1,1.1)

  test2 = ggplot(Out_growth_df,aes(x = T_mean, y = P_mean,color = Zone))+
    geom_point() +
    geom_errorbar(aes(ymin = (P_mean - P_sd), ymax = (P_mean + P_sd)))+
    geom_errorbarh(aes(xmin = (T_mean - T_sd), xmax = (T_mean + T_sd)))+
    xlim(-1.1,1.1)+
    ylim(-1.1,1.1)

