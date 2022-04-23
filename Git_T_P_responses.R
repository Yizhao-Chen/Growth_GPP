#The script is to process and plot the T&P response for GPP&growth outputs for Global synthesis
library("dplyr")
library("ggplot2")

Response_GPP_TRENDY <- read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_TRENDY_S2S3_mean_temp_prep_toshp.csv")

Response_GPP_RS <- read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_RS_mean_temp_prep_pcorr_tras_processed_828_toshp.csv")

Response_GPP_FLUXCOM <- read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_FLUXCOM_age_correct_nbr_temp_prep_pcorr_tras_processed_828_toshp.csv")

Response_growth <- read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_per_area_nbr_ndvi_mean_temp_prep_pcorr_toshp.csv")
#per tree
Response_growth_pt <- read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_per_tree_temp_prep_pcorr_tras_processed_828_toshp.csv")

#Unify the cols
Response_GPP_TRENDY1 = data.frame("Corr_Temp" = Response_GPP_TRENDY$Corr_Temp,"Corr_Prep" = Response_GPP_TRENDY$Corr_Prep,"Temp_4_9" = Response_GPP_TRENDY$Temp_4_9,"Prep_4_9" = Response_GPP_TRENDY$Prep_4_9,"Type" = rep("TRENDY",length(Response_GPP_TRENDY$Corr_Temp)))
Response_GPP_RS1 = data.frame("Corr_Temp" = Response_GPP_RS$Corr_Temp,"Corr_Prep" = Response_GPP_RS$Corr_Prep,"Temp_4_9" = Response_GPP_RS$Temp_4_9,"Prep_4_9" = Response_GPP_RS$Prep_4_9,"Type" = rep("RS",length(Response_GPP_RS$Corr_Temp)))
Response_GPP_FLUXCOM1 = data.frame("Corr_Temp" = Response_GPP_FLUXCOM$Corr_Temp,"Corr_Prep" = Response_GPP_FLUXCOM$Corr_Prep,"Temp_4_9" = Response_GPP_FLUXCOM$Temp_4_9,"Prep_4_9" = Response_GPP_FLUXCOM$Prep_4_9,"Type" = rep("FLUXCOM",length(Response_GPP_FLUXCOM$Corr_Temp)))
Response_growth1 = data.frame("Corr_Temp" = Response_growth$Corr_Temp_mean,"Corr_Prep" = Response_growth$Corr_Prep_mean,"Temp_4_9" = Response_growth$Temp_4_9,"Prep_4_9" = Response_growth$Prep_4_9,"Type" = rep("growth",length(Response_growth$Corr_Temp_mean)))
Response_growth_pt1 = data.frame("Corr_Temp" = Response_growth_pt$Corr_Temp,"Corr_Prep" = Response_growth_pt$Corr_Prep,"Temp_4_9" = Response_growth_pt$Temp_4_9,"Prep_4_9" = Response_growth_pt$Prep_4_9,"Type" = rep("growth_pt",length(Response_growth_pt$Corr_Temp)))

#Get the z-score
#Calculate mean
T_mean <- mean(Response_GPP_TRENDY$Temp_4_9)
P_mean <- mean(Response_GPP_TRENDY$Prep_4_9)

#Calculate sd
T_sd <- sd(Response_GPP_TRENDY$Temp_4_9)
P_sd <- sd(Response_GPP_TRENDY$Prep_4_9)

#Calculate z-score

Temp_zscore1 <- (Response_GPP_TRENDY1$Temp_4_9 - T_mean) / T_sd
Prep_zscore1 <- (Response_GPP_TRENDY1$Prep_4_9 - P_mean) / P_sd

Response_GPP_TRENDY1$Temp_zscore <- Temp_zscore1
Response_GPP_TRENDY1$Prep_zscore <- Prep_zscore1

Response_GPP_FLUXCOM1$Temp_zscore <- Temp_zscore1
Response_GPP_FLUXCOM1$Prep_zscore <- Prep_zscore1

Response_GPP_RS1$Temp_zscore <- Temp_zscore1
Response_GPP_RS1$Prep_zscore <- Prep_zscore1

Response_growth1$Temp_zscore <- Temp_zscore1
Response_growth1$Prep_zscore <- Prep_zscore1 

Response_growth_pt1$Temp_zscore <- Temp_zscore1
Response_growth_pt1$Prep_zscore <- Prep_zscore1 

#rbind the dfs
Response_df = rbind(Response_GPP_TRENDY1,Response_GPP_FLUXCOM1,Response_GPP_RS1,Response_growth1,Response_growth_pt1)

#Group data using Temp&Prep zscore 4 groups
{

#Warm & Wet
  Response_df$Zone[Response_df$Temp_zscore >= 0 & Response_df$Prep_zscore >= 0] <- "WW"
# Response_GPP$Zone[Response_GPP$Temp_zscore >= 0 & Response_GPP$Prep_zscore >= 0] <- "WW"
# Response_growth$Zone[Response_growth$Temp_zscore >= 0 & Response_growth$Prep_zscore >= 0] <- "WW"
# Response_growth_pt$Zone[Response_growth_pt$Temp_zscore >= 0 & Response_growth_pt$Prep_zscore >= 0] <- "WW"
# #Warm & Dry
  Response_df$Zone[Response_df$Temp_zscore >= 0 & Response_df$Prep_zscore < 0] <- "WD"
# Response_GPP$Zone[Response_GPP$Temp_zscore >= 0 & Response_GPP$Prep_zscore < 0] <- "WD"
# Response_growth$Zone[Response_growth$Temp_zscore >= 0 & Response_growth$Prep_zscore < 0] <- "WD"
# Response_growth_pt$Zone[Response_growth_pt$Temp_zscore >= 0 & Response_growth_pt$Prep_zscore < 0] <- "WD"
# #Cool & Wet
  Response_df$Zone[Response_df$Temp_zscore < 0 & Response_df$Prep_zscore >= 0] <- "CW"
# Response_GPP$Zone[Response_GPP$Temp_zscore < 0 & Response_GPP$Prep_zscore >= 0] <- "CW"
# Response_growth$Zone[Response_growth$Temp_zscore < 0 & Response_growth$Prep_zscore >= 0] <- "CW"
# Response_growth_pt$Zone[Response_growth_pt$Temp_zscore < 0 & Response_growth_pt$Prep_zscore >= 0] <- "CW"
# #Cool & Dry
  Response_df$Zone[Response_df$Temp_zscore < 0 & Response_df$Prep_zscore < 0] <- "CD"
# Response_GPP$Zone[Response_GPP$Temp_zscore < 0 & Response_GPP$Prep_zscore < 0] <- "CD"
# Response_growth$Zone[Response_growth$Temp_zscore < 0 & Response_growth$Prep_zscore < 0] <- "CD"
# Response_growth_pt$Zone[Response_growth_pt$Temp_zscore < 0 & Response_growth_pt$Prep_zscore < 0] <- "CD"
}

#Group data using Temp&Prep zscore 16 groups
{
  #Warm & Wet
  Response_df$Zone1[Response_df$Temp_zscore >= 1 & Response_df$Prep_zscore >= 1] <- "WW4"
  Response_df$Zone1[Response_df$Temp_zscore >= 0 & Response_df$Temp_zscore < 1 & Response_df$Prep_zscore >= 1] <- "WW3"
  Response_df$Zone1[Response_df$Temp_zscore >= 1 & Response_df$Prep_zscore >= 0  & Response_df$Prep_zscore <1] <- "WW2"
  Response_df$Zone1[Response_df$Temp_zscore >= 0 & Response_df$Temp_zscore < 1 & Response_df$Prep_zscore >= 0  & Response_df$Prep_zscore <1] <- "WW1"
  #GPP
  # Response_GPP$Zone1[Response_GPP$Temp_zscore >= 1 & Response_GPP$Prep_zscore >= 1] <- "WW4"
  # Response_GPP$Zone1[Response_GPP$Temp_zscore >= 0 & Response_GPP$Temp_zscore < 1 & Response_GPP$Prep_zscore >= 1] <- "WW3"
  # Response_GPP$Zone1[Response_GPP$Temp_zscore >= 1 & Response_GPP$Prep_zscore >= 0  & Response_GPP$Prep_zscore <1] <- "WW2"
  # Response_GPP$Zone1[Response_GPP$Temp_zscore >= 0 & Response_GPP$Temp_zscore < 1 & Response_GPP$Prep_zscore >= 0  & Response_GPP$Prep_zscore <1] <- "WW1"
  
  #growth
  # Response_growth$Zone1[Response_growth$Temp_zscore >= 1 & Response_growth$Prep_zscore >= 1] <- "WW4"
  # Response_growth$Zone1[Response_growth$Temp_zscore >= 0 & Response_growth$Temp_zscore < 1 & Response_growth$Prep_zscore >= 1] <- "WW3"
  # Response_growth$Zone1[Response_growth$Temp_zscore >= 1 & Response_growth$Prep_zscore >= 0  & Response_growth$Prep_zscore <1] <- "WW2"
  # Response_growth$Zone1[Response_growth$Temp_zscore >= 0 & Response_growth$Temp_zscore < 1 & Response_growth$Prep_zscore >= 0  & Response_growth$Prep_zscore <1] <- "WW1" 
  #growth_pt
  # Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore >= 1 & Response_growth_pt$Prep_zscore >= 1] <- "WW4"
  # Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore >= 0 & Response_growth_pt$Temp_zscore < 1 & Response_growth_pt$Prep_zscore >= 1] <- "WW3"
  # Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore >= 1 & Response_growth_pt$Prep_zscore >= 0  & Response_growth_pt$Prep_zscore <1] <- "WW2"
  # Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore >= 0 & Response_growth_pt$Temp_zscore < 1 & Response_growth_pt$Prep_zscore >= 0  & Response_growth_pt$Prep_zscore <1] <- "WW1"
  
  #Warm & Dry
  Response_df$Zone1[Response_df$Temp_zscore >= 1 & Response_df$Prep_zscore < -1] <- "WD4"
  Response_df$Zone1[Response_df$Temp_zscore >= 0 & Response_df$Temp_zscore < 1 & Response_df$Prep_zscore < -1] <- "WD3"
  Response_df$Zone1[Response_df$Temp_zscore >= 1 & Response_df$Prep_zscore >= -1 & Response_df$Prep_zscore < 0] <- "WD2"
  Response_df$Zone1[Response_df$Temp_zscore >= 0 & Response_df$Temp_zscore < 1 & Response_df$Prep_zscore >= -1 & Response_df$Prep_zscore < 0] <- "WD1"
  #GPP
  # Response_GPP$Zone1[Response_GPP$Temp_zscore >= 1 & Response_GPP$Prep_zscore < -1] <- "WD4"
  # Response_GPP$Zone1[Response_GPP$Temp_zscore >= 0 & Response_GPP$Temp_zscore < 1 & Response_GPP$Prep_zscore < -1] <- "WD3"
  # Response_GPP$Zone1[Response_GPP$Temp_zscore >= 1 & Response_GPP$Prep_zscore >= -1 & Response_GPP$Prep_zscore < 0] <- "WD2"
  # Response_GPP$Zone1[Response_GPP$Temp_zscore >= 0 & Response_GPP$Temp_zscore < 1 & Response_GPP$Prep_zscore >= -1 & Response_GPP$Prep_zscore < 0] <- "WD1"
  #growth
  # Response_growth$Zone1[Response_growth$Temp_zscore >= 1 & Response_growth$Prep_zscore < -1] <- "WD4"
  # Response_growth$Zone1[Response_growth$Temp_zscore >= 0 & Response_growth$Temp_zscore < 1 & Response_growth$Prep_zscore < -1] <- "WD3"
  # Response_growth$Zone1[Response_growth$Temp_zscore >= 1 & Response_growth$Prep_zscore >= -1 & Response_growth$Prep_zscore < 0] <- "WD2"
  # Response_growth$Zone1[Response_growth$Temp_zscore >= 0 & Response_growth$Temp_zscore < 1 & Response_growth$Prep_zscore >= -1 & Response_growth$Prep_zscore < 0] <- "WD1"
  #growth_pt
  # Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore >= 1 & Response_growth_pt$Prep_zscore < -1] <- "WD4"
  # Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore >= 0 & Response_growth_pt$Temp_zscore < 1 & Response_growth_pt$Prep_zscore < -1] <- "WD3"
  # Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore >= 1 & Response_growth_pt$Prep_zscore >= -1 & Response_growth_pt$Prep_zscore < 0] <- "WD2"
  # Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore >= 0 & Response_growth_pt$Temp_zscore < 1 & Response_growth_pt$Prep_zscore >= -1 & Response_growth_pt$Prep_zscore < 0] <- "WD1"
  
  #Cool & Wet
  Response_df$Zone1[Response_df$Temp_zscore < -1 & Response_df$Prep_zscore >= 1] <- "CW4"
  Response_df$Zone1[Response_df$Temp_zscore < 0 & Response_df$Temp_zscore >= -1 & Response_df$Prep_zscore >= 1] <- "CW3"
  Response_df$Zone1[Response_df$Temp_zscore < -1 & Response_df$Prep_zscore < 1 & Response_df$Prep_zscore >= 0] <- "CW2"
  Response_df$Zone1[Response_df$Temp_zscore < 0 & Response_df$Temp_zscore >= -1 & Response_df$Prep_zscore < 1 & Response_df$Prep_zscore >= 0] <- "CW1"
  #GPP
  # Response_GPP$Zone1[Response_GPP$Temp_zscore < -1 & Response_GPP$Prep_zscore >= 1] <- "CW4"
  # Response_GPP$Zone1[Response_GPP$Temp_zscore < 0 & Response_GPP$Temp_zscore >= -1 & Response_GPP$Prep_zscore >= 1] <- "CW3"
  # Response_GPP$Zone1[Response_GPP$Temp_zscore < -1 & Response_GPP$Prep_zscore < 1 & Response_GPP$Prep_zscore >= 0] <- "CW2"
  # Response_GPP$Zone1[Response_GPP$Temp_zscore < 0 & Response_GPP$Temp_zscore >= -1 & Response_GPP$Prep_zscore < 1 & Response_GPP$Prep_zscore >= 0] <- "CW1"
  #growth
  # Response_growth$Zone1[Response_growth$Temp_zscore < -1 & Response_growth$Prep_zscore >= 1] <- "CW4"
  # Response_growth$Zone1[Response_growth$Temp_zscore < 0 & Response_growth$Temp_zscore >= -1 & Response_growth$Prep_zscore >= 1] <- "CW3"
  # Response_growth$Zone1[Response_growth$Temp_zscore < -1 & Response_growth$Prep_zscore < 1 & Response_growth$Prep_zscore >= 0] <- "CW2"
  # Response_growth$Zone1[Response_growth$Temp_zscore < 0 & Response_growth$Temp_zscore >= -1 & Response_growth$Prep_zscore < 1 & Response_growth$Prep_zscore >= 0] <- "CW1"
  #growth_pt
  # Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore < -1 & Response_growth_pt$Prep_zscore >= 1] <- "CW4"
  # Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore < 0 & Response_growth_pt$Temp_zscore >= -1 & Response_growth_pt$Prep_zscore >= 1] <- "CW3"
  # Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore < -1 & Response_growth_pt$Prep_zscore < 1 & Response_growth_pt$Prep_zscore >= 0] <- "CW2"
  # Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore < 0 & Response_growth_pt$Temp_zscore >= -1 & Response_growth_pt$Prep_zscore < 1 & Response_growth_pt$Prep_zscore >= 0] <- "CW1"
  
  #Cool & Dry
  Response_df$Zone1[Response_df$Temp_zscore < -1 & Response_df$Prep_zscore < -1] <- "CD4"
  Response_df$Zone1[Response_df$Temp_zscore < 0 & Response_df$Temp_zscore >= -1 & Response_df$Prep_zscore < -1] <- "CD3"
  Response_df$Zone1[Response_df$Temp_zscore < -1 & Response_df$Prep_zscore < 0 & Response_df$Prep_zscore >= -1] <- "CD2"
  Response_df$Zone1[Response_df$Temp_zscore < 0 & Response_df$Temp_zscore >= -1 & Response_df$Prep_zscore < 0 & Response_df$Prep_zscore >= -1] <- "CD1"
  #GPP
  # Response_GPP$Zone1[Response_GPP$Temp_zscore < -1 & Response_GPP$Prep_zscore < -1] <- "CD4"
  # Response_GPP$Zone1[Response_GPP$Temp_zscore < 0 & Response_GPP$Temp_zscore >= -1 & Response_GPP$Prep_zscore < -1] <- "CD3" 
  # Response_GPP$Zone1[Response_GPP$Temp_zscore < -1 & Response_GPP$Prep_zscore < 0 & Response_GPP$Prep_zscore >= -1] <- "CD2"
  # Response_GPP$Zone1[Response_GPP$Temp_zscore < 0 & Response_GPP$Temp_zscore >= -1 & Response_GPP$Prep_zscore < 0 & Response_GPP$Prep_zscore >= -1] <- "CD1" 
  #growth
  # Response_growth$Zone1[Response_growth$Temp_zscore < -1 & Response_growth$Prep_zscore < -1] <- "CD4"
  # Response_growth$Zone1[Response_growth$Temp_zscore < 0 & Response_growth$Temp_zscore >= -1 & Response_growth$Prep_zscore < -1] <- "CD3" 
  # Response_growth$Zone1[Response_growth$Temp_zscore < -1 & Response_growth$Prep_zscore < 0 & Response_growth$Prep_zscore >= -1] <- "CD2"
  # Response_growth$Zone1[Response_growth$Temp_zscore < 0 & Response_growth$Temp_zscore >= -1 & Response_growth$Prep_zscore < 0 & Response_growth$Prep_zscore >= -1] <- "CD1" 
  #growth_pt
  # Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore < -1 & Response_growth_pt$Prep_zscore < -1] <- "CD4"
  # Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore < 0 & Response_growth_pt$Temp_zscore >= -1 & Response_growth_pt$Prep_zscore < -1] <- "CD3" 
  # Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore < -1 & Response_growth_pt$Prep_zscore < 0 & Response_growth_pt$Prep_zscore >= -1] <- "CD2"
  # Response_growth_pt$Zone1[Response_growth_pt$Temp_zscore < 0 & Response_growth_pt$Temp_zscore >= -1 & Response_growth_pt$Prep_zscore < 0 & Response_growth_pt$Prep_zscore >= -1] <- "CD1" 
  }

#Group data based on zone 4 groups
{
  WW_GPP_TRENDY_df = filter(Response_df,Zone == "WW" & Type == "TRENDY") 
  WW_GPP_RS_df = filter(Response_df,Zone == "WW" & Type == "RS") 
  WW_GPP_FLUXCOM_df = filter(Response_df,Zone == "WW" & Type == "FLUXCOM") 
  WW_growth_df = filter(Response_df,Zone == "WW" & Type == "growth") 
  WW_growth_pt_df = filter(Response_df,Zone == "WW" & Type == "growth_pt") 
  
  WD_GPP_TRENDY_df = filter(Response_df,Zone == "WD" & Type == "TRENDY") 
  WD_GPP_RS_df = filter(Response_df,Zone == "WD" & Type == "RS") 
  WD_GPP_FLUXCOM_df = filter(Response_df,Zone == "WD" & Type == "FLUXCOM") 
  WD_growth_df = filter(Response_df,Zone == "WD" & Type == "growth") 
  WD_growth_pt_df = filter(Response_df,Zone == "WD" & Type == "growth_pt") 
  
  CW_GPP_TRENDY_df = filter(Response_df,Zone == "CW" & Type == "TRENDY") 
  CW_GPP_RS_df = filter(Response_df,Zone == "CW" & Type == "RS") 
  CW_GPP_FLUXCOM_df = filter(Response_df,Zone == "CW" & Type == "FLUXCOM") 
  CW_growth_df = filter(Response_df,Zone == "CW" & Type == "growth") 
  CW_growth_pt_df = filter(Response_df,Zone == "CW" & Type == "growth_pt") 
  
  CD_GPP_TRENDY_df = filter(Response_df,Zone == "CD" & Type == "TRENDY") 
  CD_GPP_RS_df = filter(Response_df,Zone == "CD" & Type == "RS") 
  CD_GPP_FLUXCOM_df = filter(Response_df,Zone == "CD" & Type == "FLUXCOM") 
  CD_growth_df = filter(Response_df,Zone == "CD" & Type == "growth") 
  CD_growth_pt_df = filter(Response_df,Zone == "CD" & Type == "growth_pt") 
  
  # WW_GPP_df = filter(Response_GPP,Zone == "WW") 
  # WW_growth_df = filter(Response_growth,Zone == "WW") 
  # WW_growth_df = filter(Response_growth_pt,Zone == "WW") 
  # 
  # WD_GPP_df = filter(Response_GPP,Zone == "WD") 
  # WD_growth_df = filter(Response_growth,Zone == "WD") 
  # WD_growth_df = filter(Response_growth_pt,Zone == "WD")   
  # 
  # CW_GPP_df = filter(Response_GPP,Zone == "CW") 
  # CW_growth_df = filter(Response_growth,Zone == "CW") 
  # CW_growth_df = filter(Response_growth_pt,Zone == "CW") 
  # 
  # CD_GPP_df = filter(Response_GPP,Zone == "CD") 
  # CD_growth_df = filter(Response_growth,Zone == "CD") 
  # CD_growth_df = filter(Response_growth_pt,Zone == "CD")
}
#16 groups
{
  WW1_GPP_TRENDY_df = filter(Response_df,Zone1 == "WW1" & Type == "TRENDY") 
  WW1_GPP_RS_df = filter(Response_df,Zone1 == "WW1" & Type == "RS") 
  WW1_GPP_FLUXCOM_df = filter(Response_df,Zone1 == "WW1" & Type == "FLUXCOM") 
  WW1_growth_df = filter(Response_df,Zone1 == "WW1" & Type == "growth") 
  WW1_growth_pt_df = filter(Response_df,Zone1 == "WW1" & Type == "growth_pt") 
  
  WD1_GPP_TRENDY_df = filter(Response_df,Zone1 == "WD1" & Type == "TRENDY") 
  WD1_GPP_RS_df = filter(Response_df,Zone1 == "WD1" & Type == "RS") 
  WD1_GPP_FLUXCOM_df = filter(Response_df,Zone1 == "WD1" & Type == "FLUXCOM") 
  WD1_growth_df = filter(Response_df,Zone1 == "WD1" & Type == "growth") 
  WD1_growth_pt_df = filter(Response_df,Zone1 == "WD1" & Type == "growth_pt") 
  
  CW1_GPP_TRENDY_df = filter(Response_df,Zone1 == "CW1" & Type == "TRENDY") 
  CW1_GPP_RS_df = filter(Response_df,Zone1 == "CW1" & Type == "RS") 
  CW1_GPP_FLUXCOM_df = filter(Response_df,Zone1 == "CW1" & Type == "FLUXCOM") 
  CW1_growth_df = filter(Response_df,Zone1 == "CW1" & Type == "growth") 
  CW1_growth_pt_df = filter(Response_df,Zone1 == "CW1" & Type == "growth_pt") 
  
  CD1_GPP_TRENDY_df = filter(Response_df,Zone1 == "CD1" & Type == "TRENDY") 
  CD1_GPP_RS_df = filter(Response_df,Zone1 == "CD1" & Type == "RS") 
  CD1_GPP_FLUXCOM_df = filter(Response_df,Zone1 == "CD1" & Type == "FLUXCOM") 
  CD1_growth_df = filter(Response_df,Zone1 == "CD1" & Type == "growth") 
  CD1_growth_pt_df = filter(Response_df,Zone1 == "CD1" & Type == "growth_pt") 
  
  WW2_GPP_TRENDY_df = filter(Response_df,Zone1 == "WW2" & Type == "TRENDY") 
  WW2_GPP_RS_df = filter(Response_df,Zone1 == "WW2" & Type == "RS") 
  WW2_GPP_FLUXCOM_df = filter(Response_df,Zone1 == "WW2" & Type == "FLUXCOM") 
  WW2_growth_df = filter(Response_df,Zone1 == "WW2" & Type == "growth") 
  WW2_growth_pt_df = filter(Response_df,Zone1 == "WW2" & Type == "growth_pt") 
  
  WD2_GPP_TRENDY_df = filter(Response_df,Zone1 == "WD2" & Type == "TRENDY") 
  WD2_GPP_RS_df = filter(Response_df,Zone1 == "WD2" & Type == "RS") 
  WD2_GPP_FLUXCOM_df = filter(Response_df,Zone1 == "WD2" & Type == "FLUXCOM") 
  WD2_growth_df = filter(Response_df,Zone1 == "WD2" & Type == "growth") 
  WD2_growth_pt_df = filter(Response_df,Zone1 == "WD2" & Type == "growth_pt") 
  
  CW2_GPP_TRENDY_df = filter(Response_df,Zone1 == "CW2" & Type == "TRENDY") 
  CW2_GPP_RS_df = filter(Response_df,Zone1 == "CW2" & Type == "RS") 
  CW2_GPP_FLUXCOM_df = filter(Response_df,Zone1 == "CW2" & Type == "FLUXCOM") 
  CW2_growth_df = filter(Response_df,Zone1 == "CW2" & Type == "growth") 
  CW2_growth_pt_df = filter(Response_df,Zone1 == "CW2" & Type == "growth_pt") 
  
  CD2_GPP_TRENDY_df = filter(Response_df,Zone1 == "CD2" & Type == "TRENDY") 
  CD2_GPP_RS_df = filter(Response_df,Zone1 == "CD2" & Type == "RS") 
  CD2_GPP_FLUXCOM_df = filter(Response_df,Zone1 == "CD2" & Type == "FLUXCOM") 
  CD2_growth_df = filter(Response_df,Zone1 == "CD2" & Type == "growth") 
  CD2_growth_pt_df = filter(Response_df,Zone1 == "CD2" & Type == "growth_pt")
  
  WW3_GPP_TRENDY_df = filter(Response_df,Zone1 == "WW3" & Type == "TRENDY") 
  WW3_GPP_RS_df = filter(Response_df,Zone1 == "WW3" & Type == "RS") 
  WW3_GPP_FLUXCOM_df = filter(Response_df,Zone1 == "WW3" & Type == "FLUXCOM") 
  WW3_growth_df = filter(Response_df,Zone1 == "WW3" & Type == "growth") 
  WW3_growth_pt_df = filter(Response_df,Zone1 == "WW3" & Type == "growth_pt") 
  
  WD3_GPP_TRENDY_df = filter(Response_df,Zone1 == "WD3" & Type == "TRENDY") 
  WD3_GPP_RS_df = filter(Response_df,Zone1 == "WD3" & Type == "RS") 
  WD3_GPP_FLUXCOM_df = filter(Response_df,Zone1 == "WD3" & Type == "FLUXCOM") 
  WD3_growth_df = filter(Response_df,Zone1 == "WD3" & Type == "growth") 
  WD3_growth_pt_df = filter(Response_df,Zone1 == "WD3" & Type == "growth_pt") 
  
  CW3_GPP_TRENDY_df = filter(Response_df,Zone1 == "CW3" & Type == "TRENDY") 
  CW3_GPP_RS_df = filter(Response_df,Zone1 == "CW3" & Type == "RS") 
  CW3_GPP_FLUXCOM_df = filter(Response_df,Zone1 == "CW3" & Type == "FLUXCOM") 
  CW3_growth_df = filter(Response_df,Zone1 == "CW3" & Type == "growth") 
  CW3_growth_pt_df = filter(Response_df,Zone1 == "CW3" & Type == "growth_pt") 
  
  CD3_GPP_TRENDY_df = filter(Response_df,Zone1 == "CD3" & Type == "TRENDY") 
  CD3_GPP_RS_df = filter(Response_df,Zone1 == "CD3" & Type == "RS") 
  CD3_GPP_FLUXCOM_df = filter(Response_df,Zone1 == "CD3" & Type == "FLUXCOM") 
  CD3_growth_df = filter(Response_df,Zone1 == "CD3" & Type == "growth") 
  CD3_growth_pt_df = filter(Response_df,Zone1 == "CD3" & Type == "growth_pt")
  
  WW4_GPP_TRENDY_df = filter(Response_df,Zone1 == "WW4" & Type == "TRENDY") 
  WW4_GPP_RS_df = filter(Response_df,Zone1 == "WW4" & Type == "RS") 
  WW4_GPP_FLUXCOM_df = filter(Response_df,Zone1 == "WW4" & Type == "FLUXCOM") 
  WW4_growth_df = filter(Response_df,Zone1 == "WW4" & Type == "growth") 
  WW4_growth_pt_df = filter(Response_df,Zone1 == "WW4" & Type == "growth_pt") 
  
  WD4_GPP_TRENDY_df = filter(Response_df,Zone1 == "WD4" & Type == "TRENDY") 
  WD4_GPP_RS_df = filter(Response_df,Zone1 == "WD4" & Type == "RS") 
  WD4_GPP_FLUXCOM_df = filter(Response_df,Zone1 == "WD4" & Type == "FLUXCOM") 
  WD4_growth_df = filter(Response_df,Zone1 == "WD4" & Type == "growth") 
  WD4_growth_pt_df = filter(Response_df,Zone1 == "WD4" & Type == "growth_pt") 
  
  CW4_GPP_TRENDY_df = filter(Response_df,Zone1 == "CW4" & Type == "TRENDY") 
  CW4_GPP_RS_df = filter(Response_df,Zone1 == "CW4" & Type == "RS") 
  CW4_GPP_FLUXCOM_df = filter(Response_df,Zone1 == "CW4" & Type == "FLUXCOM") 
  CW4_growth_df = filter(Response_df,Zone1 == "CW4" & Type == "growth") 
  CW4_growth_pt_df = filter(Response_df,Zone1 == "CW4" & Type == "growth_pt") 
  
  CD4_GPP_TRENDY_df = filter(Response_df,Zone1 == "CD4" & Type == "TRENDY") 
  CD4_GPP_RS_df = filter(Response_df,Zone1 == "CD4" & Type == "RS") 
  CD4_GPP_FLUXCOM_df = filter(Response_df,Zone1 == "CD4" & Type == "FLUXCOM") 
  CD4_growth_df = filter(Response_df,Zone1 == "CD4" & Type == "growth") 
  CD4_growth_pt_df = filter(Response_df,Zone1 == "CD4" & Type == "growth_pt")
  # WW4_GPP_df = filter(Response_GPP,Zone1 == "WW4") 
  # WW3_GPP_df = filter(Response_GPP,Zone1 == "WW3")
  # WW2_GPP_df = filter(Response_GPP,Zone1 == "WW2")
  # WW1_GPP_df = filter(Response_GPP,Zone1 == "WW1")
  # WW4_growth_df = filter(Response_growth,Zone1 == "WW4") 
  # WW3_growth_df = filter(Response_growth,Zone1 == "WW3")
  # WW2_growth_df = filter(Response_growth,Zone1 == "WW2")
  # WW1_growth_df = filter(Response_growth,Zone1 == "WW1")
  # WW4_growth_pt_df = filter(Response_growth_pt,Zone1 == "WW4") 
  # WW3_growth_pt_df = filter(Response_growth_pt,Zone1 == "WW3")
  # WW2_growth_pt_df = filter(Response_growth_pt,Zone1 == "WW2")
  # WW1_growth_pt_df = filter(Response_growth_pt,Zone1 == "WW1")
  # 
  # WD4_GPP_df = filter(Response_GPP,Zone1 == "WD4") 
  # WD3_GPP_df = filter(Response_GPP,Zone1 == "WD3") 
  # WD2_GPP_df = filter(Response_GPP,Zone1 == "WD2") 
  # WD1_GPP_df = filter(Response_GPP,Zone1 == "WD1") 
  # WD4_growth_df = filter(Response_growth,Zone1 == "WD4") 
  # WD3_growth_df = filter(Response_growth,Zone1 == "WD3")
  # WD2_growth_df = filter(Response_growth,Zone1 == "WD2")
  # WD1_growth_df = filter(Response_growth,Zone1 == "WD1")
  # WD4_growth_pt_df = filter(Response_growth_pt,Zone1 == "WD4")
  # WD3_growth_pt_df = filter(Response_growth_pt,Zone1 == "WD3")
  # WD2_growth_pt_df = filter(Response_growth_pt,Zone1 == "WD2")
  # WD1_growth_pt_df = filter(Response_growth_pt,Zone1 == "WD1")
  # 
  # CW4_GPP_df = filter(Response_GPP,Zone1 == "CW4") 
  # CW3_GPP_df = filter(Response_GPP,Zone1 == "CW3") 
  # CW2_GPP_df = filter(Response_GPP,Zone1 == "CW2") 
  # CW1_GPP_df = filter(Response_GPP,Zone1 == "CW1") 
  # CW4_growth_df = filter(Response_growth,Zone1 == "CW4")
  # CW3_growth_df = filter(Response_growth,Zone1 == "CW3")
  # CW2_growth_df = filter(Response_growth,Zone1 == "CW2")
  # CW1_growth_df = filter(Response_growth,Zone1 == "CW1")
  # CW4_growth_pt_df = filter(Response_growth_pt,Zone1 == "CW4")
  # CW3_growth_pt_df = filter(Response_growth_pt,Zone1 == "CW3")
  # CW2_growth_pt_df = filter(Response_growth_pt,Zone1 == "CW2")
  # CW1_growth_pt_df = filter(Response_growth_pt,Zone1 == "CW1")
  # 
  # CD4_GPP_df = filter(Response_GPP,Zone1 == "CD4") 
  # CD3_GPP_df = filter(Response_GPP,Zone1 == "CD3") 
  # CD2_GPP_df = filter(Response_GPP,Zone1 == "CD2") 
  # CD1_GPP_df = filter(Response_GPP,Zone1 == "CD1") 
  # CD4_growth_df = filter(Response_growth,Zone1 == "CD4")
  # CD3_growth_df = filter(Response_growth,Zone1 == "CD3")
  # CD2_growth_df = filter(Response_growth,Zone1 == "CD2")
  # CD1_growth_df = filter(Response_growth,Zone1 == "CD1")
  # CD4_growth_pt_df = filter(Response_growth_pt,Zone1 == "CD4")
  # CD3_growth_pt_df = filter(Response_growth_pt,Zone1 == "CD3")
  # CD2_growth_pt_df = filter(Response_growth_pt,Zone1 == "CD2")
  # CD1_growth_pt_df = filter(Response_growth_pt,Zone1 == "CD1")
}

#Get mean and sd of responses for each group
{
#4 groups
#updated   
  {
    #WW
    {
      T_response_GPP_TRENDY_WW_mean = mean(WW_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WW_mean = mean(WW_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WW_mean = mean(WW_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WW_mean = mean(WW_growth_df$Corr_Temp,na.rm = T)
      T_response_growth_pt_WW_mean = mean(WW_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WW_mean = mean(WW_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WW_mean = mean(WW_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WW_mean = mean(WW_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WW_mean = mean(WW_growth_df$Corr_Prep,na.rm = T)
      P_response_growth_pt_WW_mean = mean(WW_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_WW_sd = sd(WW_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WW_sd = sd(WW_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WW_sd = sd(WW_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WW_sd = sd(WW_growth_df$Corr_Temp,na.rm = T)
      T_response_growth_pt_WW_sd = sd(WW_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WW_sd = sd(WW_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WW_sd = sd(WW_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WW_sd = sd(WW_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WW_sd = sd(WW_growth_df$Corr_Prep,na.rm = T)
      P_response_growth_pt_WW_sd = sd(WW_growth_pt_df$Corr_Prep,na.rm = T)   
    }
    #WD
    {
      T_response_GPP_TRENDY_WD_mean = mean(WD_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WD_mean = mean(WD_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WD_mean = mean(WD_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WD_mean = mean(WD_growth_df$Corr_Temp,na.rm = T)
      T_response_growth_pt_WD_mean = mean(WD_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WD_mean = mean(WD_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WD_mean = mean(WD_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WD_mean = mean(WD_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WD_mean = mean(WD_growth_df$Corr_Prep,na.rm = T)
      P_response_growth_pt_WD_mean = mean(WD_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_WD_sd = sd(WD_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WD_sd = sd(WD_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WD_sd = sd(WD_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WD_sd = sd(WD_growth_df$Corr_Temp,na.rm = T)
      T_response_growth_pt_WD_sd = sd(WD_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WD_sd = sd(WD_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WD_sd = sd(WD_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WD_sd = sd(WD_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WD_sd = sd(WD_growth_df$Corr_Prep,na.rm = T)
      P_response_growth_pt_WD_sd = sd(WD_growth_pt_df$Corr_Prep,na.rm = T)   
    }    
    #CW
    {
      T_response_GPP_TRENDY_CW_mean = mean(CW_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CW_mean = mean(CW_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CW_mean = mean(CW_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CW_mean = mean(CW_growth_df$Corr_Temp,na.rm = T)
      T_response_growth_pt_CW_mean = mean(CW_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CW_mean = mean(CW_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CW_mean = mean(CW_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CW_mean = mean(CW_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CW_mean = mean(CW_growth_df$Corr_Prep,na.rm = T)
      P_response_growth_pt_CW_mean = mean(CW_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_CW_sd = sd(CW_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CW_sd = sd(CW_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CW_sd = sd(CW_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CW_sd = sd(CW_growth_df$Corr_Temp,na.rm = T)
      T_response_growth_pt_CW_sd = sd(CW_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CW_sd = sd(CW_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CW_sd = sd(CW_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CW_sd = sd(CW_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CW_sd = sd(CW_growth_df$Corr_Prep,na.rm = T)
      P_response_growth_pt_CW_sd = sd(CW_growth_pt_df$Corr_Prep,na.rm = T)   
    }      
    #CD
    {
      T_response_GPP_TRENDY_CD_mean = mean(CD_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CD_mean = mean(CD_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CD_mean = mean(CD_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CD_mean = mean(CD_growth_df$Corr_Temp,na.rm = T)
      T_response_growth_pt_CD_mean = mean(CD_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CD_mean = mean(CD_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CD_mean = mean(CD_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CD_mean = mean(CD_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CD_mean = mean(CD_growth_df$Corr_Prep,na.rm = T)
      P_response_growth_pt_CD_mean = mean(CD_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_CD_sd = sd(CD_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CD_sd = sd(CD_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CD_sd = sd(CD_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CD_sd = sd(CD_growth_df$Corr_Temp,na.rm = T)
      T_response_growth_pt_CD_sd = sd(CD_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CD_sd = sd(CD_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CD_sd = sd(CD_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CD_sd = sd(CD_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CD_sd = sd(CD_growth_df$Corr_Prep,na.rm = T)
      P_response_growth_pt_CD_sd = sd(CD_growth_pt_df$Corr_Prep,na.rm = T)   
    }
  }  
  
#16 groups
  {
    #WW
    {
      #WW1
      T_response_GPP_TRENDY_WW1_mean = mean(WW1_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WW1_mean = mean(WW1_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WW1_mean = mean(WW1_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WW1_mean = mean(WW1_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_WW1_mean = mean(WW1_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WW1_mean = mean(WW1_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WW1_mean = mean(WW1_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WW1_mean = mean(WW1_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WW1_mean = mean(WW1_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_WW1_mean = mean(WW1_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_WW1_sd = sd(WW1_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WW1_sd = sd(WW1_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WW1_sd = sd(WW1_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WW1_sd = sd(WW1_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_WW1_sd = sd(WW1_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WW1_sd = sd(WW1_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WW1_sd = sd(WW1_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WW1_sd = sd(WW1_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WW1_sd = sd(WW1_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_WW1_sd = sd(WW1_growth_pt_df$Corr_Prep,na.rm = T)  
      #end of WW1
      ###########################################################################
      T_response_GPP_TRENDY_WW2_mean = mean(WW2_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WW2_mean = mean(WW2_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WW2_mean = mean(WW2_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WW2_mean = mean(WW2_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_WW2_mean = mean(WW2_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WW2_mean = mean(WW2_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WW2_mean = mean(WW2_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WW2_mean = mean(WW2_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WW2_mean = mean(WW2_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_WW2_mean = mean(WW2_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_WW2_sd = sd(WW2_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WW2_sd = sd(WW2_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WW2_sd = sd(WW2_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WW2_sd = sd(WW2_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_WW2_sd = sd(WW2_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WW2_sd = sd(WW2_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WW2_sd = sd(WW2_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WW2_sd = sd(WW2_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WW2_sd = sd(WW2_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_WW2_sd = sd(WW2_growth_pt_df$Corr_Prep,na.rm = T) 
      #end of WW2
      ###########################################################################
      T_response_GPP_TRENDY_WW3_mean = mean(WW3_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WW3_mean = mean(WW3_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WW3_mean = mean(WW3_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WW3_mean = mean(WW3_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_WW3_mean = mean(WW3_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WW3_mean = mean(WW3_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WW3_mean = mean(WW3_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WW3_mean = mean(WW3_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WW3_mean = mean(WW3_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_WW3_mean = mean(WW3_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_WW3_sd = sd(WW3_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WW3_sd = sd(WW3_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WW3_sd = sd(WW3_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WW3_sd = sd(WW3_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_WW3_sd = sd(WW3_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WW3_sd = sd(WW3_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WW3_sd = sd(WW3_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WW3_sd = sd(WW3_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WW3_sd = sd(WW3_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_WW3_sd = sd(WW3_growth_pt_df$Corr_Prep,na.rm = T) 
      #end of WW3
      ###########################################################################   
      T_response_GPP_TRENDY_WW4_mean = mean(WW4_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WW4_mean = mean(WW4_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WW4_mean = mean(WW4_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WW4_mean = mean(WW4_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_WW4_mean = mean(WW4_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WW4_mean = mean(WW4_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WW4_mean = mean(WW4_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WW4_mean = mean(WW4_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WW4_mean = mean(WW4_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_WW4_mean = mean(WW4_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_WW4_sd = sd(WW4_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WW4_sd = sd(WW4_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WW4_sd = sd(WW4_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WW4_sd = sd(WW4_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_WW4_sd = sd(WW4_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WW4_sd = sd(WW4_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WW4_sd = sd(WW4_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WW4_sd = sd(WW4_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WW4_sd = sd(WW4_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_WW4_sd = sd(WW4_growth_pt_df$Corr_Prep,na.rm = T) 
      #end of WW4
      ###########################################################################
    }
    #WD
    {
      #WD1
      T_response_GPP_TRENDY_WD1_mean = mean(WD1_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WD1_mean = mean(WD1_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WD1_mean = mean(WD1_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WD1_mean = mean(WD1_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_WD1_mean = mean(WD1_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WD1_mean = mean(WD1_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WD1_mean = mean(WD1_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WD1_mean = mean(WD1_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WD1_mean = mean(WD1_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_WD1_mean = mean(WD1_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_WD1_sd = sd(WD1_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WD1_sd = sd(WD1_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WD1_sd = sd(WD1_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WD1_sd = sd(WD1_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_WD1_sd = sd(WD1_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WD1_sd = sd(WD1_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WD1_sd = sd(WD1_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WD1_sd = sd(WD1_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WD1_sd = sd(WD1_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_WD1_sd = sd(WD1_growth_pt_df$Corr_Prep,na.rm = T)  
      #end of WD1
      ###########################################################################
      T_response_GPP_TRENDY_WD2_mean = mean(WD2_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WD2_mean = mean(WD2_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WD2_mean = mean(WD2_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WD2_mean = mean(WD2_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_WD2_mean = mean(WD2_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WD2_mean = mean(WD2_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WD2_mean = mean(WD2_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WD2_mean = mean(WD2_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WD2_mean = mean(WD2_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_WD2_mean = mean(WD2_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_WD2_sd = sd(WD2_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WD2_sd = sd(WD2_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WD2_sd = sd(WD2_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WD2_sd = sd(WD2_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_WD2_sd = sd(WD2_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WD2_sd = sd(WD2_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WD2_sd = sd(WD2_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WD2_sd = sd(WD2_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WD2_sd = sd(WD2_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_WD2_sd = sd(WD2_growth_pt_df$Corr_Prep,na.rm = T) 
      #end of WD2
      ###########################################################################
      T_response_GPP_TRENDY_WD3_mean = mean(WD3_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WD3_mean = mean(WD3_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WD3_mean = mean(WD3_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WD3_mean = mean(WD3_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_WD3_mean = mean(WD3_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WD3_mean = mean(WD3_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WD3_mean = mean(WD3_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WD3_mean = mean(WD3_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WD3_mean = mean(WD3_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_WD3_mean = mean(WD3_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_WD3_sd = sd(WD3_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WD3_sd = sd(WD3_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WD3_sd = sd(WD3_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WD3_sd = sd(WD3_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_WD3_sd = sd(WD3_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WD3_sd = sd(WD3_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WD3_sd = sd(WD3_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WD3_sd = sd(WD3_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WD3_sd = sd(WD3_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_WD3_sd = sd(WD3_growth_pt_df$Corr_Prep,na.rm = T) 
      #end of WD3
      ###########################################################################   
      T_response_GPP_TRENDY_WD4_mean = mean(WD4_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WD4_mean = mean(WD4_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WD4_mean = mean(WD4_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WD4_mean = mean(WD4_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_WD4_mean = mean(WD4_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WD4_mean = mean(WD4_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WD4_mean = mean(WD4_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WD4_mean = mean(WD4_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WD4_mean = mean(WD4_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_WD4_mean = mean(WD4_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_WD4_sd = sd(WD4_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_WD4_sd = sd(WD4_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_WD4_sd = sd(WD4_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_WD4_sd = sd(WD4_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_WD4_sd = sd(WD4_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_WD4_sd = sd(WD4_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_WD4_sd = sd(WD4_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_WD4_sd = sd(WD4_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_WD4_sd = sd(WD4_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_WD4_sd = sd(WD4_growth_pt_df$Corr_Prep,na.rm = T) 
      #end of WD4
      ###########################################################################   
    }    
    #CW
    {
      #CW1
      T_response_GPP_TRENDY_CW1_mean = mean(CW1_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CW1_mean = mean(CW1_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CW1_mean = mean(CW1_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CW1_mean = mean(CW1_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_CW1_mean = mean(CW1_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CW1_mean = mean(CW1_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CW1_mean = mean(CW1_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CW1_mean = mean(CW1_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CW1_mean = mean(CW1_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_CW1_mean = mean(CW1_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_CW1_sd = sd(CW1_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CW1_sd = sd(CW1_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CW1_sd = sd(CW1_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CW1_sd = sd(CW1_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_CW1_sd = sd(CW1_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CW1_sd = sd(CW1_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CW1_sd = sd(CW1_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CW1_sd = sd(CW1_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CW1_sd = sd(CW1_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_CW1_sd = sd(CW1_growth_pt_df$Corr_Prep,na.rm = T)  
      #end of CW1
      ###########################################################################
      T_response_GPP_TRENDY_CW2_mean = mean(CW2_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CW2_mean = mean(CW2_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CW2_mean = mean(CW2_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CW2_mean = mean(CW2_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_CW2_mean = mean(CW2_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CW2_mean = mean(CW2_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CW2_mean = mean(CW2_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CW2_mean = mean(CW2_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CW2_mean = mean(CW2_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_CW2_mean = mean(CW2_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_CW2_sd = sd(CW2_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CW2_sd = sd(CW2_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CW2_sd = sd(CW2_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CW2_sd = sd(CW2_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_CW2_sd = sd(CW2_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CW2_sd = sd(CW2_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CW2_sd = sd(CW2_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CW2_sd = sd(CW2_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CW2_sd = sd(CW2_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_CW2_sd = sd(CW2_growth_pt_df$Corr_Prep,na.rm = T) 
      #end of CW2
      ###########################################################################
      T_response_GPP_TRENDY_CW3_mean = mean(CW3_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CW3_mean = mean(CW3_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CW3_mean = mean(CW3_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CW3_mean = mean(CW3_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_CW3_mean = mean(CW3_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CW3_mean = mean(CW3_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CW3_mean = mean(CW3_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CW3_mean = mean(CW3_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CW3_mean = mean(CW3_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_CW3_mean = mean(CW3_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_CW3_sd = sd(CW3_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CW3_sd = sd(CW3_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CW3_sd = sd(CW3_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CW3_sd = sd(CW3_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_CW3_sd = sd(CW3_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CW3_sd = sd(CW3_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CW3_sd = sd(CW3_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CW3_sd = sd(CW3_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CW3_sd = sd(CW3_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_CW3_sd = sd(CW3_growth_pt_df$Corr_Prep,na.rm = T) 
      #end of CW3
      ###########################################################################   
      T_response_GPP_TRENDY_CW4_mean = mean(CW4_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CW4_mean = mean(CW4_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CW4_mean = mean(CW4_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CW4_mean = mean(CW4_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_CW4_mean = mean(CW4_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CW4_mean = mean(CW4_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CW4_mean = mean(CW4_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CW4_mean = mean(CW4_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CW4_mean = mean(CW4_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_CW4_mean = mean(CW4_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_CW4_sd = sd(CW4_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CW4_sd = sd(CW4_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CW4_sd = sd(CW4_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CW4_sd = sd(CW4_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_CW4_sd = sd(CW4_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CW4_sd = sd(CW4_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CW4_sd = sd(CW4_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CW4_sd = sd(CW4_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CW4_sd = sd(CW4_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_CW4_sd = sd(CW4_growth_pt_df$Corr_Prep,na.rm = T) 
      #end of CW4
      ###########################################################################   
    }      
    #CD
    {
      #CD1
      T_response_GPP_TRENDY_CD1_mean = mean(CD1_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CD1_mean = mean(CD1_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CD1_mean = mean(CD1_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CD1_mean = mean(CD1_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_CD1_mean = mean(CD1_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CD1_mean = mean(CD1_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CD1_mean = mean(CD1_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CD1_mean = mean(CD1_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CD1_mean = mean(CD1_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_CD1_mean = mean(CD1_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_CD1_sd = sd(CD1_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CD1_sd = sd(CD1_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CD1_sd = sd(CD1_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CD1_sd = sd(CD1_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_CD1_sd = sd(CD1_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CD1_sd = sd(CD1_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CD1_sd = sd(CD1_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CD1_sd = sd(CD1_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CD1_sd = sd(CD1_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_CD1_sd = sd(CD1_growth_pt_df$Corr_Prep,na.rm = T)  
      #end of CD1
      ###########################################################################
      T_response_GPP_TRENDY_CD2_mean = mean(CD2_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CD2_mean = mean(CD2_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CD2_mean = mean(CD2_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CD2_mean = mean(CD2_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_CD2_mean = mean(CD2_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CD2_mean = mean(CD2_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CD2_mean = mean(CD2_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CD2_mean = mean(CD2_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CD2_mean = mean(CD2_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_CD2_mean = mean(CD2_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_CD2_sd = sd(CD2_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CD2_sd = sd(CD2_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CD2_sd = sd(CD2_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CD2_sd = sd(CD2_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_CD2_sd = sd(CD2_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CD2_sd = sd(CD2_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CD2_sd = sd(CD2_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CD2_sd = sd(CD2_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CD2_sd = sd(CD2_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_CD2_sd = sd(CD2_growth_pt_df$Corr_Prep,na.rm = T) 
      #end of CD2
      ###########################################################################
      T_response_GPP_TRENDY_CD3_mean = mean(CD3_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CD3_mean = mean(CD3_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CD3_mean = mean(CD3_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CD3_mean = mean(CD3_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_CD3_mean = mean(CD3_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CD3_mean = mean(CD3_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CD3_mean = mean(CD3_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CD3_mean = mean(CD3_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CD3_mean = mean(CD3_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_CD3_mean = mean(CD3_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_CD3_sd = sd(CD3_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CD3_sd = sd(CD3_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CD3_sd = sd(CD3_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CD3_sd = sd(CD3_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_CD3_sd = sd(CD3_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CD3_sd = sd(CD3_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CD3_sd = sd(CD3_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CD3_sd = sd(CD3_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CD3_sd = sd(CD3_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_CD3_sd = sd(CD3_growth_pt_df$Corr_Prep,na.rm = T) 
      #end of CD3
      ###########################################################################   
      T_response_GPP_TRENDY_CD4_mean = mean(CD4_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CD4_mean = mean(CD4_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CD4_mean = mean(CD4_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CD4_mean = mean(CD4_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_CD4_mean = mean(CD4_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CD4_mean = mean(CD4_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CD4_mean = mean(CD4_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CD4_mean = mean(CD4_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CD4_mean = mean(CD4_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_CD4_mean = mean(CD4_growth_pt_df$Corr_Prep,na.rm = T)      
      
      T_response_GPP_TRENDY_CD4_sd = sd(CD4_GPP_TRENDY_df$Corr_Temp,na.rm = T)
      T_response_GPP_RS_CD4_sd = sd(CD4_GPP_RS_df$Corr_Temp,na.rm = T)
      T_response_GPP_FLUXCOM_CD4_sd = sd(CD4_GPP_FLUXCOM_df$Corr_Temp,na.rm = T)
      T_response_growth_CD4_sd = sd(CD4_growth_df$Corr_Temp_mean,na.rm = T)
      T_response_growth_pt_CD4_sd = sd(CD4_growth_pt_df$Corr_Temp,na.rm = T)    
      
      P_response_GPP_TRENDY_CD4_sd = sd(CD4_GPP_TRENDY_df$Corr_Prep,na.rm = T)
      P_response_GPP_RS_CD4_sd = sd(CD4_GPP_RS_df$Corr_Prep,na.rm = T)
      P_response_GPP_FLUXCOM_CD4_sd = sd(CD4_GPP_FLUXCOM_df$Corr_Prep,na.rm = T)
      P_response_growth_CD4_sd = sd(CD4_growth_df$Corr_Prep_mean,na.rm = T)
      P_response_growth_pt_CD4_sd = sd(CD4_growth_pt_df$Corr_Prep,na.rm = T) 
      #end of CD4
      ###########################################################################
    }
  }  
}

#creat output dfs
#4 groups
{
Out_GPP_df <- data.frame(
  Zone <- c("WW","WW","WW","WD","WD","WD","CW","CW","CW","CD","CD","CD"),
  Source <- c("TRENDY","RS","FLUXCOM","TRENDY","RS","FLUXCOM","TRENDY","RS","FLUXCOM","TRENDY","RS","FLUXCOM"),
  T_Mean <- c(T_response_GPP_TRENDY_WW_mean,T_response_GPP_RS_WW_mean,T_response_GPP_FLUXCOM_WW_mean,T_response_GPP_TRENDY_WD_mean,T_response_GPP_RS_WD_mean,T_response_GPP_FLUXCOM_WD_mean,T_response_GPP_TRENDY_CW_mean,T_response_GPP_RS_CW_mean,T_response_GPP_FLUXCOM_CW_mean,T_response_GPP_TRENDY_CD_mean,T_response_GPP_RS_CD_mean,T_response_GPP_FLUXCOM_CD_mean),
  P_Mean <- c(P_response_GPP_TRENDY_WW_mean,P_response_GPP_RS_WW_mean,P_response_GPP_FLUXCOM_WW_mean,P_response_GPP_TRENDY_WD_mean,P_response_GPP_RS_WD_mean,P_response_GPP_FLUXCOM_WD_mean,P_response_GPP_TRENDY_CW_mean,P_response_GPP_RS_CW_mean,P_response_GPP_FLUXCOM_CW_mean,P_response_GPP_TRENDY_CD_mean,P_response_GPP_RS_CD_mean,P_response_GPP_FLUXCOM_CD_mean),
  T_sd <- c(T_response_GPP_TRENDY_WW_sd,T_response_GPP_RS_WW_sd,T_response_GPP_FLUXCOM_WW_sd,T_response_GPP_TRENDY_WD_sd,T_response_GPP_RS_WD_sd,T_response_GPP_FLUXCOM_WD_sd,T_response_GPP_TRENDY_CW_sd,T_response_GPP_RS_CW_sd,T_response_GPP_FLUXCOM_CW_sd,T_response_GPP_TRENDY_CD_sd,T_response_GPP_RS_CD_sd,T_response_GPP_FLUXCOM_CD_sd),
  P_sd <- c(P_response_GPP_TRENDY_WW_sd,P_response_GPP_RS_WW_sd,P_response_GPP_FLUXCOM_WW_sd,P_response_GPP_TRENDY_WD_sd,P_response_GPP_RS_WD_sd,P_response_GPP_FLUXCOM_WD_sd,P_response_GPP_TRENDY_CW_sd,P_response_GPP_RS_CW_sd,P_response_GPP_FLUXCOM_CW_sd,P_response_GPP_TRENDY_CD_sd,P_response_GPP_RS_CD_sd,P_response_GPP_FLUXCOM_CD_sd))
names(Out_GPP_df) <- c("Zone","Source","T_mean","P_mean","T_sd","P_sd")

Out_growth_df <- data.frame(
  Zone <- c("WW","WD","CW","CD"),
  Source <- c("ABI","ABI","ABI","ABI"),
  T_Mean <- c(T_response_growth_WW_mean,T_response_growth_WD_mean,T_response_growth_CW_mean,T_response_growth_CD_mean),
  P_Mean <- c(P_response_growth_WW_mean,P_response_growth_WD_mean,P_response_growth_CW_mean,P_response_growth_CD_mean),
  T_sd <- c(T_response_growth_WW_sd,T_response_growth_WD_sd,T_response_growth_CW_sd,T_response_growth_CD_sd),
  P_sd <- c(P_response_growth_WW_sd,P_response_growth_WD_sd,P_response_growth_CW_sd,P_response_growth_CD_sd))
names(Out_growth_df) <- c("Zone","Source","T_mean","P_mean","T_sd","P_sd")

Out_growth_pt_df <- data.frame(
  Zone <- c("WW","WD","CW","CD"),
  Source <- c("growth_pt","growth_pt","growth_pt","growth_pt"),
  T_Mean <- c(T_response_growth_pt_WW_mean,T_response_growth_pt_WD_mean,T_response_growth_pt_CW_mean,T_response_growth_pt_CD_mean),
  P_Mean <- c(P_response_growth_pt_WW_mean,P_response_growth_pt_WD_mean,P_response_growth_pt_CW_mean,P_response_growth_pt_CD_mean),
  T_sd <- c(T_response_growth_pt_WW_sd,T_response_growth_pt_WD_sd,T_response_growth_pt_CW_sd,T_response_growth_pt_CD_sd),
  P_sd <- c(P_response_growth_pt_WW_sd,P_response_growth_pt_WD_sd,P_response_growth_pt_CW_sd,P_response_growth_pt_CD_sd))
names(Out_growth_pt_df) <- c("Zone","Source","T_mean","P_mean","T_sd","P_sd")

#Combine the out dfs
Out_GPP_TRENDY_df <- base::subset(Out_GPP_df,Source == "TRENDY")
#Out_GPP_df$Source <- "GPP"
#Out_growth_df$Source <-"growth"
#Out_growth_pt_df$Source <- "growth_pt"

Out_df <- rbind(Out_GPP_df,Out_growth_df,Out_growth_pt_df)
Out_df1 <- rbind(Out_GPP_df,Out_growth_df)
Out_df2 <- rbind(Out_GPP_TRENDY_df,Out_growth_df)

Out_GPP_df$T_max <- Out_GPP_df$T_mean + Out_GPP_df$T_sd
Out_GPP_df$T_min <- Out_GPP_df$T_mean - Out_GPP_df$T_sd

Out_GPP_df$P_max <- Out_GPP_df$P_mean + Out_GPP_df$P_sd
Out_GPP_df$P_min <- Out_GPP_df$P_mean - Out_GPP_df$P_sd

Out_df1$T_max <- Out_df1$T_mean + Out_df1$T_sd
Out_df1$T_min <- Out_df1$T_mean - Out_df1$T_sd

Out_df1$P_max <- Out_df1$P_mean + Out_df1$P_sd
Out_df1$P_min <- Out_df1$P_mean - Out_df1$P_sd

Out_df2$T_max <- Out_df2$T_mean + Out_df2$T_sd
Out_df2$T_min <- Out_df2$T_mean - Out_df2$T_sd

Out_df2$P_max <- Out_df2$P_mean + Out_df2$P_sd
Out_df2$P_min <- Out_df2$P_mean - Out_df2$P_sd

#Out_df2$T_max[4]:1.0339039
Out_df2$T_max[4] = 1

}
#Out_df:rbind(Out_GPP_df,Out_growth_df,Out_growth_pt_df)
#Out_df1:rbind(Out_GPP_df,Out_growth_df)
#Out_df2:rbind(Out_GPP_TRENDY_df,Out_growth_df)

#16 groups
#need revision
{
  Out_GPP_df1 <- data.frame(
    Zone <- c("WW1","WW2","WW3","WW4","WD1","WD2","WD3","WD4","CW1","CW2","CW3","CW4","CD1","CD2","CD3","CD4"),
    T_Mean <- c(T_response_GPP_TRENDY_WW1_mean,T_response_GPP_TRENDY_WW2_mean,T_response_GPP_TRENDY_WW3_mean,T_response_GPP_TRENDY_WW4_mean,T_response_GPP_TRENDY_WD1_mean,T_response_GPP_TRENDY_WD2_mean,T_response_GPP_TRENDY_WD3_mean,T_response_GPP_TRENDY_WD4_mean,T_response_GPP_TRENDY_CW1_mean,T_response_GPP_TRENDY_CW2_mean,T_response_GPP_TRENDY_CW3_mean,T_response_GPP_TRENDY_CW4_mean,T_response_GPP_TRENDY_CD1_mean,T_response_GPP_TRENDY_CD2_mean,T_response_GPP_TRENDY_CD3_mean,T_response_GPP_TRENDY_CD4_mean,    T_response_GPP_RS_WW1_mean,T_response_GPP_RS_WW2_mean,T_response_GPP_RS_WW3_mean,T_response_GPP_RS_WW4_mean,T_response_GPP_RS_WD1_mean,T_response_GPP_RS_WD2_mean,T_response_GPP_RS_WD3_mean,T_response_GPP_RS_WD4_mean,T_response_GPP_RS_CW1_mean,T_response_GPP_RS_CW2_mean,T_response_GPP_RS_CW3_mean,T_response_GPP_RS_CW4_mean,T_response_GPP_RS_CD1_mean,T_response_GPP_RS_CD2_mean,T_response_GPP_RS_CD3_mean,T_response_GPP_RS_CD4_mean,T_response_GPP_FLUXCOM_WW1_mean,T_response_GPP_FLUXCOM_WW2_mean,T_response_GPP_FLUXCOM_WW3_mean,T_response_GPP_FLUXCOM_WW4_mean,T_response_GPP_FLUXCOM_WD1_mean,T_response_GPP_FLUXCOM_WD2_mean,T_response_GPP_FLUXCOM_WD3_mean,T_response_GPP_FLUXCOM_WD4_mean,T_response_GPP_FLUXCOM_CW1_mean,T_response_GPP_FLUXCOM_CW2_mean,T_response_GPP_FLUXCOM_CW3_mean,T_response_GPP_FLUXCOM_CW4_mean,T_response_GPP_FLUXCOM_CD1_mean,T_response_GPP_FLUXCOM_CD2_mean,T_response_GPP_FLUXCOM_CD3_mean,T_response_GPP_FLUXCOM_CD4_mean),
    P_Mean <- c(P_response_GPP_TRENDY_WW1_mean,P_response_GPP_TRENDY_WW2_mean,P_response_GPP_TRENDY_WW3_mean,P_response_GPP_TRENDY_WW4_mean,P_response_GPP_TRENDY_WD1_mean,P_response_GPP_TRENDY_WD2_mean,P_response_GPP_TRENDY_WD3_mean,P_response_GPP_TRENDY_WD4_mean,P_response_GPP_TRENDY_CW1_mean,P_response_GPP_TRENDY_CW2_mean,P_response_GPP_TRENDY_CW3_mean,P_response_GPP_TRENDY_CW4_mean,P_response_GPP_TRENDY_CD1_mean,P_response_GPP_TRENDY_CD2_mean,P_response_GPP_TRENDY_CD3_mean,P_response_GPP_TRENDY_CD4_mean,    P_response_GPP_RS_WW1_mean,P_response_GPP_RS_WW2_mean,P_response_GPP_RS_WW3_mean,P_response_GPP_RS_WW4_mean,P_response_GPP_RS_WD1_mean,P_response_GPP_RS_WD2_mean,P_response_GPP_RS_WD3_mean,P_response_GPP_RS_WD4_mean,P_response_GPP_RS_CW1_mean,P_response_GPP_RS_CW2_mean,P_response_GPP_RS_CW3_mean,P_response_GPP_RS_CW4_mean,P_response_GPP_RS_CD1_mean,P_response_GPP_RS_CD2_mean,P_response_GPP_RS_CD3_mean,P_response_GPP_RS_CD4_mean,P_response_GPP_FLUXCOM_WW1_mean,P_response_GPP_FLUXCOM_WW2_mean,P_response_GPP_FLUXCOM_WW3_mean,P_response_GPP_FLUXCOM_WW4_mean,P_response_GPP_FLUXCOM_WD1_mean,P_response_GPP_FLUXCOM_WD2_mean,P_response_GPP_FLUXCOM_WD3_mean,P_response_GPP_FLUXCOM_WD4_mean,P_response_GPP_FLUXCOM_CW1_mean,P_response_GPP_FLUXCOM_CW2_mean,P_response_GPP_FLUXCOM_CW3_mean,P_response_GPP_FLUXCOM_CW4_mean,P_response_GPP_FLUXCOM_CD1_mean,P_response_GPP_FLUXCOM_CD2_mean,P_response_GPP_FLUXCOM_CD3_mean,P_response_GPP_FLUXCOM_CD4_mean),
    T_sd <- c(T_response_GPP_TRENDY_WW1_sd,T_response_GPP_TRENDY_WW2_sd,T_response_GPP_TRENDY_WW3_sd,T_response_GPP_TRENDY_WW4_sd,T_response_GPP_TRENDY_WD1_sd,T_response_GPP_TRENDY_WD2_sd,T_response_GPP_TRENDY_WD3_sd,T_response_GPP_TRENDY_WD4_sd,T_response_GPP_TRENDY_CW1_sd,T_response_GPP_TRENDY_CW2_sd,T_response_GPP_TRENDY_CW3_sd,T_response_GPP_TRENDY_CW4_sd,T_response_GPP_TRENDY_CD1_sd,T_response_GPP_TRENDY_CD2_sd,T_response_GPP_TRENDY_CD3_sd,T_response_GPP_TRENDY_CD4_sd,    T_response_GPP_RS_WW1_sd,T_response_GPP_RS_WW2_sd,T_response_GPP_RS_WW3_sd,T_response_GPP_RS_WW4_sd,T_response_GPP_RS_WD1_sd,T_response_GPP_RS_WD2_sd,T_response_GPP_RS_WD3_sd,T_response_GPP_RS_WD4_sd,T_response_GPP_RS_CW1_sd,T_response_GPP_RS_CW2_sd,T_response_GPP_RS_CW3_sd,T_response_GPP_RS_CW4_sd,T_response_GPP_RS_CD1_sd,T_response_GPP_RS_CD2_sd,T_response_GPP_RS_CD3_sd,T_response_GPP_RS_CD4_sd,T_response_GPP_FLUXCOM_WW1_sd,T_response_GPP_FLUXCOM_WW2_sd,T_response_GPP_FLUXCOM_WW3_sd,T_response_GPP_FLUXCOM_WW4_sd,T_response_GPP_FLUXCOM_WD1_sd,T_response_GPP_FLUXCOM_WD2_sd,T_response_GPP_FLUXCOM_WD3_sd,T_response_GPP_FLUXCOM_WD4_sd,T_response_GPP_FLUXCOM_CW1_sd,T_response_GPP_FLUXCOM_CW2_sd,T_response_GPP_FLUXCOM_CW3_sd,T_response_GPP_FLUXCOM_CW4_sd,T_response_GPP_FLUXCOM_CD1_sd,T_response_GPP_FLUXCOM_CD2_sd,T_response_GPP_FLUXCOM_CD3_sd,T_response_GPP_FLUXCOM_CD4_sd),
    P_sd <- c(P_response_GPP_TRENDY_WW1_sd,P_response_GPP_TRENDY_WW2_sd,P_response_GPP_TRENDY_WW3_sd,P_response_GPP_TRENDY_WW4_sd,P_response_GPP_TRENDY_WD1_sd,P_response_GPP_TRENDY_WD2_sd,P_response_GPP_TRENDY_WD3_sd,P_response_GPP_TRENDY_WD4_sd,P_response_GPP_TRENDY_CW1_sd,P_response_GPP_TRENDY_CW2_sd,P_response_GPP_TRENDY_CW3_sd,P_response_GPP_TRENDY_CW4_sd,P_response_GPP_TRENDY_CD1_sd,P_response_GPP_TRENDY_CD2_sd,P_response_GPP_TRENDY_CD3_sd,P_response_GPP_TRENDY_CD4_sd,P_response_GPP_RS_WW1_sd,P_response_GPP_RS_WW2_sd,P_response_GPP_RS_WW3_sd,P_response_GPP_RS_WW4_sd,P_response_GPP_RS_WD1_sd,P_response_GPP_RS_WD2_sd,P_response_GPP_RS_WD3_sd,P_response_GPP_RS_WD4_sd,P_response_GPP_RS_CW1_sd,P_response_GPP_RS_CW2_sd,P_response_GPP_RS_CW3_sd,P_response_GPP_RS_CW4_sd,P_response_GPP_RS_CD1_sd,P_response_GPP_RS_CD2_sd,P_response_GPP_RS_CD3_sd,P_response_GPP_RS_CD4_sd,P_response_GPP_FLUXCOM_WW1_sd,P_response_GPP_FLUXCOM_WW2_sd,P_response_GPP_FLUXCOM_WW3_sd,P_response_GPP_FLUXCOM_WW4_sd,P_response_GPP_FLUXCOM_WD1_sd,P_response_GPP_FLUXCOM_WD2_sd,P_response_GPP_FLUXCOM_WD3_sd,P_response_GPP_FLUXCOM_WD4_sd,P_response_GPP_FLUXCOM_CW1_sd,P_response_GPP_FLUXCOM_CW2_sd,P_response_GPP_FLUXCOM_CW3_sd,P_response_GPP_FLUXCOM_CW4_sd,P_response_GPP_FLUXCOM_CD1_sd,P_response_GPP_FLUXCOM_CD2_sd,P_response_GPP_FLUXCOM_CD3_sd,P_response_GPP_FLUXCOM_CD4_sd),
  Type <- c(rep("TRENDY",16),rep("RS",16),rep("FLUXCOM",16))
)
  names(Out_GPP_df1) <- c("Zone","T_mean","P_mean","T_sd","P_sd","Type")
  
  Out_growth_df1 <- data.frame(
    Zone <- c("WW1","WW2","WW3","WW4","WD1","WD2","WD3","WD4","CW1","CW2","CW3","CW4","CD1","CD2","CD3","CD4"),
    T_Mean <- c(T_response_growth_WW1_mean,T_response_growth_WW2_mean,T_response_growth_WW3_mean,T_response_growth_WW4_mean,T_response_growth_WD1_mean,T_response_growth_WD2_mean,T_response_growth_WD3_mean,T_response_growth_WD4_mean,T_response_growth_CW1_mean,T_response_growth_CW2_mean,T_response_growth_CW3_mean,T_response_growth_CW4_mean,T_response_growth_CD1_mean,T_response_growth_CD2_mean,T_response_growth_CD3_mean,T_response_growth_CD4_mean),
    P_Mean <- c(P_response_growth_WW1_mean,P_response_growth_WW2_mean,P_response_growth_WW3_mean,P_response_growth_WW4_mean,P_response_growth_WD1_mean,P_response_growth_WD2_mean,P_response_growth_WD3_mean,P_response_growth_WD4_mean,P_response_growth_CW1_mean,P_response_growth_CW2_mean,P_response_growth_CW3_mean,P_response_growth_CW4_mean,P_response_growth_CD1_mean,P_response_growth_CD2_mean,P_response_growth_CD3_mean,P_response_growth_CD4_mean),
    T_sd <- c(T_response_growth_WW1_sd,T_response_growth_WW2_sd,T_response_growth_WW3_sd,T_response_growth_WW4_sd,T_response_growth_WD1_sd,T_response_growth_WD2_sd,T_response_growth_WD3_sd,T_response_growth_WD4_sd,T_response_growth_CW1_sd,T_response_growth_CW2_sd,T_response_growth_CW3_sd,T_response_growth_CW4_sd,T_response_growth_CD1_sd,T_response_growth_CD2_sd,T_response_growth_CD3_sd,T_response_growth_CD4_sd),
    P_sd <- c(P_response_growth_WW1_sd,P_response_growth_WW2_sd,P_response_growth_WW3_sd,P_response_growth_WW4_sd,P_response_growth_WD1_sd,P_response_growth_WD2_sd,P_response_growth_WD3_sd,P_response_growth_WD4_sd,P_response_growth_CW1_sd,P_response_growth_CW2_sd,P_response_growth_CW3_sd,P_response_growth_CW4_sd,P_response_growth_CD1_sd,P_response_growth_CD2_sd,P_response_growth_CD3_sd,P_response_growth_CD4_sd),
    Type <- rep("growth",16))
  
  names(Out_growth_df1) <- c("Zone","T_mean","P_mean","T_sd","P_sd","Type")
  
  Out_growth_pt_df1 <- data.frame(
    Zone <- c("WW1","WW2","WW3","WW4","WD1","WD2","WD3","WD4","CW1","CW2","CW3","CW4","CD1","CD2","CD3","CD4"),
    T_Mean <- c(T_response_growth_pt_WW1_mean,T_response_growth_pt_WW2_mean,T_response_growth_pt_WW3_mean,T_response_growth_pt_WW4_mean,T_response_growth_pt_WD1_mean,T_response_growth_pt_WD2_mean,T_response_growth_pt_WD3_mean,T_response_growth_pt_WD4_mean,T_response_growth_pt_CW1_mean,T_response_growth_pt_CW2_mean,T_response_growth_pt_CW3_mean,T_response_growth_pt_CW4_mean,T_response_growth_pt_CD1_mean,T_response_growth_pt_CD2_mean,T_response_growth_pt_CD3_mean,T_response_growth_pt_CD4_mean),
    P_Mean <- c(P_response_growth_pt_WW1_mean,P_response_growth_pt_WW2_mean,P_response_growth_pt_WW3_mean,P_response_growth_pt_WW4_mean,P_response_growth_pt_WD1_mean,P_response_growth_pt_WD2_mean,P_response_growth_pt_WD3_mean,P_response_growth_pt_WD4_mean,P_response_growth_pt_CW1_mean,P_response_growth_pt_CW2_mean,P_response_growth_pt_CW3_mean,P_response_growth_pt_CW4_mean,P_response_growth_pt_CD1_mean,P_response_growth_pt_CD2_mean,P_response_growth_pt_CD3_mean,P_response_growth_pt_CD4_mean),
    T_sd <- c(T_response_growth_pt_WW1_sd,T_response_growth_pt_WW2_sd,T_response_growth_pt_WW3_sd,T_response_growth_pt_WW4_sd,T_response_growth_pt_WD1_sd,T_response_growth_pt_WD2_sd,T_response_growth_pt_WD3_sd,T_response_growth_pt_WD4_sd,T_response_growth_pt_CW1_sd,T_response_growth_pt_CW2_sd,T_response_growth_pt_CW3_sd,T_response_growth_pt_CW4_sd,T_response_growth_pt_CD1_sd,T_response_growth_pt_CD2_sd,T_response_growth_pt_CD3_sd,T_response_growth_pt_CD4_sd),
    P_sd <- c(P_response_growth_pt_WW1_sd,P_response_growth_pt_WW2_sd,P_response_growth_pt_WW3_sd,P_response_growth_pt_WW4_sd,P_response_growth_pt_WD1_sd,P_response_growth_pt_WD2_sd,P_response_growth_pt_WD3_sd,P_response_growth_pt_WD4_sd,P_response_growth_pt_CW1_sd,P_response_growth_pt_CW2_sd,P_response_growth_pt_CW3_sd,P_response_growth_pt_CW4_sd,P_response_growth_pt_CD1_sd,P_response_growth_pt_CD2_sd,P_response_growth_pt_CD3_sd,P_response_growth_pt_CD4_sd),
    Type <- rep("growth_pt",16))
  names(Out_growth_pt_df1) <- c("Zone","T_mean","P_mean","T_sd","P_sd","Type")
  
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

#output dfs
#Out_GPP_df Out_GPP_df1 Out_growth_df Out_growth_df1 Out_growth_pt_df Out_growth_pt_df1
#

#boxplot
  test = ggplot()+
    geom_point(Out_GPP_df,mapping = aes(x = T_mean, y = P_mean,color = Source,shape = Zone)) +
    geom_errorbar(Out_GPP_df,mapping = aes(x = T_mean,ymin = P_min, ymax = P_max,color = Source))+
    geom_errorbarh(Out_GPP_df,mapping = aes(y = P_mean,xmin = T_min, xmax = T_max,color = Source))+
    xlim(-1.1,1.1)+
    ylim(-1.1,1.1)
    
#TRENDY & ABI
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Deep_learning\\random_forest\\Graph_output\\Fig4_P1_T_P_response_2022_4_15.pdf",width = 9,height = 9)
  
  p_TP_4 = ggplot(Out_df2,aes(x = T_mean, y = P_mean,group = Source))+
    geom_point(aes(color = Source, shape = Zone),size =6) +
    geom_errorbar(aes(ymin = P_min, ymax = P_max,color = Source),size =1)+
    geom_errorbarh(aes(xmin = T_min, xmax = T_max, color = Source),size =1)+
    xlim(-1,1)+
    ylim(-1,1)+
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(labels = c(bquote("ABI"[per_area]),bquote("GPP"[TRENDY])),values = c("#f37735","#00aedb"))+
    labs(x = "T_pcor",y="P_pcor")+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = c(0.85,0.2),legend.text = element_text(size = 16),legend.title=element_text(face ="bold",size=14))+
    theme(axis.text = element_text(face="bold",size =16),axis.text.x = element_text(face="bold",size =16),axis.text.y = element_text(face="bold",size =16),axis.title.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16))+
    guides(color = guide_legend(
      override.aes=list(shape = 20)))

    p_TP_4
    dev.off()

#All GPP    
    
  test2 =   ggplot(Out_GPP_df,aes(x = T_mean, y = P_mean,group = Source))+
    geom_point(aes(color = Source, shape = Zone),size =6) +
    geom_errorbar(aes(ymin = P_min, ymax = P_max,color = Source),size =1)+
    geom_errorbarh(aes(xmin = T_min, xmax = T_max, color = Source),size =1)+
    xlim(-1,1)+
    ylim(-1,1)


#density plot for T and P z-score
#create the x-y axis
#from https://stackoverflow.com/questions/17753101/center-x-and-y-axis-with-ggplot2
  library(ggplot2)
  library(magrittr)
  
  # constants
  axis_begin  <- -3
  axis_end    <- 3
  total_ticks <- 7
  
  # DATA ----
  # point to plot
  my_point <- data.frame(x=1,y=1)
  
  # chart junk data
  tick_frame <- 
    data.frame(ticks = seq(axis_begin, axis_end, length.out = total_ticks), 
               zero=0) %>%
    subset(ticks != 0)
  
  lab_frame <- data.frame(lab = seq(axis_begin, axis_end),
                          zero = 0) %>%
    subset(lab != 0)
  
  tick_sz <- (tail(lab_frame$lab, 1) -  lab_frame$lab[1]) / 128
  
  ggplot(Response_GPP_TRENDY1,aes(x = Temp_zscore, y =Prep_zscore))+
    geom_point(color = "#81c784",size = 2)+
  
    # CHART JUNK
    # y axis line
    geom_segment(x = 0, xend = 0, 
                 y = lab_frame$lab[1], yend = tail(lab_frame$lab, 1),
                 size = 1.5) +
    # x axis line
    geom_segment(y = 0, yend = 0, 
                 x = lab_frame$lab[1], xend = tail(lab_frame$lab, 1),
                 size = 1.5) +
    # x ticks
    geom_segment(data = tick_frame, 
                 aes(x = ticks, xend = ticks, 
                     y = zero, yend = zero + tick_sz)) +
    # y ticks
    geom_segment(data = tick_frame, 
                 aes(x = zero, xend = zero + tick_sz, 
                     y = ticks, yend = ticks)) + 
    
    # labels
    geom_text(data=lab_frame, aes(x=lab, y=zero, label=lab),
              family = 'Times', vjust=1.5,size =10) +
    geom_text(data=lab_frame, aes(x=zero, y=lab, label=lab),
              family = 'Times', hjust=1.5,size =10)+
    theme_void() 
    
  
  
  pdf("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Deep_learning\\random_forest\\Graph_output\\Fig4_P1_zscore_2022_4_18.pdf",width = 9,height = 9)
  
  zscore1 <- ggplot(Response_GPP_TRENDY1,aes(x = Temp_zscore, y =Prep_zscore))+
    geom_bin2d()+
    theme_bw()
  
    geom_point(aes(color = Source, shape = Zone),size =6) +
    geom_errorbar(aes(ymin = P_min, ymax = P_max,color = Source),size =1)+
    geom_errorbarh(aes(xmin = T_min, xmax = T_max, color = Source),size =1)+
    xlim(-1,1)+
    ylim(-1,1)+
    scale_shape_manual(values = c(15,16,17,18))+
    scale_color_manual(labels = c(bquote("AABI"[per_area]),bquote("GPP"[TRENDY])),values = c("#f37735","#00aedb"))+
    labs(x = "T_pcor",y="P_pcor")+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = c(0.85,0.2),legend.text = element_text(size = 16),legend.title=element_text(face ="bold",size=14))+
    theme(axis.text = element_text(face="bold",size =16),axis.text.x = element_text(face="bold",size =16),axis.text.y = element_text(face="bold",size =16),axis.title.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16))+
    guides(color = guide_legend(
      override.aes=list(shape = 20)))
  
  p_TP_4
  dev.off() 

  
  