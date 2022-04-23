#This is a test script
#Some modifications were made here by 2022/1/2
#The complete script is:
#D:\MEGA\Scripts\R\lat_bin_plot.R
#Yizhao 2022/1/2

library("ggplot2")
library("tidyverse")
library("tidyr")
library(modeest)
#site_data = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Env_corr_North_America_map\\Output_prep\\trendy_gpp_prep_tras_processed_test1.csv")

#site_data_agg = aggregate(site_data,list(Site1 = site_data$site),mean)
#smoothScatter(site_data_agg$T7,site_data_agg$Target,xlab = "T[Apr.-Oct.] /K",ylab = #"Annual biomass increment /kg per tree",xlim = c(240,310))

#updated data input
{
  data_temp_trendyS2 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_TRENDY_S2_age_correct_nbr_temp_pcorr_tras_processed_828_toshp.csv")
  
  data_prep_trendyS2 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_TRENDY_S2_age_correct_nbr_prep_pcorr_tras_processed_828_toshp.csv")
  
  data_temp_trendyS3 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_TRENDY_S3_age_correct_nbr_temp_pcorr_tras_processed_828_toshp.csv")
  
  data_prep_trendyS3 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_TRENDY_S3_age_correct_nbr_prep_pcorr_tras_processed_828_toshp.csv")  
  
  data_temp_fluxcom = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_FLUXCOM_age_correct_nbr_temp_pcorr_tras_processed_828_toshp.csv")
  
  data_prep_fluxcom = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_FLUXCOM_age_correct_nbr_prep_pcorr_tras_processed_828_toshp.csv")
  
  data_temp_eclue = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_EC_LUE_age_correct_nbr_temp_pcorr_tras_processed_828_toshp.csv")
  
  data_prep_eclue = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_EC_LUE_age_correct_nbr_prep_pcorr_tras_processed_828_toshp.csv")
  
  data_temp_beps = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_BEPS_age_correct_nbr_temp_pcorr_tras_processed_828_toshp.csv")
  
  data_prep_beps = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_BEPS_age_correct_nbr_prep_pcorr_tras_processed_828_toshp.csv")
  
  data_temp_GPPinf = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPPinf_age_correct_nbr_temp_pcorr_tras_processed_828_toshp.csv")
  
  data_prep_GPPinf = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPPinf_age_correct_nbr_prep_pcorr_tras_processed_828_toshp.csv")

#
  data_temp_sink_nbr_exp1 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_exp1_temp_pcorr_tras_processed_828_toshp.csv")
  data_temp_sink_nbr_exp2 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_exp2_temp_pcorr_tras_processed_828_toshp.csv")
  data_temp_sink_nbr_exp3 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_exp3_temp_pcorr_tras_processed_828_toshp.csv")
  data_temp_sink_nbr_linear = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_linear_temp_pcorr_tras_processed_828_toshp.csv")
  data_temp_sink_nbr_logi1 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_logi1_temp_pcorr_tras_processed_828_toshp.csv")
  data_temp_sink_nbr_logi2 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_logi2_temp_pcorr_tras_processed_828_toshp.csv")
  data_temp_sink_nbr_logi3 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_logi3_temp_pcorr_tras_processed_828_toshp.csv")
  
  data_prep_sink_nbr_exp1 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_exp1_prep_pcorr_tras_processed_828_toshp.csv")
  data_prep_sink_nbr_exp2 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_exp2_prep_pcorr_tras_processed_828_toshp.csv")
  data_prep_sink_nbr_exp3 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_exp3_prep_pcorr_tras_processed_828_toshp.csv")
  data_prep_sink_nbr_linear = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_linear_prep_pcorr_tras_processed_828_toshp.csv")
  data_prep_sink_nbr_logi1 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_logi1_prep_pcorr_tras_processed_828_toshp.csv")
  data_prep_sink_nbr_logi2 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_logi2_prep_pcorr_tras_processed_828_toshp.csv")
  data_prep_sink_nbr_logi3 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_logi3_prep_pcorr_tras_processed_828_toshp.csv")
  
  data_temp_sink_ndvi_exp1 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_exp1_temp_pcorr_tras_processed_843_toshp.csv")
  data_temp_sink_ndvi_exp2 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_exp2_temp_pcorr_tras_processed_843_toshp.csv")
  data_temp_sink_ndvi_exp3 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_exp3_temp_pcorr_tras_processed_843_toshp.csv")
  data_temp_sink_ndvi_linear = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_linear_temp_pcorr_tras_processed_843_toshp.csv")
  data_temp_sink_ndvi_logi1 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_logi1_temp_pcorr_tras_processed_843_toshp.csv")
  data_temp_sink_ndvi_logi2 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_logi2_temp_pcorr_tras_processed_843_toshp.csv")
  data_temp_sink_ndvi_logi3 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_logi3_temp_pcorr_tras_processed_843_toshp.csv")
  
  data_prep_sink_ndvi_exp1 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_exp1_prep_pcorr_tras_processed_843_toshp.csv")
  data_prep_sink_ndvi_exp2 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_exp2_prep_pcorr_tras_processed_843_toshp.csv")
  data_prep_sink_ndvi_exp3 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_exp3_prep_pcorr_tras_processed_843_toshp.csv")
  data_prep_sink_ndvi_linear = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_linear_prep_pcorr_tras_processed_843_toshp.csv")
  data_prep_sink_ndvi_logi1 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_logi1_prep_pcorr_tras_processed_843_toshp.csv")
  data_prep_sink_ndvi_logi2 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_logi2_prep_pcorr_tras_processed_843_toshp.csv")
  data_prep_sink_ndvi_logi3 = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_logi3_prep_pcorr_tras_processed_843_toshp.csv")

}

#calculate means for sink nbr and ndvi
#nbr temp
{
data_temp_sink_nbr_mean <- data_temp_sink_nbr_exp1
#Corr
df_temp_nbr_Corr <- data.frame(data_temp_sink_nbr_exp1$Corr,data_temp_sink_nbr_exp2$Corr,data_temp_sink_nbr_exp3$Corr,data_temp_sink_nbr_linear$Corr,data_temp_sink_nbr_logi1$Corr,data_temp_sink_nbr_logi2$Corr,data_temp_sink_nbr_logi3$Corr)

#set -999 to NA
for (i in 1:length(df_temp_nbr_Corr)){
  df_temp_nbr_Corr[,i][which(df_temp_nbr_Corr[,i] == -999)] = NA
}

df_temp_nbr_Corr$mean <- rowMeans(df_temp_nbr_Corr,na.rm = T)

#T_start
df_temp_nbr_T_start <- data.frame(data_temp_sink_nbr_exp1$T_start,data_temp_sink_nbr_exp2$T_start,data_temp_sink_nbr_exp3$T_start,data_temp_sink_nbr_linear$T_start,data_temp_sink_nbr_logi1$T_start,data_temp_sink_nbr_logi2$T_start,data_temp_sink_nbr_logi3$T_start)

#set -999 to NA
for (i in 1:length(df_temp_nbr_T_start)){
  df_temp_nbr_T_start[,i][which(df_temp_nbr_T_start[,i] == -999)] = NA
}
#get means
df_temp_nbr_T_start$mean <- rowMeans(df_temp_nbr_T_start,na.rm = T)
#get int with round
df_temp_nbr_T_start$mean = round(df_temp_nbr_T_start$mean)


#T_end
df_temp_nbr_T_end <- data.frame(data_temp_sink_nbr_exp1$T_end,data_temp_sink_nbr_exp2$T_end,data_temp_sink_nbr_exp3$T_end,data_temp_sink_nbr_linear$T_end,data_temp_sink_nbr_logi1$T_end,data_temp_sink_nbr_logi2$T_end,data_temp_sink_nbr_logi3$T_end)

#set -999 to NA
for (i in 1:length(df_temp_nbr_T_end)){
  df_temp_nbr_T_end[,i][which(df_temp_nbr_T_end[,i] == -999)] = NA
}
#get means
df_temp_nbr_T_end$mean <- rowMeans(df_temp_nbr_T_end,na.rm = T)
#get int with round
df_temp_nbr_T_end$mean = round(df_temp_nbr_T_end$mean)

#T_mark
df_temp_nbr_T_mark <- data.frame(data_temp_sink_nbr_exp1$T_mark,data_temp_sink_nbr_exp2$T_mark,data_temp_sink_nbr_exp3$T_mark,data_temp_sink_nbr_linear$T_mark,data_temp_sink_nbr_logi1$T_mark,data_temp_sink_nbr_logi2$T_mark,data_temp_sink_nbr_logi3$T_mark)

#set -999 to NA
for (i in 1:length(df_temp_nbr_T_mark)){
  df_temp_nbr_T_mark[,i][which(df_temp_nbr_T_mark[,i] == -999)] = NA
}
#get mode for T_mark
df_temp_nbr_T_mark$mode <- apply(df_temp_nbr_T_mark[ ,1:length(df_temp_nbr_T_mark)], 1, mfv)

data_temp_sink_nbr_mean$Corr = df_temp_nbr_Corr$mean
data_temp_sink_nbr_mean$T_start = df_temp_nbr_T_start$mean
data_temp_sink_nbr_mean$T_end = df_temp_nbr_T_end$mean
data_temp_sink_nbr_mean$T_mark = df_temp_nbr_T_mark$mode
}

#nbr prep
{
  data_prep_sink_nbr_mean <- data_prep_sink_nbr_exp1
  #Corr
  df_prep_nbr_Corr <- data.frame(data_prep_sink_nbr_exp1$Corr,data_prep_sink_nbr_exp2$Corr,data_prep_sink_nbr_exp3$Corr,data_prep_sink_nbr_linear$Corr,data_prep_sink_nbr_logi1$Corr,data_prep_sink_nbr_logi2$Corr,data_prep_sink_nbr_logi3$Corr)
  
  #set -999 to NA
  for (i in 1:length(df_prep_nbr_Corr)){
    df_prep_nbr_Corr[,i][which(df_prep_nbr_Corr[,i] == -999)] = NA
  }
  
  df_prep_nbr_Corr$mean <- rowMeans(df_prep_nbr_Corr,na.rm = T)
  
  #T_start
  df_prep_nbr_T_start <- data.frame(data_prep_sink_nbr_exp1$T_start,data_prep_sink_nbr_exp2$T_start,data_prep_sink_nbr_exp3$T_start,data_prep_sink_nbr_linear$T_start,data_prep_sink_nbr_logi1$T_start,data_prep_sink_nbr_logi2$T_start,data_prep_sink_nbr_logi3$T_start)
  
  #set -999 to NA
  for (i in 1:length(df_prep_nbr_T_start)){
    df_prep_nbr_T_start[,i][which(df_prep_nbr_T_start[,i] == -999)] = NA
  }
  #get means
  df_prep_nbr_T_start$mean <- rowMeans(df_prep_nbr_T_start,na.rm = T)
  #get int with round
  df_prep_nbr_T_start$mean = round(df_prep_nbr_T_start$mean)
  
  
  #T_end
  df_prep_nbr_T_end <- data.frame(data_prep_sink_nbr_exp1$T_end,data_prep_sink_nbr_exp2$T_end,data_prep_sink_nbr_exp3$T_end,data_prep_sink_nbr_linear$T_end,data_prep_sink_nbr_logi1$T_end,data_prep_sink_nbr_logi2$T_end,data_prep_sink_nbr_logi3$T_end)
  
  #set -999 to NA
  for (i in 1:length(df_prep_nbr_T_end)){
    df_prep_nbr_T_end[,i][which(df_prep_nbr_T_end[,i] == -999)] = NA
  }
  #get means
  df_prep_nbr_T_end$mean <- rowMeans(df_prep_nbr_T_end,na.rm = T)
  #get int with round
  df_prep_nbr_T_end$mean = round(df_prep_nbr_T_end$mean)
  
  #T_mark
  df_prep_nbr_T_mark <- data.frame(data_prep_sink_nbr_exp1$T_mark,data_prep_sink_nbr_exp2$T_mark,data_prep_sink_nbr_exp3$T_mark,data_prep_sink_nbr_linear$T_mark,data_prep_sink_nbr_logi1$T_mark,data_prep_sink_nbr_logi2$T_mark,data_prep_sink_nbr_logi3$T_mark)
  
  #set -999 to NA
  for (i in 1:length(df_prep_nbr_T_mark)){
    df_prep_nbr_T_mark[,i][which(df_prep_nbr_T_mark[,i] == -999)] = NA
  }
  #get mode for T_mark
  df_prep_nbr_T_mark$mode <- apply(df_prep_nbr_T_mark[ ,1:length(df_prep_nbr_T_mark)], 1, mfv)
  
  data_prep_sink_nbr_mean$Corr = df_prep_nbr_Corr$mean
  data_prep_sink_nbr_mean$T_start = df_prep_nbr_T_start$mean
  data_prep_sink_nbr_mean$T_end = df_prep_nbr_T_end$mean
  data_prep_sink_nbr_mean$T_mark = df_prep_nbr_T_mark$mode
}

#ndvi temp
{
  data_temp_sink_ndvi_mean <- data_temp_sink_ndvi_exp1
  #Corr
  df_temp_ndvi_Corr <- data.frame(data_temp_sink_ndvi_exp1$Corr,data_temp_sink_ndvi_exp2$Corr,data_temp_sink_ndvi_exp3$Corr,data_temp_sink_ndvi_linear$Corr,data_temp_sink_ndvi_logi1$Corr,data_temp_sink_ndvi_logi2$Corr,data_temp_sink_ndvi_logi3$Corr)
  
  #set -999 to NA
  for (i in 1:length(df_temp_ndvi_Corr)){
    df_temp_ndvi_Corr[,i][which(df_temp_ndvi_Corr[,i] == -999)] = NA
  }
  
  df_temp_ndvi_Corr$mean <- rowMeans(df_temp_ndvi_Corr,na.rm = T)
  
  #T_start
  df_temp_ndvi_T_start <- data.frame(data_temp_sink_ndvi_exp1$T_start,data_temp_sink_ndvi_exp2$T_start,data_temp_sink_ndvi_exp3$T_start,data_temp_sink_ndvi_linear$T_start,data_temp_sink_ndvi_logi1$T_start,data_temp_sink_ndvi_logi2$T_start,data_temp_sink_ndvi_logi3$T_start)
  
  #set -999 to NA
  for (i in 1:length(df_temp_ndvi_T_start)){
    df_temp_ndvi_T_start[,i][which(df_temp_ndvi_T_start[,i] == -999)] = NA
  }
  #get means
  df_temp_ndvi_T_start$mean <- rowMeans(df_temp_ndvi_T_start,na.rm = T)
  #get int with round
  df_temp_ndvi_T_start$mean = round(df_temp_ndvi_T_start$mean)
  
  
  #T_end
  df_temp_ndvi_T_end <- data.frame(data_temp_sink_ndvi_exp1$T_end,data_temp_sink_ndvi_exp2$T_end,data_temp_sink_ndvi_exp3$T_end,data_temp_sink_ndvi_linear$T_end,data_temp_sink_ndvi_logi1$T_end,data_temp_sink_ndvi_logi2$T_end,data_temp_sink_ndvi_logi3$T_end)
  
  #set -999 to NA
  for (i in 1:length(df_temp_ndvi_T_end)){
    df_temp_ndvi_T_end[,i][which(df_temp_ndvi_T_end[,i] == -999)] = NA
  }
  #get means
  df_temp_ndvi_T_end$mean <- rowMeans(df_temp_ndvi_T_end,na.rm = T)
  #get int with round
  df_temp_ndvi_T_end$mean = round(df_temp_ndvi_T_end$mean)
  
  #T_mark
  df_temp_ndvi_T_mark <- data.frame(data_temp_sink_ndvi_exp1$T_mark,data_temp_sink_ndvi_exp2$T_mark,data_temp_sink_ndvi_exp3$T_mark,data_temp_sink_ndvi_linear$T_mark,data_temp_sink_ndvi_logi1$T_mark,data_temp_sink_ndvi_logi2$T_mark,data_temp_sink_ndvi_logi3$T_mark)
  
  #set -999 to NA
  for (i in 1:length(df_temp_ndvi_T_mark)){
    df_temp_ndvi_T_mark[,i][which(df_temp_ndvi_T_mark[,i] == -999)] = NA
  }
  #get mode for T_mark
  df_temp_ndvi_T_mark$mode <- apply(df_temp_ndvi_T_mark[ ,1:length(df_temp_ndvi_T_mark)], 1, mfv)
  
  data_temp_sink_ndvi_mean$Corr = df_temp_ndvi_Corr$mean
  data_temp_sink_ndvi_mean$T_start = df_temp_ndvi_T_start$mean
  data_temp_sink_ndvi_mean$T_end = df_temp_ndvi_T_end$mean
  data_temp_sink_ndvi_mean$T_mark = df_temp_ndvi_T_mark$mode
}

#ndvi prep
{
  data_prep_sink_ndvi_mean <- data_prep_sink_ndvi_exp1
  #Corr
  df_prep_ndvi_Corr <- data.frame(data_prep_sink_ndvi_exp1$Corr,data_prep_sink_ndvi_exp2$Corr,data_prep_sink_ndvi_exp3$Corr,data_prep_sink_ndvi_linear$Corr,data_prep_sink_ndvi_logi1$Corr,data_prep_sink_ndvi_logi2$Corr,data_prep_sink_ndvi_logi3$Corr)
  
  #set -999 to NA
  for (i in 1:length(df_prep_ndvi_Corr)){
    df_prep_ndvi_Corr[,i][which(df_prep_ndvi_Corr[,i] == -999)] = NA
  }
  
  df_prep_ndvi_Corr$mean <- rowMeans(df_prep_ndvi_Corr,na.rm = T)
  
  #T_start
  df_prep_ndvi_T_start <- data.frame(data_prep_sink_ndvi_exp1$T_start,data_prep_sink_ndvi_exp2$T_start,data_prep_sink_ndvi_exp3$T_start,data_prep_sink_ndvi_linear$T_start,data_prep_sink_ndvi_logi1$T_start,data_prep_sink_ndvi_logi2$T_start,data_prep_sink_ndvi_logi3$T_start)
  
  #set -999 to NA
  for (i in 1:length(df_prep_ndvi_T_start)){
    df_prep_ndvi_T_start[,i][which(df_prep_ndvi_T_start[,i] == -999)] = NA
  }
  #get means
  df_prep_ndvi_T_start$mean <- rowMeans(df_prep_ndvi_T_start,na.rm = T)
  #get int with round
  df_prep_ndvi_T_start$mean = round(df_prep_ndvi_T_start$mean)
  
  
  #T_end
  df_prep_ndvi_T_end <- data.frame(data_prep_sink_ndvi_exp1$T_end,data_prep_sink_ndvi_exp2$T_end,data_prep_sink_ndvi_exp3$T_end,data_prep_sink_ndvi_linear$T_end,data_prep_sink_ndvi_logi1$T_end,data_prep_sink_ndvi_logi2$T_end,data_prep_sink_ndvi_logi3$T_end)
  
  #set -999 to NA
  for (i in 1:length(df_prep_ndvi_T_end)){
    df_prep_ndvi_T_end[,i][which(df_prep_ndvi_T_end[,i] == -999)] = NA
  }
  #get means
  df_prep_ndvi_T_end$mean <- rowMeans(df_prep_ndvi_T_end,na.rm = T)
  #get int with round
  df_prep_ndvi_T_end$mean = round(df_prep_ndvi_T_end$mean)
  
  #T_mark
  df_prep_ndvi_T_mark <- data.frame(data_prep_sink_ndvi_exp1$T_mark,data_prep_sink_ndvi_exp2$T_mark,data_prep_sink_ndvi_exp3$T_mark,data_prep_sink_ndvi_linear$T_mark,data_prep_sink_ndvi_logi1$T_mark,data_prep_sink_ndvi_logi2$T_mark,data_prep_sink_ndvi_logi3$T_mark)
  
  #set -999 to NA
  for (i in 1:length(df_prep_ndvi_T_mark)){
    df_prep_ndvi_T_mark[,i][which(df_prep_ndvi_T_mark[,i] == -999)] = NA
  }
  #get mode for T_mark
  df_prep_ndvi_T_mark$mode <- apply(df_prep_ndvi_T_mark[ ,1:length(df_prep_ndvi_T_mark)], 1, mfv)
  
  data_prep_sink_ndvi_mean$Corr = df_prep_ndvi_Corr$mean
  data_prep_sink_ndvi_mean$T_start = df_prep_ndvi_T_start$mean
  data_prep_sink_ndvi_mean$T_end = df_prep_ndvi_T_end$mean
  data_prep_sink_ndvi_mean$T_mark = df_prep_ndvi_T_mark$mode
}

#write them out
write.csv(data_temp_sink_nbr_mean,"E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_mean_temp_pcorr_tras_processed_828_toshp.csv",row.names = F)
write.csv(data_prep_sink_nbr_mean,"E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_mean_prep_pcorr_tras_processed_828_toshp.csv",row.names = F)
write.csv(data_temp_sink_ndvi_mean,"E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_mean_temp_pcorr_tras_processed_843_toshp.csv",row.names = F)
write.csv(data_prep_sink_ndvi_mean,"E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_mean_prep_pcorr_tras_processed_843_toshp.csv",row.names = F)

#remove NA & -999 lines
{
  data1_temp_trendyS2 = data_temp_trendyS2[-which(data_temp_trendyS2$T_mark == -999),]
  data1_prep_trendyS2 =   data_prep_trendyS2[-which(data_prep_trendyS2$T_mark == -999),]
  
  data1_temp_trendyS3 = data_temp_trendyS3[-which(data_temp_trendyS3$T_mark == -999),]
  data1_prep_trendyS3 =   data_prep_trendyS3[-which(data_prep_trendyS3$T_mark == -999),]
  
  data1_temp_fluxcom = data_temp_fluxcom[-which(data_temp_fluxcom$T_mark == -999),]
  data1_prep_fluxcom = data_prep_fluxcom[-which(data_prep_fluxcom$T_mark == -999),]
  
  data1_temp_eclue = data_temp_eclue[-which(data_temp_eclue$T_mark == -999),]
  data1_prep_eclue = data_prep_eclue[-which(data_prep_eclue$T_mark == -999),]
  
  data1_temp_beps = data_temp_beps[-which(data_temp_beps$T_mark == -999),]
  data1_prep_beps = data_prep_beps[-which(data_prep_beps$T_mark == -999),]
  
  data1_temp_GPPinf = data_temp_GPPinf[-which(data_temp_GPPinf$T_mark == -999),]
  data1_prep_GPPinf = data_prep_GPPinf[-which(data_prep_GPPinf$T_mark == -999),]
  
  
  data1_temp_sink_nbr_mean = data_temp_sink_nbr_mean[!is.na(data_temp_sink_nbr_mean$T_mark),]
  data1_prep_sink_nbr_mean = data_prep_sink_nbr_mean[!is.na(data_prep_sink_nbr_mean$T_mark),]  
  
  data1_temp_sink_ndvi_mean = data_temp_sink_ndvi_mean[!is.na(data_temp_sink_ndvi_mean$T_mark),]
  data1_prep_sink_ndvi_mean = data_prep_sink_ndvi_mean[!is.na(data_prep_sink_ndvi_mean$T_mark),] 
}


#old data input
{
#data_temp_trendy = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\trendy\\Point_temp_trendy_test1.csv")

#data_prep_trendy = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\trendy\\Point_prep_trendy_test1.csv")

#data_temp_fluxcom = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\fluxcom\\Point_temp_fluxcom.csv")

#data_prep_fluxcom = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\fluxcom\\Point_prep_fluxcom.csv")

#data_temp_eclue = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\eclue\\Point_temp_eclue.csv")

#data_prep_eclue = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\eclue\\Point_prep_eclue.csv")

#data_temp_sink = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink\\Point_temp_sink.csv")

#data_prep_sink = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink\\Point_prep_sink.csv")
  
  #remove -999 lines
  {
    data1_temp_trendy = data_temp_trendy[-which(data_temp_trendy$T_mark == -999),]
    data1_prep_trendy = data_prep_trendy[-which(data_prep_trendy$T_mark == -999),]
    
    data1_temp_fluxcom = data_temp_fluxcom[-which(data_temp_fluxcom$T_mark == -999),]
    data1_prep_fluxcom = data_prep_fluxcom[-which(data_prep_fluxcom$T_mark == -999),]
    
    data1_temp_eclue = data_temp_eclue[-which(data_temp_eclue$T_mark == -999),]
    data1_prep_eclue = data_prep_eclue[-which(data_prep_eclue$T_mark == -999),]
    
    data1_temp_sink = data_temp_sink[-which(data_temp_sink$T_mark == -999),]
    data1_prep_sink = data_prep_sink[-which(data_prep_sink$T_mark == -999),]
  }
}



#bulid subsets for north america only
subset_bin <- function(x){
list = list()
for (i in 1:22){
  sub1 = base::subset(x,lat > (26+2*(i-1)) & lat <= (28+2*i))
  sub1_list = c()
for (j in 1:length(sub1$lat)){
  sub1_list = append(sub1_list,c(sub1$T_start[j]:sub1$T_end[j]))
}
list[[i]] = sub1_list
}

bin_df = setNames(do.call(cbind.data.frame, lapply(lapply(list, unlist), `length<-`, max(lengths(list)))), paste0("V", 1:22))

return(bin_df)
}

data1 = data1_temp_sink_nbr_mean
bin_df = subset_bin(data1)
bin_df_long = gather(bin_df,bin,out)

#subset for corr
{
  list_corr = list()
  for (i in 1:22){
    sub1_corr = base::subset(data1,lat > (26+2*(i-1)) & lat <= (28+2*i))
    sub1_list_corr = c()
    for (j in 1:length(sub1_corr$lat)){
      sub1_list_corr = append(sub1_list_corr,sub1_corr$Corr)
    }
    list_corr[[i]] = sub1_list_corr
  }
  
  
  bin_df_corr = setNames(do.call(cbind.data.frame, lapply(lapply(list_corr, unlist), `length<-`, max(lengths(list_corr)))), paste0("V", 1:22))
  
  bin_df_corr_long = gather(bin_df_corr,bin_corr,out_corr)
}


#smoothScatter(data$T_mean,data$North_America_per_tree,xlab = "T[Apr.-Oct.] /K",ylab = "Annual biomass increment /kg per tree",xlim = c(240,310))

#data binning
# set up cut-off values 
breaks <- seq(26,70,2)

# specify interval/bin labels
tags <- c("[26-28)","[28-30)", "[30-32)", "[32-34)", "[34-36)", "[36-38)","[38-40)", "[40-42)","[42-44)","[44-46)","[46-48)","[48-50)","[50-52)","[52-54)","[54-56)","[56-58)","[58-60)","[60-62)","[62-64)","[64-66)","[66-68)","[68-70)")

tags1 <- c("27","29","31","33","35","37","39","41","43","45","47","49","51","53","55","57","59","61","63","65","67","69")

# bucketing values into bins
group_tags <- cut(data1$lat, 
                  breaks=breaks, 
                  include.lowest=FALSE, 
                  right=FALSE, 
                  labels=tags)
# inspect bins
summary(group_tags)

#temp_group <- factor(group_tags,levels = labels,ordered = TRUE)

v <- bin_df_long %>% select(bin,out)
vgroup <- as_tibble(v) %>% 
  mutate(tag = case_when(
    bin == "V1" ~ tags1[1],
    bin == "V2" ~ tags1[2],
    bin == "V3" ~ tags1[3],
    bin == "V4" ~ tags1[4],
    bin == "V5" ~ tags1[5],
    bin == "V6" ~ tags1[6],
    bin == "V7" ~ tags1[7],
    bin == "V8" ~ tags1[8],
    bin == "V9"  ~ tags1[9],
    bin == "V10" ~ tags1[10],
    bin == "V11" ~ tags1[11],
    bin == "V12" ~ tags1[12],
    bin == "V13" ~ tags1[13],
    bin == "V14" ~ tags1[14],
    bin == "V15"  ~ tags1[15],    
    bin == "V16"  ~ tags1[16],
    bin == "V17" ~ tags1[17],
    bin == "V18" ~ tags1[18],
    bin == "V19" ~ tags1[19],
    bin == "V20" ~ tags1[20],
    bin == "V21" ~ tags1[21],
    bin == "V22"  ~ tags1[22], 
  ))

ggplot(data = vgroup, mapping = aes(x=tag,y=out)) + 
  #geom_jitter(aes(color='blue'),alpha=0.2) +
  stat_smooth(method="loess", span=0.1, se=TRUE, aes(fill=bin), alpha=0.3) +
  theme_bw()
  geom_boxplot(fill="bisque",color="black",alpha=0.3) + 
  labs(x='T_mean[Apr.-Oct.] /K',y='Annual biomass increment /kg per tree') +
  guides(color=FALSE) +
  theme_minimal()+
  coord_flip()


  bin_df_mean = apply(bin_df[1:nrow(bin_df),],mean,na.rm=TRUE)

  ggplot()+
    geom_ribbon(data = bin44_df,aes(x=c(1:43),ymin= col_min, 
                                                ymax= col_max),fill = "gray",alpha = 0.4)+
    geom_line(data = bin44_df,aes(x = c(1:43), y=col_mean))+
    ylim(-12,11)+
    coord_flip()

  col_mean = apply(bin_df,2,mean,na.rm=TRUE)
  col_sd = apply(bin_df,2,sd,na.rm=TRUE) 
  col_max = col_mean + col_sd
  col_min = col_mean - col_sd
  bin44_df=data.frame(col_mean,col_sd,col_max,col_min)
  
  
  
#corr  
  col_mean = apply(bin_df_corr,2,mean,na.rm=TRUE)
  col_sd = apply(bin_df_corr,2,sd,na.rm=TRUE) 
  col_max = col_mean + col_sd
  col_min = col_mean - col_sd
  bin22_sink_corr_df=data.frame(col_mean,col_sd,col_max,col_min)
  
  
  ggplot()+
    geom_ribbon(data = bin22_sink_corr_df,aes(x=c(1:22),ymin= col_min, 
                                    ymax= col_max),fill = "gray",alpha = 0.4)+
    geom_line(data = bin22_sink_corr_df,aes(x = c(1:22), y=col_mean))+
    ylim(-1,1)+
    coord_flip()

  
  bin22_trendy_corr_df=data.frame(col_mean,col_sd,col_max,col_min)
  ggplot()+
    geom_ribbon(data = bin22_trendy_corr_df,aes(x=c(1:22),ymin= col_min, 
                                              ymax= col_max),fill = "gray",alpha = 0.4)+
    geom_line(data = bin22_trendy_corr_df,aes(x = c(1:22), y=col_mean))+
    ylim(-1,1)+
    coord_flip()