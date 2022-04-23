library("ggplot2")
library("tidyverse")
library("tidyr")
library(modeest)
#site_data = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Env_corr_North_America_map\\Output_prep\\trendy_gpp_prep_tras_processed_test1.csv")

#site_data_agg = aggregate(site_data,list(Site1 = site_data$site),mean)
#smoothScatter(site_data_agg$T7,site_data_agg$Target,xlab = "T[Apr.-Oct.] /K",ylab = #"Annual biomass increment /kg per tree",xlim = c(240,310))


#old data input
{
#normal corr
{
data_temp_trendy = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\trendy\\Point_temp_trendy_test1.csv")

data_prep_trendy = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\trendy\\Point_prep_trendy_test1.csv")

data_temp_fluxcom = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\fluxcom\\Point_temp_fluxcom.csv")

data_prep_fluxcom = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\fluxcom\\Point_prep_fluxcom.csv")

data_temp_eclue = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\eclue\\Point_temp_eclue.csv")

data_prep_eclue = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\eclue\\Point_prep_eclue.csv")

data_temp_sink = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink\\Point_temp_sink.csv")

data_prep_sink = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink\\Point_prep_sink.csv")
}

#pcorr
{
  data_temp_trendy = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\trendy\\Point_temp_trendy_pcorr.csv")
  
  data_prep_trendy = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\trendy\\Point_prep_trendy_pcorr.csv")
  
  data_temp_fluxcom = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\fluxcom\\Point_temp_fluxcom_pcorr.csv")
  
  data_prep_fluxcom = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\fluxcom\\Point_prep_fluxcom_pcorr.csv")
  
  data_temp_eclue = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\eclue\\Point_temp_eclue_pcorr.csv")
  
  data_prep_eclue = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\eclue\\Point_prep_eclue_pcorr.csv")
  
  data_temp_sink = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink\\Point_temp_sink_pcorr.csv")
  
  data_prep_sink = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\ITRDB\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink\\Point_prep_sink_pcorr.csv")
}

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
  
  data_temp_RS_mean = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_RS_mean_temp_pcorr_tras_processed_toshp.csv")
  
  data_prep_RS_mean = read.csv("E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_RS_mean_prep_pcorr_tras_processed_toshp.csv")  
  
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

#calculate means for TRENDY maybe used later
#temp
{
  data_temp_TRENDY_nbr_mean <- data_temp_trendyS2
  #Corr
  df_temp_TRENDY_mean_Corr <- data.frame(data_temp_trendyS2$Corr,data_temp_trendyS3$Corr)
  
  #set -999 to NA
  for (i in 1:length(df_temp_TRENDY_mean_Corr)){
    df_temp_TRENDY_mean_Corr[,i][which(df_temp_TRENDY_mean_Corr[,i] == -999)] = NA
  }
  
  df_temp_TRENDY_mean_Corr$mean <- rowMeans(df_temp_TRENDY_mean_Corr,na.rm = T)
  
  #T_start
  df_temp_TRENDY_mean_T_start <- data.frame(data_temp_trendyS2$T_start,data_temp_trendyS3$T_start)
  
  #set -999 to NA
  for (i in 1:length(df_temp_TRENDY_mean_T_start)){
    df_temp_TRENDY_mean_T_start[,i][which(df_temp_TRENDY_mean_T_start[,i] == -999)] = NA
  }
  #get means
  df_temp_TRENDY_mean_T_start$mean <- rowMeans(df_temp_TRENDY_mean_T_start,na.rm = T)
  #get int with round
  df_temp_TRENDY_mean_T_start$mean = round(df_temp_TRENDY_mean_T_start$mean)
  
  
  #T_end
  df_temp_TRENDY_mean_T_end <- data.frame(data_temp_trendyS2$T_end,data_temp_trendyS3$T_end)
  
  #set -999 to NA
  for (i in 1:length(df_temp_TRENDY_mean_T_end)){
    df_temp_TRENDY_mean_T_end[,i][which(df_temp_TRENDY_mean_T_end[,i] == -999)] = NA
  }
  #get means
  df_temp_TRENDY_mean_T_end$mean <- rowMeans(df_temp_TRENDY_mean_T_end,na.rm = T)
  #get int with round
  df_temp_TRENDY_mean_T_end$mean = round(df_temp_TRENDY_mean_T_end$mean)
  
  #T_mark
  df_temp_TRENDY_mean_T_mark <- data.frame(data_temp_trendyS2$T_mark,data_temp_trendyS3$T_mark)
  
  #set -999 to NA
  for (i in 1:length(df_temp_TRENDY_mean_T_mark)){
    df_temp_TRENDY_mean_T_mark[,i][which(df_temp_TRENDY_mean_T_mark[,i] == -999)] = NA
  }
  #get mode for T_mark
  df_temp_TRENDY_mean_T_mark$mode <- apply(df_temp_TRENDY_mean_T_mark[ ,1:length(df_temp_TRENDY_mean_T_mark)], 1, mfv)
  
  data_temp_TRENDY_nbr_mean$Corr = df_temp_TRENDY_mean_Corr$mean
  data_temp_TRENDY_nbr_mean$T_start = df_temp_TRENDY_mean_T_start$mean
  data_temp_TRENDY_nbr_mean$T_end = df_temp_TRENDY_mean_T_end$mean
  data_temp_TRENDY_nbr_mean$T_mark = df_temp_TRENDY_mean_T_mark$mode
}
#prep
{
  data_prep_TRENDY_nbr_mean <- data_prep_trendyS2
  #Corr
  df_prep_TRENDY_mean_Corr <- data.frame(data_prep_trendyS2$Corr,data_prep_trendyS3$Corr)
  
  #set -999 to NA
  for (i in 1:length(df_prep_TRENDY_mean_Corr)){
    df_prep_TRENDY_mean_Corr[,i][which(df_prep_TRENDY_mean_Corr[,i] == -999)] = NA
  }
  
  df_prep_TRENDY_mean_Corr$mean <- rowMeans(df_prep_TRENDY_mean_Corr,na.rm = T)
  
  #T_start
  df_prep_TRENDY_mean_T_start <- data.frame(data_prep_trendyS2$T_start,data_prep_trendyS3$T_start)
  
  #set -999 to NA
  for (i in 1:length(df_prep_TRENDY_mean_T_start)){
    df_prep_TRENDY_mean_T_start[,i][which(df_prep_TRENDY_mean_T_start[,i] == -999)] = NA
  }
  #get means
  df_prep_TRENDY_mean_T_start$mean <- rowMeans(df_prep_TRENDY_mean_T_start,na.rm = T)
  #get int with round
  df_prep_TRENDY_mean_T_start$mean = round(df_prep_TRENDY_mean_T_start$mean)
  
  
  #T_end
  df_prep_TRENDY_mean_T_end <- data.frame(data_prep_trendyS2$T_end,data_prep_trendyS3$T_end)
  
  #set -999 to NA
  for (i in 1:length(df_prep_TRENDY_mean_T_end)){
    df_prep_TRENDY_mean_T_end[,i][which(df_prep_TRENDY_mean_T_end[,i] == -999)] = NA
  }
  #get means
  df_prep_TRENDY_mean_T_end$mean <- rowMeans(df_prep_TRENDY_mean_T_end,na.rm = T)
  #get int with round
  df_prep_TRENDY_mean_T_end$mean = round(df_prep_TRENDY_mean_T_end$mean)
  
  #T_mark
  df_prep_TRENDY_mean_T_mark <- data.frame(data_prep_trendyS2$T_mark,data_prep_trendyS3$T_mark)
  
  #set -999 to NA
  for (i in 1:length(df_prep_TRENDY_mean_T_mark)){
    df_prep_TRENDY_mean_T_mark[,i][which(df_prep_TRENDY_mean_T_mark[,i] == -999)] = NA
  }
  #get mode for T_mark
  df_prep_TRENDY_mean_T_mark$mode <- apply(df_prep_TRENDY_mean_T_mark[ ,1:length(df_prep_TRENDY_mean_T_mark)], 1, mfv)
  
  data_prep_TRENDY_nbr_mean$Corr = df_prep_TRENDY_mean_Corr$mean
  data_prep_TRENDY_nbr_mean$T_start = df_prep_TRENDY_mean_T_start$mean
  data_prep_TRENDY_nbr_mean$T_end = df_prep_TRENDY_mean_T_end$mean
  data_prep_TRENDY_nbr_mean$T_mark = df_prep_TRENDY_mean_T_mark$mode
}

data_temp_TRENDY_nbr_mean = data.frame(lapply(data_temp_TRENDY_nbr_mean, as.character), stringsAsFactors=FALSE)
data_prep_TRENDY_nbr_mean = data.frame(lapply(data_prep_TRENDY_nbr_mean, as.character), stringsAsFactors=FALSE)


#write them out
# write.csv(data_temp_sink_nbr_mean,"E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_mean_temp_pcorr_tras_processed_828_toshp.csv",row.names = F)
# write.csv(data_prep_sink_nbr_mean,"E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_nbr_mean_prep_pcorr_tras_processed_828_toshp.csv",row.names = F)
# write.csv(data_temp_sink_ndvi_mean,"E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_mean_temp_pcorr_tras_processed_843_toshp.csv",row.names = F)
# write.csv(data_prep_sink_ndvi_mean,"E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\sink_age_correct_ndvi_mean_prep_pcorr_tras_processed_843_toshp.csv",row.names = F)
# write.csv(data_temp_TRENDY_nbr_mean,"E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_TRENDY_S2S3_mean_age_correct_nbr_temp_pcorr_tras_processed_828_toshp.csv",row.names = F)
# write.csv(data_prep_TRENDY_nbr_mean,"E:\\Corr_Clim_GPP_sink_global_synthesis\\Point_GLC_gpp_sink_North_America_maxcorr_crtperiod\\GPP_TRENDY_S2S3_mean_age_correct_nbr_prep_pcorr_tras_processed_828_toshp.csv",row.names = F)


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

  data1_temp_trendy_mean = data_temp_TRENDY_nbr_mean[!is.na(data_temp_TRENDY_nbr_mean$T_mark),]
  data1_prep_trendy_mean = data_prep_TRENDY_nbr_mean[!is.na(data_prep_TRENDY_nbr_mean$T_mark),]  
  
  data1_temp_sink_nbr_mean = data_temp_sink_nbr_mean[!is.na(data_temp_sink_nbr_mean$T_mark),]
  data1_prep_sink_nbr_mean = data_prep_sink_nbr_mean[!is.na(data_prep_sink_nbr_mean$T_mark),]  
  
  data1_temp_sink_ndvi_mean = data_temp_sink_ndvi_mean[!is.na(data_temp_sink_ndvi_mean$T_mark),]
  data1_prep_sink_ndvi_mean = data_prep_sink_ndvi_mean[!is.na(data_prep_sink_ndvi_mean$T_mark),] 
  
  data1_temp_RS_mean = data_temp_RS_mean[!is.na(data_temp_RS_mean$T_mark),]
  data1_prep_RS_mean = data_prep_RS_mean[!is.na(data_prep_RS_mean$T_mark),] 
  

}


#bulid subsets for north america only
#subset for period
subset_period <- function(x){
list = list()
for (i in 1:21){
  sub1 = base::subset(x,lat > (26+2*(i-1)) & lat <= (28+2*i))
  sub1_list = c()
for (j in 1:length(sub1$lat)){
  sub1_list = append(sub1_list,c(sub1$T_start[j]:sub1$T_end[j]))
}
list[[i]] = sub1_list
}

bin_df = setNames(do.call(cbind.data.frame, lapply(lapply(list, unlist), `length<-`, max(lengths(list)))), paste0("V", 1:21))
 
bin_df_long = gather(bin_df,bin,out)

outlist = list(bin_df,bin_df_long)
return(outlist)
}

#subset for corr
subset_corr <- function(x){
  list_corr = list()
  for (i in 1:21){
    sub1_corr = base::subset(x,lat > (26+2*(i-1)) & lat <= (28+2*i))
    sub1_list_corr = c()
    for (j in 1:length(sub1_corr$lat)){
      sub1_list_corr = append(sub1_list_corr,sub1_corr$Corr)
    }
    list_corr[[i]] = sub1_list_corr
  }

  bin_df_corr = setNames(do.call(cbind.data.frame, lapply(lapply(list_corr, unlist), `length<-`, max(lengths(list_corr)))), paste0("V", 1:21))
  
  bin_df_corr_long = gather(bin_df_corr,bin_corr,out_corr)
  
  outlist = list(bin_df_corr,bin_df_corr_long)
  return(outlist)
}

#get plotable dfs
#period bin_dfs
{
#TRENDY
bin_df_temp_trendy_period = data.frame(subset_period(data1_temp_trendy_mean)[1])
bin_df_prep_trendy_period = data.frame(subset_period(data1_prep_trendy_mean)[1])
bin_df_long_temp_trendy_period = data.frame(subset_period(data1_temp_trendy_mean)[2])
bin_df_long_prep_trendy_period = data.frame(subset_period(data1_prep_trendy_mean)[2])

bin_df_temp_trendyS2_period = data.frame(subset_period(data1_temp_trendyS2)[1])
bin_df_prep_trendyS2_period = data.frame(subset_period(data1_prep_trendyS2)[1])
bin_df_long_temp_trendyS2_period = data.frame(subset_period(data1_temp_trendyS2)[2])
bin_df_long_prep_trendyS2_period = data.frame(subset_period(data1_prep_trendyS2)[2])

bin_df_temp_trendyS3_period = data.frame(subset_period(data1_temp_trendyS3)[1])
bin_df_prep_trendyS3_period = data.frame(subset_period(data1_prep_trendyS3)[1])
bin_df_long_temp_trendyS3_period = data.frame(subset_period(data1_temp_trendyS3)[2])
bin_df_long_prep_trendyS3_period = data.frame(subset_period(data1_prep_trendyS3)[2])

#FLUXCOM
bin_df_temp_fluxcom_period = data.frame(subset_period(data1_temp_fluxcom)[1])
bin_df_prep_fluxcom_period = data.frame(subset_period(data1_prep_fluxcom)[1])
bin_df_long_temp_fluxcom_period = data.frame(subset_period(data1_temp_fluxcom)[2])
bin_df_long_prep_fluxcom_period = data.frame(subset_period(data1_prep_fluxcom)[2])
#ECLUE
bin_df_temp_eclue_period = data.frame(subset_period(data1_temp_eclue)[1])
bin_df_prep_eclue_period = data.frame(subset_period(data1_prep_eclue)[1])
bin_df_long_temp_eclue_period = data.frame(subset_period(data1_temp_eclue)[2])
bin_df_long_prep_eclue_period = data.frame(subset_period(data1_prep_eclue)[2])
#BEPS
bin_df_temp_beps_period = data.frame(subset_period(data1_temp_beps)[1])
bin_df_prep_beps_period = data.frame(subset_period(data1_prep_beps)[1])
bin_df_long_temp_beps_period = data.frame(subset_period(data1_temp_beps)[2])
bin_df_long_prep_beps_period = data.frame(subset_period(data1_prep_beps)[2])
#GPPinf
bin_df_temp_GPPinf_period = data.frame(subset_period(data1_temp_GPPinf)[1])
bin_df_prep_GPPinf_period = data.frame(subset_period(data1_prep_GPPinf)[1])
bin_df_long_temp_GPPinf_period = data.frame(subset_period(data1_temp_GPPinf)[2])
bin_df_long_prep_GPPinf_period = data.frame(subset_period(data1_prep_GPPinf)[2])
#sink nbr
bin_df_temp_sink_nbr_period = data.frame(subset_period(data1_temp_sink_nbr_mean)[1])
bin_df_prep_sink_nbr_period = data.frame(subset_period(data1_prep_sink_nbr_mean)[1])
bin_df_long_temp_sink_nbr_period = data.frame(subset_period(data1_temp_sink_nbr_mean)[2])
bin_df_long_prep_sink_nbr_period = data.frame(subset_period(data1_prep_sink_nbr_mean)[2])
#sink ndvi
bin_df_temp_sink_ndvi_period = data.frame(subset_period(data1_temp_sink_ndvi_mean)[1])
bin_df_prep_sink_ndvi_period = data.frame(subset_period(data1_prep_sink_ndvi_mean)[1])
bin_df_long_temp_sink_ndvi_period = data.frame(subset_period(data1_temp_sink_ndvi_mean)[2])
bin_df_long_prep_sink_ndvi_period = data.frame(subset_period(data1_prep_sink_ndvi_mean)[2])
#RS_mean
bin_df_temp_RS_mean_period = data.frame(subset_period(data1_temp_RS_mean)[1])
bin_df_prep_RS_mean_period = data.frame(subset_period(data1_prep_RS_mean)[1])
bin_df_long_temp_RS_mean_period = data.frame(subset_period(data1_temp_RS_mean)[2])
bin_df_long_prep_RS_mean_period = data.frame(subset_period(data1_prep_RS_mean)[2])
}

#corr bin_dfs
{
  #TRENDY
  bin_df_temp_trendy_corr = data.frame(subset_corr(data1_temp_trendy_mean)[1])
  bin_df_prep_trendy_corr = data.frame(subset_corr(data1_prep_trendy_mean)[1])
  bin_df_long_temp_trendy_corr = data.frame(subset_corr(data1_temp_trendy_mean)[2])
  bin_df_long_prep_trendy_corr = data.frame(subset_corr(data1_prep_trendy_mean)[2])
  
  bin_df_temp_trendyS2_corr = data.frame(subset_corr(data1_temp_trendyS2)[1])
  bin_df_prep_trendyS2_corr = data.frame(subset_corr(data1_prep_trendyS2)[1])
  bin_df_long_temp_trendyS2_corr = data.frame(subset_corr(data1_temp_trendyS2)[2])
  bin_df_long_prep_trendyS2_corr = data.frame(subset_corr(data1_prep_trendyS2)[2])
  
  bin_df_temp_trendyS3_corr = data.frame(subset_corr(data1_temp_trendyS3)[1])
  bin_df_prep_trendyS3_corr = data.frame(subset_corr(data1_prep_trendyS3)[1])
  bin_df_long_temp_trendyS3_corr = data.frame(subset_corr(data1_temp_trendyS3)[2])
  bin_df_long_prep_trendyS3_corr = data.frame(subset_corr(data1_prep_trendyS3)[2])
  
  #FLUXCOM
  bin_df_temp_fluxcom_corr = data.frame(subset_corr(data1_temp_fluxcom)[1])
  bin_df_prep_fluxcom_corr = data.frame(subset_corr(data1_prep_fluxcom)[1])
  bin_df_long_temp_fluxcom_corr = data.frame(subset_corr(data1_temp_fluxcom)[2])
  bin_df_long_prep_fluxcom_corr = data.frame(subset_corr(data1_prep_fluxcom)[2])
  #ECLUE
  bin_df_temp_eclue_corr = data.frame(subset_corr(data1_temp_eclue)[1])
  bin_df_prep_eclue_corr = data.frame(subset_corr(data1_prep_eclue)[1])
  bin_df_long_temp_eclue_corr = data.frame(subset_corr(data1_temp_eclue)[2])
  bin_df_long_prep_eclue_corr = data.frame(subset_corr(data1_prep_eclue)[2])
  #BEPS
  bin_df_temp_beps_corr = data.frame(subset_corr(data1_temp_beps)[1])
  bin_df_prep_beps_corr = data.frame(subset_corr(data1_prep_beps)[1])
  bin_df_long_temp_beps_corr = data.frame(subset_corr(data1_temp_beps)[2])
  bin_df_long_prep_beps_corr = data.frame(subset_corr(data1_prep_beps)[2])
  #GPPinf
  bin_df_temp_GPPinf_corr = data.frame(subset_corr(data1_temp_GPPinf)[1])
  bin_df_prep_GPPinf_corr = data.frame(subset_corr(data1_prep_GPPinf)[1])
  bin_df_long_temp_GPPinf_corr = data.frame(subset_corr(data1_temp_GPPinf)[2])
  bin_df_long_prep_GPPinf_corr = data.frame(subset_corr(data1_prep_GPPinf)[2])
  #sink nbr
  bin_df_temp_sink_nbr_corr = data.frame(subset_corr(data1_temp_sink_nbr_mean)[1])
  bin_df_prep_sink_nbr_corr = data.frame(subset_corr(data1_prep_sink_nbr_mean)[1])
  bin_df_long_temp_sink_nbr_corr = data.frame(subset_corr(data1_temp_sink_nbr_mean)[2])
  bin_df_long_prep_sink_nbr_corr = data.frame(subset_corr(data1_prep_sink_nbr_mean)[2])
  #sink ndvi
  bin_df_temp_sink_ndvi_corr = data.frame(subset_corr(data1_temp_sink_ndvi_mean)[1])
  bin_df_prep_sink_ndvi_corr = data.frame(subset_corr(data1_prep_sink_ndvi_mean)[1])
  bin_df_long_temp_sink_ndvi_corr = data.frame(subset_corr(data1_temp_sink_ndvi_mean)[2])
  bin_df_long_prep_sink_ndvi_corr = data.frame(subset_corr(data1_prep_sink_ndvi_mean)[2])
  #RS mean
  bin_df_temp_RS_mean_corr = data.frame(subset_corr(data1_temp_RS_mean)[1])
  bin_df_prep_RS_mean_corr = data.frame(subset_corr(data1_prep_RS_mean)[1])
  bin_df_long_temp_RS_mean_corr = data.frame(subset_corr(data1_temp_RS_mean)[2])
  bin_df_long_prep_RS_mean_corr = data.frame(subset_corr(data1_prep_RS_mean)[2])
}

#calculate stats.
stats = function(x){
  
  col_mean = apply(x,2,mean,na.rm=TRUE)
  col_sd = apply(x,2,sd,na.rm=TRUE) 
  col_max = col_mean + col_sd
  col_min = col_mean - col_sd
  bin_df_stats = data.frame(col_mean,col_sd,col_max,col_min)  
  return(bin_df_stats)
}

#period stats
{
  #TRENDY
  bin_stats_temp_trendy_period = stats(bin_df_temp_trendy_period)
  bin_stats_prep_trendy_period = stats(bin_df_prep_trendy_period)
  
  bin_stats_temp_trendyS2_period = stats(bin_df_temp_trendyS2_period)
  bin_stats_prep_trendyS2_period = stats(bin_df_prep_trendyS2_period)
  
  bin_stats_temp_trendyS3_period = stats(bin_df_temp_trendyS3_period)
  bin_stats_prep_trendyS3_period = stats(bin_df_prep_trendyS3_period)
  #FLUXCOM
  bin_stats_temp_fluxcom_period = stats(bin_df_temp_fluxcom_period)
  bin_stats_prep_fluxcom_period = stats(bin_df_prep_fluxcom_period)
  #ECLUE
  bin_stats_temp_eclue_period = stats(bin_df_temp_eclue_period)
  bin_stats_prep_eclue_period = stats(bin_df_prep_eclue_period)
  #BEPS
  bin_stats_temp_beps_period = stats(bin_df_temp_beps_period)
  bin_stats_prep_beps_period = stats(bin_df_prep_beps_period)
  #GPPinf
  bin_stats_temp_GPPinf_period = stats(bin_df_temp_GPPinf_period)
  bin_stats_prep_GPPinf_period = stats(bin_df_prep_GPPinf_period)
  #sink nbr
  bin_stats_temp_sink_nbr_period = stats(bin_df_temp_sink_nbr_period)
  bin_stats_prep_sink_nbr_period = stats(bin_df_prep_sink_nbr_period)
  #sink ndvi
  bin_stats_temp_sink_ndvi_period = stats(bin_df_temp_sink_ndvi_period)
  bin_stats_prep_sink_ndvi_period = stats(bin_df_prep_sink_ndvi_period)
  #RS_mean
  bin_stats_temp_RS_mean_period = stats(bin_df_temp_RS_mean_period)
  bin_stats_prep_RS_mean_period = stats(bin_df_prep_RS_mean_period)  
}

#corr stats
{
  #TRENDY
  bin_stats_temp_trendy_corr = stats(bin_df_temp_trendy_corr)
  bin_stats_prep_trendy_corr = stats(bin_df_prep_trendy_corr)
  
  bin_stats_temp_trendyS2_corr = stats(bin_df_temp_trendyS2_corr)
  bin_stats_prep_trendyS2_corr = stats(bin_df_prep_trendyS2_corr)
  
  bin_stats_temp_trendyS3_corr = stats(bin_df_temp_trendyS3_corr)
  bin_stats_prep_trendyS3_corr = stats(bin_df_prep_trendyS3_corr)
  #FLUXCOM
  bin_stats_temp_fluxcom_corr = stats(bin_df_temp_fluxcom_corr)
  bin_stats_prep_fluxcom_corr = stats(bin_df_prep_fluxcom_corr)
  #ECLUE
  bin_stats_temp_eclue_corr = stats(bin_df_temp_eclue_corr)
  bin_stats_prep_eclue_corr = stats(bin_df_prep_eclue_corr)
  #BEPS
  bin_stats_temp_beps_corr = stats(bin_df_temp_beps_corr)
  bin_stats_prep_beps_corr = stats(bin_df_prep_beps_corr)
  #GPPinf
  bin_stats_temp_GPPinf_corr = stats(bin_df_temp_GPPinf_corr)
  bin_stats_prep_GPPinf_corr = stats(bin_df_prep_GPPinf_corr)
  #sink nbr
  bin_stats_temp_sink_nbr_corr = stats(bin_df_temp_sink_nbr_corr)
  bin_stats_prep_sink_nbr_corr = stats(bin_df_prep_sink_nbr_corr)
  #sink ndvi
  bin_stats_temp_sink_ndvi_corr = stats(bin_df_temp_sink_ndvi_corr)
  bin_stats_prep_sink_ndvi_corr = stats(bin_df_prep_sink_ndvi_corr)
  #RS mean
  bin_stats_temp_RS_mean_corr = stats(bin_df_temp_RS_mean_corr)
  bin_stats_prep_RS_mean_corr = stats(bin_df_prep_RS_mean_corr)
}

#lines
{
  #parameters for plots
  {
    txt_size1 = 40
    txt_size2 = 40
    txt_size3 = 40
    line_size1 = 4  #main lines
    line_size2 = 2  #hline 
    line_size3 = 2  #axis lines
  }
  #period
  #temp trendy-sink
  {
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Deep_learning\\random_forest\\Graph_output\\Lat_distribution\\TRENDY_sink_T_period_2022_1_3.pdf",width = 10,height = 20)
  
    pp1 <- ggplot()+
    geom_ribbon(data = bin_stats_temp_trendyS2_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                      ymax= col_max),fill = "#cc9999",alpha = 0.2)+
    geom_ribbon(data = bin_stats_temp_trendyS3_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                          ymax= col_max),fill = "#cc0000",alpha = 0.2)+
    geom_ribbon(data = bin_stats_temp_sink_nbr_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                    ymax= col_max),fill = "#FFFF66",alpha = 0.4)+
    geom_ribbon(data = bin_stats_temp_sink_ndvi_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#FFF59D",alpha = 0.4)+
    geom_line(data = bin_stats_temp_trendyS2_period,aes(x = seq(28,68,2), y=col_mean,colour = 'aa'),size = line_size1)+
    geom_line(data = bin_stats_temp_trendyS3_period,aes(x = seq(28,68,2), y=col_mean,colour = 'bb'),size = line_size1)+
    geom_line(data = bin_stats_temp_sink_nbr_period,aes(x = seq(28,68,2), y=col_mean,colour = 'cc'),size = line_size1)+ 
    geom_line(data = bin_stats_temp_sink_ndvi_period,aes(x = seq(28,68,2), y=col_mean,colour = 'dd'),size = line_size1)+ 
    geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
    ylab("Month")+
    xlab("")+
    scale_color_manual(name = '', 
                       values =c(aa="#FF3300",bb = "#FF6633",cc = "#FFCC66", dd = "#FF9900"), labels = c('TRENDYS2_T_Period','TRENDYS3_T_Period','Sink_nbr_T_Period','Sink_ndvi_T_Period'))+

    scale_x_continuous(limits = c(3,87),breaks = c(10,30,50,70),labels = c(expression(10^o~N),expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
    scale_y_continuous(limits = c(-12,11.5),breaks=seq(-12, 11.5, 3),labels = c("-J","-A","-J","-O","J","A","J","O"))+
    #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme_set(theme_classic())+
    theme(legend.position = c(0.5,0.1),legend.key.size = unit(1.5, "cm"),legend.text = element_text(size = txt_size1,face="bold"))+
    theme(axis.text = element_text(face="bold",size =txt_size2),axis.text.x = element_text(face="bold",size =txt_size2),axis.text.y = element_text(face="bold",size =txt_size2),axis.title.x=element_text(face="bold",size=txt_size3),axis.title.y=element_text(face="bold",size=txt_size3))+
    theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
    theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
    coord_flip()
  
    pp1
    
    dev.off()
  
  }
  
  #temp trendy-sink 2022/1/29
  {
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Deep_learning\\random_forest\\Graph_output\\Lat_distribution\\TRENDY_sink_T_period_2022_1_29.pdf",width = 10,height = 20)
    
    pp1 <- ggplot()+
      geom_ribbon(data = bin_stats_temp_trendyS2_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#31a354ff",alpha = 0.2)+
      geom_ribbon(data = bin_stats_temp_trendyS3_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#74c476ff",alpha = 0.3)+
      geom_ribbon(data = bin_stats_temp_sink_nbr_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#756bb1ff",alpha = 0.2)+
      geom_ribbon(data = bin_stats_temp_sink_ndvi_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                             ymax= col_max),fill = "#9e9ac8ff",alpha = 0.3)+
      geom_line(data = bin_stats_temp_trendyS2_period,aes(x = seq(28,68,2), y=col_mean,colour = 'aa'),size = line_size1)+
      geom_line(data = bin_stats_temp_trendyS3_period,aes(x = seq(28,68,2), y=col_mean,colour = 'bb'),size = line_size1)+
      geom_line(data = bin_stats_temp_sink_nbr_period,aes(x = seq(28,68,2), y=col_mean,colour = 'cc'),size = line_size1)+ 
      geom_line(data = bin_stats_temp_sink_ndvi_period,aes(x = seq(28,68,2), y=col_mean,colour = 'dd'),size = line_size1)+ 
      geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
      ylab("Month")+
      xlab("")+
      scale_color_manual(name = '', 
                         values =c(aa="#31a354ff",bb = "#74c476ff",cc = "#756bb1ff", dd = "#9e9ac8ff"), labels = c('TRENDYS2_T_Period','TRENDYS3_T_Period','Sink_nbr_T_Period','Sink_ndvi_T_Period'))+
      
      scale_x_continuous(limits = c(3,87),breaks = c(10,30,50,70),labels = c(expression(10^o~N),expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      scale_y_continuous(limits = c(-12,11.5),breaks=seq(-12, 11.5, 3),labels = c("-J","-A","-J","-O","J","A","J","O"))+
      #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.1),legend.key.size = unit(1.5, "cm"),legend.text = element_text(size = txt_size1,face="bold"))+
      theme(axis.text = element_text(face="bold",size =txt_size2),axis.text.x = element_text(face="bold",size =txt_size2),axis.text.y = element_text(face="bold",size =txt_size2),axis.title.x=element_text(face="bold",size=txt_size3),axis.title.y=element_text(face="bold",size=txt_size3))+
      theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
      theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
      coord_flip()
    
    pp1
    
    dev.off()
    
  }
  
  #prep trendy-sink
  {
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Deep_learning\\random_forest\\Graph_output\\Lat_distribution\\TRENDY_sink_P_period_2022_1_3.pdf",width = 10,height = 20)
    
    pp2 <- ggplot()+
      geom_ribbon(data = bin_stats_prep_trendyS2_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                          ymax= col_max),fill = "#33ccff",alpha = 0.4)+
      geom_ribbon(data = bin_stats_prep_trendyS3_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#99ffff",alpha = 0.4)+
      geom_ribbon(data = bin_stats_prep_sink_nbr_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                        ymax= col_max),fill = "#99FF99",alpha = 0.4)+
      geom_ribbon(data = bin_stats_prep_sink_nbr_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#99FFcc",alpha = 0.4)+
      geom_line(data = bin_stats_prep_trendyS2_period,aes(x = seq(28,68,2), y=col_mean,colour = 'aa'),size = line_size1)+
      geom_line(data = bin_stats_prep_trendyS3_period,aes(x = seq(28,68,2), y=col_mean,colour = 'bb'),size = line_size1)+
      geom_line(data = bin_stats_prep_sink_nbr_period,aes(x = seq(28,68,2), y=col_mean,colour = 'cc'),size = line_size1)+ 
      geom_line(data = bin_stats_prep_sink_ndvi_period,aes(x = seq(28,68,2), y=col_mean,colour = 'dd'),size = line_size1)+ 
      geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
      ylab("Month")+
      xlab("")+
      scale_color_manual(name = '', 
                         values = c(aa ="#3333FF",bb = "#0000CC", cc = "#00FF00",dd = "#66FF66"), labels = c('TRENDYS2_P_Period','TRENDYS3_P_Period','Sink_nbr_P_Period','Sink_ndvi_P_Period'))+
      scale_y_continuous(limits = c(-12,11.5),breaks=seq(-12, 11.5, 3),labels = c("-J","-A","-J","-O","J","A","J","O"))+
      scale_x_continuous(limits = c(3,87),breaks = c(10,30,50,70),labels = c(expression(10^o~N),expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.1),legend.key.size = unit(1.5, "cm"),legend.spacing.y = unit(2.0, 'cm'),legend.text = element_text(size =txt_size1,face="bold"))+
      theme(axis.text = element_text(face="bold",size =txt_size2),axis.text.x = element_text(face="bold",size =txt_size2),axis.text.y = element_text(face="bold",size =txt_size2),axis.title.x=element_text(face="bold",size=txt_size3),axis.title.y=element_text(face="bold",size=txt_size3))+
      theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
      theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
      coord_flip()
    
    pp2
    
    dev.off()
    
  }
  
  #prep trendy-sink 2022/1/29
  {
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Deep_learning\\random_forest\\Graph_output\\Lat_distribution\\TRENDY_sink_P_period_2022_1_29.pdf",width = 10,height = 20)
    
    pp2 <- ggplot()+
      geom_ribbon(data = bin_stats_prep_trendyS2_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#31a354ff",alpha = 0.2)+
      geom_ribbon(data = bin_stats_prep_trendyS3_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#74c476ff",alpha = 0.2)+
      geom_ribbon(data = bin_stats_prep_sink_nbr_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#756bb1ff",alpha = 0.2)+
      geom_ribbon(data = bin_stats_prep_sink_nbr_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#9e9ac8ff",alpha = 0.2)+
      geom_line(data = bin_stats_prep_trendyS2_period,aes(x = seq(28,68,2), y=col_mean,colour = 'aa'),size = line_size1)+
      geom_line(data = bin_stats_prep_trendyS3_period,aes(x = seq(28,68,2), y=col_mean,colour = 'bb'),size = line_size1)+
      geom_line(data = bin_stats_prep_sink_nbr_period,aes(x = seq(28,68,2), y=col_mean,colour = 'cc'),size = line_size1)+ 
      geom_line(data = bin_stats_prep_sink_ndvi_period,aes(x = seq(28,68,2), y=col_mean,colour = 'dd'),size = line_size1)+ 
      geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
      ylab("Month")+
      xlab("")+
      scale_color_manual(name = '', 
                         values = c(aa="#31a354ff",bb = "#74c476ff",cc = "#756bb1ff", dd = "#9e9ac8ff"), labels = c('TRENDYS2_P_Period','TRENDYS3_P_Period','Sink_nbr_P_Period','Sink_ndvi_P_Period'))+
      scale_y_continuous(limits = c(-12,11.5),breaks=seq(-12, 11.5, 3),labels = c("-J","-A","-J","-O","J","A","J","O"))+
      scale_x_continuous(limits = c(3,87),breaks = c(10,30,50,70),labels = c(expression(10^o~N),expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.1),legend.key.size = unit(1.5, "cm"),legend.spacing.y = unit(2.0, 'cm'),legend.text = element_text(size =txt_size1,face="bold"))+
      theme(axis.text = element_text(face="bold",size =txt_size2),axis.text.x = element_text(face="bold",size =txt_size2),axis.text.y = element_text(face="bold",size =txt_size2),axis.title.x=element_text(face="bold",size=txt_size3),axis.title.y=element_text(face="bold",size=txt_size3))+
      theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
      theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
      coord_flip()
    
    pp2
    
    dev.off()
    
  }
  
  
  #temp FLUXCOM-RS_mean
  {
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Deep_learning\\random_forest\\Graph_output\\Lat_distribution\\FLUXCOM_RS_mean_T_period_2022_1_12.pdf",width = 10,height = 20)
    
    pp3 <- ggplot()+
      geom_ribbon(data = bin_stats_temp_fluxcom_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#cc9999",alpha = 0.2)+
      geom_ribbon(data = bin_stats_temp_RS_mean_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#FFFF66",alpha = 0.4)+
      geom_line(data = bin_stats_temp_fluxcom_period,aes(x = seq(28,68,2), y=col_mean,colour = 'aa'),size = line_size1)+
      geom_line(data = bin_stats_temp_RS_mean_period,aes(x = seq(28,68,2), y=col_mean,colour = 'cc'),size = line_size1)+ 
      geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
      ylab("Month")+
      xlab("")+
      scale_color_manual(name = '', 
                         values =c(aa="#FF3300",cc = "#FFCC66"), labels = c('FLUXCOM_T_Period','RS_mean_T_Period'))+
      
      scale_x_continuous(limits = c(3,87),breaks = c(10,30,50,70),labels = c(expression(10^o~N),expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      scale_y_continuous(limits = c(-12,11.5),breaks=seq(-12, 11.5, 3),labels = c("-J","-A","-J","-O","J","A","J","O"))+
      #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.1),legend.key.size = unit(1.5, "cm"),legend.text = element_text(size = txt_size1,face="bold"))+
      theme(axis.text = element_text(face="bold",size =txt_size2),axis.text.x = element_text(face="bold",size =txt_size2),axis.text.y = element_text(face="bold",size =txt_size2),axis.title.x=element_text(face="bold",size=txt_size3),axis.title.y=element_text(face="bold",size=txt_size3))+
      theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
      theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
      coord_flip()
    
    pp3
    
    dev.off()
    
  }  
  
  #prep FLUXCOM-RS_mean
  {
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Deep_learning\\random_forest\\Graph_output\\Lat_distribution\\FLUXCOM_RS_mean_P_period_2022_1_12.pdf",width = 10,height = 20)
    
    pp4 <- ggplot()+
      geom_ribbon(data = bin_stats_prep_fluxcom_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#33ccff",alpha = 0.4)+
      geom_ribbon(data = bin_stats_prep_RS_mean_period,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#99FF99",alpha = 0.4)+

      geom_line(data = bin_stats_prep_fluxcom_period,aes(x = seq(28,68,2), y=col_mean,colour = 'aa'),size = line_size1)+
      geom_line(data = bin_stats_prep_RS_mean_period,aes(x = seq(28,68,2), y=col_mean,colour = 'cc'),size = line_size1)+ 
      geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
      ylab("Month")+
      xlab("")+
      scale_color_manual(name = '', 
                         values = c(aa ="#3333FF",cc = "#00FF00"), labels = c('FLUXCOM_P_Period','RS_mean_P_Period'))+
      scale_y_continuous(limits = c(-12,11.5),breaks=seq(-12, 11.5, 3),labels = c("-J","-A","-J","-O","J","A","J","O"))+
      scale_x_continuous(limits = c(3,87),breaks = c(10,30,50,70),labels = c(expression(10^o~N),expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      #theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.1),legend.key.size = unit(1.5, "cm"),legend.spacing.y = unit(2.0, 'cm'),legend.text = element_text(size =txt_size1,face="bold"))+
      theme(axis.text = element_text(face="bold",size =txt_size2),axis.text.x = element_text(face="bold",size =txt_size2),axis.text.y = element_text(face="bold",size =txt_size2),axis.title.x=element_text(face="bold",size=txt_size3),axis.title.y=element_text(face="bold",size=txt_size3))+
      theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
      theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
      coord_flip()
    
    pp4
    
    dev.off()
    
  }   
  
  
#corr  
  #temp trendy sink
  {
  ggplot()+
    geom_ribbon(data = bin_stats_temp_trendy_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                      ymax= col_max),fill = "#cc9999",alpha = 0.4)+

    geom_ribbon(data = bin_stats_temp_sink_nbr_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                    ymax= col_max),fill = "#FFFF66",alpha = 0.4)+
    geom_line(data = bin_stats_temp_trendy_corr,aes(x = seq(28,68,2), y=col_mean, colour='one'),size = 2)+
    geom_line(data =bin_stats_temp_sink_nbr_corr,aes(x = seq(28,68,2), y=col_mean,colour='two'),size = 2)+
    geom_hline(yintercept = 0,linetype="dashed",size =1)+
    ylim(-1.2,1.2)+
    xlab("")+
    ylab("Correlation coefficient(r)")+
    #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
    scale_color_manual(name = '', 
                          values =c(one="#660000",two = "#cc6600"), labels = c('TRENDY_T_Corr','Sink_T_Corr'))+
    scale_x_continuous(limits = c(3,87),breaks = c(10,30,50,70),labels = c(expression(10^o~N),expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
    theme_set(theme_classic())+
    theme(legend.position = c(0.5,0.1),legend.text = element_text(size =16,face="bold"))+
    theme(axis.text = element_text(face="bold",size =18),axis.text.x = element_text(face="bold",size =18),axis.text.y = element_text(face="bold",size =18),axis.title.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=18))+
    coord_flip()
  }
  
  #prep trendy sink
  {
    ggplot()+
      geom_ribbon(data = bin_stats_prep_trendy_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                        ymax= col_max),fill = "#33ccff",alpha = 0.4)+
      
      geom_ribbon(data = bin_stats_prep_sink_nbr_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                      ymax= col_max),fill = "#99FF99",alpha = 0.4)+
      geom_line(data = bin_stats_prep_trendy_corr,aes(x = seq(28,68,2), y=col_mean, colour='one'),size = 2)+
      geom_line(data = bin_stats_prep_sink_nbr_corr,aes(x = seq(28,68,2), y=col_mean,colour='two'),size = 2)+
      geom_hline(yintercept = 0,linetype="dashed",size =1)+
      ylim(-1,1)+
      xlab("")+
      ylab("Correlation coefficient(r)")+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c(one="#330066",two = "#336633"), labels = c('TRENDY_P_Corr','Sink_P_Corr'))+
      scale_x_continuous(limits = c(3,87),breaks = c(10,30,50,70),labels = c(expression(10^o~N),expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.1),legend.text = element_text(size =16,face="bold"))+
      theme(axis.text = element_text(face="bold",size =18),axis.text.x = element_text(face="bold",size =18),axis.text.y = element_text(face="bold",size =18),axis.title.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=18))+
      coord_flip()
  }
  
#pcorr
  #temp trendy sink
  {
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Deep_learning\\random_forest\\Graph_output\\Lat_distribution\\Trendy_sink_T_pCorr_2022_1_30.pdf",width = 10,height = 20)
    p3 <- ggplot()+
      geom_ribbon(data = bin_stats_temp_trendyS2_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#3182bdff",alpha = 0.2)+
      geom_ribbon(data = bin_stats_temp_trendyS3_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#6baed6ff",alpha = 0.3)+
      geom_ribbon(data = bin_stats_temp_sink_nbr_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                            ymax= col_max),fill = "#e6550dff",alpha = 0.2)+
      geom_ribbon(data = bin_stats_temp_sink_ndvi_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                             ymax= col_max),fill = "#fd8d3cff",alpha = 0.3)+
      geom_line(data = bin_stats_temp_trendyS2_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'aa'),size = line_size1)+
      geom_line(data = bin_stats_temp_trendyS3_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'bb'),size = line_size1)+
      geom_line(data = bin_stats_temp_sink_nbr_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'cc'),size = line_size1)+ 
      geom_line(data = bin_stats_temp_sink_ndvi_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'dd'),size = line_size1)+ 
      geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
      ylim(-1.12,1.12)+
      xlab("")+
      ylab("pcor")+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values = c(aa="#3182bdff",bb = "#6baed6ff",cc = "#e6550dff", dd = "#fd8d3cff"), labels = c('TRENDYS2_T_pcor','TRENDYS3_T_pcor','Sink_nbr_T_pcor','Sink_ndvi_T_pcor'))+
      scale_x_continuous(limits = c(3,87),breaks = c(10,30,50,70),labels = c(expression(10^o~N),expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.1),legend.key.size = unit(1.5, "cm"),legend.text = element_text(size =txt_size1,face="bold"))+
      theme(axis.text = element_text(face="bold",size =txt_size2),axis.text.x = element_text(face="bold",size =txt_size2),axis.text.y = element_text(face="bold",size =txt_size2),axis.title.x=element_text(face="bold",size=txt_size3),axis.title.y=element_text(face="bold",size=txt_size3))+
      theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
      theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
      coord_flip()
    p3
    dev.off()
  }
    
  #prep trendy sink
  {
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Deep_learning\\random_forest\\Graph_output\\Lat_distribution\\Trendy_sink_P_pCorr_2022_1_30.pdf",width = 10,height = 20)
    p4 <- ggplot()+
      geom_ribbon(data = bin_stats_prep_trendyS2_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                          ymax= col_max),fill = "#3182bdff",alpha = 0.2)+
      geom_ribbon(data = bin_stats_prep_trendyS3_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                          ymax= col_max),fill = "#6baed6ff",alpha = 0.3)+
      geom_ribbon(data = bin_stats_prep_sink_nbr_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                          ymax= col_max),fill = "#e6550dff",alpha = 0.2)+
      geom_ribbon(data = bin_stats_prep_sink_ndvi_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                           ymax= col_max),fill = "#fd8d3cff",alpha = 0.3)+
      geom_line(data = bin_stats_prep_trendyS2_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'aa'),size = line_size1)+
      geom_line(data = bin_stats_prep_trendyS3_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'bb'),size = line_size1)+
      geom_line(data = bin_stats_prep_sink_nbr_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'cc'),size = line_size1)+ 
      geom_line(data = bin_stats_prep_sink_ndvi_corr,aes(x = seq(28,68,2), y=col_mean,colour = 'dd'),size = line_size1)+
      geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
      ylim(-1.12,1.12)+
      xlab("")+
      ylab("pcor")+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values = c(aa="#3182bdff",bb = "#6baed6ff",cc = "#e6550dff", dd = "#fd8d3cff"), labels = c('TRENDYS2_P_pcor','TRENDYS3_P_pcor','Sink_nbr_P_pcor','Sink_ndvi_P_pcor'))+
      scale_x_continuous(limits = c(3,87),breaks = c(10,30,50,70),labels = c(expression(10^o~N),expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.1),legend.key.size = unit(1.5, "cm"),legend.text = element_text(size = txt_size1,face="bold"))+
      theme(axis.text = element_text(face="bold",size = txt_size2),axis.text.x = element_text(face="bold",size = txt_size2),axis.text.y = element_text(face="bold",size = txt_size2),axis.title.x=element_text(face="bold",size= txt_size3),axis.title.y=element_text(face="bold",size= txt_size3))+
      theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
      theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
      coord_flip()
    
    p4
    dev.off()
  } 

  
  #temp rs fluxcom
  {
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Deep_learning\\random_forest\\Graph_output\\Lat_distribution\\FLUXCOM_RS_mean_T_pCorr_2022_1_13.pdf",width = 10,height = 20)
    p3 <- ggplot()+
      geom_ribbon(data = bin_stats_temp_fluxcom_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                         ymax= col_max),fill = "#cc9999",alpha = 0.2)+
      geom_ribbon(data = bin_stats_temp_RS_mean_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                         ymax= col_max),fill = "#FFFF66",alpha = 0.4)+
      geom_line(data = bin_stats_temp_fluxcom_corr,aes(x = seq(28,68,2), y=col_mean, colour='aa'),size = line_size1)+
      geom_line(data = bin_stats_temp_RS_mean_corr,aes(x = seq(28,68,2), y=col_mean,colour='cc'),size = line_size1)+
      geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
      ylim(-1.12,1.12)+
      xlab("")+
      ylab("pcor")+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c(aa = "#660000",cc = "#cc6600"), labels = c('FLUXCOM_T_pCorr','RS_mean_T_pCorr'))+
      scale_x_continuous(limits = c(3,87),breaks = c(10,30,50,70),labels = c(expression(10^o~N),expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.1),legend.key.size = unit(1.5, "cm"),legend.text = element_text(size =txt_size1,face="bold"))+
      theme(axis.text = element_text(face="bold",size =txt_size2),axis.text.x = element_text(face="bold",size =txt_size2),axis.text.y = element_text(face="bold",size =txt_size2),axis.title.x=element_text(face="bold",size=txt_size3),axis.title.y=element_text(face="bold",size=txt_size3))+
      theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
      theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
      coord_flip()
    p3
    dev.off()
  }
  
  #prep rs fluxcom
  {
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Deep_learning\\random_forest\\Graph_output\\Lat_distribution\\FLUXCOM_RS_mean_P_pCorr_2022_1_3.pdf",width = 10,height = 20)
    p4 <- ggplot()+
      geom_ribbon(data = bin_stats_prep_fluxcom_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                         ymax= col_max),fill = "#33ccff",alpha = 0.4)+
      geom_ribbon(data = bin_stats_prep_RS_mean_corr,aes(x=seq(28,68,2),ymin= col_min, 
                                                         ymax= col_max),fill = "#99ff99",alpha = 0.4)+
      geom_line(data = bin_stats_prep_fluxcom_corr,aes(x = seq(28,68,2), y=col_mean, colour='aa'),size = line_size1)+
      geom_line(data = bin_stats_prep_RS_mean_corr,aes(x = seq(28,68,2), y=col_mean,colour='cc'),size = line_size1)+
      geom_hline(yintercept = 0,linetype="dashed",size =line_size2)+
      ylim(-1.12,1.12)+
      xlab("")+
      ylab("pcor")+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c(aa="#330066",cc = "#336633"), labels = c('FLUXCOM_P_pCorr','RS_mean_P_nbr_pCorr'))+
      scale_x_continuous(limits = c(3,87),breaks = c(10,30,50,70),labels = c(expression(10^o~N),expression(30^o~N),expression(50^o~N),expression(70^o~N)))+
      theme_set(theme_classic())+
      theme(legend.position = c(0.5,0.1),legend.key.size = unit(1.5, "cm"),legend.text = element_text(size = txt_size1,face="bold"))+
      theme(axis.text = element_text(face="bold",size = txt_size2),axis.text.x = element_text(face="bold",size = txt_size2),axis.text.y = element_text(face="bold",size = txt_size2),axis.title.x=element_text(face="bold",size= txt_size3),axis.title.y=element_text(face="bold",size= txt_size3))+
      theme(axis.line.x=element_line(linetype=1,color="black",size=line_size3))+
      theme(axis.line.y=element_line(linetype=1,color="black",size=line_size3))+
      coord_flip()
    
    p4
    dev.off()
  } 
}


#for box plot 
{
  # #data binning
  # {
  # # set up cut-off values 
  # breaks <- seq(26,70,2)
  # 
  # # specify interval/bin labels
  # tags <- c("[26-28)","[28-30)", "[30-32)", "[32-34)", "[34-36)", "[36-38)","[38-40)", "[40-42)","[42-44)","[44-46)","[46-48)","[48-50)","[50-52)","[52-54)","[54-56)","[56-58)","[58-60)","[60-62)","[62-64)","[64-66)","[66-68)","[68-70)")
  # 
  # tags1 <- c("27","29","31","33","35","37","39","41","43","45","47","49","51","53","55","57","59","61","63","65","67","69")
  # 
  # # bucketing values into bins
  # group_tags <- cut(data$lat, 
  #                   breaks=breaks, 
  #                   include.lowest=FALSE, 
  #                   right=FALSE, 
  #                   labels=tags)
  # # inspect bins
  # summary(group_tags)
  # }
  # #temp_group <- factor(group_tags,levels = labels,ordered = TRUE)
  # #boxplot
  # {
  # v <- bin_df_long %>% select(bin,out)
  # vgroup <- as_tibble(v) %>% 
  #   mutate(tag = case_when(
  #     bin == "V1" ~ tags1[1],
  #     bin == "V2" ~ tags1[2],
  #     bin == "V3" ~ tags1[3],
  #     bin == "V4" ~ tags1[4],
  #     bin == "V5" ~ tags1[5],
  #     bin == "V6" ~ tags1[6],
  #     bin == "V7" ~ tags1[7],
  #     bin == "V8" ~ tags1[8],
  #     bin == "V9"  ~ tags1[9],
  #     bin == "V10" ~ tags1[10],
  #     bin == "V11" ~ tags1[11],
  #     bin == "V12" ~ tags1[12],
  #     bin == "V13" ~ tags1[13],
  #     bin == "V14" ~ tags1[14],
  #     bin == "V15"  ~ tags1[15],    
  #     bin == "V16"  ~ tags1[16],
  #     bin == "V17" ~ tags1[17],
  #     bin == "V18" ~ tags1[18],
  #     bin == "V19" ~ tags1[19],
  #     bin == "V20" ~ tags1[20],
  #     bin == "V21" ~ tags1[21],
  #     bin == "V22"  ~ tags1[22], 
  # 
  #   ))
  # 
  # ggplot(data = vgroup, mapping = aes(x=tag,y=out)) + 
  #   #geom_jitter(aes(color='blue'),alpha=0.2) +
  #   stat_smooth(method="loess", span=0.1, se=TRUE, aes(fill=bin), alpha=0.3) +
  #   theme_bw()
  #   geom_boxplot(fill="bisque",color="black",alpha=0.3) + 
  #   labs(x='T_mean[Apr.-Oct.] /K',y='Annual biomass increment /kg per tree') +
  #   guides(color=FALSE) +
  #   theme_minimal()+
  #   coord_flip()
  # }
}