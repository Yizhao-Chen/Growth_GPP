#IAV contribution analysis
#setwd("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\IAV_contribution_interpretation\\")
setwd("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\IAV_contribution_interpretation\\point_renew_2022_1_6\\")

#PART 1 get the contribution of each pixel
{
  #sink
  input_sink = read.csv("point_sink_1982_2010_detrend.csv")
  #TRENDY
  input_trendy = read.csv("point_trendy_gpp_1982_2010_detrend.csv")
  #fluxcom
  input_fluxcom = read.csv("point_fluxcom_1982_2010_detrend.csv")
  #eclue
  input_eclue = read.csv("point_eclue_gpp_1982_2010_detrend.csv")
  
  input_sink_nbr_exp1 = read.csv("point_sink_age_correct_nbr_exp1_1984_2010_detrend.csv",header = T)
  
  #Equation form Anders et al 2015
  #fj = sum((xjt*|Xt|)/Xt)/sum(|Xt|) t:year j:pixel
  
  #sink
  f_sink = numeric()
  
  for (m in 2:(length(input_sink_nbr_exp1)-1)){
    sum1 = 0
    sum2 = 0
    for (i in 1:length(input_sink_nbr_exp1$Year)){
      sum_sub1 = ((input_sink_nbr_exp1[i,m])*abs(input_sink_nbr_exp1$SUM[i]))/(input_sink_nbr_exp1$SUM[i])
      sum_sub2 = abs(input_sink_nbr_exp1$SUM[i])
      sum1 = sum1 + sum_sub1
      sum2 = sum2 + sum_sub2
    }
    f_sink[m-1] = sum1/sum2
  }
  
  write.csv(f_sink,"D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\IAV_contribution_interpretation\\IAV_contribution_renew_2022_1_6\\IAV_contribution_sink_nbr_exp1.csv")
  
  
  
  #trendy
  f_trendy = numeric()
  
  for (m in 2:(length(input_trendy)-1)){
    sum1 = 0
    sum2 = 0
    for (i in 1:length(input_trendy$Year)){
      sum_sub1 = ((input_trendy[i,m])*abs(input_trendy$SUM[i]))/input_trendy$SUM[i]
      sum_sub2 = abs(input_trendy$SUM[i])
      sum1 = sum1 + sum_sub1
      sum2 = sum2 + sum_sub2
    }
    f_trendy[m-1] = sum1/sum2
  }
  
  
  #fluxcom
  f_fluxcom = numeric()
  
  for (m in 2:(length(input_fluxcom)-1)){
    sum1 = 0
    sum2 = 0
    for (i in 1:length(input_fluxcom$Year)){
      sum_sub1 = ((input_fluxcom[i,m])*abs(input_fluxcom$SUM[i]))/input_fluxcom$SUM[i]
      sum_sub2 = abs(input_fluxcom$SUM[i])
      sum1 = sum1 + sum_sub1
      sum2 = sum2 + sum_sub2
    }
    f_fluxcom[m-1] = sum1/sum2
  } 
  
  #eclue
  f_eclue = numeric()
  
  for (m in 2:(length(input_eclue)-1)){
    sum1 = 0
    sum2 = 0
    for (i in 1:length(input_eclue$X)){
      sum_sub1 = ((input_eclue[i,m])*abs(input_eclue$SUM[i]))/input_eclue$SUM[i]
      sum_sub2 = abs(input_eclue$SUM[i])
      sum1 = sum1 + sum_sub1
      sum2 = sum2 + sum_sub2
    }
    f_eclue[m-1] = sum1/sum2
  } 
}

#PART 2 analysis the reason for a relatively good correlation between TRENDY and sink at 
#regional scale
{
  library("ggplot2")
  library("tidyr")
  
  input = read.csv("TRENDY_sink_1982_2010_detrend_IAV_analysis.csv")
  input_long = read.csv("TRENDY_sink_1982_2010_detrend_IAV_analysis_long.csv")
  input_scaled = scale(input[,2:length(input)],center = TRUE,scale = TRUE)
  input_scaled_df = data.frame(input_scaled)
  input_scaled_df_long = gather(input_scaled_df,type,value)
  
  p1 <- ggplot(input_scaled_df)+
    geom_point(aes(x=sink_all, y=trendy_all))+ 
    geom_smooth(aes(x=sink_all, y=trendy_all),method=lm)+
    xlim(-3,3)+
    ylim(-3,3)+
    xlab("Normalized_sink_all")+
    ylab("Normalized_TRENDY_all")+
    theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))
  
  p11 <- ggplot(input_scaled_df)+
    geom_point(aes(x=sink_25, y=trendy_25))+ 
    geom_smooth(aes(x=sink_25, y=trendy_25),method=lm)+
    xlim(-3,3)+
    ylim(-3,3)+
    xlab("Normalized_sink_25")+
    ylab("Normalized_TRENDY_25")+
    theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))
  
  p2 <- ggplot(input_scaled_df)+
    geom_point(aes(x=sink_with_trendy_25_pixel, y=trendy_25))+ 
    geom_smooth(aes(x=sink_with_trendy_25_pixel, y=trendy_25),method=lm)+
    xlim(-3,3)+
    ylim(-3,3)+
    xlab("Normalized_sink_pixel_at_TRENDY_25")+
    ylab("Normalized_TRENDY_25")+
    theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16)) 
  
  p3 <- ggplot(input_scaled_df)+
    geom_point(aes(x=sink_25, y=trendy_with_sink_25_pixel))+ 
    geom_smooth(aes(x=sink_25, y=trendy_with_sink_25_pixel),method=lm)+
    xlim(-3,3)+
    ylim(-3,3)+
    xlab("Normalized_sink_25")+
    ylab("Normalized_TRENDY_pixel_at_sink_25")+
    theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16)) 
  
  p4 <- ggplot(input_scaled_df)+
    geom_point(aes(x=sink_25, y=sink_with_trendy_25_pixel))+ 
    geom_smooth(aes(x=sink_25, y=sink_with_trendy_25_pixel),method=lm)+
    xlim(-3,3)+
    ylim(-3,3)+
    xlab("Normalized_sink_25")+
    ylab("Normalized_sink_pixel_with_TRENDY_25")+
    theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16)) 
  
  p5 <- ggplot(input_scaled_df)+
    geom_point(aes(x=trendy_25, y=trendy_with_sink_25_pixel))+ 
    geom_smooth(aes(x=trendy_25, y=trendy_with_sink_25_pixel),method=lm)+
    xlim(-3,3)+
    ylim(-3,3)+
    xlab("Normalized_TRENDY_25")+
    ylab("Normalized_TRENDY_pixel_with_sink_25")+
    theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16)) 
  
}

#test all pixel ratio
{
  #old output
  #input_all_ratio = read.csv("TRENDY_sink_1982_2010_detrend_IAV_all_ratio_analysis.csv")
  #updated 2022/1/10
  input_all_ratio = read.csv("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\IAV_contribution_interpretation\\point_renew_2022_1_6\\summary\\Trendy_sink_1984_2010_detrend_IAV_all_ratio_analysis.csv")
  input_all_ratio_scaled = scale(input_all_ratio[,2:length(input_all_ratio)],center = TRUE,scale = TRUE)
  input_scaled_all_ratio_df = data.frame(input_all_ratio_scaled)

  #r calculation and record
  rs25 = cor(input_scaled_all_ratio_df$trendy_top25_mean,input_scaled_all_ratio_df$sink_top25_mean)
  rs30 = cor(input_scaled_all_ratio_df$trendy_top30_mean,input_scaled_all_ratio_df$sink_top30_mean)
  #rs35 = cor(input_scaled_all_ratio_df$trendy_top35_mean,input_scaled_all_ratio_df$sink_top35_mean)
  rs40 = cor(input_scaled_all_ratio_df$trendy_top40_mean,input_scaled_all_ratio_df$sink_top40_mean)
  rs50 = cor(input_scaled_all_ratio_df$trendy_top50_mean,input_scaled_all_ratio_df$sink_top50_mean)
  rs60 = cor(input_scaled_all_ratio_df$trendy_top60_mean,input_scaled_all_ratio_df$sink_top60_mean)
  rs70 = cor(input_scaled_all_ratio_df$trendy_top70_mean,input_scaled_all_ratio_df$sink_top70_mean)
  rs80 = cor(input_scaled_all_ratio_df$trendy_top80_mean,input_scaled_all_ratio_df$sink_top80_mean)
  #rs90 = cor(input_scaled_all_ratio_df$trendy_top90_mean,input_scaled_all_ratio_df$sink_top90_mean)
  

  
  
  #rs_sum = data.frame(perc = c(0.25,0.3,0.35,0.4,0.5,0.6,0.7,0.8,0.9,1),rs = c(rs25,rs30,rs35,rs40,rs50,rs60,rs70,rs80,rs90,rsall))
  rs_sum = data.frame(perc = c(0.25,0.3,0.4,0.5,0.6,0.7,0.8),rs = c(rs25,rs30,rs40,rs50,rs60,rs70,rs80))
  
  p10 = ggplot(rs_sum,aes(x=perc,y=rs)) + 
    geom_line()+
    ylim(0,0.65)+
    xlab("Pixel fraction")+
    ylab(expression(r))+
    theme(axis.text = element_text(size =16),axis.text.x = element_text(size =16),axis.text.y = element_text(size =16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16)) 
}