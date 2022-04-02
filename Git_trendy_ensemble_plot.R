library("ggplot2")
library("dplR")
library("ggthemes")
library("RColorBrewer")

#TRENDY analysis
{
  setwd("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\TRENDY\\")
  
  input_delta_VegC_GLC = read.csv("Global_delta_VegC_s2_GLC_mask_summary.csv")
  input_npp_GLC = read.csv("Global_npp_s2_GLC_mask_summary.csv")
  input_gpp_GLC = read.csv("Global_gpp_s2_GLC_mask_summary.csv")
  input_nbp_GLC = read.csv("Global_nbps2_GLC_mask_summary.csv")
  
  input_delta_VegC_td = read.csv("Global_delta_VegC_s2_tree_density_mask_summary.csv")
  input_npp_td = read.csv("Global_npp_s2_tree_density_mask_summary.csv")
  input_gpp_td = read.csv("Global_gpp_s2_tree_density_mask_summary.csv")
  
  
  #correct the year col
  input_delta_VegC_GLC$X = c(1982:2010)
  input_npp_GLC$X = c(1982:2010)
  input_gpp_GLC$X = c(1982:2010)
  input_nbp_GLC$X = c(1982:2010)
  
  input_delta_VegC_td$X = c(1982:2010)
  input_npp_td$X = c(1982:2010)
  input_gpp_td$X = c(1982:2010)
  
  #remove the na cols
  input_npp_GLC1 = input_npp_GLC[,-17]
  input_npp_td1 = input_npp_td[,-17]
  
  input_gpp_GLC1 = input_gpp_GLC[,-12]
  input_gpp_td1 = input_gpp_td[,-12]
  
  input_nbp_GLC1 = input_nbp_GLC[,-10]
  input_nbp_GLC1 = input_nbp_GLC1[,-11]
  
  
  #trend output
  {
    #data normalization
    #GLC
    input_delta_VegC_GLC_scaled = scale(input_delta_VegC_GLC[,2:length(input_delta_VegC_GLC)],center = TRUE,scale = TRUE)
    input_npp_GLC_scaled = scale(input_npp_GLC[,2:length(input_npp_GLC)],center = TRUE,scale = TRUE)
    input_gpp_GLC_scaled = scale(input_gpp_GLC[,2:length(input_gpp_GLC)],center = TRUE,scale = TRUE)
    
    #tree density
    input_delta_VegC_td_scaled = scale(input_delta_VegC_td[,2:length(input_delta_VegC_GLC)],center = TRUE,scale = TRUE)
    input_npp_td_scaled = scale(input_npp_td[,2:length(input_npp_td)],center = TRUE,scale = TRUE)
    input_gpp_td_scaled = scale(input_gpp_td[,2:length(input_gpp_td)],center = TRUE,scale = TRUE)
    
    #convert to dfs
    #GLC
    dVegC_GLC_scaled_df = data.frame(input_delta_VegC_GLC_scaled)
    npp_GLC_scaled_df = data.frame(input_npp_GLC_scaled)
    gpp_GLC_scaled_df = data.frame(input_gpp_GLC_scaled)
    
    #tree density
    dVegC_td_scaled_df = data.frame(input_delta_VegC_td_scaled)
    npp_td_scaled_df = data.frame(input_npp_td_scaled)
    gpp_td_scaled_df = data.frame(input_gpp_td_scaled)
    
    #calculate mean and sd
    #GLC
    delta_VegC_GLC_mean = apply(dVegC_GLC_scaled_df[,1:ncol(dVegC_GLC_scaled_df)],1,mean,na.rm=TRUE)
    npp_GLC_mean = apply(npp_GLC_scaled_df[,1:ncol(npp_GLC_scaled_df)],1,mean,na.rm=TRUE)
    gpp_GLC_mean = apply(gpp_GLC_scaled_df[,1:ncol(gpp_GLC_scaled_df)],1,mean,na.rm=TRUE)
    
    delta_VegC_GLC_rsd = apply(dVegC_GLC_scaled_df[,1:ncol(dVegC_GLC_scaled_df)],1,sd,na.rm=TRUE)
    npp_GLC_rsd = apply(npp_GLC_scaled_df[,1:ncol(npp_GLC_scaled_df)],1,sd,na.rm=TRUE)
    gpp_GLC_rsd = apply(gpp_GLC_scaled_df[,1:ncol(gpp_GLC_scaled_df)],1,sd,na.rm=TRUE)
    
    #tree density
    delta_VegC_td_mean = apply(dVegC_td_scaled_df[,1:ncol(dVegC_td_scaled_df)],1,mean,na.rm=TRUE)
    npp_td_mean = apply(npp_td_scaled_df[,1:ncol(npp_td_scaled_df)],1,mean,na.rm=TRUE)
    gpp_td_mean = apply(gpp_td_scaled_df[,1:ncol(gpp_td_scaled_df)],1,mean,na.rm=TRUE)
    
    delta_VegC_td_rsd = apply(dVegC_td_scaled_df[,1:ncol(dVegC_td_scaled_df)],1,sd,na.rm=TRUE)
    npp_td_rsd = apply(npp_td_scaled_df[,1:ncol(npp_td_scaled_df)],1,sd,na.rm=TRUE)
    gpp_td_rsd = apply(gpp_td_scaled_df[,1:ncol(gpp_td_scaled_df)],1,sd,na.rm=TRUE)
    
    #add to dfs
    #GLC
    dVegC_GLC_scaled_df$mean = delta_VegC_GLC_mean
    dVegC_GLC_scaled_df$sd = delta_VegC_GLC_rsd
    dVegC_GLC_scaled_df$min = delta_VegC_GLC_mean - 2 * delta_VegC_GLC_rsd
    dVegC_GLC_scaled_df$max = delta_VegC_GLC_mean + 2 * delta_VegC_GLC_rsd
    
    npp_GLC_scaled_df$mean = npp_GLC_mean
    npp_GLC_scaled_df$sd = npp_GLC_rsd
    npp_GLC_scaled_df$min = npp_GLC_mean - 2 * npp_GLC_rsd
    npp_GLC_scaled_df$max = npp_GLC_mean + 2 * npp_GLC_rsd
    
    gpp_GLC_scaled_df$mean = gpp_GLC_mean
    gpp_GLC_scaled_df$sd = gpp_GLC_rsd
    gpp_GLC_scaled_df$min = gpp_GLC_mean - 2 * gpp_GLC_rsd
    gpp_GLC_scaled_df$max = gpp_GLC_mean + 2 * gpp_GLC_rsd
    
    #tree density
    dVegC_td_scaled_df$mean = delta_VegC_td_mean
    dVegC_td_scaled_df$sd = delta_VegC_td_rsd
    dVegC_td_scaled_df$min = delta_VegC_td_mean - 2 * delta_VegC_td_rsd
    dVegC_td_scaled_df$max = delta_VegC_td_mean + 2 * delta_VegC_td_rsd
    
    npp_td_scaled_df$mean = npp_td_mean
    npp_td_scaled_df$sd = npp_td_rsd
    npp_td_scaled_df$min = npp_td_mean - 2 * npp_td_rsd
    npp_td_scaled_df$max = npp_td_mean + 2 * npp_td_rsd
    
    gpp_td_scaled_df$mean = gpp_td_mean
    gpp_td_scaled_df$sd = gpp_td_rsd
    gpp_td_scaled_df$min = gpp_td_mean - 2 * gpp_td_rsd
    gpp_td_scaled_df$max = gpp_td_mean + 2 * gpp_td_rsd
    
    #add year to dfs
    #GLC
    dVegC_GLC_scaled_df$Year = c(1982:2010)
    npp_GLC_scaled_df$Year = c(1982:2010)
    gpp_GLC_scaled_df$Year = c(1982:2010)
    #tree density
    dVegC_td_scaled_df$Year = c(1982:2010)
    npp_td_scaled_df$Year = c(1982:2010)
    gpp_td_scaled_df$Year = c(1982:2010)
  }
  
  #detrended output
  {
    #GLC
    #input_npp_GLC$X = c(1982:2010)
    input_dVegC_GLC_de <- input_delta_VegC_GLC
    for (i in 2:(ncol(input_dVegC_GLC_de))){
      output_lm <- lm(input_dVegC_GLC_de[,i]~input_dVegC_GLC_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_dVegC_GLC_de[,i] <- output_ret
    }  
    
    input_npp_GLC_de <- input_npp_GLC1
    for (i in 2:(ncol(input_npp_GLC_de))){
      output_lm <- lm(input_npp_GLC_de[,i]~input_npp_GLC_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_npp_GLC_de[,i] <- output_ret
    }
    
    input_gpp_GLC_de <- input_gpp_GLC1
    for (i in 2:(ncol(input_gpp_GLC_de))){
      output_lm <- lm(input_gpp_GLC_de[,i]~input_gpp_GLC_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_gpp_GLC_de[,i] <- output_ret
    }  
    
    input_nbp_GLC_de <- input_nbp_GLC1
    for (i in 2:(ncol(input_nbp_GLC_de))){
      output_lm <- lm(input_nbp_GLC_de[,i]~input_nbp_GLC_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_nbp_GLC_de[,i] <- output_ret
    } 
    
    
    
    
    
    #tree density
    input_dVegC_td_de <- input_delta_VegC_td
    for (i in 2:(ncol(input_dVegC_td_de))){
      output_lm <- lm(input_dVegC_td_de[,i]~input_dVegC_td_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_dVegC_td_de[,i] <- output_ret
    }  
    
    input_npp_td_de <- input_npp_td1
    for (i in 2:(ncol(input_npp_td_de))){
      output_lm <- lm(input_npp_td_de[,i]~input_npp_td_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_npp_td_de[,i] <- output_ret
    }
    
    input_gpp_td_de <- input_gpp_td1
    for (i in 2:(ncol(input_gpp_td_de))){
      output_lm <- lm(input_gpp_td_de[,i]~input_gpp_td_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_gpp_td_de[,i] <- output_ret
    }  
    
    #data normalization
    #GLC
    input_dVegC_GLC_de_scaled = scale(input_dVegC_GLC_de[,2:length(input_dVegC_GLC_de)],center = TRUE,scale = TRUE)
    input_npp_GLC_de_scaled = scale(input_npp_GLC_de[,2:length(input_npp_GLC_de)],center = TRUE,scale = TRUE)
    input_gpp_GLC_de_scaled = scale(input_gpp_GLC_de[,2:length(input_gpp_GLC_de)],center = TRUE,scale = TRUE)
    input_nbp_GLC_de_scaled = scale(input_nbp_GLC_de[,2:length(input_nbp_GLC_de)],center = TRUE,scale = TRUE)
    
    #tree density
    input_dVegC_td_de_scaled = scale(input_dVegC_td_de[,2:length(input_dVegC_td_de)],center = TRUE,scale = TRUE)
    input_npp_td_de_scaled = scale(input_npp_td_de[,2:length(input_npp_td_de)],center = TRUE,scale = TRUE)
    input_gpp_td_de_scaled = scale(input_gpp_td_de[,2:length(input_gpp_td_de)],center = TRUE,scale = TRUE)
    
    #convert to dfs
    #GLC
    dVegC_GLC_de_scaled_df = data.frame(input_dVegC_GLC_de_scaled)
    npp_GLC_de_scaled_df = data.frame(input_npp_GLC_de_scaled)
    gpp_GLC_de_scaled_df = data.frame(input_gpp_GLC_de_scaled)
    nbp_GLC_de_scaled_df = data.frame(input_nbp_GLC_de_scaled)
    
    #tree density
    dVegC_td_de_scaled_df = data.frame(input_dVegC_td_de_scaled)
    npp_td_de_scaled_df = data.frame(input_npp_td_de_scaled)
    gpp_td_de_scaled_df = data.frame(input_gpp_td_de_scaled)
    
    #calculate mean and sd
    #GLC
    delta_VegC_GLC_de_mean = apply(dVegC_GLC_de_scaled_df[,1:ncol(dVegC_GLC_de_scaled_df)],1,mean,na.rm=TRUE)
    npp_GLC_de_mean = apply(npp_GLC_de_scaled_df[,1:ncol(npp_GLC_de_scaled_df)],1,mean,na.rm=TRUE)
    gpp_GLC_de_mean = apply(gpp_GLC_de_scaled_df[,1:ncol(gpp_GLC_de_scaled_df)],1,mean,na.rm=TRUE)
    nbp_GLC_de_mean = apply(nbp_GLC_de_scaled_df[,1:ncol(nbp_GLC_de_scaled_df)],1,mean,na.rm=TRUE)  
    
    delta_VegC_GLC_de_rsd = apply(dVegC_GLC_de_scaled_df[,1:ncol(dVegC_GLC_de_scaled_df)],1,sd,na.rm=TRUE)
    npp_GLC_de_rsd = apply(npp_GLC_de_scaled_df[,1:ncol(npp_GLC_de_scaled_df)],1,sd,na.rm=TRUE)
    gpp_GLC_de_rsd = apply(gpp_GLC_de_scaled_df[,1:ncol(gpp_GLC_de_scaled_df)],1,sd,na.rm=TRUE)
    nbp_GLC_de_rsd = apply(nbp_GLC_de_scaled_df[,1:ncol(nbp_GLC_de_scaled_df)],1,sd,na.rm=TRUE)
    
    #tree density
    delta_VegC_td_de_mean = apply(dVegC_td_de_scaled_df[,1:ncol(dVegC_td_de_scaled_df)],1,mean,na.rm=TRUE)
    npp_td_de_mean = apply(npp_td_de_scaled_df[,1:ncol(npp_td_de_scaled_df)],1,mean,na.rm=TRUE)
    gpp_td_de_mean = apply(gpp_td_de_scaled_df[,1:ncol(gpp_td_de_scaled_df)],1,mean,na.rm=TRUE)
    
    delta_VegC_td_de_rsd = apply(dVegC_td_de_scaled_df[,1:ncol(dVegC_td_de_scaled_df)],1,sd,na.rm=TRUE)
    npp_td_de_rsd = apply(npp_td_de_scaled_df[,1:ncol(npp_td_de_scaled_df)],1,sd,na.rm=TRUE)
    gpp_td_de_rsd = apply(gpp_td_de_scaled_df[,1:ncol(gpp_td_de_scaled_df)],1,sd,na.rm=TRUE)
    
    #add to dfs
    #GLC
    dVegC_GLC_de_scaled_df$mean = delta_VegC_GLC_de_mean
    dVegC_GLC_de_scaled_df$sd = delta_VegC_GLC_de_rsd
    dVegC_GLC_de_scaled_df$min = delta_VegC_GLC_de_mean - 2 * delta_VegC_GLC_de_rsd
    dVegC_GLC_de_scaled_df$max = delta_VegC_GLC_de_mean + 2 * delta_VegC_GLC_de_rsd
    
    npp_GLC_de_scaled_df$mean = npp_GLC_de_mean
    npp_GLC_de_scaled_df$sd = npp_GLC_de_rsd
    npp_GLC_de_scaled_df$min = npp_GLC_de_mean - 2 * npp_GLC_de_rsd
    npp_GLC_de_scaled_df$max = npp_GLC_de_mean + 2 * npp_GLC_de_rsd
    
    gpp_GLC_de_scaled_df$mean = gpp_GLC_de_mean
    gpp_GLC_de_scaled_df$sd = gpp_GLC_de_rsd
    gpp_GLC_de_scaled_df$min = gpp_GLC_de_mean - 2 * gpp_GLC_de_rsd
    gpp_GLC_de_scaled_df$max = gpp_GLC_de_mean + 2 * gpp_GLC_de_rsd
    
    nbp_GLC_de_scaled_df$mean = nbp_GLC_de_mean
    nbp_GLC_de_scaled_df$sd = nbp_GLC_de_rsd
    nbp_GLC_de_scaled_df$min = nbp_GLC_de_mean - 2 * nbp_GLC_de_rsd
    nbp_GLC_de_scaled_df$max = nbp_GLC_de_mean + 2 * nbp_GLC_de_rsd
    
    #tree density
    dVegC_td_de_scaled_df$mean = delta_VegC_td_de_mean
    dVegC_td_de_scaled_df$sd = delta_VegC_td_de_rsd
    dVegC_td_de_scaled_df$min = delta_VegC_td_de_mean - 2 * delta_VegC_td_de_rsd
    dVegC_td_de_scaled_df$max = delta_VegC_td_de_mean + 2 * delta_VegC_td_de_rsd
    
    npp_td_de_scaled_df$mean = npp_td_de_mean
    npp_td_de_scaled_df$sd = npp_td_de_rsd
    npp_td_de_scaled_df$min = npp_td_de_mean - 2 * npp_td_de_rsd
    npp_td_de_scaled_df$max = npp_td_de_mean + 2 * npp_td_de_rsd
    
    gpp_td_de_scaled_df$mean = gpp_td_de_mean
    gpp_td_de_scaled_df$sd = gpp_td_de_rsd
    gpp_td_de_scaled_df$min = gpp_td_de_mean - 2 * gpp_td_de_rsd
    gpp_td_de_scaled_df$max = gpp_td_de_mean + 2 * gpp_td_de_rsd 
    
    #add year to dfs
    #GLC
    dVegC_GLC_de_scaled_df$Year = c(1982:2010)
    npp_GLC_de_scaled_df$Year = c(1982:2010)
    gpp_GLC_de_scaled_df$Year = c(1982:2010)
    nbp_GLC_de_scaled_df$Year = c(1982:2010)  
    #tree density
    dVegC_td_de_scaled_df$Year = c(1982:2010)
    npp_td_de_scaled_df$Year = c(1982:2010)
    gpp_td_de_scaled_df$Year = c(1982:2010)
  }
  
  #gpp npp dVegC
  p2 <- ggplot()+
    geom_ribbon(data = gpp_td_de_scaled_df,aes(x=Year,ymin= min, 
                                               ymax= max),fill = "lightblue",alpha = 0.4)+
    geom_ribbon(data = npp_td_de_scaled_df,aes(x=Year,ymin= min, 
                                               ymax= max),fill = "green",alpha = 0.4)+
    geom_ribbon(data = dVegC_td_de_scaled_df,aes(x=Year,ymin= min, 
                                                 ymax= max),fill = "pink",alpha = 0.4)+
    
    geom_line(data = gpp_td_de_scaled_df,aes(x=Year, 
                                             y=mean,color = "royalblue4"),linetype="solid",size =1)+
    geom_line(data = npp_td_de_scaled_df,aes(x=Year,y= mean,color = "green4"),linetype="solid",size = 1)+
    geom_line(data = dVegC_td_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
    ylab("Nomalized index")+
    xlab("Year")+
    #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
    scale_color_manual(name = '', 
                       values =c("royalblue4"="royalblue4","green4" = "green4","lightcoral"="lightcoral"), labels = c('GPP_rhythm','NPP_rhythm','d_VegC_rhythm'))+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = "bottom",legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  p2
  
  p1 <- ggplot()+
    geom_ribbon(data = gpp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
                                                ymax= max),fill = "lightblue",alpha = 0.4)+
    geom_ribbon(data = npp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
                                                ymax= max),fill = "green",alpha = 0.4)+
    geom_ribbon(data = dVegC_GLC_de_scaled_df,aes(x=Year,ymin= min, 
                                                  ymax= max),fill = "pink",alpha = 0.4)+
    geom_line(data = gpp_GLC_de_scaled_df,aes(x=Year, 
                                              y=mean,color = "royalblue4"),linetype="solid",size =1)+
    geom_line(data = npp_GLC_de_scaled_df,aes(x=Year,y= mean,color = "green4"),linetype="solid",size = 1)+
    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
    ylab("Nomalized index")+
    xlab("Year")+
    #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
    scale_color_manual(name = '', 
                       values =c("royalblue4"="royalblue4","green4" = "green4","lightcoral"="lightcoral"), labels = c('GPP_rhythm','NPP_rhythm','d_VegC_rhythm'))+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = "bottom",legend.text = element_text(size = 12))+
    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
  
  
  p1
  
  #gpp npp dVegC nbp GLC
  
  #ECLUE_sub$Climate <- factor(ECLUE_sub$Climate,levels = ECLUE_sub$Climate)
  
  p3 <- ggplot()+
    geom_ribbon(data = nbp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
                                                ymax= max),fill = "lightblue",alpha = 0.2)+
    geom_ribbon(data = dVegC_GLC_de_scaled_df,aes(x=Year,ymin= min, 
                                                  ymax= max),fill = "pink",alpha = 0.2)+
    geom_ribbon(data = npp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
                                                ymax= max),fill = "green",alpha = 0.2)+
    geom_ribbon(data = gpp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
                                                ymax= max),fill = "gray",alpha = 0.2)+
    geom_line(data = nbp_GLC_de_scaled_df,aes(x=Year,y= mean,color = "royalblue4"),linetype="solid",size = 1)+
    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+ 
    geom_line(data = npp_GLC_de_scaled_df,aes(x=Year,y= mean,color = "green4"),linetype="solid",size = 1)+
    geom_line(data = gpp_GLC_de_scaled_df,aes(x=Year, 
                                              y=mean,color = "gray21"),linetype="solid",size =1)+
    ylab("Nomalized index")+
    xlab("Year")+
    ylim(-4,4)+
    #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
    scale_color_manual(name = '', 
                       values =c("gray21"="gray21","green4" = "green4","lightcoral" = "lightcoral","royalblue4"="royalblue4"), labels = c('GPP_rhythm','NPP_rhythm','dVeg_rhythm','NBP_rhythm'))+
    theme_set(theme_bw())+
    theme(panel.grid.major=element_line(colour=NA))+
    theme(legend.position = "bottom",legend.text = element_text(face="bold",size = 16))+
    theme(axis.text = element_text(face="bold",size =16),axis.text.x = element_text(face="bold",size =16),axis.text.y = element_text(face="bold",size =16),axis.title.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16))
  
  p3
  
  
}

#TRENDY vs sink-based map North America
{
  setwd("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\TRENDY\\")
  
  input_delta_VegC_GLC = read.csv("North_America_delta_VegC_GLC_mask_summary.csv")
  input_npp_GLC = read.csv("North_America_npps2_GLC_mask_summary.csv")
  input_gpp_GLC = read.csv("North_America_gpps2_GLC_mask_summary.csv")
  
  #input_delta_VegC_td = read.csv("North_America_delta_VegC_tree_density_mask_summary.csv")
  #input_npp_td = read.csv("North_America_npps2_tree_density_mask_summary.csv")
  #input_gpp_td = read.csv("North_America_gpps2_tree_density_mask_summary.csv")
  
  #input_sink_td = read.csv("North_America_s808_tree_density_mask_summary.csv")
  input_sink_glc = read.csv("North_America_s808_GLC_mask_summary.csv")
  
  
  #correct the year col
  input_delta_VegC_GLC$X = c(1982:2010)
  input_npp_GLC$X = c(1982:2010)
  input_gpp_GLC$X = c(1982:2010)
  
  input_delta_VegC_td$X = c(1982:2010)
  input_npp_td$X = c(1982:2010)
  input_gpp_td$X = c(1982:2010)
  
  input_sink_glc$X = c(1982:2010)
  input_sink_td$X = c(1982:2010)
  
  #remove the na cols
  input_npp_GLC1 = input_npp_GLC[,-17]
  input_npp_td1 = input_npp_td[,-17]
  
  input_gpp_GLC1 = input_gpp_GLC[,-12]
  input_gpp_td1 = input_gpp_td[,-12]
  
  
  #trend output
  {
    #data normalization
    #GLC
    input_delta_VegC_GLC_scaled = scale(input_delta_VegC_GLC[,2:length(input_delta_VegC_GLC)],center = TRUE,scale = TRUE)
    input_npp_GLC_scaled = scale(input_npp_GLC[,2:length(input_npp_GLC)],center = TRUE,scale = TRUE)
    input_gpp_GLC_scaled = scale(input_gpp_GLC[,2:length(input_gpp_GLC)],center = TRUE,scale = TRUE)
    input_sink_GLC_scaled = scale(input_sink_glc[,2:length(input_sink_glc)],center = TRUE,scale = TRUE)
    
    
    #tree density
    input_delta_VegC_td_scaled = scale(input_delta_VegC_td[,2:length(input_delta_VegC_GLC)],center = TRUE,scale = TRUE)
    input_npp_td_scaled = scale(input_npp_td[,2:length(input_npp_td)],center = TRUE,scale = TRUE)
    input_gpp_td_scaled = scale(input_gpp_td[,2:length(input_gpp_td)],center = TRUE,scale = TRUE)
    input_sink_td_scaled = scale(input_sink_td[,2:length(input_sink_td)],center = TRUE,scale = TRUE)
    
    #convert to dfs
    #GLC
    dVegC_GLC_scaled_df = data.frame(input_delta_VegC_GLC_scaled)
    npp_GLC_scaled_df = data.frame(input_npp_GLC_scaled)
    gpp_GLC_scaled_df = data.frame(input_gpp_GLC_scaled)
    sink_GLC_scaled_df = data.frame(input_sink_GLC_scaled)
    
    #tree density
    dVegC_td_scaled_df = data.frame(input_delta_VegC_td_scaled)
    npp_td_scaled_df = data.frame(input_npp_td_scaled)
    gpp_td_scaled_df = data.frame(input_gpp_td_scaled)
    sink_td_scaled_df = data.frame(input_sink_td_scaled)
    
    #calculate mean and sd
    #GLC
    delta_VegC_GLC_mean = apply(dVegC_GLC_scaled_df[,1:ncol(dVegC_GLC_scaled_df)],1,mean,na.rm=TRUE)
    npp_GLC_mean = apply(npp_GLC_scaled_df[,1:ncol(npp_GLC_scaled_df)],1,mean,na.rm=TRUE)
    gpp_GLC_mean = apply(gpp_GLC_scaled_df[,1:ncol(gpp_GLC_scaled_df)],1,mean,na.rm=TRUE)
    #sink_GLC_mean = apply(sink_GLC_scaled_df[,1:ncol(sink_GLC_scaled_df)],1,mean,na.rm=TRUE)
    
    delta_VegC_GLC_rsd = apply(dVegC_GLC_scaled_df[,1:ncol(dVegC_GLC_scaled_df)],1,sd,na.rm=TRUE)
    npp_GLC_rsd = apply(npp_GLC_scaled_df[,1:ncol(npp_GLC_scaled_df)],1,sd,na.rm=TRUE)
    gpp_GLC_rsd = apply(gpp_GLC_scaled_df[,1:ncol(gpp_GLC_scaled_df)],1,sd,na.rm=TRUE)
    #sink_GLC_rsd = apply(sink_GLC_scaled_df[,1:ncol(sink_GLC_scaled_df)],1,sd,na.rm=TRUE)
    
    #tree density
    delta_VegC_td_mean = apply(dVegC_td_scaled_df[,1:ncol(dVegC_td_scaled_df)],1,mean,na.rm=TRUE)
    npp_td_mean = apply(npp_td_scaled_df[,1:ncol(npp_td_scaled_df)],1,mean,na.rm=TRUE)
    gpp_td_mean = apply(gpp_td_scaled_df[,1:ncol(gpp_td_scaled_df)],1,mean,na.rm=TRUE)
    #sink_td_mean = apply(sink_td_scaled_df[,1:ncol(sink_td_scaled_df)],1,mean,na.rm=TRUE)
    
    delta_VegC_td_rsd = apply(dVegC_td_scaled_df[,1:ncol(dVegC_td_scaled_df)],1,sd,na.rm=TRUE)
    npp_td_rsd = apply(npp_td_scaled_df[,1:ncol(npp_td_scaled_df)],1,sd,na.rm=TRUE)
    gpp_td_rsd = apply(gpp_td_scaled_df[,1:ncol(gpp_td_scaled_df)],1,sd,na.rm=TRUE)
    #sink_td_rsd = apply(sink_td_scaled_df[,1:ncol(sink_td_scaled_df)],1,sd,na.rm=TRUE)
    #add to dfs
    #GLC
    dVegC_GLC_scaled_df$mean = delta_VegC_GLC_mean
    dVegC_GLC_scaled_df$sd = delta_VegC_GLC_rsd
    dVegC_GLC_scaled_df$min = delta_VegC_GLC_mean - 2 * delta_VegC_GLC_rsd
    dVegC_GLC_scaled_df$max = delta_VegC_GLC_mean + 2 * delta_VegC_GLC_rsd
    
    npp_GLC_scaled_df$mean = npp_GLC_mean
    npp_GLC_scaled_df$sd = npp_GLC_rsd
    npp_GLC_scaled_df$min = npp_GLC_mean - 2 * npp_GLC_rsd
    npp_GLC_scaled_df$max = npp_GLC_mean + 2 * npp_GLC_rsd
    
    gpp_GLC_scaled_df$mean = gpp_GLC_mean
    gpp_GLC_scaled_df$sd = gpp_GLC_rsd
    gpp_GLC_scaled_df$min = gpp_GLC_mean - 2 * gpp_GLC_rsd
    gpp_GLC_scaled_df$max = gpp_GLC_mean + 2 * gpp_GLC_rsd
    
    #sink_GLC_scaled_df$mean = sink_GLC_mean
    #sink_GLC_scaled_df$sd = sink_GLC_rsd
    #sink_GLC_scaled_df$min = sink_GLC_mean - 2 * sink_GLC_rsd
    #sink_GLC_scaled_df$max = sink_GLC_mean + 2 * sink_GLC_rsd    
    
    #tree density
    dVegC_td_scaled_df$mean = delta_VegC_td_mean
    dVegC_td_scaled_df$sd = delta_VegC_td_rsd
    dVegC_td_scaled_df$min = delta_VegC_td_mean - 2 * delta_VegC_td_rsd
    dVegC_td_scaled_df$max = delta_VegC_td_mean + 2 * delta_VegC_td_rsd
    
    npp_td_scaled_df$mean = npp_td_mean
    npp_td_scaled_df$sd = npp_td_rsd
    npp_td_scaled_df$min = npp_td_mean - 2 * npp_td_rsd
    npp_td_scaled_df$max = npp_td_mean + 2 * npp_td_rsd
    
    gpp_td_scaled_df$mean = gpp_td_mean
    gpp_td_scaled_df$sd = gpp_td_rsd
    gpp_td_scaled_df$min = gpp_td_mean - 2 * gpp_td_rsd
    gpp_td_scaled_df$max = gpp_td_mean + 2 * gpp_td_rsd
    
    #sink_td_scaled_df$mean = sink_td_mean
    #sink_td_scaled_df$sd = sink_td_rsd
    #sink_td_scaled_df$min = sink_td_mean - 2 * sink_td_rsd
    #sink_td_scaled_df$max = sink_td_mean + 2 * sink_td_rsd
    
    #add year to dfs
    #GLC
    dVegC_GLC_scaled_df$Year = c(1982:2010)
    npp_GLC_scaled_df$Year = c(1982:2010)
    gpp_GLC_scaled_df$Year = c(1982:2010)
    sink_GLC_scaled_df$Year = c(1982:2010)
    #tree density
    dVegC_td_scaled_df$Year = c(1982:2010)
    npp_td_scaled_df$Year = c(1982:2010)
    gpp_td_scaled_df$Year = c(1982:2010)
    sink_td_scaled_df$Year = c(1982:2010)    
    
    
  }
  
  #detrended output
  {
    #GLC
    #input_npp_GLC$X = c(1982:2010)
    input_dVegC_GLC_de <- input_delta_VegC_GLC
    for (i in 2:(ncol(input_dVegC_GLC_de))){
      output_lm <- lm(input_dVegC_GLC_de[,i]~input_dVegC_GLC_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_dVegC_GLC_de[,i] <- output_ret
    }  
    
    input_npp_GLC_de <- input_npp_GLC1
    for (i in 2:(ncol(input_npp_GLC_de))){
      output_lm <- lm(input_npp_GLC_de[,i]~input_npp_GLC_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_npp_GLC_de[,i] <- output_ret
    }
    
    input_gpp_GLC_de <- input_gpp_GLC1
    for (i in 2:(ncol(input_gpp_GLC_de))){
      output_lm <- lm(input_gpp_GLC_de[,i]~input_gpp_GLC_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_gpp_GLC_de[,i] <- output_ret
    }  
    
    input_sink_GLC_de <- input_sink_glc
    for (i in 2:(ncol(input_sink_GLC_de))){
      output_lm <- lm(input_sink_GLC_de[,i]~input_sink_GLC_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_sink_GLC_de[,i] <- output_ret
    }  
    
    #tree density
    input_dVegC_td_de <- input_delta_VegC_td
    for (i in 2:(ncol(input_dVegC_td_de))){
      output_lm <- lm(input_dVegC_td_de[,i]~input_dVegC_td_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_dVegC_td_de[,i] <- output_ret
    }  
    
    input_npp_td_de <- input_npp_td1
    for (i in 2:(ncol(input_npp_td_de))){
      output_lm <- lm(input_npp_td_de[,i]~input_npp_td_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_npp_td_de[,i] <- output_ret
    }
    
    input_gpp_td_de <- input_gpp_td1
    for (i in 2:(ncol(input_gpp_td_de))){
      output_lm <- lm(input_gpp_td_de[,i]~input_gpp_td_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_gpp_td_de[,i] <- output_ret
    }  
    
    input_sink_td_de <- input_sink_td
    for (i in 2:(ncol(input_sink_td_de))){
      output_lm <- lm(input_sink_td_de[,i]~input_sink_td_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_sink_td_de[,i] <- output_ret
    } 
    
    #data normalization
    #GLC
    input_dVegC_GLC_de_scaled = scale(input_dVegC_GLC_de[,2:length(input_dVegC_GLC_de)],center = TRUE,scale = TRUE)
    input_npp_GLC_de_scaled = scale(input_npp_GLC_de[,2:length(input_npp_GLC_de)],center = TRUE,scale = TRUE)
    input_gpp_GLC_de_scaled = scale(input_gpp_GLC_de[,2:length(input_gpp_GLC_de)],center = TRUE,scale = TRUE)
    input_sink_GLC_de_scaled = scale(input_sink_GLC_de[,2:length(input_sink_GLC_de)],center = TRUE,scale = TRUE)
    
    #tree density
    input_dVegC_td_de_scaled = scale(input_dVegC_td_de[,2:length(input_dVegC_td_de)],center = TRUE,scale = TRUE)
    input_npp_td_de_scaled = scale(input_npp_td_de[,2:length(input_npp_td_de)],center = TRUE,scale = TRUE)
    input_gpp_td_de_scaled = scale(input_gpp_td_de[,2:length(input_gpp_td_de)],center = TRUE,scale = TRUE)
    input_sink_td_de_scaled = scale(input_sink_td_de[,2:length(input_sink_td_de)],center = TRUE,scale = TRUE)
    
    #convert to dfs
    #GLC
    dVegC_GLC_de_scaled_df = data.frame(input_dVegC_GLC_de_scaled)
    npp_GLC_de_scaled_df = data.frame(input_npp_GLC_de_scaled)
    gpp_GLC_de_scaled_df = data.frame(input_gpp_GLC_de_scaled)
    sink_GLC_de_scaled_df = data.frame(input_sink_GLC_de_scaled)
    
    #tree density
    dVegC_td_de_scaled_df = data.frame(input_dVegC_td_de_scaled)
    npp_td_de_scaled_df = data.frame(input_npp_td_de_scaled)
    gpp_td_de_scaled_df = data.frame(input_gpp_td_de_scaled)
    sink_td_de_scaled_df = data.frame(input_sink_td_de_scaled)
    
    #calculate mean and sd
    #GLC
    delta_VegC_GLC_de_mean = apply(dVegC_GLC_de_scaled_df[,1:ncol(dVegC_GLC_de_scaled_df)],1,mean,na.rm=TRUE)
    npp_GLC_de_mean = apply(npp_GLC_de_scaled_df[,1:ncol(npp_GLC_de_scaled_df)],1,mean,na.rm=TRUE)
    gpp_GLC_de_mean = apply(gpp_GLC_de_scaled_df[,1:ncol(gpp_GLC_de_scaled_df)],1,mean,na.rm=TRUE)
    
    delta_VegC_GLC_de_rsd = apply(dVegC_GLC_de_scaled_df[,1:ncol(dVegC_GLC_de_scaled_df)],1,sd,na.rm=TRUE)
    npp_GLC_de_rsd = apply(npp_GLC_de_scaled_df[,1:ncol(npp_GLC_de_scaled_df)],1,sd,na.rm=TRUE)
    gpp_GLC_de_rsd = apply(gpp_GLC_de_scaled_df[,1:ncol(gpp_GLC_de_scaled_df)],1,sd,na.rm=TRUE)
    
    #tree density
    delta_VegC_td_de_mean = apply(dVegC_td_de_scaled_df[,1:ncol(dVegC_td_de_scaled_df)],1,mean,na.rm=TRUE)
    npp_td_de_mean = apply(npp_td_de_scaled_df[,1:ncol(npp_td_de_scaled_df)],1,mean,na.rm=TRUE)
    gpp_td_de_mean = apply(gpp_td_de_scaled_df[,1:ncol(gpp_td_de_scaled_df)],1,mean,na.rm=TRUE)
    
    delta_VegC_td_de_rsd = apply(dVegC_td_de_scaled_df[,1:ncol(dVegC_td_de_scaled_df)],1,sd,na.rm=TRUE)
    npp_td_de_rsd = apply(npp_td_de_scaled_df[,1:ncol(npp_td_de_scaled_df)],1,sd,na.rm=TRUE)
    gpp_td_de_rsd = apply(gpp_td_de_scaled_df[,1:ncol(gpp_td_de_scaled_df)],1,sd,na.rm=TRUE)
    
    #add to dfs
    #GLC
    dVegC_GLC_de_scaled_df$mean = delta_VegC_GLC_de_mean
    dVegC_GLC_de_scaled_df$sd = delta_VegC_GLC_de_rsd
    dVegC_GLC_de_scaled_df$min = delta_VegC_GLC_de_mean - 2 * delta_VegC_GLC_de_rsd
    dVegC_GLC_de_scaled_df$max = delta_VegC_GLC_de_mean + 2 * delta_VegC_GLC_de_rsd
    
    npp_GLC_de_scaled_df$mean = npp_GLC_de_mean
    npp_GLC_de_scaled_df$sd = npp_GLC_de_rsd
    npp_GLC_de_scaled_df$min = npp_GLC_de_mean - 2 * npp_GLC_de_rsd
    npp_GLC_de_scaled_df$max = npp_GLC_de_mean + 2 * npp_GLC_de_rsd
    
    gpp_GLC_de_scaled_df$mean = gpp_GLC_de_mean
    gpp_GLC_de_scaled_df$sd = gpp_GLC_de_rsd
    gpp_GLC_de_scaled_df$min = gpp_GLC_de_mean - 2 * gpp_GLC_de_rsd
    gpp_GLC_de_scaled_df$max = gpp_GLC_de_mean + 2 * gpp_GLC_de_rsd
    
    #tree density
    dVegC_td_de_scaled_df$mean = delta_VegC_td_de_mean
    dVegC_td_de_scaled_df$sd = delta_VegC_td_de_rsd
    dVegC_td_de_scaled_df$min = delta_VegC_td_de_mean - 2 * delta_VegC_td_de_rsd
    dVegC_td_de_scaled_df$max = delta_VegC_td_de_mean + 2 * delta_VegC_td_de_rsd
    
    npp_td_de_scaled_df$mean = npp_td_de_mean
    npp_td_de_scaled_df$sd = npp_td_de_rsd
    npp_td_de_scaled_df$min = npp_td_de_mean - 2 * npp_td_de_rsd
    npp_td_de_scaled_df$max = npp_td_de_mean + 2 * npp_td_de_rsd
    
    gpp_td_de_scaled_df$mean = gpp_td_de_mean
    gpp_td_de_scaled_df$sd = gpp_td_de_rsd
    gpp_td_de_scaled_df$min = gpp_td_de_mean - 2 * gpp_td_de_rsd
    gpp_td_de_scaled_df$max = gpp_td_de_mean + 2 * gpp_td_de_rsd 
    
    #add year to dfs
    #GLC
    dVegC_GLC_de_scaled_df$Year = c(1982:2010)
    npp_GLC_de_scaled_df$Year = c(1982:2010)
    gpp_GLC_de_scaled_df$Year = c(1982:2010)
    sink_GLC_de_scaled_df$Year = c(1982:2010)
    
    #tree density
    dVegC_td_de_scaled_df$Year = c(1982:2010)
    npp_td_de_scaled_df$Year = c(1982:2010)
    gpp_td_de_scaled_df$Year = c(1982:2010)
    sink_td_de_scaled_df$Year = c(1982:2010)
  }
  
  #plot code for global
  {
    p2 <- ggplot()+
      geom_ribbon(data = gpp_td_de_scaled_df,aes(x=Year,ymin= min, 
                                                 ymax= max),fill = "lightblue",alpha = 0.4)+
      geom_ribbon(data = npp_td_de_scaled_df,aes(x=Year,ymin= min, 
                                                 ymax= max),fill = "green",alpha = 0.4)+
      geom_ribbon(data = dVegC_td_de_scaled_df,aes(x=Year,ymin= min, 
                                                   ymax= max),fill = "pink",alpha = 0.4)+
      geom_line(data = gpp_td_de_scaled_df,aes(x=Year, 
                                               y=mean,color = "royalblue4"),linetype="solid",size =1)+
      geom_line(data = npp_td_de_scaled_df,aes(x=Year,y= mean,color = "green4"),linetype="solid",size = 1)+
      geom_line(data = dVegC_td_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
      ylab("Nomalized index")+
      xlab("Year")+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("royalblue4"="royalblue4","green4" = "green4","lightcoral"="lightcoral"), labels = c('GPP_rhythm','NPP_rhythm','d_VegC_rhythm'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = "bottom",legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    
    p2
    
    
    p1 <- ggplot()+
      geom_ribbon(data = gpp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
                                                  ymax= max),fill = "lightblue",alpha = 0.4)+
      geom_ribbon(data = npp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
                                                  ymax= max),fill = "green",alpha = 0.4)+
      geom_ribbon(data = dVegC_GLC_de_scaled_df,aes(x=Year,ymin= min, 
                                                    ymax= max),fill = "pink",alpha = 0.4)+
      geom_line(data = gpp_GLC_de_scaled_df,aes(x=Year, 
                                                y=mean,color = "royalblue4"),linetype="solid",size =1)+
      geom_line(data = npp_GLC_de_scaled_df,aes(x=Year,y= mean,color = "green4"),linetype="solid",size = 1)+
      geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
      ylab("Nomalized index")+
      xlab("Year")+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("royalblue4"="royalblue4","green4" = "green4","lightcoral"="lightcoral"), labels = c('GPP_rhythm','NPP_rhythm','d_VegC_rhythm'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = "bottom",legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    
    
    p1
  }
  #plot code for North America
  {
    p2 <- ggplot()+
      geom_ribbon(data = gpp_td_de_scaled_df,aes(x=Year,ymin= min, 
                                                 ymax= max),fill = "lightblue",alpha = 0.4)+
      geom_ribbon(data = npp_td_de_scaled_df,aes(x=Year,ymin= min, 
                                                 ymax= max),fill = "green",alpha = 0.4)+
      geom_ribbon(data = dVegC_td_de_scaled_df,aes(x=Year,ymin= min, 
                                                   ymax= max),fill = "pink",alpha = 0.4)+
      geom_line(data = gpp_td_de_scaled_df,aes(x=Year, 
                                               y=mean,color = "royalblue4"),linetype="solid",size =1)+
      geom_line(data = npp_td_de_scaled_df,aes(x=Year,y= mean,color = "green4"),linetype="solid",size = 1)+
      geom_line(data = dVegC_td_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
      geom_line(data = sink_td_de_scaled_df,aes(x=Year,y= input_sink_td_de_scaled,color = "black"),linetype="solid",size = 1)+
      ylab("Nomalized index")+
      xlab("Year")+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("royalblue4"="royalblue4","green4" = "green4","lightcoral"="lightcoral","black"="black"), labels = c('sink_rhythm','NPP_rhythm','d_VegC_rhythm','GPP_rhythm'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = "bottom",legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    
    p2
    
    
    p1 <- ggplot()+
      geom_ribbon(data = gpp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
                                                  ymax= max),fill = "lightblue",alpha = 0.4)+
      geom_ribbon(data = npp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
                                                  ymax= max),fill = "green",alpha = 0.4)+
      geom_ribbon(data = dVegC_GLC_de_scaled_df,aes(x=Year,ymin= min, 
                                                    ymax= max),fill = "pink",alpha = 0.4)+
      geom_line(data = gpp_GLC_de_scaled_df,aes(x=Year, 
                                                y=mean,color = "royalblue4"),linetype="solid",size =1)+
      geom_line(data = npp_GLC_de_scaled_df,aes(x=Year,y= mean,color = "green4"),linetype="solid",size = 1)+
      geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
      geom_line(data = sink_GLC_de_scaled_df,aes(x=Year,y= input_sink_GLC_de_scaled,color = "black"),linetype="solid",size = 1)+
      ylab("Nomalized index")+
      xlab("Year")+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("royalblue4"="royalblue4","green4" = "green4","lightcoral"="lightcoral","black" = "black"), labels = c('sink_rhythm','NPP_rhythm','d_VegC_rhythm','GPP_rhythm'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = "bottom",legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    
    
    p1
    
    
  }
}

#GPP analysis
#TRENDY vs sink-based map North America
{
  setwd("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\TRENDY\\")
  
  #input_delta_VegC_GLC = read.csv("North_America_delta_VegC_GLC_mask_summary.csv")
  #input_npp_GLC = read.csv("North_America_npps2_GLC_mask_summary.csv")
  
  #input_delta_VegC_td = read.csv("North_America_delta_VegC_tree_density_mask_summary.csv")
  #input_npp_td = read.csv("North_America_npps2_tree_density_mask_summary.csv")
  #input_gpp_td = read.csv("North_America_gpps2_tree_density_mask_summary.csv")
  
  #input_sink_td = read.csv("North_America_s808_tree_density_mask_summary.csv")
  input_sink_glc = read.csv("North_America_s808_age_corrected_GLC_mask_per_area_area_ratio_correct_summary.csv")
  
  input_gpp_FLUXCOM = read.csv("North_America_FLUXCOM_gpp_GLC_mask_summary_area_corrected.csv")
  input_gpp_EC_LUE = read.csv("North_America_EC_LUE_gpp_GLC_mask_summary_area_corrected.csv")
  
  
  #correct the year col
  input_gpp_GLC$X = c(1982:2010)
  input_gpp_FLUXCOM$X = c(1982:2010)
  input_gpp_EC_LUE$X = c(1982:2010)
  #input_delta_VegC_GLC$X = c(1982:2010)
  #input_npp_GLC$X = c(1982:2010)
  #input_gpp_GLC$X = c(1982:2010)
  
  #input_delta_VegC_td$X = c(1982:2010)
  #input_npp_td$X = c(1982:2010)
  #input_gpp_td$X = c(1982:2010)
  
  input_sink_glc$X = c(1982:2010)
  #input_sink_td$X = c(1982:2010)
  
  #remove the na cols
  #input_npp_GLC1 = input_npp_GLC[,-17]
  #input_npp_td1 = input_npp_td[,-17]
  
  input_gpp_GLC1 = input_gpp_GLC[,-12]
  #input_gpp_td1 = input_gpp_td[,-12]
  
  #detrended output
  {
    #GLC
    #input_npp_GLC$X = c(1982:2010)
    #input_dVegC_GLC_de <- input_delta_VegC_GLC
    #for (i in 2:(ncol(input_dVegC_GLC_de))){
    #  output_lm <- lm(input_dVegC_GLC_de[,i]~input_dVegC_GLC_de[,1],na.action=na.exclude)
    #  output_ret <- residuals(output_lm)
    #  input_dVegC_GLC_de[,i] <- output_ret
    #}  
    
    #input_npp_GLC_de <- input_npp_GLC1
    #for (i in 2:(ncol(input_npp_GLC_de))){
    #  output_lm <- lm(input_npp_GLC_de[,i]~input_npp_GLC_de[,1],na.action=na.exclude)
    #  output_ret <- residuals(output_lm)
    #  input_npp_GLC_de[,i] <- output_ret
    #}
    
    input_gpp_GLC_de <- input_gpp_GLC1
    for (i in 2:(ncol(input_gpp_GLC_de))){
      output_lm <- lm(input_gpp_GLC_de[,i]~input_gpp_GLC_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_gpp_GLC_de[,i] <- output_ret
    } 
    
    input_gpp_FLUXCOM_GLC_de <- input_gpp_FLUXCOM
    for (i in 2:(ncol(input_gpp_FLUXCOM_GLC_de))){
      output_lm <- lm(input_gpp_FLUXCOM_GLC_de[,i]~input_gpp_FLUXCOM_GLC_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_gpp_FLUXCOM_GLC_de[,i] <- output_ret
    }
    
    input_gpp_EC_LUE_GLC_de <- input_gpp_EC_LUE
    for (i in 2:(ncol(input_gpp_EC_LUE_GLC_de))){
      output_lm <- lm(input_gpp_EC_LUE_GLC_de[,i]~input_gpp_EC_LUE_GLC_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_gpp_EC_LUE_GLC_de[,i] <- output_ret
    }
    
    #input_gpp_GLC_de <- input_gpp_GLC1
    #for (i in 2:(ncol(input_gpp_GLC_de))){
    #  output_lm <- lm(input_gpp_GLC_de[,i]~input_gpp_GLC_de[,1],na.action=na.exclude)
    #  output_ret <- residuals(output_lm)
    #  input_gpp_GLC_de[,i] <- output_ret
    #}
    
    input_sink_GLC_de <- input_sink_glc
    for (i in 2:(ncol(input_sink_GLC_de))){
      output_lm <- lm(input_sink_GLC_de[,i]~input_sink_GLC_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_sink_GLC_de[,i] <- output_ret
    }
    
    #tree density
    #input_dVegC_td_de <- input_delta_VegC_td
    #for (i in 2:(ncol(input_dVegC_td_de))){
    #  output_lm <- lm(input_dVegC_td_de[,i]~input_dVegC_td_de[,1],na.action=na.exclude)
    #  output_ret <- residuals(output_lm)
    #  input_dVegC_td_de[,i] <- output_ret
    #}  
    
    #input_npp_td_de <- input_npp_td1
    #for (i in 2:(ncol(input_npp_td_de))){
    #  output_lm <- lm(input_npp_td_de[,i]~input_npp_td_de[,1],na.action=na.exclude)
    #  output_ret <- residuals(output_lm)
    #  input_npp_td_de[,i] <- output_ret
    #}
    
    #input_gpp_td_de <- input_gpp_td1
    #for (i in 2:(ncol(input_gpp_td_de))){
    #  output_lm <- lm(input_gpp_td_de[,i]~input_gpp_td_de[,1],na.action=na.exclude)
    #  output_ret <- residuals(output_lm)
    #  input_gpp_td_de[,i] <- output_ret
    #}  
    
    #input_sink_td_de <- input_sink_td
    #for (i in 2:(ncol(input_sink_td_de))){
    #  output_lm <- lm(input_sink_td_de[,i]~input_sink_td_de[,1],na.action=na.exclude)
    #  output_ret <- residuals(output_lm)
    #  input_sink_td_de[,i] <- output_ret
    #} 
    
    #data normalization
    #GLC
    #input_dVegC_GLC_de_scaled = scale(input_dVegC_GLC_de[,2:length(input_dVegC_GLC_de)],center = TRUE,scale = TRUE)
    #input_npp_GLC_de_scaled = scale(input_npp_GLC_de[,2:length(input_npp_GLC_de)],center = TRUE,scale = TRUE)
    input_gpp_GLC_de_scaled = scale(input_gpp_GLC_de[,2:length(input_gpp_GLC_de)],center = TRUE,scale = TRUE)
    input_gpp_EC_LUE_GLC_de_scaled = scale(input_gpp_EC_LUE_GLC_de[,2:length(input_gpp_EC_LUE_GLC_de)],center = TRUE,scale = TRUE)
    input_gpp_FLUXCOM_GLC_de_scaled = scale(input_gpp_FLUXCOM_GLC_de[,2:length(input_gpp_FLUXCOM_GLC_de)],center = TRUE,scale = TRUE)
    input_sink_GLC_de_scaled = scale(input_sink_GLC_de[,2:length(input_sink_GLC_de)],center = TRUE,scale = TRUE)
    
    #tree density
    #input_dVegC_td_de_scaled = scale(input_dVegC_td_de[,2:length(input_dVegC_td_de)],center = TRUE,scale = TRUE)
    #input_npp_td_de_scaled = scale(input_npp_td_de[,2:length(input_npp_td_de)],center = TRUE,scale = TRUE)
    #input_gpp_td_de_scaled = scale(input_gpp_td_de[,2:length(input_gpp_td_de)],center = TRUE,scale = TRUE)
    #input_sink_td_de_scaled = scale(input_sink_td_de[,2:length(input_sink_td_de)],center = TRUE,scale = TRUE)
    
    #convert to dfs
    #GLC
    #dVegC_GLC_de_scaled_df = data.frame(input_dVegC_GLC_de_scaled)
    #npp_GLC_de_scaled_df = data.frame(input_npp_GLC_de_scaled)
    gpp_GLC_de_scaled_df = data.frame(input_gpp_GLC_de_scaled)
    sink_GLC_de_scaled_df = data.frame(input_sink_GLC_de_scaled)
    gpp_FLUXCOM_GLC_de_scaled_df = data.frame(input_gpp_FLUXCOM_GLC_de_scaled)    
    gpp_EC_LUE_GLC_de_scaled_df = data.frame(input_gpp_EC_LUE_GLC_de_scaled)    
    
    #tree density
    #dVegC_td_de_scaled_df = data.frame(input_dVegC_td_de_scaled)
    #npp_td_de_scaled_df = data.frame(input_npp_td_de_scaled)
    #gpp_td_de_scaled_df = data.frame(input_gpp_td_de_scaled)
    #sink_td_de_scaled_df = data.frame(input_sink_td_de_scaled)
    
    #calculate mean and sd
    #GLC
    #delta_VegC_GLC_de_mean = apply(dVegC_GLC_de_scaled_df[,1:ncol(dVegC_GLC_de_scaled_df)],1,mean,na.rm=TRUE)
    #npp_GLC_de_mean = apply(npp_GLC_de_scaled_df[,1:ncol(npp_GLC_de_scaled_df)],1,mean,na.rm=TRUE)
    gpp_GLC_de_mean = apply(gpp_GLC_de_scaled_df[,1:ncol(gpp_GLC_de_scaled_df)],1,mean,na.rm=TRUE)
    # gpp_FLUXCOM_GLC_de_mean = apply(gpp_FLUXCOM_GLC_de_scaled_df[,1:ncol(gpp_FLUXCOM_GLC_de_scaled_df)],1,mean,na.rm=TRUE)
    # gpp_EC_LUE_GLC_de_mean = apply(gpp_EC_LUE_GLC_de_scaled_df[,1:ncol(gpp_EC_LUE_GLC_de_scaled_df)],1,mean,na.rm=TRUE)
    
    #delta_VegC_GLC_de_rsd = apply(dVegC_GLC_de_scaled_df[,1:ncol(dVegC_GLC_de_scaled_df)],1,sd,na.rm=TRUE)
    #npp_GLC_de_rsd = apply(npp_GLC_de_scaled_df[,1:ncol(npp_GLC_de_scaled_df)],1,sd,na.rm=TRUE)
    gpp_GLC_de_rsd = apply(gpp_GLC_de_scaled_df[,1:ncol(gpp_GLC_de_scaled_df)],1,sd,na.rm=TRUE)
    #gpp_FLUXCOM_GLC_de_rsd = apply(gpp_FLUXCOM_GLC_de_scaled_df[,1:ncol(gpp_FLUXCOM_GLC_de_scaled_df)],1,sd,na.rm=TRUE)
    #gpp_EC_LUE_GLC_de_rsd = apply(gpp_EC_LUE_GLC_de_scaled_df[,1:ncol(gpp_EC_LUE_GLC_de_scaled_df)],1,sd,na.rm=TRUE)
    #tree density
    #delta_VegC_td_de_mean = apply(dVegC_td_de_scaled_df[,1:ncol(dVegC_td_de_scaled_df)],1,mean,na.rm=TRUE)
    #npp_td_de_mean = apply(npp_td_de_scaled_df[,1:ncol(npp_td_de_scaled_df)],1,mean,na.rm=TRUE)
    #gpp_td_de_mean = apply(gpp_td_de_scaled_df[,1:ncol(gpp_td_de_scaled_df)],1,mean,na.rm=TRUE)
    
    #delta_VegC_td_de_rsd = apply(dVegC_td_de_scaled_df[,1:ncol(dVegC_td_de_scaled_df)],1,sd,na.rm=TRUE)
    #npp_td_de_rsd = apply(npp_td_de_scaled_df[,1:ncol(npp_td_de_scaled_df)],1,sd,na.rm=TRUE)
    #gpp_td_de_rsd = apply(gpp_td_de_scaled_df[,1:ncol(gpp_td_de_scaled_df)],1,sd,na.rm=TRUE)
    
    #add to dfs
    #GLC
    #dVegC_GLC_de_scaled_df$mean = delta_VegC_GLC_de_mean
    #dVegC_GLC_de_scaled_df$sd = delta_VegC_GLC_de_rsd
    #dVegC_GLC_de_scaled_df$min = delta_VegC_GLC_de_mean - 2 * delta_VegC_GLC_de_rsd
    #dVegC_GLC_de_scaled_df$max = delta_VegC_GLC_de_mean + 2 * delta_VegC_GLC_de_rsd
    
    #npp_GLC_de_scaled_df$mean = npp_GLC_de_mean
    #npp_GLC_de_scaled_df$sd = npp_GLC_de_rsd
    #npp_GLC_de_scaled_df$min = npp_GLC_de_mean - 2 * npp_GLC_de_rsd
    #npp_GLC_de_scaled_df$max = npp_GLC_de_mean + 2 * npp_GLC_de_rsd
    
    gpp_GLC_de_scaled_df$mean = gpp_GLC_de_mean
    gpp_GLC_de_scaled_df$sd = gpp_GLC_de_rsd
    gpp_GLC_de_scaled_df$min = gpp_GLC_de_mean -  * gpp_GLC_de_rsd
    gpp_GLC_de_scaled_df$max = gpp_GLC_de_mean +  * gpp_GLC_de_rsd
    
    #gpp_FLUXCOM_GLC_de_scaled_df$mean = gpp_FLUXCOM_GLC_de_mean
    #gpp_FLUXCOM_GLC_de_scaled_df$sd = gpp_FLUXCOM_GLC_de_rsd
    #gpp_FLUXCOM_GLC_de_scaled_df$min = gpp_FLUXCOM_GLC_de_mean - 2 * gpp_FLUXCOM_GLC_de_rsd
    #gpp_FLUXCOM_GLC_de_scaled_df$max = gpp_FLUXCOM_GLC_de_mean + 2 * gpp_FLUXCOM_GLC_de_rsd
    
    #gpp_EC_LUE_GLC_de_scaled_df$mean = gpp_EC_LUE_GLC_de_mean
    #gpp_EC_LUE_GLC_de_scaled_df$sd = gpp_EC_LUE_GLC_de_rsd
    #gpp_EC_LUE_GLC_de_scaled_df$min = gpp_EC_LUE_GLC_de_mean - 2 * gpp_EC_LUE_GLC_de_rsd
    #gpp_EC_LUE_GLC_de_scaled_df$max = gpp_EC_LUE_GLC_de_mean + 2 * gpp_EC_LUE_GLC_de_rsd
    
    #tree density
    #dVegC_td_de_scaled_df$mean = delta_VegC_td_de_mean
    #dVegC_td_de_scaled_df$sd = delta_VegC_td_de_rsd
    #dVegC_td_de_scaled_df$min = delta_VegC_td_de_mean - 2 * delta_VegC_td_de_rsd
    #dVegC_td_de_scaled_df$max = delta_VegC_td_de_mean + 2 * delta_VegC_td_de_rsd
    
    #npp_td_de_scaled_df$mean = npp_td_de_mean
    #npp_td_de_scaled_df$sd = npp_td_de_rsd
    #npp_td_de_scaled_df$min = npp_td_de_mean - 2 * npp_td_de_rsd
    #npp_td_de_scaled_df$max = npp_td_de_mean + 2 * npp_td_de_rsd
    
    #gpp_td_de_scaled_df$mean = gpp_td_de_mean
    #gpp_td_de_scaled_df$sd = gpp_td_de_rsd
    #gpp_td_de_scaled_df$min = gpp_td_de_mean - 2 * gpp_td_de_rsd
    #gpp_td_de_scaled_df$max = gpp_td_de_mean + 2 * gpp_td_de_rsd 
    
    #add year to dfs
    #GLC
    #dVegC_GLC_de_scaled_df$Year = c(1982:2010)
    #npp_GLC_de_scaled_df$Year = c(1982:2010)
    gpp_GLC_de_scaled_df$Year = c(1982:2010)
    sink_GLC_de_scaled_df$Year = c(1982:2010)
    gpp_FLUXCOM_GLC_de_scaled_df$Year = c(1982:2010)
    gpp_EC_LUE_GLC_de_scaled_df$Year = c(1982:2010)
    #tree density
    #dVegC_td_de_scaled_df$Year = c(1982:2010)
    #npp_td_de_scaled_df$Year = c(1982:2010)
    #gpp_td_de_scaled_df$Year = c(1982:2010)
    #sink_td_de_scaled_df$Year = c(1982:2010)
  }
  
  #plot code for global
  {
    #p2 <- ggplot()+
    #  geom_ribbon(data = gpp_td_de_scaled_df,aes(x=Year,ymin= min, 
    #                                             ymax= max),fill = "lightblue",alpha = 0.4)+
    #geom_ribbon(data = npp_td_de_scaled_df,aes(x=Year,ymin= min, 
    #                                           ymax= max),fill = "green",alpha = 0.4)+
    #geom_ribbon(data = dVegC_td_de_scaled_df,aes(x=Year,ymin= min, 
    #                                             ymax= max),fill = "pink",alpha = 0.4)+
    #  geom_line(data = gpp_td_de_scaled_df,aes(x=Year, 
    #                                          y=mean,color = "royalblue4"),linetype="solid",size =1)+
    #    geom_line(data = npp_td_de_scaled_df,aes(x=Year,y= mean,color = "green4"),linetype="solid",size = 1)+
    #    geom_line(data = dVegC_td_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
    #    ylab("Nomalized index")+
    #    xlab("Year")+
    #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
    #    scale_color_manual(name = '', 
    #                       values =c("royalblue4"="royalblue4","green4" = "green4","lightcoral"="lightcoral"), labels = c('GPP_rhythm','NPP_rhythm','d_VegC_rhythm'))+
    #    theme_set(theme_bw())+
    #    theme(panel.grid.major=element_line(colour=NA))+
    #    theme(legend.position = "bottom",legend.text = element_text(size = 12))+
    #    theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    
    #  p2
    
    
    p1 <- ggplot()+
      geom_ribbon(data = gpp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
                                                  ymax= max),fill = "lightblue",alpha = 0.4)+
      #   geom_ribbon(data = npp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
      #                                                ymax= max),fill = "green",alpha = 0.4)+
      #    geom_ribbon(data = dVegC_GLC_de_scaled_df,aes(x=Year,ymin= min, 
      #                                                  ymax= max),fill = "pink",alpha = 0.4)+
      geom_line(data = gpp_GLC_de_scaled_df,aes(x=Year, 
                                                y=mean,color = "royalblue4"),linetype="solid",size =1)+
      #    geom_line(data = npp_GLC_de_scaled_df,aes(x=Year,y= mean,color = "green4"),linetype="solid",size = 1)+
      #    geom_line(data = dVegC_GLC_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
      ylab("z-score")+
      xlab("Year")+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("royalblue4"="royalblue4","green4" = "green4","lightcoral"="lightcoral"), labels = c('GPP_rhythm','NPP_rhythm','d_VegC_rhythm'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = "bottom",legend.text = element_text(size = 12))+
      theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    
    
    p1
  }
  #plot code for North America
  {
    # p2 <- ggplot()+
    #   geom_ribbon(data = gpp_td_de_scaled_df,aes(x=Year,ymin= min, 
    #                                              ymax= max),fill = "lightblue",alpha = 0.4)+
    #   geom_ribbon(data = npp_td_de_scaled_df,aes(x=Year,ymin= min, 
    #                                              ymax= max),fill = "green",alpha = 0.4)+
    #   geom_ribbon(data = dVegC_td_de_scaled_df,aes(x=Year,ymin= min, 
    #                                                ymax= max),fill = "pink",alpha = 0.4)+
    #   geom_line(data = gpp_td_de_scaled_df,aes(x=Year, 
    #                                            y=mean,color = "royalblue4"),linetype="solid",size =1)+
    #   geom_line(data = npp_td_de_scaled_df,aes(x=Year,y= mean,color = "green4"),linetype="solid",size = 1)+
    #   geom_line(data = dVegC_td_de_scaled_df,aes(x=Year,y= mean,color = "lightcoral"),linetype="solid",size = 1)+
    #   geom_line(data = sink_td_de_scaled_df,aes(x=Year,y= input_sink_td_de_scaled,color = "black"),linetype="solid",size = 1)+
    #   ylab("Nomalized index")+
    #   xlab("Year")+
    #   #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
    #   scale_color_manual(name = '', 
    #                      values =c("royalblue4"="royalblue4","green4" = "green4","lightcoral"="lightcoral","black"="black"), labels = c('sink_rhythm','NPP_rhythm','d_VegC_rhythm','GPP_rhythm'))+
    #   theme_set(theme_bw())+
    #   theme(panel.grid.major=element_line(colour=NA))+
    #   theme(legend.position = "bottom",legend.text = element_text(size = 12))+
    #   theme(axis.text = element_text(size =12),axis.text.x = element_text(size =12),axis.text.y = element_text(size =12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=12))
    # 
    # p2
    
    
    p1 <- ggplot()+
      geom_ribbon(data = gpp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
                                                  ymax= max),fill = "gray",alpha = 0.4)+
      #geom_ribbon(data = npp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
      #                                            ymax= max),fill = "green",alpha = 0.4)+
      #geom_ribbon(data = dVegC_GLC_de_scaled_df,aes(x=Year,ymin= min, 
      #                                              ymax= max),fill = "pink",alpha = 0.4)+
      geom_line(data = gpp_GLC_de_scaled_df,aes(x=Year, 
                                                y=mean,color = "gray21"),linetype="solid",size =1)+
      geom_line(data = gpp_FLUXCOM_GLC_de_scaled_df,aes(x=Year,y= input_gpp_FLUXCOM_GLC_de_scaled,color = "green4"),linetype="solid",size = 1)+
      geom_line(data = gpp_EC_LUE_GLC_de_scaled_df,aes(x=Year,y= input_gpp_EC_LUE_GLC_de_scaled,color = "lightcoral"),linetype="solid",size = 1)+
      geom_line(data = sink_GLC_de_scaled_df,aes(x=Year,y= input_sink_GLC_de_scaled,color = "royalblue"),linetype="solid",size = 1)+
      ylab("z-score")+
      xlab("Year")+
      ylim(-4,4)+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("gray21"="gray21","green4" = "green4","lightcoral"="lightcoral","royalblue"="royalblue"), labels = c('TRENDY_rhythm','FLUXCOM_rhythm','EC-LUE_rhythm','SINK_rhythm'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = "bottom",legend.text = element_text(face="bold",size = 16))+
      theme(axis.text = element_text(face="bold",size =16),axis.text.x = element_text(face="bold",size =16),axis.text.y = element_text(face="bold",size =16),axis.title.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16))
    
    
    p1
    
    
  }
}

#GPP analysis updated 2021/12/23
#GPP analysis updated 2022/3/29
#from 1984 to 2010 with tree density variations explicitly considered
{
  setwd("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\TRENDY\\")
  
  #input_delta_VegC_GLC = read.csv("North_America_delta_VegC_GLC_mask_summary.csv")
  #input_npp_GLC = read.csv("North_America_npps2_GLC_mask_summary.csv")
  
  #input_delta_VegC_td = read.csv("North_America_delta_VegC_tree_density_mask_summary.csv")
  #input_npp_td = read.csv("North_America_npps2_tree_density_mask_summary.csv")
  #input_gpp_td = read.csv("North_America_gpps2_tree_density_mask_summary.csv")
  
  #input_sink_td = read.csv("North_America_s808_tree_density_mask_summary.csv")
  input_sink_glc_nbr = read.csv("North_America_s808_GLC_mask_per_area_area_ratio_correct_nbr_summary.csv")
  input_sink_glc_ndvi = read.csv("North_America_s808_GLC_mask_per_area_area_ratio_correct_ndvi_summary.csv")
  input_sink_glc_per_tree = read.csv("North_America_s808_age_corrected_GLC_mask_per_area_area_ratio_correct_summary_84_10.csv")
  #ML GPP
  input_gpp_FLUXCOM = read.csv("North_America_FLUXCOM_gpp_GLC_mask_summary_area_corrected_ndvi.csv")
  #Rs GPP
  input_gpp_BEPS = read.csv("North_America_BEPS_gpp_GLC_mask_summary_area_corrected_nbr.csv")
  input_gpp_inf = read.csv("North_America_GPPinf_GLC_mask_summary_area_corrected_nbr.csv")
  input_gpp_EC_LUE = read.csv("North_America_EC_LUE_gpp_GLC_mask_summary_area_corrected_nbr.csv")
  input_gpp_RS_mean = read.csv("North_America_RS_gpp_mean_GLC_mask_summary_area_corrected.csv")
  #PC GPP
  #mean value files
  #input_gpp_trendys2 = read.csv("North_America_Trendys2_gpp_GLC_mask_summary_area_corrected_nbr.csv")
  #input_gpp_trendys3 = read.csv("North_America_Trendys3_gpp_GLC_mask_summary_area_corrected_nbr.csv")
  # input_gpp_trendys2_all = read.csv("North_America_gpps2_GLC_mask_summary_area_corrected_84_10_scaled.csv") 
  # input_gpp_trendys3_all = read.csv("North_America_gpps3_GLC_mask_summary_area_corrected_84_10_scaled.csv")
  # input_gpp_trendys3_all = input_gpp_trendys3_all[,1:18]
  
  
  
  #calculate the sink mean
  input_sink_glc_mean = merge(input_sink_glc_nbr,input_sink_glc_ndvi,by="Year")
  names(input_sink_glc_mean) = c("Year","exp1.nbr","exp2.nbr","exp3.nbr","linear.nbr","logi1.nbr","logi2.nbr","logi3.nbr","mean.nbr","sd.nbr","exp1.ndvi","exp2.ndvi","exp3.ndvi","linear.ndvi","logi1.ndvi","logi2.ndvi","logi3.ndvi","mean.ndvi","sd.ndvi")
  input_sink_glc_mean = input_sink_glc_mean[,c(1,2,3,4,5,6,7,8,11,12,13,14,15,16,17,9,10,18,19)]
  #remove the two sd cols
  input_sink_glc_mean = input_sink_glc_mean[,-c(17,19)]
  input_sink_glc_mean$all_mean = apply(input_sink_glc_mean[,2:(ncol(input_sink_glc_mean)-2)],1,mean,na.rm=TRUE)
  
  #calculate trendy mean
  # input_gpp_trendy_mean = merge(input_gpp_trendys2,input_gpp_trendys3,by="Year")
  # input_gpp_trendy_mean$mean = apply(input_gpp_trendy_mean[,2:3],1,mean,na.rm=TRUE)
  
  
  
  #detrended output
  {
    input_sink_glc_nbr_de <- input_sink_glc_nbr
    for (i in 2:(ncol(input_sink_glc_nbr_de))){
      output_lm <- lm(input_sink_glc_nbr_de[,i]~input_sink_glc_nbr_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_sink_glc_nbr_de[,i] <- output_ret
    } 
    
    input_sink_glc_ndvi_de <- input_sink_glc_ndvi
    for (i in 2:(ncol(input_sink_glc_ndvi_de))){
      output_lm <- lm(input_sink_glc_ndvi_de[,i]~input_sink_glc_ndvi_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_sink_glc_ndvi_de[,i] <- output_ret
    } 
    
    input_sink_glc_per_tree_de <- input_sink_glc_per_tree
    for (i in 2:(ncol(input_sink_glc_per_tree_de))){
      output_lm <- lm(input_sink_glc_per_tree_de[,i]~input_sink_glc_per_tree_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_sink_glc_per_tree_de[,i] <- output_ret
    } 
    
    input_sink_glc_mean_de <- input_sink_glc_mean
    for (i in 2:(ncol(input_sink_glc_mean_de))){
      output_lm <- lm(input_sink_glc_mean_de[,i]~input_sink_glc_mean_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_sink_glc_mean_de[,i] <- output_ret
    }
    
    
    
    input_gpp_FLUXCOM_de <- input_gpp_FLUXCOM
    for (i in 2:(ncol(input_gpp_FLUXCOM_de))){
      output_lm <- lm(input_gpp_FLUXCOM_de[,i]~input_gpp_FLUXCOM_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_gpp_FLUXCOM_de[,i] <- output_ret
    }
    
    input_gpp_EC_LUE_de <- input_gpp_EC_LUE
    for (i in 2:(ncol(input_gpp_EC_LUE_de))){
      output_lm <- lm(input_gpp_EC_LUE_de[,i]~input_gpp_EC_LUE_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_gpp_EC_LUE_de[,i] <- output_ret
    }
    
    input_gpp_BEPS_de <- input_gpp_BEPS
    for (i in 2:(ncol(input_gpp_BEPS_de))){
      output_lm <- lm(input_gpp_BEPS_de[,i]~input_gpp_BEPS_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_gpp_BEPS_de[,i] <- output_ret
    }
    
    input_gpp_RS_mean_de <- input_gpp_RS_mean
    for (i in 2:(ncol(input_gpp_RS_mean_de))){
      output_lm <- lm(input_gpp_RS_mean_de[,i]~input_gpp_RS_mean_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_gpp_RS_mean_de[,i] <- output_ret
    }
    
    input_gpp_inf_de <- input_gpp_inf
    for (i in 2:(ncol(input_gpp_inf_de))){
      output_lm <- lm(input_gpp_inf_de[,i]~input_gpp_inf_de[,1],na.action=na.exclude)
      output_ret <- residuals(output_lm)
      input_gpp_inf_de[,i] <- output_ret
    }
    
    # input_gpp_trendys2_de <- input_gpp_trendys2
    # for (i in 2:(ncol(input_gpp_trendys2_de))){
    #   output_lm <- lm(input_gpp_trendys2_de[,i]~input_gpp_trendys2_de[,1],na.action=na.exclude)
    #   output_ret <- residuals(output_lm)
    #   input_gpp_trendys2_de[,i] <- output_ret
    # }
    # 
    # input_gpp_trendys3_de <- input_gpp_trendys3
    # for (i in 2:(ncol(input_gpp_trendys3_de))){
    #   output_lm <- lm(input_gpp_trendys3_de[,i]~input_gpp_trendys3_de[,1],na.action=na.exclude)
    #   output_ret <- residuals(output_lm)
    #   input_gpp_trendys3_de[,i] <- output_ret
    #   
    #   
    #   input_gpp_trendy_mean_de <- input_gpp_trendy_mean
    #   for (i in 2:(ncol(input_gpp_trendy_mean_de))){
    #     output_lm <- lm(input_gpp_trendy_mean_de[,i]~input_gpp_trendy_mean_de[,1],na.action=na.exclude)
    #     output_ret <- residuals(output_lm)
    #     input_gpp_trendy_mean_de[,i] <- output_ret
    #   }
    }    
    
    #for individual model inputs
     {
    # input_gpp_trendys2_all_de <- input_gpp_trendys2_all
    # for (i in 2:(ncol(input_gpp_trendys2_all_de))){
    #   output_lm <- lm(input_gpp_trendys2_all_de[,i]~input_gpp_trendys2_all_de[,1],na.action=na.exclude)
    #   output_ret <- residuals(output_lm)
    #   input_gpp_trendys2_all_de[,i] <- output_ret
    # }
    # 
    # input_gpp_trendys3_all_de <- input_gpp_trendys3_all
    # for (i in 2:(ncol(input_gpp_trendys3_all_de))){
    #   output_lm <- lm(input_gpp_trendys3_all_de[,i]~input_gpp_trendys3_all_de[,1],na.action=na.exclude)
    #   output_ret <- residuals(output_lm)
    #   input_gpp_trendys3_all_de[,i] <- output_ret
    # }
    # #write the individual model parts out
    # {
    #   write.csv(input_gpp_trendys2_all_de,"North_America_gpps2_GLC_mask_summary_area_corrected_84_10_scaled_de.csv")
    #   write.csv(input_gpp_trendys3_all_de,"North_America_gpps3_GLC_mask_summary_area_corrected_84_10_scaled_de.csv")
    #   
    #   
    # }
     }
    

    input_sink_glc_nbr_de_scaled = scale(input_sink_glc_nbr_de[,2:length(input_sink_glc_nbr_de)],center = TRUE,scale = TRUE)
    input_sink_glc_ndvi_de_scaled = scale(input_sink_glc_ndvi_de[,2:length(input_sink_glc_ndvi_de)],center = TRUE,scale = TRUE)
    input_sink_glc_per_tree_de_scaled = scale(input_sink_glc_per_tree_de[,2:length(input_sink_glc_per_tree_de)],center = TRUE,scale = TRUE)
    input_sink_glc_mean_de_scaled = scale(input_sink_glc_mean_de[,2:length(input_sink_glc_mean_de)],center = TRUE,scale = TRUE)
    input_gpp_FLUXCOM_de_scaled = scale(input_gpp_FLUXCOM_de[,2:length(input_gpp_FLUXCOM_de)],center = TRUE,scale = TRUE)
    input_gpp_EC_LUE_de_scaled = scale(input_gpp_EC_LUE_de[,2:length(input_gpp_EC_LUE_de)],center = TRUE,scale = TRUE)
    input_gpp_BEPS_de_scaled = scale(input_gpp_BEPS_de[,2:length(input_gpp_BEPS_de)],center = TRUE,scale = TRUE)
    input_gpp_inf_de_scaled = scale(input_gpp_inf_de[,2:length(input_gpp_inf_de)],center = TRUE,scale = TRUE)
    input_gpp_RS_mean_de_scaled = scale(input_gpp_RS_mean_de[,2:length(input_gpp_RS_mean_de)],center = TRUE,scale = TRUE)
    #input_gpp_trendys2_de_scaled = scale(input_gpp_trendys2_de[,2:length(input_gpp_trendys2_de)],center = TRUE,scale = TRUE)
    #input_gpp_trendys3_de_scaled = scale(input_gpp_trendys3_de[,2:length(input_gpp_trendys3_de)],center = TRUE,scale = TRUE)  
    #input_gpp_trendy_mean_de_scaled = scale(input_gpp_trendy_mean_de[,2:length(input_gpp_trendy_mean_de)],center = TRUE,scale = TRUE)
    
    
    #convert to dfs
    #GLC
    #dVegC_GLC_de_scaled_df = data.frame(input_dVegC_GLC_de_scaled)
    #npp_GLC_de_scaled_df = data.frame(input_npp_GLC_de_scaled)
    #gpp_GLC_de_scaled_df = data.frame(input_gpp_GLC_de_scaled)
    #sink_GLC_de_scaled_df = data.frame(input_sink_GLC_de_scaled)
    #gpp_FLUXCOM_GLC_de_scaled_df = data.frame(input_gpp_FLUXCOM_GLC_de_scaled)    
    #gpp_EC_LUE_GLC_de_scaled_df = data.frame(input_gpp_EC_LUE_GLC_de_scaled)    
    
    input_sink_glc_nbr_de_scaled_df = data.frame(input_sink_glc_nbr_de_scaled)
    input_sink_glc_ndvi_de_scaled_df = data.frame(input_sink_glc_ndvi_de_scaled)
    input_sink_glc_per_tree_de_scaled_df = data.frame(input_sink_glc_per_tree_de_scaled)
    input_sink_glc_mean_de_scaled_df = data.frame(input_sink_glc_mean_de_scaled)
    input_gpp_FLUXCOM_de_scaled_df = data.frame(input_gpp_FLUXCOM_de_scaled)
    input_gpp_EC_LUE_de_scaled_df = data.frame(input_gpp_EC_LUE_de_scaled)
    input_gpp_BEPS_de_scaled_df = data.frame(input_gpp_BEPS_de_scaled)
    input_gpp_inf_de_scaled_df = data.frame(input_gpp_inf_de_scaled)
    input_gpp_RS_mean_de_scaled_df = data.frame(input_gpp_RS_mean_de_scaled)
    #input_gpp_trendys2_de_scaled_df = data.frame(input_gpp_trendys2_de_scaled)
    #input_gpp_trendys3_de_scaled_df = data.frame(input_gpp_trendys3_de_scaled)
    #input_gpp_trendy_mean_de_scaled_df = data.frame(input_gpp_trendy_mean_de_scaled)
    
    
    year = c(1984:2010)
    input_sink_glc_nbr_de_scaled_df$Year = year
    input_sink_glc_ndvi_de_scaled_df$Year = year
    input_sink_glc_per_tree_de_scaled_df$Year = year
    input_sink_glc_mean_de_scaled_df$Year = year
    input_gpp_FLUXCOM_de_scaled_df$Year = year
    input_gpp_EC_LUE_de_scaled_df$Year = year
    input_gpp_BEPS_de_scaled_df$Year = year
    input_gpp_inf_de_scaled_df$Year = year
    input_gpp_RS_mean_de_scaled_df$Year = year
    #input_gpp_trendys2_de_scaled_df$Year = year
    #input_gpp_trendys3_de_scaled_df$Year = year
    #input_gpp_trendy_mean_de_scaled_df$Year = year
    
    #input the individual model outputs of trendy directly
    input_gpp_trendy_mean_de_scaled_df = read.csv("North_America_gpp_GLC_mask_summary_area_corrected_84_10_scaled_de_s2s3_combined.csv")
    
    #tree density
    #dVegC_td_de_scaled_df = data.frame(input_dVegC_td_de_scaled)
    #npp_td_de_scaled_df = data.frame(input_npp_td_de_scaled)
    #gpp_td_de_scaled_df = data.frame(input_gpp_td_de_scaled)
    #sink_td_de_scaled_df = data.frame(input_sink_td_de_scaled)
    
    #calculate mean and sd
    #GLC
    #delta_VegC_GLC_de_mean = apply(dVegC_GLC_de_scaled_df[,1:ncol(dVegC_GLC_de_scaled_df)],1,mean,na.rm=TRUE)
    #npp_GLC_de_mean = apply(npp_GLC_de_scaled_df[,1:ncol(npp_GLC_de_scaled_df)],1,mean,na.rm=TRUE)
    #gpp_GLC_de_mean = apply(gpp_GLC_de_scaled_df[,1:ncol(gpp_GLC_de_scaled_df)],1,mean,na.rm=TRUE)
    # gpp_FLUXCOM_GLC_de_mean = apply(gpp_FLUXCOM_GLC_de_scaled_df[,1:ncol(gpp_FLUXCOM_GLC_de_scaled_df)],1,mean,na.rm=TRUE)
    # gpp_EC_LUE_GLC_de_mean = apply(gpp_EC_LUE_GLC_de_scaled_df[,1:ncol(gpp_EC_LUE_GLC_de_scaled_df)],1,mean,na.rm=TRUE)
    
    #delta_VegC_GLC_de_rsd = apply(dVegC_GLC_de_scaled_df[,1:ncol(dVegC_GLC_de_scaled_df)],1,sd,na.rm=TRUE)
    #npp_GLC_de_rsd = apply(npp_GLC_de_scaled_df[,1:ncol(npp_GLC_de_scaled_df)],1,sd,na.rm=TRUE)
    input_sink_glc_nbr_de_scaled_rsd = apply(input_sink_glc_nbr_de_scaled_df[,1:(ncol(input_sink_glc_nbr_de_scaled_df)-2)],1,sd,na.rm=TRUE)
    input_sink_glc_ndvi_de_scaled_rsd = apply(input_sink_glc_ndvi_de_scaled_df[,1:(ncol(input_sink_glc_ndvi_de_scaled_df)-2)],1,sd,na.rm=TRUE)    
    input_sink_glc_mean_de_scaled_rsd = apply(input_sink_glc_mean_de_scaled_df[,1:(ncol(input_sink_glc_mean_de_scaled_df)-3)],1,sd,na.rm=TRUE)
    
    #gpp_GLC_de_scaled_df$mean = gpp_GLC_de_mean
    input_sink_glc_nbr_de_scaled_df$sd = input_sink_glc_nbr_de_scaled_rsd
    input_sink_glc_ndvi_de_scaled_df$sd = input_sink_glc_ndvi_de_scaled_rsd
    input_sink_glc_mean_de_scaled_df$all_sd = input_sink_glc_mean_de_scaled_rsd
    input_sink_glc_nbr_de_scaled_df$min = input_sink_glc_nbr_de_scaled_df$mean - input_sink_glc_nbr_de_scaled_rsd
    input_sink_glc_nbr_de_scaled_df$max = input_sink_glc_nbr_de_scaled_df$mean + input_sink_glc_nbr_de_scaled_rsd
    input_sink_glc_ndvi_de_scaled_df$min = input_sink_glc_ndvi_de_scaled_df$mean - input_sink_glc_ndvi_de_scaled_rsd
    input_sink_glc_ndvi_de_scaled_df$max = input_sink_glc_ndvi_de_scaled_df$mean + input_sink_glc_ndvi_de_scaled_rsd
    input_sink_glc_mean_de_scaled_df$all_min = input_sink_glc_mean_de_scaled_df$all_mean + input_sink_glc_mean_de_scaled_rsd
    input_sink_glc_mean_de_scaled_df$all_max = input_sink_glc_mean_de_scaled_df$all_mean - input_sink_glc_mean_de_scaled_rsd
    #calculate min and max for trendy mean
    input_gpp_trendy_mean_de_scaled_df$Z_score_all_min = input_gpp_trendy_mean_de_scaled_df$Z_score_all_mean - input_gpp_trendy_mean_de_scaled_df$Z_score_all_sd
    input_gpp_trendy_mean_de_scaled_df$Z_score_all_max = input_gpp_trendy_mean_de_scaled_df$Z_score_all_mean + input_gpp_trendy_mean_de_scaled_df$Z_score_all_sd
  
  }
  
  #plot code for global
  {
    
    pdf("D:\\MEGA\\Live_cases\\Hybrid\\Global_Synthesis\\Deep_learning\\random_forest\\Graph_output\\Fig3_P2_Z_score_2022_3_29.pdf",width = 15,height = 6)
    p1 <- ggplot()+
      geom_ribbon(data = input_sink_glc_mean_de_scaled_df,aes(x=Year,ymin= all_min,
                                                              ymax= all_max),fill = "gray",alpha = 0.4)+
      geom_ribbon(data = input_gpp_trendy_mean_de_scaled_df,aes(x=Year,ymin= Z_score_all_min,
                                                              ymax= Z_score_all_max),fill = "green4",alpha = 0.2)+
      #geom_ribbon(data = npp_GLC_de_scaled_df,aes(x=Year,ymin= min,
      #                                            ymax= max),fill = "green",alpha = 0.4)+
      #geom_ribbon(data = dVegC_GLC_de_scaled_df,aes(x=Year,ymin= min,
      #                                              ymax= max),fill = "pink",alpha = 0.4)+
      geom_line(data = input_sink_glc_mean_de_scaled_df,aes(x=Year,
                                                            y=all_mean,color = "aa"),linetype="solid",size =1.5)+
      #geom_ribbon(data = input_sink_glc_ndvi_de_scaled_df,aes(x=Year,ymin= min,
      #ymax= max),fill = "gray",alpha = 0.4)+
      #geom_ribbon(data = npp_GLC_de_scaled_df,aes(x=Year,ymin= min,
      #                                            ymax= max),fill = "green",alpha = 0.4)+
      #geom_ribbon(data = dVegC_GLC_de_scaled_df,aes(x=Year,ymin= min,
      #                                              ymax= max),fill = "pink",alpha = 0.4)+
      #geom_line(data = input_sink_glc_ndvi_de_scaled_df,aes(x=Year,
      #y=mean,color = "gray21"),linetype="solid",size =1)+
      geom_line(data = input_gpp_FLUXCOM_de_scaled_df,aes(x=Year,y= input_gpp_FLUXCOM_de_scaled,color = "bb"),linetype="solid",size = 1.5)+
      geom_line(data = input_gpp_RS_mean_de_scaled_df,aes(x=Year,y= input_gpp_RS_mean_de_scaled,color = "cc"),linetype="solid",size = 1.5)+
      geom_line(data = input_gpp_trendy_mean_de_scaled_df,aes(x=Year,y= Z_score_all_mean,color = "dd"),linetype="solid",size = 1.5)+
      #geom_line(data = input_gpp_trendys3_de_scaled_df,aes(x=Year,y= input_gpp_trendys3_de_scaled,color = "royalblue"),linetype="solid",size = 1)+
      ylab("z-score")+
      xlab("Year")+
      ylim(-4,4)+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '',
                         values =c("aa"="gray21","bb" = "royalblue","cc"="lightcoral","dd"="green4"), labels = c('AABI','FLUXCOM','RS_mean','TRENDY'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = "bottom",legend.text = element_text(face="bold",size = 24))+
      theme(axis.text = element_text(face="bold",size =24),axis.text.x = element_text(face="bold",size =24),axis.text.y = element_text(face="bold",size =24),axis.title.x=element_text(face="bold",size=24),axis.title.y=element_text(face="bold",size=24))
    p1
    dev.off()
    
    
    p2 <- ggplot()+
      #geom_ribbon(data = input_sink_glc_nbr_de_scaled_df,aes(x=Year,ymin= min, 
      #                                                      ymax= max),fill = "gray",alpha = 0.4)+
      #geom_ribbon(data = npp_GLC_de_scaled_df,aes(x=Year,ymin= min, 
      #                                            ymax= max),fill = "green",alpha = 0.4)+
      #geom_ribbon(data = dVegC_GLC_de_scaled_df,aes(x=Year,ymin= min, 
      #                                              ymax= max),fill = "pink",alpha = 0.4)+
      #geom_line(data = input_sink_glc_nbr_de_scaled_df,aes(x=Year, 
      #                                                     y=mean,color = "gray21"),linetype="solid",size =1)+
      geom_line(data = input_gpp_FLUXCOM_de_scaled_df,aes(x=Year,y= input_gpp_FLUXCOM_de_scaled,color = "green4"),linetype="solid",size = 1)+
      geom_line(data = input_gpp_EC_LUE_de_scaled_df,aes(x=Year,y= input_gpp_EC_LUE_de_scaled,color = "lightcoral"),linetype="solid",size = 1)+
      geom_line(data = input_gpp_trendys2_de_scaled_df,aes(x=Year,y= input_gpp_trendys2_de_scaled,color = "royalblue"),linetype="solid",size = 1)+
      ylab("z-score")+
      xlab("Year")+
      ylim(-4,4)+
      #scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1')) #+
      scale_color_manual(name = '', 
                         values =c("green4" = "green4","lightcoral"="lightcoral","royalblue"="royalblue"), labels = c('FLUXCOM_rhythm','EC-LUE_rhythm','TRENDYS2_rhythm'))+
      theme_set(theme_bw())+
      theme(panel.grid.major=element_line(colour=NA))+
      theme(legend.position = "bottom",legend.text = element_text(face="bold",size = 16))+
      theme(axis.text = element_text(face="bold",size =16),axis.text.x = element_text(face="bold",size =16),axis.text.y = element_text(face="bold",size =16),axis.title.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16))
    
    
    
    
    
    
  }
}
