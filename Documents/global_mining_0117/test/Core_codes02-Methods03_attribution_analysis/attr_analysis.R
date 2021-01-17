attr_analysis<-function(
  
){
  library(reshape2)
  library(ggplot2)
  library(scales)
  library(RColorBrewer)
  input_ggrace = 'D:\\Mining_project\\global\\Data\\new\\ts_index\\ggrace_mat_ts.csv'
  input_iei = 'D:\\Mining_project\\global\\Data\\new\\ts_index\\output_iei_ts.csv'
  
  sm_trend = read.csv('D:\\Mining_project\\global\\Data\\new\\ts_index\\output_sm_ts.csv',
                      header = T)
  sm_trend = sm_trend[,-1]
 
  
  pr_trend = read.csv('output/pr_trend_item.csv',header = T)
  pr_trend = pr_trend[,-1]
  
  ev_trend = read.csv('output/ev_trend.csv',header = T)
  ev_trend = ev_trend[,-1]
  
  
  
  
  ggrace = read.csv(input_ggrace,header = T)
  iei = read.csv(input_iei,header = T)
  
  ggrace = ggrace[,-1]
  iei = iei[,-1]
  
  stan<-function(x){
    x = (x-mean(x))/(max(x)-min(x))
    return(x)
  }
  
  pr_trend = apply(pr_trend,2,stan)
  ev_trend = apply(ev_trend,2,stan)
  
  write.csv(pr_trend,'output/pr_trend_stan.csv')
  pr_trend = read.csv('output/pr_trend_stan.csv',header = T)
  pr_trend = pr_trend[,-1]
  
  
  
  lm_test <- function(sm,ggrace,pr){
    #print(length(sm))
    #print(length(ggrace))
    #print(length(pr))
    lm.model1 = lm(sm~pr+ggrace)
    tmp = as.numeric(lm.model1$coefficients)
    print(lm.model1)
    return(tmp)
  }
  
  coeff = mapply(lm_test,sm = sm_trend,ggrace=ggrace,pr = pr_trend)
  
  coeff = t(coeff)
  
  coeff[,1] = round(coeff[,1],3)
  coeff[,2] = round(coeff[,2],3)
  coeff[,3] = round(coeff[,3],3)
  
  source('D:/Mining_project/global/R/cor_analysis_global.R')
  ret_box = cor_analysis_global()
  
  loc = ret_box[,1:2]
  loc[,1] = loc[,1]-180
  

  df1 = data.frame(
    long = loc[,1],
    lat = loc[,2],
    Coeff_PR = coeff[,2]
  )
  
  
  df2 = data.frame(
    long = loc[,1],
    lat = loc[,2],
    Coeff_GGRACE = coeff[,3]
  )
  
  df3 =  data.frame(
    long = loc[,1],
    lat = loc[,2],
    Coeff_diff = abs(df2$Coeff_GGRACE) - df1$Coeff_PR
  )
  
  write.csv(df3,'output/Coeff_diff.csv')
  
  df1m = melt(df1,c('long','lat'))
  df2m = melt(df2,c('long','lat'))
  df3m = melt(df3,c('long','lat'))
  
  
  df1m$type = 4
  df2m$type = 5
  df3m$type = 6
  
  id_pos1 = which(df1m$value>0)
  id_pos2 = which(df2m$value>0)
  id_pos3 = which(df3m$value>0)
  
  
  print(length(id_pos1))
  print(nrow(df1m)-length(id_pos1))
  
  print(length(id_pos2))
  print(nrow(df2m)-length(id_pos2))
  
  print(length(id_pos3))
  print(nrow(df3m)-length(id_pos3))
  
  df1m$type[id_pos1] = 7
  df2m$type[id_pos2] = 8
  df3m$type[id_pos3] = 9
  
  df1s = df1m
  df2s = df2m
  df3s = df3m
  
  df1s$type = 1
  df2s$type = 2
  df3s$type = 3
  
  df1s$fill = 'Negative Coefficients'
  df2s$fill = 'Negative Coefficients'
  df3s$fill = 'Negative Coefficients'
  
  df1s$fill[id_pos1] = 'Positive Coefficients'
  df2s$fill[id_pos2] = 'Positive Coefficients'
  df3s$fill[id_pos3] = 'Positive Coefficients'
  
  
  dfm = rbind(df1m,df2m,df3m)
  dfm$cuts = cut(dfm$value,breaks = seq(-2,2,0.25))
  
  dfs = rbind(df1s,df2s,df3s)
  dfs$cuts = cut(dfs$value,breaks = seq(-2,2,0.25))
  
  
  
  library(RColorBrewer)
  mycolor =  colorRampPalette(brewer.pal(11,'Spectral'))(12)
  
  
  
  mycolor = c('(-1.25,-1]' ='#5E4FA2',
              '(-1,-0.75]' = '#3682BA',
              '(-0.75,-0.5]' = '#5CB7A9',
              '(-0.5,-0.25]' = '#98D5A4',
              '(-0.25,0]' = '#D0EC9C',
              '(0,0.25]' = '#F3FAAD',
              '(0.25,0.5]' = '#FEF0A7',
              '(0.5,0.75]' = '#FDCD7B',
              '(0.75,1]' = '#FA9C58',
              '(1,1.25]' ='#EE6445',
              '(1.25,1.5]' ='#D0384D',
              '(1.5,1.75]' = '#9E0142')
  
  mysize = c('(-1.25,-1]' = 10,
             '(-1,-0.75]' = 8,
             '(-0.75,-0.5]' = 6,
             '(-0.5,-0.25]' = 4,
             '(-0.25,0]' = 2,
             '(0,0.25]' = 2,
             '(0.25,0.5]' = 4,
             '(0.5,0.75]' = 6,
             '(0.75,1]' = 8,
             '(1,1.25]' = 10,
             '(1.25,1.5]' = 12,
             '(1.5,1.75]' = 14)
  
  myfill = c('Negative Coefficients' = '#3C5488FF',
             'Positive Coefficients' = '#E64B35FF')
  
  library(raster)
  world = shapefile('D:\\Mining_project\\global\\Data\\new\\climate_zone\\world_continent2.shp')
  #world_df = as.data.frame(world,xy = T)
  #world_df[,1] = world_df[,1] + 180
  world = crop(world, extent(-180,180,-60,90))
  
  prov_map = fortify(world,region = "Region2")
  prov_map2 = rbind(prov_map,prov_map,prov_map,prov_map,prov_map,prov_map)
  prov_map2$type = rep(4:9,each = nrow(prov_map))
  
  source('D:/Mining_project/global/R/theme_set_evolution_cor_map.R')
  print('start')
  theme_set_evolution<-theme_set_evolution_cor_map(12,
                                                   width = 3)
  fontsize = 14
  
  legend_set<-theme(legend.background = element_rect(fill='white'),
                    legend.key = element_blank(),
                    legend.key.size = unit(3,'lines'),
                    legend.key.height=unit(0.1,"inches"),
                    legend.key.width=unit(0.5,"inches"),
                    legend.text=element_text(colour="black",size=fontsize,face = 'bold'),
                    legend.text.align=0,
                    #legend.title=element_text(colour="black",size=15,face='bold'),
                    legend.title = element_blank(),
                    legend.title.align=1,
                    legend.position=c('bottom'), 
                    # two-element numeric vector,(0,0)-(1,1)
                    legend.direction="horizontal",
                    legend.justification=c('center'),#"center" or two-element numeric vector
                    legend.box="vertical",#
                    legend.box.just="top"
                    ,plot.background = element_rect(color='transparent')
                    #strip.text = element_text(colour="black",size=fontsize,face = 'bold')
  )
  
  
  
  label_df = data.frame(
    x = c(-0.25,0.50,-0.75,0.325,-0.5,0.75),
    y = c(370,370,270,270,260,260),
    label = paste0('MN: ',c(84,650,671,63,164,570)),
    type = rep(c(1,2,3),each =2)
  )
  
  seg_df1 =data.frame(
    x = c(-0.5,0,0,1),
    xend = c(0,1,-0.5,0),
    y = c(350,350,350,350),
    yend = c(350,350,350,350),
    type = 1
  )
  
  
  seg_df2 =data.frame(
    x = c(-1.25,0,0,0.75),
    xend = c(0,0.75,-1.25,0),
    y = c(255,255),
    yend = c(255,255),
    type = 2
  )

  seg_df3 =data.frame(
    x = c(-1,0,0,1.5),
    xend = c(0,1.5,-1,0),
    y = c(245,245),
    yend = c(245,245),
    type = 3
  )
  
  
  seg_df11 =data.frame(
    x = c(-0.5,1),
    xend = c(-0.5,1),
    y = c(330,330),
    yend = c(370,370),
    type = 1
  )
  seg_df21 =data.frame(
    x = c(-1.25,0.75),
    xend = c(-1.25,0.75),
    y = c(240,240),
    yend = c(270,270),
    type = 2
  )
  
  seg_df31 =data.frame(
    x = c(-1,1.5),
    xend = c(-1,1.5),
    y = c(230,230),
    yend = c(260,260),
    type = 3
  )
  
  
  df2 <- expand.grid(
    lineend = c('round', 'butt', 'square'),
    linejoin = c('round', 'mitre', 'bevel'),
    stringsAsFactors = FALSE
  )
  p = ggplot()+
    geom_histogram(data = dfs, aes(x = value,fill = fill),
                   binwidth = 0.25,center = 0.125,color = 'white',alpha= 0.7)+
    geom_polygon(data=prov_map2,aes(x=long, y= lat,group = group),
                 colour='black',fill = '#CFCFCF',size = 0.5,alpha = 0.5
                 #'#CFCFCF'
    )+
    geom_vline(data = seg_df1,aes(xintercept = 0),size = 0.5,linetype = 'dashed',
               color = 'black')+
    
    geom_vline(data = seg_df2,aes(xintercept = 0),size = 0.5,linetype = 'dashed',
               color = 'black')+
    
    geom_vline(data = seg_df3,aes(xintercept = 0),size = 0.5,linetype = 'dashed',
               color = 'black')+
    geom_point(data = dfm,aes(x = long, y = lat,color = cuts,
                                    size = cuts), alpha = 0.5)+
    
    geom_text(data = label_df,aes(x = x,y = y, label = label),
              color = 'black',size = 4)+
    geom_segment(data = seg_df1,aes(x = x, xend = xend, y = y,yend = yend),color = 'black',size = 1,
                 lineend = df2$lineend[2],linejoin = df2$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    geom_segment(data = seg_df2,aes(x = x, xend = xend, y = y,yend = yend),color = 'black',size = 1,
                 lineend = df2$lineend[2],linejoin = df2$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    geom_segment(data = seg_df3,aes(x = x, xend = xend, y = y,yend = yend),color = 'black',size = 1,
                 lineend = df2$lineend[2],linejoin = df2$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    
    #geom_segment(data = seg_df1,aes(x = x, xend = xend, y = y,yend = yend),color = 'black',size = 1,
    #             lineend = df2$lineend[2],linejoin = df2$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    geom_segment(data = seg_df11,aes(x = x, xend = xend, y = y,yend = yend),color = 'black',size = 1)+
    geom_segment(data = seg_df21,aes(x = x, xend = xend, y = y,yend = yend),color = 'black',size = 1)+
    geom_segment(data = seg_df31,aes(x = x, xend = xend, y = y,yend = yend),color = 'black',size = 1)+
    
    scale_color_manual(values = mycolor)+
    scale_fill_manual(values = myfill)+
    scale_size_manual(values = mysize)+
    facet_wrap(~type, nrow = 3,ncol = 3,scales = 'free')+
    theme_set_evolution+legend_set+
    guides(fill = guide_legend(nrow = 1),
           color = guide_legend(nrow = 2))+
    xlab('Longitude')+
    ylab('Latitude')
  
  png('plot/coeff.png',
      width = 30,
      height = 27,
      units = 'cm',
      res = 1000)
  print(p)
  
  dev.off()  
  
  
  source('E:/Desktop/global_mining_extended/crop_by_continent_index.R')
  index_cont <- crop_by_continent_index()
  
  pr_con_box = 1
  gg_con_box = 1
  iei_con_box = 1
  ev_con_box = 1
  sm_con_box = 1
  for(i in 1:length(index_cont)){
    tmpid = index_cont[[i]]
    
    tmppr = pr_trend[,tmpid]
    tmpgg = ggrace[,tmpid]
    tmpiei = iei[,tmpid]
    tmpev = ev_trend[,tmpid]
    tmpsm = sm_trend[,tmpid]
    
    tmpmean = apply(tmppr,1,mean)
    tmpgg = apply(tmpgg,1,mean)
    tmpiei = apply(tmpiei,1,mean)
    tmpev =apply(tmpev,1,mean)
    tmpsm = apply(tmpsm,1,mean)
    
    pr_con_box = cbind(pr_con_box,tmpmean)
    gg_con_box = cbind(gg_con_box,tmpgg)
    iei_con_box = cbind(iei_con_box,tmpiei)
    ev_con_box = cbind(ev_con_box,tmpev)
    sm_con_box = cbind(sm_con_box,tmpsm)
  }
  pr_con_box = pr_con_box[,-1]
  gg_con_box = gg_con_box[,-1]
  iei_con_box = iei_con_box[,-1]
  ev_con_box = ev_con_box[,-1]
  sm_con_box  =sm_con_box[,-1]
  
  colnames(pr_con_box) = c('AF',"AS", "EU",  "NA", "OC", "SA")
  colnames(iei_con_box) = c('AF',"AS", "EU",  "NA", "OC", "SA")
  colnames(gg_con_box) = c('AF',"AS", "EU",  "NA", "OC", "SA")
  colnames(ev_con_box) = c('AF',"AS", "EU",  "NA", "OC", "SA")
  colnames(sm_con_box) = c('AF',"AS", "EU",  "NA", "OC", "SA")
  
  stan<-function(x){
    x = (x-mean(x))/(max(x)-min(x))
    return(x)
  }
  pr_stan = apply(pr_con_box,2,stan)
  ev_stan = apply(ev_con_box,2,stan)
 
  
  
 
  lm_test <- function(sm,ggrace,pr){
    lm.model1 = lm(sm~pr+ggrace)
    tmp = as.character(lm.model1$coefficients)
    print(summary(lm.model1))
    return(tmp)
  }
  coeff_box = 1
  fitted_box = 1
  for(i in 1:ncol(sm_con_box)){
    
    iei = iei_con_box[,i]
    ggrace = gg_con_box[,i]
    pr =pr_stan[,i]
    ev = ev_stan[,i]
    sm = sm_con_box[,i]
    print(i)
    coeff = lm_test(iei,ggrace,pr)
    
    coeff_box =rbind(coeff_box,coeff)
    
  }
  coeff_box = coeff_box[-1,]
  
}





















