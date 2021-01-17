coanalyze_cor_attr<-function(
  
){
  
  coeff_diff =read.csv('output/Coeff_diff.csv',header = T)
  coeff_diff = coeff_diff[,-1]
  source('D:/Mining_project/global/R/cor_analysis_global.R')
  ret_box = cor_analysis_global()

  neg_index = which(ret_box[,3]<=0)
  pos_index = which(ret_box[,3]>0)
  
  
  cor_box = ret_box
  
  cor_box$pattern = 3
  cor_box$pattern[pos_index]= 4
  
  neg_index2 =which(coeff_diff[,3]<=0)
  pos_index2 =which(coeff_diff[,3]>0)
  
  coeff_diff$pattern = 1
  coeff_diff$pattern[pos_index2] = 2
  
  df = data.frame(
    long = coeff_diff[,1],
    lat = coeff_diff[,2],
    pattern = cor_box$pattern * coeff_diff$pattern
  )
  
  
  class<-function(x){
    if(x == 3){
      tmp = 'Precipitation_Negative'
    }else if(x == 4){
      tmp = 'Precipitation_Positive'
    }else if(x == 6){
      tmp = 'Groundwater_Negative'
    }else if(x == 8){
      tmp = 'Groundwater_Positive'
    }
    return(tmp)
  }
  
  class_group = sapply(df$pattern,class)
  
  df$class =class_group
  
  len1 = length(which(df$pattern ==3))
  len2 = length(which(df$pattern ==4))
  len3 = length(which(df$pattern ==6))
  len4 = length(which(df$pattern ==8))
  
  print(len1)
  print(len2)
  print(len3)
  print(len4)
  
  df2 = data.frame(
    x = c('Precipitation_Negative','Groundwater_Negative',
          'Precipitation_Positive','Groundwater_Positive'),
    x2 = c(190,191,192,193),
    y = c(len1,len3,len2,len4)
  )
  df$type = 3
  id = which(df$pattern == 3 |df$pattern == 6)
  df$type[id] = 2
  
  df2$type = 1
  
  library(raster)
  world = shapefile('D:\\Mining_project\\global\\Data\\new\\climate_zone\\world_continent2.shp')
  #world_df = as.data.frame(world,xy = T)
  #world_df[,1] = world_df[,1] + 180
  world = crop(world, extent(-180,180,-60,90))
  
  prov_map = fortify(world,region = "Region2")
  prov_map2 = rbind(prov_map,prov_map)
  prov_map2$type = rep(c(2,3),nrow(prov_map))
  
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
  
  mycolor = c('Precipitation_Negative' = '#4DBBD5FF',
              'Precipitation_Positive' = '#F39B7FFF',
              'Groundwater_Negative' = '#3C5488FF',
              'Groundwater_Positive' = '#E64B35FF')
  myfill = mycolor
  
  breaks = function(x){
    if(min(x)>180){
      breaks = c(190,191,192,193)
    }else{
      breaks = c(-150,0,150)
    }
  }
  
  labels = function(x){
    if(min(x)>180){
      labels = c('Precipitation_Negative','Groundwater_Negative',
                 'Precipitation_Positive','Groundwater_Positive')
    }else{
      labels = as.character(c(-150,0,150))
    }
  }
  
  label_df = data.frame(
    x = df2$x2,
    y = df2$y*1.2,
    label = df2$y,
    type = 1
  )
  p1 = ggplot()+
    geom_bar(data = df2,aes(x =x2,y = y, fill = x),
             stat = 'identity',position = 'dodge',width = 0.5,alpha= 0.7)+
    geom_polygon(data=prov_map2,aes(x=long, y= lat,group = group),
                 colour='black',fill = '#CFCFCF',size = 0.5,alpha = 0.5
                 #'#CFCFCF'
    )+
    geom_point(data = df,aes(x = long, y = lat,color = class), alpha = 0.5,size = 3)+
    geom_text(data = label_df,aes(x = x,y = y, label = label),
              color = 'black',size = 4)+
    scale_color_manual(values = mycolor)+
    scale_x_continuous(breaks = breaks,labels = labels)+
    scale_fill_manual(values = myfill)+
    facet_wrap(~type, nrow = 3,scales = 'free')+
    theme_set_evolution+legend_set+
    guides(fill = guide_legend(nrow = 2),
           color = guide_legend(nrow = 2))+
    xlab('Longitude')+
    ylab('Latitude')
  
  p1
  
  png('plot/coeff_cor2.png',
      width = 22,
      height = 30,
      units = 'cm',
      res = 1000)
  print(p1)
  
  dev.off()  
  
  
}