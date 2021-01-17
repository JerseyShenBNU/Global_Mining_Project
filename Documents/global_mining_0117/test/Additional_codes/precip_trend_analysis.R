precip_trend_analysis<-function(
  
){
  library(reshape2)
  library(ggplot2)
  data = read.csv('output/precip_df.csv',header = T)
  data = data[,-1]
  
  
  annual_mean <- function(x){
    tmp = matrix(x,nrow = 12)
    tmp = apply(tmp,2,mean)
    return(tmp)
  }
  
  annual_mean = apply(data,2,annual_mean)
  
  
  source('E:/Desktop/global_mining_extended/crop_by_continent_index.R')
  
  index_cont <- crop_by_continent_index()
  
  
  con_mean = 1
  for(i in 1:length(index_cont)){
    
    tmpid = index_cont[[i]]
    tmpmat = annual_mean[,tmpid]
    tmpmat = apply(tmpmat,1,mean)
    con_mean = cbind(con_mean,tmpmat)
  }
  
  con_mean =con_mean[,-1]
  
  colnames(con_mean) = c('AF',"AS", "EU",  "NA", "OC", "SA")
 
  mmkTrend(con_mean[,6])[[7]]
  
    
  date = seq(as.Date('1960-01-01'),
             as.Date('2015-01-01'),
             '12 month')
  
  df1 = data.frame(date,con_mean)
  
  df1 = melt(df1,'date')
  df1$color = 'Pr'
  
  
  
  p = ggplot()+
    geom_line(data = df1,aes(x = date,y = value,color = color))+
    facet_wrap(~variable,nrow = 2,scales = 'free')+
    scale_color_npg()+
    theme_bw()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  zengfu_calc<-function(x){
    #tmp = ts(x,start = c(2002,4),frequency = 12)
    #tmp = decompose(tmp)$trend
    
    #naid = which(is.na(tmp))
    #tmp = tmp[-naid]
    #mmk = mmkTrend(x)$Zc
    #return(mmk)
    
    x = (x -mean(x))/(max(x)-min(x))
    
    len = length(x)
    tmp = (x[len] - x[1])/2
    tmp = tmp*100
    return(tmp)
  }
  
  
  zengfu_mat = apply(annual_mean,2,zengfu_calc)
  
  loc = read.csv('data/loc/loc.csv')
  
  loc = loc[,-1]
  loc = loc[,-3]
  
  loc[,1] = loc[,1]-180
  
  df = data.frame(
    long = loc[,1],
    lat = loc[,2],
    GR = as.numeric(zengfu_mat)
  )
  
  df = melt(df,c('long','lat'))
  df$type = 1
  
  negid = which(df$value <0)
  posid = which(df$value >0)
  df$fill = 'Negative Growth Ratio'
  df$fill[posid] = 'Positive Growth Ratio'
  
  df2 = df
  
  p_b = data.frame(
    long = loc[,1],
    lat = loc[,2],
    GR =  as.numeric(zengfu_mat)
  )
  
  
  p_b$cuts = cut(p_b$GR, breaks = seq(-50,50,25))
  p_b$type = 2
  
  
  library(raster)
  world = shapefile('D:\\Mining_project\\global\\Data\\new\\climate_zone\\world_continent2.shp')
  #world_df = as.data.frame(world,xy = T)
  #world_df[,1] = world_df[,1] + 180
  world = crop(world, extent(-180,180,-60,90))
  
  prov_map = fortify(world,region = "Region2")
  prov_map2 = rbind(prov_map)
  prov_map2$type = rep(2,nrow(prov_map))
  
  
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
  
  myfill = c('Negative Growth Ratio' = '#4DBBD5FF',
              'Positive Growth Ratio' = '#F39B7FFF')
  mycolor = c('(-50,-25]'= '#013F87',
              '(-25,0]' = '#2A4FA0',
              '(0,25]' = '#FEB666',
              '(25,50]' = '#E8002D')
  mysize = c('(-50,-25]'= 4,
             '(-25,0]' = 2,
             '(0,25]' = 2,
             '(25,50]' = 4)
  
  point_box = df2
  p1 = ggplot()+
    geom_histogram(data = df, aes(x = value,fill = fill),
                   binwidth = 10,center = 5,color = 'white',alpha= 0.7)+
    
    geom_polygon(data=prov_map2,aes(x=long, y= lat,group = group),
                 colour='black',fill = '#CFCFCF',size = 0.5,alpha = 0.5
                 #'#CFCFCF'
    )+
    geom_point(data = p_b,aes(x = long,y = lat,
                              color = cuts,
                              size = cuts),
                              alpha = 0.5)+
    #geom_text(data = label_df,aes(x = x,y = y, label = label),
    #          color = 'black',size = 4)+
    scale_color_manual(values = mycolor)+
    #scale_x_continuous(breaks = breaks,labels = labels)+
    scale_fill_manual(values = myfill)+
    #scale_size_manual(values = mysize)+
    facet_wrap(~type, nrow = 2,scales = 'free')+
    theme_set_evolution+legend_set+
    guides(fill = guide_legend(nrow = 2),
           color = guide_legend(nrow = 2))+
    xlab('Longitude')+
    ylab('Latitude')
  
  png('plot\\precip_mmk.png',
      width = 27,
      height = 22,
      units = 'cm',
      res = 1000)
  print(p1)
  
  dev.off() 
  
  
  
  
  hist(df)
  
  
  
}








































