pr_trend_analysis <-function(
  
){
  library(reshape2)
  pr_trend = read.csv('output/precip_minus_e.csv',header = T)
  pr_trend = pr_trend[,-1]
  
  
  mmk_calc<-function(x){
    len = length(x)
    tmp = x[len]-x[1]
    return(tmp)
  }
  
  pr_mmk = apply(pr_trend,2,mmk_calc)
  
  source('D:/Mining_project/global/R/cor_analysis_global.R')
  ret_box = cor_analysis_global()
  
  loc = ret_box[,1:2]
  loc[,1] = loc[,1]-180
  
  
  df = data.frame(long = loc[,1],
                  lat = loc[,2],
                  Pr_MMK = pr_mmk)
  dfm = melt(df,c('long','lat'))
  neg_index = which(df$Pr_MMK<=0)
  pos_index = which(df$Pr_MMK>0)  
  
  
  world = shapefile('D:\\Mining_project\\global\\Data\\new\\climate_zone\\world_continent2.shp')
  #world_df = as.data.frame(world,xy = T)
  #world_df[,1] = world_df[,1] + 180
  world = crop(world, extent(-180,180,-60,90))
  
  
  library(ggplot2)
  
  p1 =  ggplot()+
    geom_polygon(data=world,aes(x=long, y= lat,group = group),
                 colour='black',fill = '#CFCFCF',size = 0.5,alpha = 0.5
                 #'#CFCFCF'
    )+
    geom_point(data = dfm,aes(x = long, y = lat,color = value,
                                    size = value), alpha = 0.5)
}
