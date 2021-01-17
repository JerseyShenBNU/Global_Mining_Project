map_record_match<-function(
  
){
  library(raster)
  world = shapefile('data/shp/world_arid_country.shp')
  world_name = world$NAME
  
  statistic_by_country = read.csv('output/mine_statistic_by_country.csv',
                                  header = T)
  statistic_by_country = statistic_by_country[,-1]
  
  country_name = as.character(statistic_by_country[,1])
  country_num = statistic_by_country[,-1]
  country_num = apply(country_num,2,as.numeric)
  
  yemenid = which(country_name == 'Yemen (PDR)' |
                    country_name == 'Yemen, Republic of') 
  
  country_num[165] = country_num[165]+country_num[166]
  country_name[165] = 'Yemen'
  country_num = country_num[-166,]
  country_name = country_name[-166]
  
  
  
  over_id = 1
  for(i in 1:length(world_name)){
    tmpid = which(country_name==world_name[i])
    
    if(length(tmpid)!=0){
      over_id = c(over_id,tmpid)
    }
  }
  over_id = over_id[-1]
  
  df = data.frame(country_name,country_num)
  country_name =country_name[over_id]
  country_num = country_num[over_id,]
  df = df[over_id,]
  
  zengfu_calc<-function(x){
    len = length(x)
    
    zengfu = x[len] -x[1]
    return(zengfu)
  }
  
  zengfu = apply(country_num,1,zengfu_calc)
  df$zengfu = zengfu
  
  write.csv(df,'output/arid_coun_mine.csv')
  
  less_name = 1
  for(i in 1:length(world_name)){
    tmpid = which(zengfuless==world_name[i])
    
    if(length(tmpid)!=0){
      less_name = c(less_name,world_name[i])
    }
  }
  less_name = less_name[-1]
}