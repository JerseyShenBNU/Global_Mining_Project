continent_index_reback <- function(
 
){
  require(ggplot2)
  require(raster)
  
  loc = read.csv('D:\\Mining_project\\global\\Data\\new\\pattern_analysis\\loc_cor_index.csv',
                 header = T)
  
  loc = loc[,-1]
  index1 = which(loc[,1]<180)
  index2 = which(loc[,1]>180)
  loc[index1,1] = loc[index1,1]+180
  loc[index2,1] = loc[index2,1] - 180
  loc = loc[,-3]
  index2 = which(loc[,1]>180)
  loc[index2,1] = loc[index2,1] - 360
  
  na_index = read.csv('D:\\Mining_project\\global\\Data\\new\\global_sm_data\\loc_na_box.csv',
                      header  = T)
  na_index = as.numeric(na_index[,-1])
  loc = loc[-na_index,]
  
  inputpath_continent = 'D:\\Mining_project\\global\\Data\\new\\world_continent_shp'
  
  filesin = list.files(inputpath_continent,pattern = '.shp$',
                       full.names = T)
  
  
  
  world =  shapefile('D:\\Mining_project\\global\\Data\\new\\climate_zone\\world_continent2.shp')
  ###########
  world =  crop(world, extent(-180,180,-60,90))
  
  
  loc = as.data.frame(loc)
  coordinates(loc) = ~ long + lat
  proj4string(loc) = crs(world)
  
  index_box = list()
  for(i in 1:length(filesin)){
    
    temp = shapefile(filesin[i])
    
    
    temp_loc = over(loc,temp)
    loc_index = which(!is.na(temp_loc))
    
    index_box[[i]] = loc_index
    
  }
  
  return(index_box)
  
}