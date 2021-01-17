crop_by_continent_index <- function(
  
){
  
  input_shp = list.files('data/continent_shp',full.names = T,
                         pattern = '*.shp$')
  
  
  
  # order AF, AS, EU,  NA, OC, SA
  
  shp_list = sapply(input_shp,shapefile)
  print(extent(shp_list[[2]]))
  
  source('D:/Mining_project/global/R/cor_analysis_global.R')
  ret_box = cor_analysis_global()
  loc = ret_box[,1:2]
  
  loc[,1] = loc[,1]-180
  loc2 = loc
  
  coordinates(loc) = ~ long + lat
  proj4string(loc) = crs(shp_list[[1]])
  
  
  index_list = list()
  for(i in 1:length(shp_list)){
    tmp = over(loc,shp_list[[i]])
    tmp_index = which(!is.na(tmp))
    
    index_list[[i]] = tmp_index
  }
  
  return(index_list)
  
}