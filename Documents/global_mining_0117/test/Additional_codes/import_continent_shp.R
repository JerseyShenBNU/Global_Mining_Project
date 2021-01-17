import_continent_shp<-function(
  
){
  
  con_shp_path = 'D:\\Mining_project\\global\\Data\\new\\world_continent_shp'
  
  shp_files = list.files(con_shp_path,pattern = '.shp$',
                         full.names = T)
  
  shp_list = list()
  for(i in 1:length(shp_files)){
    shp_list[[i]] = shapefile(shp_files[i])
  }
  
  # saving structure 
  # africa,asia, europe, north_america, oceania, south_america
  
  return(shp_list)
  
  
  
  
  
}