produ_by_continent <- function(
  
){
  
  library(raster)
  world = shapefile('data/shp/world_arid_country.shp')
  
  
  input_shp = list.files('data/continent_shp',full.names = T,
                         pattern = '*.shp$')
  # order AF, AS, EU,  NA, OC, SA
  
  shp_list = sapply(input_shp,shapefile)
  
  country_list = list()
  for(i in 1:length(shp_list)){
    temp_extent = extent(shp_list[[i]])
    
    tmpworld = crop(world,temp_extent)
    
    name_list = tmpworld$NAME
    country_list[[i]] = name_list
  }
  
  print(country_list)
  
  arid_count_produ = read.csv('output/arid_coun_mine.csv',
                              header = T)
  arid_count_produ = arid_count_produ[,-1]
  
  country_name_pro = arid_count_produ[,1]
  
  arid_count_produ = arid_count_produ[,-c(1,31)]
  loc_2002 = length(1990:2002)
  loc_2015 = length(1990:2015)
  
  prod_box = arid_count_produ[,loc_2002:loc_2015]
  prod_box = apply(prod_box,2,as.numeric)
  
  id_list = list()
  for(i in 1:length(country_list)){
    tmp1 = country_list[[i]]
    id_box = 1
    for(j in 1:length(tmp1)){
      id = which(country_name_pro == tmp1[j])
      id_box = c(id_box,id)
    }
    id_box = id_box[-1]
    id_list[[i]] = id_box
  }
  
  prod_by_conti = 1
  for(i in 1:length(id_list)){
    tmp1 = id_list[[i]]
    
    tmpprod = prod_box[tmp1,]
    tmpprod = apply(tmpprod,2,sum)
    
    prod_by_conti = cbind(prod_by_conti,tmpprod)
    
  }
  
  prod_by_conti = prod_by_conti[,-1]
  
  prod_by_conti = prod_by_conti/10000000000
  
  return(prod_by_conti)
  
  
  
}

