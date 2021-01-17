ndvi_import_by_point <- function(
  loc_only
){
  #NDVI date:2000-2 ~ 
  #import ndvi 
  loc_200204 = length(2000:2002)*12-11+3+1
  loc_201512 = length(2000:2015)*12+1
  
  #inputpath_ndvi = "E:\\Desktop\\integrated_ndvi"
  #files_ndvi = paste(inputpath_ndvi,
  #                   list.files(inputpath_ndvi,pattern = '.TIF$'),
  #                   sep = "\\")
  
  files_ndvi = list.files('L:\\Global_NDVI_tif',full.names = T)
  
  ndvi_stack = stack(files_ndvi)
  ndvi_stack = ndvi_stack[[loc_200204:loc_201512]]
  #import ndvi done
  
  long = loc_only[,1]
  lat = loc_only[,2]
  
  sp = SpatialPoints(loc_only)
  temp = extract(ndvi_stack,sp)
  ndvi_box = t(temp)
  
  
  vci_box = 1
  
  for(i in 1:ncol(ndvi_box)){
    
    temp = 100*(ndvi_box[,i] - min(ndvi_box[,i],na.rm = T))/
      (max(ndvi_box[,i],na.rm = T) - min(ndvi_box[,i],na.rm = T))
    temp =as.numeric(temp)
    #temp = (temp -mean(temp,na.rm= T))/(max(temp,na.rm = T)-min(temp,na.rm = T))
    
    vci_box = cbind(vci_box,temp)
    
    
  }
  vci_box = vci_box[,-1]
  
  output_vci = 'D:\\Mining_project\\global\\Data\\output_of_vci\\output_vci.csv'
  write.csv(vci_box,output_vci)
  print('VCI has been standardized!')
  return(vci_box)
  
  
}