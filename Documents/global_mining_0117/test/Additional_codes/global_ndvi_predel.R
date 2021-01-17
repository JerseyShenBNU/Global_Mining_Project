global_ndvi_predel<-function(
  
){
  inputpath = 'L:\\Global_NDVI_new'
  output = 'L:\\Global_NDVI_tif'
  dir.create(output)
  
  files_name = list.files(inputpath)
  output = paste(output,files_name,sep = "\\")
  files = list.files(inputpath,full.names = T)
  
  #start_date = 2000-02
  #end_date = 2019-08
  
  for(i in 1:length(files)){
    
    temp = raster(files[i])
    temp = temp * 0.0001
    
    writeRaster(temp,output[i],format = 'GTiff',
                overwrite = TRUE)
    print(i)
  }
  
  
}