hdf2nc <- function(
  inputpath = 'J:\\NDVI'
){
  require(gdalUtils)
  require(raster)
  
  files = list.files(inputpath,full.names = T)
  filesname = list.files(inputpath)
  output = 'J:\\NDVI_global_nc'
  output = 'E:\\Desktop\\NDVI_globe'
  dir.create(output)
  output = paste(output,filesname,sep = "\\")
  
  for(i in 1:length(files)){
    sds = get_subdatasets(files[i])
    name = sds[1]
    gdal_translate(sds[1],output[i])
    print(i)
  }
    
  
  
}