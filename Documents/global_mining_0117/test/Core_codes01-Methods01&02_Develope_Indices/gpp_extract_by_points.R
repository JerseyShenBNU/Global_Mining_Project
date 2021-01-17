gpp_extract_by_points <- function(
  loc_only
){
  #Create Date: 2019-09-11
  #Goal to pre-process the gpp data
  
  # import gpp stack data
  library(raster)
  library(maptools)
  library(maps)
  inputpath_gpp = 'K:\\gpp_monthly'
  files = list.files(inputpath_gpp,full.names = T)
  
  files_stack = stack(files)
  
  loc_200204 = length(2000:2002)*12-11+3
  loc_201512 = length(files)
  
  extent(files_stack) = extent(0,360,-90,90)
  
  files_stack = files_stack[[loc_200204:loc_201512]]
  
  gpp_box = 1
  
  sp = SpatialPoints(loc_only)
  temp = extract(files_stack,sp)
  gpp_box = t(temp)
  
  
  #for(i in 1:ncol(gpp_box)){
  #  gpp_box[,i] = (gpp_box[,i] - mean(gpp_box[,i],na.rm = T))/(max(gpp_box[,i],na.rm = T)-min(gpp_box[,i],na.rm = T))
  #}
  
  print('GPP data has been standardized!')
  output_gpp = 'D:\\Mining_project\\global\\Data\\output_of_gpp\\gpp_out.csv'
  write.csv(gpp_box,output_gpp)
  return(gpp_box)
  
}