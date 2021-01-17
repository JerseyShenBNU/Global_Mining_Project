gldas_extract_by_point <- function(
  loc_only
){
  library(ncdf4)
  inputpath_gldas = 'E:\\gldas_sm_200206_20106'
  files = list.files(inputpath_gldas)
  inputpath_gldas = paste(inputpath_gldas,files,sep ="\\")
  
  varname = c(
    "SWE_inst",
    "SoilMoi0_10cm_inst",
    "SoilMoi10_40cm_inst",
    "SoilMoi40_100cm_inst",
    "SoilMoi100_200cm_inst",
    "CanopInt_inst",
    'Qs_acc')
  
  
  swe_gldas = stack(inputpath_gldas,varname = varname[1])
  soil_0_10 = stack(inputpath_gldas,varname = varname[2])
  soil_10_40 = stack(inputpath_gldas,varname = varname[3])
  soil_40_100 = stack(inputpath_gldas,varname = varname[4])
  soil_100_200 = stack(inputpath_gldas,varname = varname[5])
  can_gldas = stack(inputpath_gldas,varname = varname[6])
  run_off = stack(inputpath_gldas,varname = varname[7])
  
  extent(swe_gldas) = extent(c(0,360,-60,90))
  extent(soil_0_10) = extent(c(0,360,-60,90))
  extent(soil_10_40) = extent(c(0,360,-60,90))
  extent(soil_40_100) = extent(c(0,360,-60,90))
  extent(soil_100_200) = extent(c(0,360,-60,90))
  extent(can_gldas) = extent(c(0,360,-60,90))
  extent(run_off) = extent(c(0,360,-60,90))
  
  
  #print('pass import');break

  # extract by point
  gldas_box = 1
  long = loc_only[,1]
  lat = loc_only[,2]
  
  
  swe_gldas_box = 1
  soil_0_10_box = 1
  soil_10_40_box = 1
  soil_40_100_box = 1
  soil_100_200_box = 1
  can_gldas_box = 1
  run_off_box = 1
  
  sp = SpatialPoints(loc_only)
  
  swe_temp = extract(swe_gldas,sp)
  soil_0_10_temp = extract(soil_0_10,sp)
  soil_10_40_temp = extract(soil_10_40,sp)
  soil_40_100_temp = extract(soil_40_100,sp)
  soil_100_200_temp = extract(soil_100_200,sp)
  can_gldas_temp = extract(can_gldas,sp)
  run_off_temp = extract(run_off,sp)
  
  swe_temp = t(swe_temp)
  soil_0_10_temp = t(soil_0_10_temp)
  soil_10_40_temp = t(soil_10_40_temp)
  soil_40_100_temp = t(soil_40_100_temp)
  soil_100_200_temp = t(soil_100_200_temp)
  can_gldas_temp = t(can_gldas_temp)
  run_off_temp = t(run_off_temp)
  
  swe_gldas_box = swe_temp
  soil_0_10_box = soil_0_10_temp
  soil_10_40_box = soil_10_40_temp
  soil_40_100_box = soil_40_100_temp
  soil_100_200_box = soil_100_200_temp
  can_gldas_box = can_gldas_temp
  run_off_box = run_off_temp
  
  box = list(swe_gldas_box,soil_0_10_box,
             soil_10_40_box,soil_40_100_box,
             soil_100_200_box,can_gldas_box,run_off_box)
  
  for(i in 1:length(box)){
    temp = box[[i]]
    for(j in 1:ncol(temp)){
      temp[,j] = temp[,j] - mean(temp[,j],na.rm =T)
    }
    box[[i]] = temp
  }
  
  output = c('D:\\Mining_project\\global\\Data\\output_of_gldas\\swe_csv.csv',
             'D:\\Mining_project\\global\\Data\\output_of_gldas\\soil010.csv',
             'D:\\Mining_project\\global\\Data\\output_of_gldas\\soil1040.csv',
             'D:\\Mining_project\\global\\Data\\output_of_gldas\\soil40100.csv',
             'D:\\Mining_project\\global\\Data\\output_of_gldas\\soil100200.csv',
             'D:\\Mining_project\\global\\Data\\output_of_gldas\\can_gldas.csv',
             'D:\\Mining_project\\global\\Data\\output_of_gldas\\runoff_gldas.csv')
  write.csv(box[[1]],output[1])
  write.csv(box[[2]],output[2])
  write.csv(box[[3]],output[3])
  write.csv(box[[4]],output[4])
  write.csv(box[[5]],output[5])
  write.csv(box[[6]],output[6])
  write.csv(box[[7]],output[7])
  
  return(box)
  # extract by point end
  
  
  
  
  
  
}






































