grace_extract_by_point<- function(
  loc_only# only long and lat consisted
){
  
  inputpath_grace = 'E:\\项目管理\\内蒙古矿区\\grace\\CSR_GRACE_RL05_Mascons_v01.nc'
  
  grace = stack(inputpath_grace,varname = 'lwe_thickness') #resolution 0.5 0.5
  # z management
  z = raster(nrow = 720,ncol = 1440)
  extent(z) = extent(0,360,-90,90)
  # z management
  
  # resolution management
  grace = resample(grace,z)
  
  
  year = rep(2002:2017,each = 12)
  month = rep(1:12,length(2002:2017))
  
  mat = cbind(year,month)
  
  mat = mat[-c(1,2,3,187:192),]
  lack_year = c(2002,2002,2003,2011,2011,2012,2012,2013,2013,2013,2014,
                2014,2014,2015,2015,2015,2016,2016,2016,2017)
  lack_mon = c(6,7,6,1,6,5,10,3,8,9,2,7,12,6,10,11,4,9,10,2)
  
  rem_index = 1
  for(i in 1:length(lack_year)){
    rem_index = c(rem_index,
                  which(mat[,1]==lack_year[i]&
                          mat[,2]==lack_mon[i]))
  }
  rem_index = rem_index[-1]
  mat_rem = mat[-rem_index,]
  
  grace_box = 1
  # extract section
  long = loc_only[,1]
  lat = loc_only[,2]
  
  
  for( i in 1:length(long)){
    xy = cbind(long[i],lat[i])
    sp = SpatialPoints(xy)
    
    temp = extract(grace,sp)
    temp = t(temp)
    temp = temp * 10 
    grace_box = cbind(grace_box,temp)
  }
  grace_box = as.matrix(grace_box[,-1],ncol = 1)#test 20191119
  #grace_box = grace_box[,-1]
  # extract section end 
  
  # fill na vlaue
  mat_na = matrix(NA,nrow = nrow(mat),ncol = ncol(grace_box))
  
  mat_rem = cbind(mat_rem,grace_box)
  mat = cbind(mat,mat_na)
  
  for(i in 1:nrow(mat)){
    
    index = which(mat_rem[,1] == mat[i,1] & 
                    mat_rem[,2] == mat[i,2])
    #print(index)
    if(length(index) == 0){
      mat[i,3:ncol(mat)] = rep(NA,(ncol(mat)-2))
      
    }else{
      mat[i,3:ncol(mat)] = mat_rem[index,3:ncol(mat_rem)]
    } # end of if
    
  } # end of for 
  source('E:/项目管理/内蒙古矿区/R/na_value_fill.R')
  
  for(i in 3:ncol(mat)){
    mat[,i] = na_value_fill(mat[,i])
  }
  
  output_grace = 'D:\\Mining_project\\global\\Data\\output_of_grace\\out_grace.csv'
  write.csv(mat,output_grace) #data with time
  return(mat)
  
  
  
}

































