main_extract_global2<-function(
  
){
  
  #source management
  ############
  
  source('D:/Mining_project/global/R/gpp_extract_by_points.R')
  source('D:/Mining_project/global/R/ndvi_import_by_point.R')
  source('D:/Mining_project/global/R/grace_extract_by_point.R')
  source('D:/Mining_project/global/R/gldas_extract_by_point.R')
  source('D:/Mining_project/global/R/iei_compute_global.R')
  source('D:/Mining_project/global/R/na_check.R')
  ############
  #source management
  
  input_loc = 'D:\\Mining_project\\global\\Data\\global_mine_loc\\global_mine_longlat.csv'
  loc = read.csv(input_loc,header = T)
  loc = loc[,-1]
  loc_only = data.frame(long = loc[,1],lat = loc[,2])
  loc_with_neg = loc_only # for gpp and gldas
  loc_with_neg[,1] = loc_with_neg[,1]+180
  
  
  loc_neg = which(loc_only[,1]<0)
  loc_only[loc_neg,1] = loc_only[loc_neg,1]+360
  # Index Extraction & Computation
  #####
  # Grace Extraction
  source('D:/Mining_project/global/R/grace_extract_by_point.R')
  grace_mat = grace_extract_by_point(loc_only)#correct
  print('Pass GRACE section')
  #Grace data structure: year+month+...
  #Grace data has not been standardized
  
  # GLDAS Extraction within SM export
  source('D:/Mining_project/global/R/gldas_extract_by_point.R')
  # GLDAS data has not been standardized 
  # GlDAS data structure ...
  gldas_mat = gldas_extract_by_point(loc_with_neg)
  print('Pass GLDAS section')
  
  ncol_gldas = ncol(gldas_mat[[1]])
  #
  
  #GGRACE computation
  
  grace_mat = grace_mat[,-c(1,2)]
  
  for(i in 1:ncol_gldas){
    grace_mat[,i] = grace_mat[,i] - gldas_mat[[1]][,i]
    grace_mat[,i] = grace_mat[,i] - gldas_mat[[2]][,i]
    grace_mat[,i] = grace_mat[,i] - gldas_mat[[3]][,i]
    grace_mat[,i] = grace_mat[,i] - gldas_mat[[4]][,i]
    grace_mat[,i] = grace_mat[,i] - gldas_mat[[5]][,i]
    grace_mat[,i] = grace_mat[,i] - gldas_mat[[6]][,i]
    grace_mat[,i] = grace_mat[,i] - gldas_mat[[7]][,i]
  }
  
  for(i in 1:ncol(grace_mat)){
    grace_mat[,i] = (grace_mat[,i])/(max(grace_mat[,i])-min(grace_mat[,i]))
  }
  print("GGRACE has been standardized!")
  
  grace_mat2 = grace_mat
  #na check adding on 2019-09-24
  na_grace = na_check(grace_mat)
  dir.create('E:\\Desktop\\temp')
  write.csv(grace_mat,'E:\\Desktop\\temp\\grace.csv')
  ## GGRACE NA value remove 
  
  
  
  print('Pass GGRACE section')
  
  # Computation of Soil Moisture
  sm_box = gldas_mat[[2]]+gldas_mat[[3]]+gldas_mat[[4]]+gldas_mat[[5]]
  for(i in 1:ncol(sm_box)){
    sm_box[,i] = (sm_box[,i])/(max(sm_box[,i],na.rm = T)-min(sm_box[,i],na.rm = T))
  }
  
  na_sm = na_check(sm_box)
  
  
  
  # VCI Extraction and computation
  vci_box = ndvi_import_by_point(loc_with_neg)
  na_vci = na_check(vci_box)
  # VCI data structure ...
  # VCI data has been standardized
  
  # GPP import
  gpp_box = gpp_extract_by_points(loc_with_neg)
  na_gpp = na_check(gpp_box)
  # GPP data structure ...
  # GPP data has been standardized
  
  # IEI computation
  
  na_index = c(na_sm,na_vci,na_gpp,na_grace)
  na_index = unique(na_index)
  na_index = as.numeric(na_index)
  
  write.csv(na_index,'D:\\Mining_project\\global\\Data\\new\\na_index\\na_index.csv')
  #print(length(na_index));break
  
  grace_mat = grace_mat[,-na_index]
  sm_box = sm_box[,-na_index]
  vci_box = vci_box[,-na_index]
  gpp_box = gpp_box[,-na_index]
  
  #test route
  ###########
  write.csv(grace_mat,'E:\\Desktop\\temp\\grace.csv')
  write.csv(vci_box,'E:\\Desktop\\temp\\vci.csv')
  write.csv(sm_box,'E:\\Desktop\\temp\\sm.csv')
  write.csv(gpp_box,'E:\\Desktop\\temp\\gpp.csv');
  ###########
  #final export route
  grace_mat = grace_mat[1:165,]
  sm_box = sm_box[1:165,]
  
  write.csv(grace_mat,'D:\\Mining_project\\global\\Data\\new\\extract\\grace.csv')
  write.csv(vci_box,'D:\\Mining_project\\global\\Data\\new\\extract\\vci.csv')
  write.csv(sm_box,'D:\\Mining_project\\global\\Data\\new\\extract\\sm.csv')
  write.csv(gpp_box,'D:\\Mining_project\\global\\Data\\new\\extract\\gpp.csv');
  
  
  
  iei_compute_global(gpp_box,
                     vci_box,
                     sm_box)
  
  
  print('Pass IEI section')
  print('Section Completed') 
  #####
  
  ####output layer 
  #######
  # output of GGRACE 
  
  ts_grace = 1
  for(i in 1:ncol(grace_mat)){
    temp = ts_index(grace_mat[,i])[,2]
    temp = as.numeric(temp)
    na_index = which(is.na(temp))
    temp = temp[-na_index]
    ts_grace = cbind(ts_grace,temp)
  }
  ts_grace = ts_grace[,-1]
  write.csv(ts_grace,'D:\\Mining_project\\global\\Data\\new\\ts_index\\ggrace_mat_ts.csv')
  
  
  #######
  
  #####
}

























































