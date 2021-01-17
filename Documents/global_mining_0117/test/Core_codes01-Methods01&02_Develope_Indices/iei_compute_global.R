iei_compute_global<-function(
  gpp_box,
  vci_box,
  sm_box
){
  require(psych)
  source('D:/Mining_project/R/ts_index.R')
  #Column number check 
  col_gpp = ncol(gpp_box)
  col_vci = ncol(vci_box)
  col_sm = ncol(sm_box)
  
  sm_box = sm_box[1:165,]
  
  if(col_gpp == col_vci){
    if(col_gpp == col_sm){
      print('Pass Column Check')
    }
  }
 
  #do not remove influence
  gpp_ts_box = 1
  vci_ts_box = 1
  sm_ts_box = 1
  iei_ts_box = 1
  iei_box = 1
  
  na_index_id = 1
  for(i in 1:col_gpp){
    temp_gpp = gpp_box[,i]
    temp_vci = vci_box[,i]
    temp_sm = sm_box[,i]
    
    #na_index = which(is.na(temp_gpp))
    temp_gpp_ts = ts_index(temp_gpp)[,2]
    temp_vci_ts = ts_index(temp_vci)[,2]
    temp_sm_ts = ts_index(temp_sm)[,2]
    
    temp_gpp_ts = as.numeric(temp_gpp_ts)
    temp_vci_ts = as.numeric(temp_vci_ts)
    temp_sm_ts = as.numeric(temp_sm_ts)
    
    na_index = which(is.na(as.numeric(temp_gpp_ts)))
    
    temp_gpp_ts = temp_gpp_ts[-na_index]
    temp_vci_ts = temp_vci_ts[-na_index]
    temp_sm_ts = temp_sm_ts[-na_index]
    
    
    gpp_ts_box = cbind(gpp_ts_box,temp_gpp_ts)
    vci_ts_box = cbind(vci_ts_box,temp_vci_ts)
    sm_ts_box = cbind(sm_ts_box,temp_sm_ts)
    
    
    # computation of iei
    
    w = principal(data.frame(temp_gpp_ts,temp_vci_ts,temp_sm_ts),
                  nfactors = 1,
                  rotate = 'varimax')$weights
    p = w[1]*temp_gpp_ts + w[2]*temp_vci_ts + w[3]*temp_sm_ts
    
    w2 = principal(data.frame(temp_gpp,temp_vci,temp_sm),
                   nfactors = 1,
                   rotate = 'varimax')$weights
    p2 = w2[1]*temp_gpp + w2[2]*temp_vci + w2[3]*temp_sm
    
    iei_ts_box = cbind(iei_ts_box,p)    
    iei_box = cbind(iei_box,p2)  
    
    
  }
  gpp_ts_box = gpp_ts_box[,-1]
  vci_ts_box = vci_ts_box[,-1]
  sm_ts_box = sm_ts_box[,-1]
  iei_ts_box = iei_ts_box[,-1]
  
  iei_box = iei_box[,-1]
  #na_index_id = na_index_id[-1]
  
  
  
  output_gpp_ts = 'D:\\Mining_project\\global\\Data\\new\\ts_index\\output_gpp_ts.csv'
  output_vci_ts = 'D:\\Mining_project\\global\\Data\\new\\ts_index\\output_vci_ts.csv'
  output_sm_ts = 'D:\\Mining_project\\global\\Data\\new\\ts_index\\output_sm_ts.csv'
  output_iei_ts = 'D:\\Mining_project\\global\\Data\\new\\ts_index\\output_iei_ts.csv'
  
  output_iei = 'D:\\Mining_project\\global\\Data\\new\\extract\\output_iei.csv'
  
  print(ncol(gpp_ts_box))
  print(ncol(iei_ts_box))
  print(ncol(vci_ts_box))
  print(ncol(sm_ts_box))
    
  
  write.csv(gpp_ts_box,output_gpp_ts)
  write.csv(vci_ts_box,output_vci_ts)
  write.csv(sm_ts_box,output_sm_ts)
  write.csv(iei_ts_box,output_iei_ts)
  
  write.csv(iei_box,output_iei)
  
  #return(na_index_id)
  
}













