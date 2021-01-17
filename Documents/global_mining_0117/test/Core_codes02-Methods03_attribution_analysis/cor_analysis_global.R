cor_analysis_global<-function(
  input_ggrace = 'D:\\Mining_project\\global\\Data\\new\\ts_index\\ggrace_mat_ts.csv',
  input_iei = 'D:\\Mining_project\\global\\Data\\new\\ts_index\\output_iei_ts.csv'
){
  
  ggrace = read.csv(input_ggrace,header = T)
  iei = read.csv(input_iei,header = T)
  
  ggrace = ggrace[,-1]
  iei = iei[,-1]

  if(ncol(iei) == ncol(ggrace)){
    print('Pass column check')
  }
  
  cor_box = 1
  for(i in 1:ncol(iei)){
    temp_ggrace = as.numeric(ggrace[,i])
    temp_iei = as.numeric(iei[,i])
    temp_cor = cor(temp_ggrace,temp_iei)
    
    cor_box = c(cor_box,temp_cor)
  }  
  cor_box = cor_box[-1]
  hist(cor_box)
  
  cor_neg = which(cor_box<0)
  cor_pos = which(cor_box>0)
  
  print(length(cor_neg))
  print(length(cor_pos))
  
  
  input_loc = 'D:\\Mining_project\\global\\Data\\global_mine_loc\\global_mine_longlat.csv'
  loc = read.csv(input_loc,header = T)
  loc = loc[,-1]
  loc_only = data.frame(long = loc[,1],lat = loc[,2])
  loc_with_neg = loc_only # for gpp and gldas
  loc_with_neg[,1] = loc_with_neg[,1]+180
  
  na_index = read.csv('D:\\Mining_project\\global\\Data\\new\\na_index\\na_index.csv',
                      header = T)
  
  na_index = na_index[,-1]
  
  loc_na_remove = loc_with_neg[-na_index,]

  loc_cor_neg = loc_na_remove[cor_neg,]
  loc_cor_pos = loc_na_remove[cor_pos,]
  
  ret_box = cbind(loc_na_remove,cor_box)
  
  colnames(ret_box) = c('long','lat','cor')
  ret_box = as.data.frame(ret_box)
  
  dir.create('D:\\Mining_project\\global\\Data\\new\\pattern_analysis')
  write.csv(ret_box,'D:\\Mining_project\\global\\Data\\new\\pattern_analysis\\loc_cor_index.csv')
  return(ret_box)
}