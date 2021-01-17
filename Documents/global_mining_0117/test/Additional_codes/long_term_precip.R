long_term_precip<-function(
  
){
  input_gpcc = 'data/precip/precip.mon.total.v2018.nc'
  
  gpcc = stack(input_gpcc)
  
  gpcc = gpcc[[829:1500]]
  #source('D:/Mining_project/global/R/cor_analysis_global.R')
  #ret_box = cor_analysis_global()
  #dir.create('data/loc')
  #write.csv(ret_box,'data/loc/loc.csv')
  
  loc = read.csv('loc.csv',header = T)
  loc = loc[,-c(1,2)]
  #neg_select = which(ret_box[,3]<=0)
  loc = ret_box[,1:2]
  loc[,1] = loc[,1]-180
  id = which(loc[,1]<0)
  loc[id,1] = loc[id,1]+360

  library(foreach)
  library(doParallel)
  library(parallel)
  
  cl = makeCluster(4)
  clusterExport(cl = cl,varlist = c('loc','gpcc'))
  clusterEvalQ(cl,source('E:/Desktop/20200901-project-transfer/Xinjiang_hetian_proj/R/Xinjiang_hetian/mmkTrend.R')
  )
  registerDoParallel(cl)
  
  system.time(
    
    pr_60_15 <- parLapply(cl,1:nrow(loc),
                              
                              function(i){
                                source('E:/Desktop/20200901-project-transfer/Xinjiang_hetian_proj/R/Xinjiang_hetian/mmkTrend.R')
                                library(raster)
                                xy = cbind(loc[i,1],loc[i,2])
                                xy = SpatialPoints(xy)
                                
                                tmp = extract(gpcc,xy)
                                
                                tmp = as.numeric(tmp)
                                return(tmp)
                              })
  )
  stopCluster(cl)
  
  
  pr_box = do.call('cbind',pr_60_15)
  
  write.csv(pr_box,'precip_df.csv')
  
}