import_gldas_evapor<- function(
  
){
  files = list.files('data/gldas',full.names = T)
  input = 1
  for(i in 1:length(files)){
    input = c(input,list.files(files[i],full.names = T))
  }
  input = input[-1]
  
  source('D:/Mining_project/global/R/cor_analysis_global.R')
  ret_box = cor_analysis_global()
  loc_gldas = ret_box[,1:2]
  
  #id = which(loc_gldas[,1]>180)
  loc_gldas[,1] = loc_gldas[,1]-180
  
  evapor = stack(input,varname = 'Evap_tavg')
  evapor = evapor * 86400 *30.5
  
  cl = makeCluster(4)
  clusterExport(cl = cl,varlist = c('loc_gldas','evapor'))
  clusterEvalQ(cl,source('E:/Desktop/20200901-project-transfer/Xinjiang_hetian_proj/R/Xinjiang_hetian/mmkTrend.R')
  )
  registerDoParallel(cl)
  
  system.time(
    
    gldas_evapor <- parLapply(cl,1:nrow(loc_gldas),
                              
                              function(i){
                                #source('E:/Desktop/20200901-project-transfer/Xinjiang_hetian_proj/R/Xinjiang_hetian/mmkTrend.R')
                                library(raster)
                                xy = cbind(loc_gldas[i,1],loc_gldas[i,2])
                                xy = SpatialPoints(xy)
                                
                                tmp = extract(evapor,xy)
                                
                                tmp = as.numeric(tmp)
                                return(tmp)
                              })
  )
  stopCluster(cl)
  
  evapor_df = do.call('cbind',gldas_evapor)
  
  write.csv(evapor_df,'output/evapor.csv')
  evapor =read.csv('output/evapor.csv',header = T)
  evapor =evapor[,-1]
  
}