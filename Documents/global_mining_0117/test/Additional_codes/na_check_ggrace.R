na_check_ggrace<-function(
  input_file = 'D:\\Mining_project\\global\\Data\\new\\global_raster\\ggrace'
){
  
  files = list.files(input_file,full.names = T)
  files_name =  list.files(input_file)
  for(i in 1:length(files)){
    temp = read.csv(files[i],header = T)
    temp = temp[,-1]
    
    tell = mean_by_col(temp)
    na_index = which(is.na(tell))
    
    if(length(na_index)>0 ){
      print(files_name[i])
    }
  }
}