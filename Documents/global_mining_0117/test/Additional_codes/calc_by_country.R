calc_by_country<-function(
  
){
  
  files = list.files('data/production_aggret',full.names = T)
  
  country_name = 1
  for(i in 1:length(files)){
    tmp = read.csv(files[i],header = T)
    tmp = tmp[,-1]
    
    country_name = c(country_name,as.character(tmp[,1]))
  }
  country_name = country_name[-1]
  country_name = as.character(unique(country_name))
  

  country_box = 1
  for(i in 1:length(country_name)){
    tmpcount_name= country_name[i]
    tmpmat = 1
    for(j in 1:length(files)){
      tmp = read.csv(files[j],header = T)
      tmp = tmp[,-1]
      tmpname = as.character(tmp[,1])
      id = which(tmpname == tmpcount_name)
      #print(id)
      if(length(id)!=0){
        tmpnum = tmp[,-1]
        tmpnum = apply(tmpnum,2,as.numeric)
        
        tmpmat = rbind(tmpmat,tmpnum[id,])
      }
      
    }
    if(nrow(tmpmat)==2){
      tmpmat = tmpmat[-1,]
      tmpsum = as.vector(tmpmat)
      tmpsum = c(tmpcount_name,tmpsum)
    }else{
      tmpmat = tmpmat[-1,]
      sum_re_na<-function(x){
        tmpx = sum(x,na.rm = T)
      }
      
      tmpsum = as.vector(apply(tmpmat,2,sum_re_na))
      tmpsum = c(tmpcount_name,tmpsum)
      
    }
    
    country_box = rbind(country_box,tmpsum)
    print(i)
  }
  country_box = country_box[-1,]
  
  country_num = country_box[,-1]
  country_num = apply(country_num,2,as.numeric)
  
  taiwan = country_num[134,]
  xianggang = country_num[148,]
  china = country_num[38,]
  
  country_num[38,] = country_num[38,]+country_num[134,]+country_num[148,]
  country_num = country_num[-c(134,148),]
  country_name = country_name[-c(134,148)]
  
  df = data.frame(country_name,country_num)
  write.csv(df,'output/mine_statistic_by_country.csv')
  
}