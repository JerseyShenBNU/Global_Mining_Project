clean_mine_production_data<-function(
  
){
  files = list.files('data/production',full.names = T)
  files_name = list.files('data/production')
  dir.create('data/production_aggret')
  files_out = paste0('data/production_aggret/',files_name,'.csv')
  tmplist = list()
  for(i in 1:length(files)){
    
    tmpfiles = list.files(files[i],
                       full.names = T)
    
    files_90_00 = read.csv(tmpfiles[3],header = T)
    files_01_10 = read.csv(tmpfiles[1],header = T)
    files_11_18 = read.csv(tmpfiles[2],header = T)
    
    na_fill_with_zero<-function(x){
      tmp = x
      for(i in 1:length(tmp)){
        if(is.na(tmp[i])){
          tmp[i]=0
        }
      }
      return(tmp)
    }
    
    files_90_00 = apply(files_90_00,2,na_fill_with_zero)
    files_01_10 = apply(files_01_10,2,na_fill_with_zero)
    files_11_18 = apply(files_11_18,2,na_fill_with_zero)
    
    files_01_10 = as.data.frame(files_01_10)
    files_90_00 = as.data.frame(files_90_00)
    files_11_18 = as.data.frame(files_11_18)
    
    if(i == 1){
      files_01_10_name = files_01_10[,1:2]
      files_90_00_name = files_90_00[,1:2]
      files_11_18_name = files_11_18[,1:2]
      
      files_01_10 = files_01_10[,-c(1,2)]
      files_90_00 = files_90_00[,-c(1,2)]
      files_11_18 = files_11_18[,-c(1,2)]
    }else{
      files_01_10_name = files_01_10[,1:2]
      files_90_00_name = files_90_00[,1:2]
      files_11_18_name = files_11_18[,1:2]
      
      files_01_10 = files_01_10[,-1]
      files_90_00 = files_90_00[,-1]
      files_11_18 = files_11_18[,-1]
      
    }
    
    
    files_01_10 = apply(files_01_10,2,as.numeric)
    files_90_00 = apply(files_90_00,2,as.numeric)
    files_11_18 = apply(files_11_18,2,as.numeric)
    
    sum_by_country <- function(mat_name,mat){
      coun = as.character(unique(mat_name[,1]))
      coun_box = 1
      for(i in 1:length(coun)){
        id = which(mat_name[,1]==coun[i])
        tmp_box = mat[id,]
        if(length(id)>1){
          tmp_box = apply(tmp_box,2,sum)
        }
        coun_box = rbind(coun_box,tmp_box)
      }
      coun_box= coun_box[-1,]
      coun_box = data.frame(coun,coun_box)
      return(coun_box)
    }
    
    files_01_10 = sum_by_country(files_01_10_name,files_01_10)
    files_90_00 = sum_by_country(files_90_00_name,files_90_00)
    files_11_18 = sum_by_country(files_11_18_name,files_11_18)
    
    count_union <- function(a,b,c){
      ##########
      a_name = as.character(a[,1])
      b_name = as.character(b[,1])
      c_name = as.character(c[,1])
      
      a = a[,-1]
      b = b[,-1]
      c = c[,-1]
      
      a = apply(a,2,as.numeric)
      b = apply(b,2,as.numeric)
      c = apply(c,2,as.numeric)
      
      name_full = as.character(unique(c(a_name,
                                        b_name,
                                        c_name)))
      
      mat_full_series = 1
      
      for(i in 1:
          length(name_full)
      ){
        #print(i)
        id1 = which(a_name == name_full[i])
        id2 = which(b_name == name_full[i])
        id3 = which(c_name == name_full[i])
        
        tmp = 0
        if(length(id1)!=0){
          tmp = c(tmp,a[id1,])
        }else{
          tmp = c(tmp,rep(0,ncol(a)))
        }
        
        if(length(id2)!=0){
          tmp = c(tmp,b[id2,])
        }else{
          tmp = c(tmp,rep(0,ncol(b)))
        }
        
        if(length(id3)!=0){
          tmp = c(tmp,c[id3,])
        }else{
          tmp = c(tmp,rep(0,ncol(c)))
        }
        tmp = tmp[-1]
        mat_full_series = rbind(mat_full_series,tmp)
      }
      mat_full_series = mat_full_series[-1,]
      mat_full_series = data.frame(name_full,mat_full_series)
      
      id_empty = which(mat_full_series$name_full == '')
      
      if(length(id_empty)!=0){
        mat_full_series = mat_full_series[-id_empty,]
      }
      return(mat_full_series)
    }
    
    files_full = count_union(files_90_00,
                             files_01_10,
                             files_11_18)
    
    # ncol check 
    ncol_file = ncol(files_full)
    if(ncol_file ==30){
      print(paste0('pass ',i))
      write.csv(files_full,files_out[i])
    }else{
      print(colnames(files_full))
      print(paste0('not pass ',i))
      files_full = files_full[,1:30]
      write.csv(files_full,files_out[i])
    }
    tmplist[[i]] = files_full
  }
  return(tmplist)
  
  
}