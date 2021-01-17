find_peak_valley_p_trial <-function(
  index,
  tell_final = 0.01
){
  
  
  
  index = as.numeric(index)
  # 1. select #points by slope initially
  source('D:/Mining_project/R/slope_select.R')
  tp_box = slope_select(index)
  
  #plot(index)
  ##points(tp_box,index[tp_box],col = 'red')#
  #break
  #return(tp_box);break
  #break
  
  ##print(length(tp_box))
  index_tp = index[tp_box]
  tp_box2 = tp_box
  
  id_list = list()
  for(i in 1:length(tp_box)){
    temp_id = tp_box[i]
    id_group = 1
    for(j in 1:length(tp_box2)){
      temp_id2 = tp_box2[j]
      diff = abs(temp_id2 -temp_id)
      
      if(diff <= 2 & diff !=0){
        id_group =c(id_group,tp_box2[j])
      }
    } 
    id_group = id_group[-1]
    
    if(length(id_group)>0){
      id_group = c(temp_id,id_group)  
      id_group = sort(id_group)
      id_list[[i]] = id_group
    }
  }
  
  if(length(id_list) ==0){
    
  }else{
    re_id_box = 1
    id_keep = 1
    for(i in 1:length(id_list)){
      temp_id = id_list[[i]]
      ##print(temp_id)
      if(length(temp_id)==0){
        
      }else if(length(temp_id)==2){
        if(temp_id[1]>2){
          index_1 = index[temp_id[1]-2]
          index_2 = index[temp_id[1]]
          index_3 = index[temp_id[2]]
          index_4 = index[temp_id[2]+2]
          
          diff32 = index_3 - index_2
          diff41 = index_4 - index_1
          diff = diff41/diff32
          
          
          if(diff < 0 & abs(diff) >1 & abs(index_4)>abs(index_2)){
            re_id_box = c(re_id_box,temp_id)
          }else{
            
          }
        }
        
        
      }else{
        index_temp = index[temp_id]
        mean_box = mean(index_temp)
        diff = abs(index_temp - mean_box)
        
        temp_loc = which(diff == max(diff))
        temp_id = temp_id[temp_loc]
        id_keep = c(id_keep,temp_id)
        ###print(id_keep)
      }
      
    }
    
    re_id_box = re_id_box[-1]
    re_id_box = unique(re_id_box)
    id_keep = id_keep[-1]
    id_keep = unique(id_keep)
    #return(id_list)
    #(re_id_box);break
    
    re_keep_box = 1
    for(i in 1:length(re_id_box)){
      temp_re = which(id_keep == re_id_box[i])
      re_keep_box =c(re_keep_box,temp_re)
    }
    
    re_keep_box = re_keep_box[-1]
    id_keep = id_keep[-re_keep_box]
    
    #print(id_keep)
    ##print(re_id_box)
    
    re_box = 1
    for(i in 1:length(re_id_box)){
      temp_re = which(tp_box == re_id_box[i])
      re_box = c(re_box,temp_re)
    }
    re_box = re_box[-1]
    ##print(re_box)
    #tp_box = tp_box[-re_box]
    
    #plot(index,type = 'b')
    #points(tp_box,index[tp_box],col = 'red')
    tp_box2 = tp_box
    #print(tp_box)
    #print(re_box)
    #break
    
    re_too_close = 1
    for(i in 1:(length(tp_box)-1)){
      
      tell = abs(index[tp_box[i]]-index[tp_box[i+1]])
      #print(tell)
      if(tell <tell_final){
        re_too_close = c(re_too_close,i)
      }
      
    }
    #print(re_too_close)
    #plot(index,type='l')
    #points(tp_box,index[tp_box],col = 'red',cex = 2)
    
    re_too_close = re_too_close[-1]
    if(length(re_too_close) !=0){
      #print(re_too_close)
      tp_box = tp_box[-re_too_close]
      
    }
    ##print(tp_box)
    ##print(tp_box2)
  }
  
  #plot(index,type='l')
  #points(tp_box,index[tp_box],col = 'red',cex = 2)
  
  
  return(tp_box)
  
}




























