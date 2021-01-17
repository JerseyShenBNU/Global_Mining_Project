cor_match<-function(
  loc,cor
){
  source('D:/Mining_project/global/R/name_match.R')
  index_box = 1
  
  for(i in 1:nrow(loc)){
    temp_long = loc[i,1]
    
    temp_index = which(round(cor[,1],3) == round(temp_long,3))
    index_box = c(index_box,temp_index)
  }
  index_box = index_box[-1]
  
  cor = cor[index_box,]
  
  neg_cor = cor[which(cor[,3]<0),]
  pos_cor = cor[which(cor[,3]>0),]
  
  #print(cor);break
  max_neg_cor = neg_cor[which(abs(neg_cor[,3]) == max(abs(neg_cor[,3]))),]
  max_pos_cor = pos_cor[which(abs(pos_cor[,3]) == max(abs(pos_cor[,3]))),]
  
  max_neg_cor = max_neg_cor[1,]
  max_pos_cor = max_pos_cor[1,]
  # front 5 
  ret_box = list(max_neg_cor,max_pos_cor)
  
  return(ret_box)
    
}