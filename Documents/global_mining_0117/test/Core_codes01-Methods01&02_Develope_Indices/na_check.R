na_check <-function(
  in_matrix
){
  
  na_index = 1
  for(i in 1:ncol(in_matrix)){
    
    temp = mean(in_matrix[,i])
    if(is.na(temp)){
      na_index = c(na_index,i)
      
    }
    
  }
  na_index = na_index[-1]
  
  return(na_index)
}
  
