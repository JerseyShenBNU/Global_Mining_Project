partial_cor_analysis_global<-function(
 neg_line_df,
 neg_iei_tp_list
 ){
  
  line_df = neg_line_df
  p_df = neg_iei_tp_list
  
  cor_df = 1
  index = seq(1,14,2)
  for(i in 1:7){
    loc_tell = index[i]
    # group 
    temp_line_df = line_df[which(line_df$type == loc_tell),]
    iei = temp_line_df[which(temp_line_df$color =='IEI'),3]
    ggrace = temp_line_df[which(temp_line_df$color == 'GGRACE'),3]
    
    iei = as.numeric(iei)
    ggrace = as.numeric(ggrace)
    
    temp_p_df = p_df[[i]]
    
    start_p = c(1,temp_p_df)
    end_p = c(temp_p_df,length(iei))
    
    cor_box  = 1
    for(j in 1:length(start_p)){
      temp_cor = cor(iei[start_p[j]:end_p[j]],
                     ggrace[start_p[j]:end_p[j]])
      
      cor_box = c(cor_box,temp_cor)
    }
    cor_box = round(cor_box[-1],2)
    label = as.character(cor_box)
    
    cor_box = cbind(1:length(cor_box),cor_box)
    cor_box = as.data.frame(cor_box)
    cor_box$type = rep(loc_tell+1, nrow(cor_box))
    
    
    colnames(cor_box) = c('x','cor','type')
    cor_box$label = label
    cor_df = rbind(cor_df,cor_box)
  }
  cor_df = cor_df[-1,]
  
  return(cor_df)
  
}