ab_date_index_extract <- function(
  
){
  source('D:/Mining_project/R/find_peak_valley_p_trial.R')
  library(ggplot2)
  library(reshape2)
  input_data = 'D:\\Mining_project\\global\\Data\\new\\route_analysis\\data_for_ap_analysis.csv'
  df = read.csv(input_data, header = T)
  df = df[,-1]
  # Structure neg: IEI GGRACE, pos:IEI GGRACE
  # date 2002-10-01 to 2015-06-01
  neg_iei_seq = seq(1,28,4)
  pos_iei_seq = seq(3,28,4)
  date = seq(as.Date('2002-10-01'),as.Date('2015-06-01'),'1 month')
  
  neg_iei_df = df[,neg_iei_seq]
  pos_iei_df = df[,pos_iei_seq]
  
  tp_list = list()
  tell_final = rep(0.4,28)
  tell_final[neg_iei_seq] = c(0.4,0.5,0.2,0.2,0.35,0.45,0.45)
  tell_final[pos_iei_seq] = c(0.3,0.2,0.2,0.4,0.4,0.4,0.4)
  for(i in 1:ncol(df)){
    temp = df[,i]
    temp_tp = find_peak_valley_p_trial(temp,tell_final = tell_final[i])
    
    tp_list[[i]] = c(1,temp_tp,length(as.numeric(temp)))
    print(i)
    #break
  }
  
  neg_iei_tp_list = tp_list[neg_iei_seq]
  pos_iei_tp_list = tp_list[pos_iei_seq]
  
  neg_date_list = list()
  pos_date_list = list()
  
  for(i in 1:length(neg_iei_tp_list)){
    
    
    temp_neg_tp = neg_iei_tp_list[[i]]
    temp_pos_tp = pos_iei_tp_list[[i]]
    
    temp_neg_date = as.character(date[temp_neg_tp])
    temp_pos_date = as.character(date[temp_pos_tp])
    
    neg_date_list[[i]]  = temp_neg_date
    pos_date_list[[i]] = temp_pos_date
    
  }
  
  ret_date_list = list(neg_date_list,pos_date_list)
  ret_ap_index = list(neg_iei_tp_list,pos_iei_tp_list)
  
  return(list(ret_date_list,ret_ap_index))
  
  
}