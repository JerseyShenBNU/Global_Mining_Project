available_pr_compare <- function(
  input_ggrace = 'D:\\Mining_project\\global\\Data\\new\\ts_index\\ggrace_mat_ts.csv',
  input_iei = 'D:\\Mining_project\\global\\Data\\new\\ts_index\\output_iei_ts.csv'
){
  pr_trend = read.csv('output/pr_trend_item.csv',header = T)
  pr_trend = pr_trend[,-1]
  evapor =read.csv('output/evapor.csv',header = T)
  evapor =evapor[,-1]
  
  
  trend_ext <- function(x){
    tmp = ts(x,start = c(2002,4),frequency = 12)
    tmp = decompose(tmp)$trend
    tmp = as.numeric(tmp)
    naid = which(is.na(tmp))
    tmp = tmp[-naid]
    return(tmp)
  }
  
  e_trend = apply(evapor,2,trend_ext)
  
  
  pe = pr_trend - e_trend
  
  write.csv(pe,'output/precip_minus_e.csv')
  
  stan<-function(x){
    x = (x-mean(x))/(max(x)-min(x))
    return(x)
  }
  pr_stan = apply(pe,2,stan)
  e_stan = apply(e_trend,2,stan)
  
  ggrace = read.csv(input_ggrace,header = T)
  iei = read.csv(input_iei,header = T)
  
  ggrace = ggrace[,-1]
  iei = iei[,-1]
  
  source('D:/Mining_project/global/R/cor_analysis_global.R')
  ret_box = cor_analysis_global()
  
  neg_index = which(ret_box[,3]<=0)
  pos_index = which(ret_box[,3]>0)
  
  
  stat_by_group <- function(index,groupid){
    
    index = index[,groupid]
    index = apply(index,1,mean)
    
    return(index)
  }
  
  pr_pos1 = stat_by_group(pr_trend,pos_index)
  e_pos1 = stat_by_group(e_trend,pos_index)
  
  pr_pos = stat_by_group(pr_stan,pos_index)
  ggrace_pos = stat_by_group(ggrace,pos_index)
  iei_pos = stat_by_group(iei,pos_index)
  e_pos = stat_by_group(e_stan,pos_index)
  
  pr_neg = stat_by_group(pr_stan,neg_index)
  ggrace_neg = stat_by_group(ggrace,neg_index)
  iei_neg = stat_by_group(iei,neg_index)
  e_neg = stat_by_group(e_stan,neg_index)
}