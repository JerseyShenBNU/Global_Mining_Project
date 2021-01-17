pe_gg_iei_evolution<-function(
  
){
  
  pe_trendd = read.csv('output/precip_minus_e.csv',header = T)
  pe_trendd = pe_trendd[,-1]
  
  ggrace = read.csv(input_ggrace,header = T)
  iei = read.csv(input_iei,header = T)
  
  ggrace = ggrace[,-1]
  iei = iei[,-1]
  
  
  
  source('E:/Desktop/global_mining_extended/crop_by_continent_index.R')
  
  index_cont <- crop_by_continent_index()
  
  pr_con_box = 1
  gg_con_box = 1
  iei_con_box = 1
  for(i in 1:length(index_cont)){
    tmpid = index_cont[[i]]
    
    tmppr = pe_trendd[,tmpid]
    tmpgg = ggrace[,tmpid]
    tmpiei = iei[,tmpid]
    tmpmean = apply(tmppr,1,mean)
    tmpgg = apply(tmpgg,1,mean)
    tmpiei = apply(tmpiei,1,mean)
    pr_con_box = cbind(pr_con_box,tmpmean)
    gg_con_box = cbind(gg_con_box,tmpgg)
    iei_con_box = cbind(iei_con_box,tmpiei)
  }
  pr_con_box = pr_con_box[,-1]
  gg_con_box = gg_con_box[,-1]
  iei_con_box = iei_con_box[,-1]
  
  colnames(pr_con_box) = c('AF',"AS", "EU",  "NA", "OC", "SA")
  colnames(iei_con_box) = c('AF',"AS", "EU",  "NA", "OC", "SA")
  colnames(gg_con_box) = c('AF',"AS", "EU",  "NA", "OC", "SA")
  
  stan<-function(x){
    x = (x-mean(x))/(max(x)-min(x))
    return(x)
  }
  pr_stan = apply(pr_con_box,2,stan)
  
  date = seq(as.Date('2002-10-01'),
             as.Date('2015-06-01'),
             '1 month')
  
  df1 = data.frame(date,pr_stan)
  
  df2 = data.frame(date,gg_con_box)
  df3 = data.frame(date,iei_con_box)
  
  df1 = melt(df1,'date')
  df2 = melt(df2,'date')
  df3 = melt(df3,'date')
  
  df1$color = 'Pr'
  df2$color = 'GGRACE'
  df3$color = 'IEI'
  
  cor1 = mapply(cor_calc,pr_stan,gg_con_box)
  
  df = rbind(df1,df2,df3)
  
  p = ggplot()+
    geom_line(data = df,aes(x = date,y = value,color = color))+
    facet_wrap(~variable,nrow = 2)+
    scale_color_npg()+
    theme_bw()
  
  p
}