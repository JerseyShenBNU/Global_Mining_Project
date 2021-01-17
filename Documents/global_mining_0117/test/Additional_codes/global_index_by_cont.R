global_index_by_cont<-function(
  
){
  library(reshape2)
  # import of ndvi 
  input_vci = 'D:\\Mining_project\\global\\Data\\output_of_vci\\output_vci.csv'
  vci_box = read.csv(input_vci,header = T)
  vci_box = vci_box[,-1]
  
  input_gpp = 'D:\\Mining_project\\global\\Data\\output_of_gpp\\gpp_out.csv'
  gpp_box = read.csv(input_gpp,header = T)
  gpp_box = gpp_box[,-1]
  
  sm_box = read.csv('output_analysis/sm_box.csv',header = T)
  sm_box = sm_box[,-1]
  
  ggrace_box = read.csv('output_analysis/grace_mat.csv')
  ggrace_box = ggrace_box[,-1]
  
  
  na_index = read.csv('D:\\Mining_project\\global\\Data\\new\\na_index\\na_index.csv',
                      header = T)
  na_index = na_index[,-1]
  
  ## remoe na_index
  
  vci_box = vci_box[,-na_index]
  gpp_box = gpp_box[,-na_index]
  sm_box = sm_box[1:165,]  
  ggrace_box = ggrace_box[1:165,-na_index]
  
  ts_stan_calc<-function(x,mode = 'iei'){
    tmp = ts(x,c(2002,4),frequency = 12)
    tmp = decompose(tmp)$trend
    
    naid = which(is.na(tmp))
    tmp = tmp[-naid]
    if(mode == 'ggrace'){
      tmp = tmp/(max(tmp) - min(tmp))
    }else{
      tmp = (tmp - mean(tmp))/(max(tmp) - min(tmp))
    }
    
    return(tmp)
  }
  
  index2annual <- function(x){
    tmp = c(rep(NA,9),x,rep(NA,6))
    tmp = matrix(tmp,nrow = 12)
    mean_by_col <- function(x){
      x = mean(x,na.rm = T)
    }
    tmp = apply(tmp,2,mean_by_col)
    tmp = as.numeric(tmp)
    return(tmp)
  }
  
  # calculation of whole globe
  #######
  vci_mean = apply(vci_box,1,mean)
  gpp_mean = apply(gpp_box,1,mean)
  sm_mean = apply(sm_box,1,mean)
  
  iei_mean = iei_calc(gpp_mean,vci_mean,sm_mean)
  ggrace_mean = apply(ggrace_box,1,mean)
  
  iei_trend_stan = ts_stan_calc(iei_mean)
  ggrace_trend_stan = ts_stan_calc(ggrace_mean,mode = 'ggrace')
  
  iei_an = index2annual(iei_trend_stan)
  gg_an = index2annual(ggrace_trend_stan)
  
  mine_an = read.csv('output/arid_coun_mine.csv',
                     header = T)
  mine_an = mine_an[,-c(1,2,ncol(mine_an))]
  mine_an = apply(mine_an,2,sum)
  
  stan_fun <- function(x){
    x = (x - mean(x))/(max(x)-min(x))
    return(x)
  }
  
  #mine_an = mine_an[13:26]
  mine_an = stan_fun(mine_an)
 
  
  date = seq(as.Date('1990-01-01'),
             as.Date('2018-01-01'),
             '1 year')
  
  df_iei_global = data.frame(date, NIEI = c(rep(NA,12),iei_an,rep(NA,3)))
  df_gg_global = data.frame(date, NGGRACE = c(rep(NA,12),gg_an,rep(NA,3)))
  df_mine_global = data.frame(date, Mineral = mine_an)
  
  df_iei_m = melt(df_iei_global,'date')
  df_gg_m = melt(df_gg_global,'date')
  df_mine_m = melt(df_mine_global,'date')
  
  df_iei_m$color = 'NIEI'
  df_gg_m$color = 'NGGRACE'
  df_mine_m$color = 'Mineral Production'
  
  mycolor = c('Mineral Production' = 'red',
              'NGGRACE'= '#2453A9',
              'NIEI' = '#24A994')
  
  df_g = rbind(df_iei_m,df_gg_m,df_mine_m)
  
  source('D:/Mining_project/global/R/theme_set_evolution_cor_map.R')
  print('start')
  theme_set_evolution<-theme_set_evolution_cor_map(14,
                                                   width = 3)
  fontsize = 14
  
  legend_set<-theme(legend.background = element_rect(fill='white'),
                    legend.key = element_blank(),
                    legend.key.size = unit(3,'lines'),
                    legend.key.height=unit(0.1,"inches"),
                    legend.key.width=unit(0.5,"inches"),
                    legend.text=element_text(colour="black",size=fontsize,face = 'bold'),
                    legend.text.align=0,
                    #legend.title=element_text(colour="black",size=15,face='bold'),
                    legend.title = element_blank(),
                    legend.title.align=1,
                    legend.position=c('bottom'), 
                    # two-element numeric vector,(0,0)-(1,1)
                    legend.direction="horizontal",
                    legend.justification=c('center'),#"center" or two-element numeric vector
                    legend.box="vertical",#
                    legend.box.just="top"
                    ,plot.background = element_rect(color='transparent')
                    #strip.text = element_text(colour="black",size=fontsize,face = 'bold')
  )
  
  write.csv(df_g,'output_analysis/global_mean_index.csv')
  p1 = ggplot(data = df_g,aes(x = date,y = value,color = color))+
    
    # geom_vline(xintercept = as.Date('2007-01-01'),color = 'black',
    #            linetype = 'dashed',size = 1)+
    # geom_vline(xintercept = as.Date('2009-01-01'),color = 'black',
    #            linetype = 'dashed',size = 1)+
    # geom_vline(xintercept = as.Date('2011-01-01'),color = 'black',
    #            linetype = 'dashed',size = 1)+
    # geom_vline(xintercept = as.Date('2015-01-01'),color = 'black',
    #            linetype = 'dashed',size = 1)+
     geom_hline(yintercept = 0,color = 'black',
                linetype = 'dashed',size = 1)+
    geom_line(size = 2)+
    geom_point(size = 3)+
    scale_color_manual(values = mycolor)+
    theme_bw()+
    theme_set_evolution+legend_set+
    guides(color = guide_legend(nrow = 1))+
    xlab('Date')+
    ylab("Indices")
  
  
  
  # calculation of each continent 
  source('E:/Desktop/global_mining_extended/crop_by_continent_index.R')
  index_cont <- crop_by_continent_index()
  
  iei_cont_box = 1
  gg_cont_box = 1
  for(i in 1:length(index_cont)){
    id = index_cont[[i]]
    
    tmpvci = vci_box[,id]
    tmpgpp = gpp_box[,id]
    tmpsm = sm_box[,id]
    tmpgg = ggrace_box[,id]
    
    tmpvci = apply(tmpvci,1,mean)
    tmpgpp = apply(tmpgpp,1,mean)
    tmpsm = apply(tmpsm,1,mean)
    tmpgg = apply(tmpgg,1,mean)
    
    tmpiei = iei_calc(tmpgpp,tmpvci,tmpsm)
    
    tmpiei = ts_stan_calc(tmpiei)
    tmpgg = ts_stan_calc(tmpgg,mode = 'ggrace')
    
    iei_cont_box = cbind(iei_cont_box,tmpiei)
    gg_cont_box = cbind(gg_cont_box,tmpgg)
  }
  
  iei_cont_box = iei_cont_box[,-1]
  gg_cont_box = gg_cont_box[,-1]
  
  colnames(iei_cont_box) = c('AF',"AS", "EU",  "NA", "OC", "SA")
  colnames(gg_cont_box) = c('AF',"AS", "EU",  "NA", "OC", "SA")
  
  iei_con_an = apply(iei_cont_box,2,index2annual)
  gg_con_an = apply(gg_cont_box,2,index2annual)
  
  write.csv(iei_con_an,'output_analysis/iei_con_an.csv')
  write.csv(gg_con_an,'output_analysis/gg_con_an.csv')
  mine_an = read.csv('output_analysis/mine_box_s.csv',header = T)
  mine_an = mine_an[,-1]
  
  date = seq(as.Date('2002-01-01'),
             as.Date('2015-01-01'),
             '1 year')
  df1 = data.frame(date,mine_an)
  df2 = data.frame(date,gg_con_an)
  df3 = data.frame(date,iei_con_an)
  
  df1 = melt(df1,'date')
  df2 = melt(df2,'date')
  df3 = melt(df3,'date')
  
  library(RColorBrewer)
  mycolor = colorRampPalette(brewer.pal(11,'Spectral'))(12)
  color_group = paste0('a',1:12)
  
  df1$color = 'Mineral Production'
  df2$color = 'NGGRACE'
  df3$color = 'NIEI'
  
  mycolor = c('Mineral Production' = 'red',
              'NGGRACE'= '#2453A9',
              'NIEI' = '#24A994')
  
  
  
  df = rbind(df1,df2,df3)
  
  source('D:/Mining_project/global/R/theme_set_evolution_cor_map.R')
  print('start')
  theme_set_evolution<-theme_set_evolution_cor_map(14,
                                                   width = 3)
  fontsize = 14
  
  legend_set<-theme(legend.background = element_rect(fill='white'),
                    legend.key = element_blank(),
                    legend.key.size = unit(3,'lines'),
                    legend.key.height=unit(0.1,"inches"),
                    legend.key.width=unit(0.5,"inches"),
                    legend.text=element_text(colour="black",size=fontsize,face = 'bold'),
                    legend.text.align=0,
                    #legend.title=element_text(colour="black",size=15,face='bold'),
                    legend.title = element_blank(),
                    legend.title.align=1,
                    legend.position=c('bottom'), 
                    # two-element numeric vector,(0,0)-(1,1)
                    legend.direction="horizontal",
                    legend.justification=c('center'),#"center" or two-element numeric vector
                    legend.box="vertical",#
                    legend.box.just="top"
                    ,plot.background = element_rect(color='transparent')
                    #strip.text = element_text(colour="black",size=fontsize,face = 'bold')
  )
  
  p = ggplot()+
    geom_line(data = df,aes(x = date,y = value,color = color),
              size = 2)+
    facet_wrap(~variable,nrow = 2,scales = 'free')+
    scale_color_manual(values = mycolor)+
    theme_bw()+
    theme_set_evolution+legend_set+
    guides(color = guide_legend(nrow = 1))+
    xlab('Date')+
    ylab("Indices")
  
  p 
  
  
  
  
  
  
  
  
}