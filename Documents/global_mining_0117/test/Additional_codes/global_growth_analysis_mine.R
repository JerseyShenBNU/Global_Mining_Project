global_growth_analysis_mine <-function(
  input_ggrace = 'D:\\Mining_project\\global\\Data\\new\\ts_index\\ggrace_mat_ts.csv',
  input_iei = 'D:\\Mining_project\\global\\Data\\new\\ts_index\\output_iei_ts.csv'
){
  source('D:/Mining_project/global/R/cor_analysis_global.R')
  ret_box = cor_analysis_global()
  
  #neg_select = which(ret_box[,3]<=0)
  
  loc = ret_box[,1:2]
  
  world = shapefile('D:\\Mining_project\\global\\Data\\new\\climate_zone\\world_continent2.shp')
  world = crop(world, extent(-180,180,-60,90))
  
  shapefile(world,'data/shp/world_crop.shp',overwrite = T)
  prov_map = fortify(world,region = "Region2")
  prov_map2 = rbind(prov_map,prov_map)
  prov_map2$type = rep(c(1,2),each = nrow(prov_map))
 
  grace = read.csv(input_ggrace,header = T)
  iei = read.csv(input_iei,header = T)
  grace = as.matrix(grace)
  iei = as.matrix(iei)
  
  grace = grace[,-1]
  iei = iei[,-1]
  
  grace_box = grace
  iei_box = iei 
  #grace = grace[,neg_select]
  #iei = iei[,neg_select]
  #loc = loc[neg_select,]
  
  zengfu_calc <- function(x){
    len = length(x)
    tmp = (x[len]-x[1])/2*100
    return(tmp)
  }
  
  grace_zengfu = apply(grace,2,zengfu_calc)
  iei_zengfu = apply(iei,2,zengfu_calc)
  
  
  df_grace = data.frame(long = loc[,1]-180,
                        lat = loc[,2],
                        Growth_NGGRACE=grace_zengfu)
  
  
  df_iei = data.frame(
    long = loc[,1] - 180,
    lat = loc[,2],
    Growth_NIEI = iei_zengfu
  )
  
  
  df_grace = melt(df_grace,c('long','lat'))
  df_iei = melt(df_iei,c('long','lat'))
  
  df_grace_stat = df_grace
  df_iei_stat = df_iei
  
  index_neg_grace = which(df_grace$value <=0)
  index_pos_grace = which(df_grace$value >0)
  index_neg_iei = which(df_iei$value <=0)
  index_pos_iei = which(df_iei$value >0)
  
  df_grace_pos = df_grace[index_pos_grace,]
  df_grace_neg = df_grace[index_neg_grace,]
  df_iei_pos = df_iei[index_pos_iei,]
  df_iei_neg = df_iei[index_neg_iei,]
  
  df_grace_pos$cuts = cut(df_grace_pos$value,breaks = c(0,25,50,75,100))
  df_grace_neg$cuts = cut(df_grace_neg$value,breaks = c(-100,-75,-50,-25,0))
  df_iei_pos$cuts = cut(df_iei_pos$value,breaks = c(0,25,50,75,100))
  df_iei_neg$cuts = cut(df_iei_neg$value,breaks = c(-100,-75,-50,-25,0))
  
  df_grace = rbind(df_grace_neg,df_grace_pos)
  df_iei = rbind(df_iei_neg,df_iei_pos)
  
  df_grace$type = c(rep(5,nrow(df_grace_neg)),rep(8,nrow(df_grace_pos)))
  df_iei$type = c(rep(9,nrow(df_iei_neg)),rep(6,nrow(df_iei_pos)))
  
  df_grace2 = df_grace
  df_iei2 = df_iei
  
  df_grace2$type = 2
  df_iei2$type = 3
  
  df_grace2$fill = c(rep('Negative Production Growth',nrow(df_grace_neg)),
                     rep('Positive Production Growth',nrow(df_grace_pos)))
  
  df_iei2$fill = c(rep('Negative Production Growth',nrow(df_iei_neg)),
                   rep('Positive Production Growth',nrow(df_iei_pos)))
  
  print(nrow(df_grace_pos))
  print(nrow(df_grace_neg))
  print(nrow(df_iei_pos))
  print(nrow(df_iei_neg))
  # 
  # [1] 390
  # [1] 344
  # [1] 411
  # [1] 323
  
  source('E:/Desktop/global_mining_extended/crop_by_continent_index.R')
  index_list = crop_by_continent_index()
  
  con_name = c('AF',"AS", "EU",  "NA", "OC", "SA")
  
  neg_grace_con = 1
  pos_grace_con = 1
  neg_iei_con = 1
  pos_iei_con = 1
  for(i in 1:length(index_list)){
    tmpcongrace = df_grace_stat$value[index_list[[i]]]
    tmpconiei = df_iei_stat$value[index_list[[i]]]
    
    tmp_neg_con_grace = length(which(tmpcongrace<=0))
    tmp_pos_con_grace = length(which(tmpcongrace>0))
    tmp_neg_con_iei = length(which(tmpconiei<=0))
    tmp_pos_con_iei = length(which(tmpconiei>0))
    
    neg_grace_con = c(neg_grace_con,tmp_neg_con_grace)
    pos_grace_con = c(pos_grace_con,tmp_pos_con_grace)
    neg_iei_con = c(neg_iei_con,tmp_neg_con_iei)
    pos_iei_con = c(pos_iei_con,tmp_pos_con_iei)
  }
  
  neg_grace_con = neg_grace_con[-1]
  pos_grace_con = pos_grace_con[-1]
  neg_iei_con = neg_iei_con[-1]
  pos_iei_con = pos_iei_con[-1]
  
  stat_df_grace = data.frame(con_name,
                             Negative_Growth_Number = neg_grace_con*-1,
                             Positive_Growth_Number = pos_grace_con)
  stat_df_iei = data.frame(con_name,
                           Negative_Growth_Number = neg_iei_con*-1,
                           Positive_Growth_Number = pos_iei_con)
  
  stat_df_grace = melt(stat_df_grace,'con_name')
  stat_df_iei = melt(stat_df_iei,'con_name')
  
  stat_df_grace$type = 2
  stat_df_iei$type = 3
  
  stat_df_grace$fill = rep(c("Negative Growth Number",'Positive Growth Number'),
                           each = 6)
  stat_df_iei$fill = rep(c("Negative Growth Number",'Positive Growth Number'),
                         each = 6)
  
  stat_df_grace$x = rep(1:6,2)
  stat_df_iei$x = rep(1:6,2)
  
  
  
  
  ## calculation of mean index for each continent
  
  mean_grace_box = 1
  mean_iei_box = 1
  
  
  for(i in 1:length(index_list)){
    tmpgrace = apply(grace_box[,index_list[[i]]],1,mean)
    tmpiei = apply(iei_box[,index_list[[i]]],1,mean)
    
    mean_grace_box = cbind(mean_grace_box,tmpgrace)
    mean_iei_box = cbind(mean_iei_box,tmpiei)
    
  }
  
  mean_grace_box = mean_grace_box[,-1]
  mean_iei_box = mean_iei_box[,-1]
  
  colnames(mean_grace_box) = paste0(con_name)
  colnames(mean_iei_box) = paste0(con_name)
  

  mean_zengfu_grace = apply(mean_grace_box,2,zengfu_calc)
  mean_zengfu_iei = apply(mean_iei_box,2,zengfu_calc)
  
  
  
  
  
  
  
  
  
  
  df_zengfu = data.frame(con_name,
                         Growth_NNGRACE = mean_zengfu_grace,
                         Growth_NIEI = mean_zengfu_iei)
  dfm = melt(df_zengfu,'con_name')
  
  mycolor = c('Growth_NNGRACE'='#4DBBD5B2',
              'Growth_NIEI' = '#00A087B2')
  source('D:/Mining_project/global/R/theme_set_evolution_cor_map.R')
  print('start')
  theme_set_evolution<-theme_set_evolution_cor_map(12,
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
  
  p1 = ggplot()+
    geom_bar(data = dfm,aes( x= con_name,y = value,fill = variable),
             stat = 'identity',position = 'stack',width= 0.5)+
    scale_fill_manual(values = mycolor)+
    geom_hline(yintercept = 0)+
    theme_set_evolution+
    #theme_bw()+
    legend_set+
    theme(
      axis.text.y = element_text(face = 'bold',color = 'black',size = 14,hjust = 0.5,angle = 90),
      axis.text.x = element_text(face = 'bold',color = 'black',size = 14,hjust = 0.5),
      axis.title = element_text(face = 'bold',color = 'black',size = 14,hjust = 0.5),
      legend.text = element_text(face = 'bold',color = 'black',size = 14,hjust = 0.5),
      legend.title = element_blank(),
      legend.position = 'none',
      legend.direction = 'horizontal',
      legend.key.height=unit(0.1,"inches"),
      legend.key.width=unit(0.5,"inches")
    )+
    xlab('Continents')+
    ylab('Mean Growth Ratio (%)')
  
  
  png('plot/statistic_bar2.png',
      width = 28.45,
      height = 10.76,
      units = 'cm',
      res = 1000)
  print(p1)
  dev.off()
  
  return(list(df_grace,df_iei,stat_df_grace,stat_df_iei))
  
  
  
}













































