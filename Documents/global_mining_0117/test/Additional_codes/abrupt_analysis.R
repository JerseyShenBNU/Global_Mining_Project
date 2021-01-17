abrupt_ponit_analysis <-function(
  
){
  
  # 0. import data 
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
  neg_ggrace_seq = seq(2,28,4)
  pos_ggrace_seq = seq(4,28,4)
  
  date = seq(as.Date('2002-10-01'),as.Date('2015-06-01'),'1 month')
  
  neg_iei_df = df[,neg_iei_seq]
  pos_iei_df = df[,pos_iei_seq]
  neg_gg_df = df[,neg_ggrace_seq]
  pos_gg_df = df[,pos_ggrace_seq]
  
  
  tp_list = list()
  tell_final = rep(0.4,28)
  tell_final[neg_iei_seq] = c(0.4,0.38,0.2,0.2,0.2,0.2,0.2)
  tell_final[pos_iei_seq] = c(0.25,0.2,0.2,0.3,0.4,0.4,0.4)
  for(i in 1:ncol(df)){
    temp = df[,i]
    temp_tp = find_peak_valley_p_trial(temp,tell_final = tell_final[i])
    
    tp_list[[i]] = temp_tp
    print(i)
    #break
  }
  
  neg_iei_tp_list = tp_list[neg_iei_seq]
  pos_iei_tp_list = tp_list[pos_iei_seq]
  
  neg_p_df_box = list()
  pos_p_df_box = list()
  
  neg_ggrace_box = 1
  pos_ggrace_box = 1
  
  index = seq(1,14,2)
  for(i in 1:length(neg_iei_tp_list)){
    
    neg_temp_tp = neg_iei_tp_list[[i]]  
    pos_temp_tp = pos_iei_tp_list[[i]]
    
    neg_temp = neg_iei_df[,i]
    pos_temp = pos_iei_df[,i]
    
    neg_gg_temp = neg_gg_df[,i]
    pos_gg_temp = pos_gg_df[,i]
    
    neg_p_df = data.frame(
      date = date[neg_temp_tp],
      IEI = neg_temp[neg_temp_tp],
      
      #date = as.numeric(neg_temp_tp),
      #IEI = neg_temp[neg_temp_tp],
      type = rep(index[i],length(neg_temp_tp))
    )
    
    pos_p_df = data.frame(
      date = date[pos_temp_tp],
      IEI = pos_temp[pos_temp_tp],
      #date = as.numeric(pos_temp_tp),
      #IEI = pos_temp[pos_temp_tp],
      type = rep(index[i],length(pos_temp_tp))
      
    )
    
    temp_neg_GGRACE = neg_gg_temp[neg_temp_tp]
    temp_pos_GGRACE = pos_gg_temp[pos_temp_tp]
    
    neg_p_df_box[[i]] = neg_p_df
    pos_p_df_box[[i]] = pos_p_df
    neg_ggrace_box = c(neg_ggrace_box,temp_neg_GGRACE)
    pos_ggrace_box = c(pos_ggrace_box,temp_pos_GGRACE)
  }
  
  neg_p_df_bd = neg_p_df_box[[1]]
  pos_p_df_bd = pos_p_df_box[[1]]
  
  neg_ggrace_box = neg_ggrace_box[-1]
  pos_ggrace_box = pos_ggrace_box[-1]
  
  
  ####### just for mutation analysis
  #####
   
  #####

  
  #write.csv(neg_ggrace_box,'D:\\Mining_project\\global\\Data\\new\\mutation_rate_analysis\\neg_ggrace_box.csv')
  #write.csv(pos_ggrace_box,'D:\\Mining_project\\global\\Data\\new\\mutation_rate_analysis\\pos_ggrace_box.csv')
  
  for(i in 2:length(neg_p_df_box)){
    neg_p_df_bd = rbind(neg_p_df_bd,
                        neg_p_df_box[[i]])
    pos_p_df_bd = rbind(pos_p_df_bd,
                        pos_p_df_box[[i]])
  }
  
  neg_p_df_box = as.data.frame(neg_p_df_bd)
  pos_p_df_box = as.data.frame(pos_p_df_bd)
  
  #write.csv(neg_p_df_box,'D:\\Mining_project\\global\\Data\\new\\mutation_rate_analysis\\neg_p_df_box.csv')
  #write.csv(pos_p_df_box,'D:\\Mining_project\\global\\Data\\new\\mutation_rate_analysis\\pos_p_df_box.csv')
  
  print("Pass");  #break for mutation analysis
  #point structure date, IEI, type
  # line data frame plot
  library(reshape2)
  neg_ggrace_seq = seq(2,28,4)
  pos_ggrace_seq = seq(4,28,4)
  
  neg_line_seq = sort(c(neg_iei_seq,neg_ggrace_seq))
  pos_line_seq = sort(c(pos_iei_seq,pos_ggrace_seq))
  
  
  neg_line_df = df[,neg_line_seq]
  pos_line_df = df[,pos_line_seq]
  
  
  #date_re = 1:length(date)
  #date_re = as.character(date_re)
  
  neg_line_df = data.frame(date = date,neg_line_df)
  pos_line_df = data.frame(date = date,pos_line_df)
  
 
  
  neg_line_df_m = melt(neg_line_df,'date')
  pos_line_df_m = melt(pos_line_df,'date')
  
  colors_neg = rep(c('IEI','GGRACE'),each = nrow(neg_line_df))
  colors_neg = rep(colors_neg,7)
  
  colors_pos = colors_neg
  
  neg_line_df_m$color = colors_neg
  pos_line_df_m$color = colors_pos
  
  type = rep(seq(1,14,2),each = (nrow(neg_line_df)*2))
  #type = paste0("type",type)
  
  neg_line_df_m$type = type
  pos_line_df_m$type = type
  
  source('D:/Mining_project/global/R/partial_cor_analysis_global.R')
  
  #return(list(neg_line_df_m,neg_iei_tp_list))
  neg_cor_df = partial_cor_analysis_global(neg_line_df_m,
                                      neg_iei_tp_list)
  pos_cor_df = partial_cor_analysis_global(pos_line_df_m,
                                           pos_iei_tp_list)
  
  write.csv(neg_cor_df,'D:\\Mining_project\\global\\plot\\abrupt_point_analysis\\neg_cor_df.csv')
  write.csv(pos_cor_df,'D:\\Mining_project\\global\\plot\\abrupt_point_analysis\\pos_cor_df.csv')
  
  #return(neg_cor_df)
  
  require('gridExtra')
  
  df = read.csv('D:\\Mining_project\\global\\plot\\abrupt_point_analysis\\neg_cor_df.csv',header = T)
  neg_cor_df = df[,-1]
  pos_df = read.csv('D:\\Mining_project\\global\\plot\\abrupt_point_analysis\\pos_cor_df.csv',
                    header = T)
  pos_cor_df = pos_df[,-1]
  
  #plot parameters
  #####
  theme_set_evolution<-theme_set_evolution_cor_map(12,
                                                   width = 3)
  fontsize = 15
  
  legend_set<-theme(legend.background = element_rect(fill='white'),
                    legend.key = element_blank(),
                    legend.key.size = unit(3,'lines'),
                    legend.key.height=unit(0.1,"inches"),#图例分类符号高度
                    legend.key.width=unit(0.5,"inches"),#图例符号的宽度
                    legend.text=element_text(colour="black",size=fontsize,face = 'bold'),#图例分类标签设置
                    legend.text.align=0,#0左，1右，0.5居中， 图例分类标签的对齐方式
                    #legend.title=element_text(colour="black",size=15,face='bold'),#图例标题设置
                    legend.title = element_blank(),
                    legend.title.align=1,#图例标题对齐方式
                    legend.position=c('bottom'),#"none","left","right","bottom","top",or 
                    # two-element numeric vector,(0,0)-(1,1)
                    legend.direction="horizontal",#"vertical" 图例排列方向
                    legend.justification=c('center'),#"center" or two-element numeric vector
                    legend.box="vertical",#"horizontal",对图例的排列方式
                    legend.box.just="top"#多图例的居中方式
                    ,plot.background = element_rect(color='transparent')
  )
  #####
  
  
  
  #return(neg_p_df_box)
  library(facetscales)
  write.csv(neg_line_df_m,'D:\\Mining_project\\global\\plot\\abrupt_point_analysis\\neg_line_df_m.csv')
  #break
  label_size = 4
  neg_plot_p = ggplot()+ 
    theme_set_evolution+legend_set
  
  
  neg_bar_fill = rep('#D46A6A',nrow(neg_cor_df))
  pos_bar_fill = rep('#D46A6A',nrow(pos_cor_df))
  
  neg_bar_fill[which(neg_cor_df$cor <0)] = '#50e0e7'
  pos_bar_fill[which(pos_cor_df$cor <0)] = '#50e0e7'
  
  neg_cor_df$fill = neg_bar_fill
  pos_cor_df$fill = pos_bar_fill
  
  neg_plot_p_bar = neg_plot_p+
    geom_bar(data = neg_cor_df, aes(x = x, y = cor,fill = fill),stat = 'identity')+
    geom_text(data = neg_cor_df,aes(x = x, y = cor,label = label),
              vjust = -0.3, color = 'black',size = label_size)+
    ylim(-1,1)+
    scale_fill_manual(values= c('#50e0e7','#D46A6A'),
                      labels = c('Negative Relation','Positive Relation'))+
    facet_wrap(~ type, ncol = 2,scales = 'free')+
    xlab('Time Period')+
    ylab('Correlation Coefficients')
    
  #############
  print('pass2')
  rect_plot1 = neg_p_df_box[which(neg_p_df_box[,3]==1),]
  rect_plot2 = neg_p_df_box[which(neg_p_df_box[,3]==3),]
  rect_plot3 = neg_p_df_box[which(neg_p_df_box[,3]==5),]
  rect_plot4 = neg_p_df_box[which(neg_p_df_box[,3]==7),]
  rect_plot5 = neg_p_df_box[which(neg_p_df_box[,3]==9),]
  rect_plot6 = neg_p_df_box[which(neg_p_df_box[,3]==11),]
  rect_plot7 = neg_p_df_box[which(neg_p_df_box[,3]==13),]
  
  rect_box  = list(rect_plot1,
                   rect_plot2,
                   rect_plot3,
                   rect_plot4,
                   rect_plot5,
                   rect_plot6,
                   rect_plot7)
  
  
  for(i in 1:7){
    
    temp_type = rect_box[[i]][1,3]
    temp_rect0 = data.frame(date = as.Date('2002-10-01'),
                            IEI = 0,
                            #GGRACE = 0,
                            type = temp_type)
    
    temp_rect1 = data.frame(date = as.Date('2015-06-01'),
                            IEI = 0,
                            #GGRACE = 0,
                            type = temp_type)
    
    rect_box[[i]] = rbind(temp_rect0,rect_box[[i]],temp_rect1)
    
  }
  rect_plot1 = rect_box[[1]]
  rect_plot2 = rect_box[[2]]
  rect_plot3 = rect_box[[3]]
  rect_plot4 = rect_box[[4]]
  rect_plot5 = rect_box[[5]]
  rect_plot6 = rect_box[[6]]
  rect_plot7 = rect_box[[7]]
  
  print(rect_plot1)
  print(rect_plot2)
  print(rect_plot3)
  print(rect_plot4)
  print(rect_plot5)
  print(rect_plot6)
  print(rect_plot7)
  ################
  write.csv(neg_p_df_box,'D:\\Mining_project\\global\\plot\\abrupt_point_analysis\\neg_p_df_box.csv')
  write.csv(pos_p_df_box,'D:\\Mining_project\\global\\plot\\abrupt_point_analysis\\pos_p_df_box.csv')
  
  neg_plot_p_line = neg_plot_p+
    geom_rect(data = rect_plot1,aes(xmin=date[1],xmax=date[2],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot1,aes(xmin=date[2],xmax=date[3],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot1,aes(xmin=date[3],xmax=date[4],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot1,aes(xmin=date[4],xmax=date[5],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot1,aes(xmin=date[5],xmax=date[6],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot1,aes(xmin=date[6],xmax=date[7],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot1,aes(xmin=date[7],xmax=date[8],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    
    
    geom_rect(data = rect_plot2,aes(xmin=date[1],xmax=date[2],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot2,aes(xmin=date[2],xmax=date[3],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot2,aes(xmin=date[3],xmax=date[4],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot2,aes(xmin=date[4],xmax=date[5],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot2,aes(xmin=date[5],xmax=date[6],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot2,aes(xmin=date[6],xmax=date[7],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot2,aes(xmin=date[7],xmax=date[8],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot2,aes(xmin=date[8],xmax=date[9],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot2,aes(xmin=date[9],xmax=date[10],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot2,aes(xmin=date[10],xmax=date[11],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
  
    
    geom_rect(data = rect_plot3,aes(xmin=date[1],xmax=date[2],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot3,aes(xmin=date[2],xmax=date[3],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot3,aes(xmin=date[3],xmax=date[4],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    
    geom_rect(data = rect_plot4,aes(xmin=date[1],xmax=date[2],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot4,aes(xmin=date[2],xmax=date[3],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot4,aes(xmin=date[3],xmax=date[4],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    
    geom_rect(data = rect_plot5,aes(xmin=date[1],xmax=date[2],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot5,aes(xmin=date[2],xmax=date[3],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot5,aes(xmin=date[3],xmax=date[4],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot5,aes(xmin=date[4],xmax=date[5],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot5,aes(xmin=date[5],xmax=date[6],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot5,aes(xmin=date[6],xmax=date[7],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot5,aes(xmin=date[7],xmax=date[8],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot5,aes(xmin=date[8],xmax=date[9],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    
    geom_rect(data = rect_plot6,aes(xmin=date[1],xmax=date[2],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[2],xmax=date[3],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[3],xmax=date[4],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[4],xmax=date[5],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[5],xmax=date[6],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[6],xmax=date[7],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[7],xmax=date[8],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[8],xmax=date[9],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[9],xmax=date[10],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[10],xmax=date[11],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    
    geom_rect(data = rect_plot7,aes(xmin=date[1],xmax=date[2],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot7,aes(xmin=date[2],xmax=date[3],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot7,aes(xmin=date[3],xmax=date[4],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot7,aes(xmin=date[4],xmax=date[5],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot7,aes(xmin=date[5],xmax=date[6],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot7,aes(xmin=date[6],xmax=date[7],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot7,aes(xmin=date[7],xmax=date[8],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot7,aes(xmin=date[8],xmax=date[9],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    
    geom_line(data = neg_line_df_m,
              aes(x = date, y = value,color = color),lwd = 1)+
    geom_point(data = neg_p_df_box,
               aes(x = date,y = IEI), color = '#8FBC8F',size = 3,shape =16)+
    scale_color_manual(values = c('#1E90FF','#8FBC8F'))+
    facet_wrap(~ type, ncol = 2,scales = 'free')+
    xlab("Date (month)")+
    ylab("Index")
  
  dir.create('D:\\Mining_project\\global\\plot\\abrupt_point_analysis1')
  png('D:\\Mining_project\\global\\plot\\abrupt_point_analysis1\\neg_plot_p_line.png',
      height = 20,
      width = 20,
      units = 'cm',
      res = 1000)
  print(neg_plot_p_line)
  dev.off()
  
  png('D:\\Mining_project\\global\\plot\\abrupt_point_analysis1\\neg_plot_p_bar.png',
      height = 20,
      width = 20,
      units = 'cm',
      res = 1000)
  print(neg_plot_p_bar)
  dev.off()
  
  
  
  # pos abrupt check 
  
  ###########
  rect_plot1 = pos_p_df_box[which(pos_p_df_box[,3]==1),]
  rect_plot2 = pos_p_df_box[which(pos_p_df_box[,3]==3),]
  rect_plot3 = pos_p_df_box[which(pos_p_df_box[,3]==5),]
  rect_plot4 = pos_p_df_box[which(pos_p_df_box[,3]==7),]
  rect_plot5 = pos_p_df_box[which(pos_p_df_box[,3]==9),]
  rect_plot6 = pos_p_df_box[which(pos_p_df_box[,3]==11),]
  rect_plot7 = pos_p_df_box[which(pos_p_df_box[,3]==13),]
  
  rect_box  = list(rect_plot1,
                   rect_plot2,
                   rect_plot3,
                   rect_plot4,
                   rect_plot5,
                   rect_plot6,
                   rect_plot7)
  
  for(i in 1:7){
    
    temp_type = rect_box[[i]][1,3]
    temp_rect0 = data.frame(date = as.Date('2002-10-01'),
                            IEI = 0,
                            #GGRACE = 0,
                            type = temp_type)
    
    temp_rect1 = data.frame(date = as.Date('2015-06-01'),
                            IEI = 0,
                            #GGRACE = 0,
                            type = temp_type)
    
    rect_box[[i]] = rbind(temp_rect0,rect_box[[i]],temp_rect1)
    
  }
  rect_plot1 = rect_box[[1]]
  rect_plot2 = rect_box[[2]]
  rect_plot3 = rect_box[[3]]
  rect_plot4 = rect_box[[4]]
  rect_plot5 = rect_box[[5]]
  rect_plot6 = rect_box[[6]]
  rect_plot7 = rect_box[[7]]
  
  ret_plot_box = rbind(rect_plot1,
                       rect_plot2,
                       rect_plot3,
                       rect_plot4,
                       rect_plot5,
                       rect_plot6,
                       rect_plot7)
  write.csv(ret_plot_box,'E:\\Desktop\\123.csv')
  print(rect_plot1)
  print(rect_plot2)
  print(rect_plot3)
  print(rect_plot4)
  print(rect_plot5)
  print(rect_plot6)
  print(rect_plot7)
  ###########
  
  neg_plot_p = ggplot()+
    theme_set_evolution+legend_set
  
  neg_plot_p_bar = neg_plot_p+
    geom_bar(data = pos_cor_df, aes(x = x, y = cor,fill = fill),stat = 'identity')+
    geom_text(data = pos_cor_df,aes(x = x, y = cor,label = label),
              vjust = -0.3,color = 'black',size = label_size)+
    ylim(-1.3,1.3)+
    scale_fill_manual(values= c('#50e0e7','#D46A6A'),
                      labels = c('Negative Relation','Positive Relation'))+
    facet_wrap(~ type, ncol = 2,scales = 'free')+
    xlab('Time Period')+
    ylab('Correlation Coefficients')
  
  
  neg_plot_p_line = neg_plot_p+
    geom_rect(data = rect_plot1,aes(xmin=date[1],xmax=date[2],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot1,aes(xmin=date[2],xmax=date[3],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot1,aes(xmin=date[3],xmax=date[4],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot1,aes(xmin=date[4],xmax=date[5],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot1,aes(xmin=date[5],xmax=date[6],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot1,aes(xmin=date[6],xmax=date[7],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    
    
    geom_rect(data = rect_plot2,aes(xmin=date[1],xmax=date[2],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot2,aes(xmin=date[2],xmax=date[3],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot2,aes(xmin=date[3],xmax=date[4],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot2,aes(xmin=date[4],xmax=date[5],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot2,aes(xmin=date[5],xmax=date[6],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot2,aes(xmin=date[6],xmax=date[7],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    
    geom_rect(data = rect_plot3,aes(xmin=date[1],xmax=date[2],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot3,aes(xmin=date[2],xmax=date[3],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot3,aes(xmin=date[3],xmax=date[4],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot3,aes(xmin=date[4],xmax=date[5],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    
    geom_rect(data = rect_plot4,aes(xmin=date[1],xmax=date[2],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot4,aes(xmin=date[2],xmax=date[3],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot4,aes(xmin=date[3],xmax=date[4],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot4,aes(xmin=date[4],xmax=date[5],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot4,aes(xmin=date[5],xmax=date[6],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot4,aes(xmin=date[6],xmax=date[7],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot4,aes(xmin=date[7],xmax=date[8],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot4,aes(xmin=date[8],xmax=date[9],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    
    geom_rect(data = rect_plot5,aes(xmin=date[1],xmax=date[2],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot5,aes(xmin=date[2],xmax=date[3],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot5,aes(xmin=date[3],xmax=date[4],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot5,aes(xmin=date[4],xmax=date[5],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    
    geom_rect(data = rect_plot6,aes(xmin=date[1],xmax=date[2],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[2],xmax=date[3],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[3],xmax=date[4],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[4],xmax=date[5],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[5],xmax=date[6],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[6],xmax=date[7],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[7],xmax=date[8],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[8],xmax=date[9],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[9],xmax=date[10],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[10],xmax=date[11],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[11],xmax=date[12],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot6,aes(xmin=date[12],xmax=date[13],ymin=-Inf,ymax=Inf),
              fill='#50e0e7',alpha = 0.1)+
    
    geom_rect(data = rect_plot7,aes(xmin=date[1],xmax=date[2],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot7,aes(xmin=date[2],xmax=date[3],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot7,aes(xmin=date[3],xmax=date[4],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot7,aes(xmin=date[4],xmax=date[5],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot7,aes(xmin=date[5],xmax=date[6],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot7,aes(xmin=date[6],xmax=date[7],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    geom_rect(data = rect_plot7,aes(xmin=date[7],xmax=date[8],ymin=-Inf,ymax=Inf),
              fill='#D46A6A',alpha = 0.1)+
    
    geom_line(data = pos_line_df_m,
              aes(x = date, y = value,color = color),lwd = 1)+
    
    geom_point(data = pos_p_df_box,
               aes(x = date,y = IEI), color = '#8FBC8F',size = 3,shape =16)+
    scale_color_manual(values = c('#1E90FF','#8FBC8F'))+
    facet_wrap(~ type, ncol = 2,scales = 'free')+
    xlab("Date (month)")+
    ylab("Index")
  
  
  #return(neg_p_df_box)
  
  png('D:\\Mining_project\\global\\plot\\abrupt_point_analysis1\\pos_plot_p_line.png',
      height = 20,
      width = 20,
      units = 'cm',
      res = 1000)
  print(neg_plot_p_line)
  dev.off()
  png('D:\\Mining_project\\global\\plot\\abrupt_point_analysis1\\pos_plot_p_bar.png',
      height = 20,
      width = 20,
      units = 'cm',
      res = 1000)
  print(neg_plot_p_bar)
  dev.off()
  
  
  break
    
    
    geom_hline(data = neg_cor_df,aes(yintercept = 0),color= 'black',size = 1)+
    geom_line(data = neg_line_df_m,
              aes(x = date, y = value,color = color),lwd = 1)+
    geom_point(data = neg_p_df_box,
               aes(x = date,y = IEI), color = '#8FBC8F',size = 3,shape =16)+
    scale_color_manual(values = c('#1E90FF','#8FBC8F'))+
    facet_wrap(~ type, ncol = 2,scale = 'free')
  
  pos_plot = ggplot()+
    geom_line(data = pos_line_df_m,
              aes(x = date, y = value,color = color),lwd = 1)+
    geom_point(data = pos_p_df_box,
               aes(x = date,y = IEI), color = '#8FBC8F',size = 3,shape =16)+
    #ylim(-1,1)+
    scale_color_manual(values = c('#1E90FF','#8FBC8F'))+
    facet_wrap(~ type, ncol = 1)
  
  png('D:\\Mining_project\\global\\plot\\abrupt_point_analysis\\neg_trial.png',
      width = 15,
      height = 30,
      units = 'cm',
      res = 1000)
  print(neg_plot)
  dev.off()
  
  
  png('D:\\Mining_project\\global\\plot\\abrupt_point_analysis\\pos_trial2.png',
      width = 15,
      height = 30,
      units = 'cm',
      res = 1000)
  print(pos_plot)
  dev.off()
  
  
  
  
  
  
  
}





















