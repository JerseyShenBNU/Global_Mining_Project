mutation_rate_analysis <-function(
  
){
  input_neg_iei = 'D:\\Mining_project\\global\\Data\\new\\mutation_rate_analysis\\neg_p_df_box.csv'
  input_pos_iei = 'D:\\Mining_project\\global\\Data\\new\\mutation_rate_analysis\\pos_p_df_box.csv'
  
  input_neg_gg = 'D:\\Mining_project\\global\\Data\\new\\mutation_rate_analysis\\neg_ggrace_box.csv'
  input_pos_gg = 'D:\\Mining_project\\global\\Data\\new\\mutation_rate_analysis\\pos_ggrace_box.csv'

  neg_box_iei = read.csv(input_neg_iei,header = T)
  pos_box_iei = read.csv(input_pos_iei,header = T)
  neg_gg = read.csv(input_neg_gg,header = T)
  pos_gg = read.csv(input_pos_gg,header = T)
  
  neg_box_iei = as.data.frame(neg_box_iei[,-1])
  pos_box_iei = as.data.frame(pos_box_iei[,-1])
  neg_gg = neg_gg[,-1]
  pos_gg = pos_gg[,-1]
  # 
  neg_box_iei$gg = neg_gg
  pos_box_iei$gg = pos_gg
  
  
  con_name = c('NA','SA','NAF','SAF','AS','EU','OC')
  con_iei = paste0(con_name,'-IEI')
  con_gg = paste0(con_name,'-GGRACE')
  
  con_name2 = rep(con_name,each = 2)
  index1 = seq(1,14,2)
  index2 = seq(2,14,2)
  
  con_name2[index1] = paste0(con_name2[index1],'-IEI')
  con_name2[index2] = paste0(con_name2[index2],'-GGRACE')
  
  
  df = neg_box_iei[,1:3]
  df_m = melt(df,c('date','type'))
  df2 = neg_box_iei[,c(1,4,3)]
  df_m2 = melt(df2,c('date','type'))
  
  g1 = which(df_m$type == 1)
  g2 = which(df_m$type == 3)
  g3 = which(df_m$type == 5)
  g4 = which(df_m$type == 7)
  g5 = which(df_m$type == 9)
  g6 = which(df_m$type == 11)
  g7 = which(df_m$type == 13)
  
  df_m$con = con_iei[1]
  df_m$con[g2] = con_iei[2]
  df_m$con[g3] = con_iei[3]
  df_m$con[g4] = con_iei[4]
  df_m$con[g5] = con_iei[5]
  df_m$con[g6] = con_iei[6]
  df_m$con[g7] = con_iei[7]
  
  df_m2$con = con_gg[1]
  df_m2$con[g2] = con_gg[2]
  df_m2$con[g3] = con_gg[3]
  df_m2$con[g4] = con_gg[4]
  df_m2$con[g5] = con_gg[5]
  df_m2$con[g6] = con_gg[6]
  df_m2$con[g7] = con_gg[7]
  
  
  df_m = rbind(df_m,df_m2)
  
  df_m$con = factor(df_m$con,levels = rev(con_name2))
  df_m$cuts = cut(df_m$value, breaks = c(-1,seq(round(min(df_m$value),1),round(max(df_m$value),1),0.1),1))  
  
  
  
  library(viridis)
  len = length(unique(df_m$cuts))
  mycolors = colorRampPalette(brewer.pal(10,'Spectral'))(len)
  
  #theme_set
  #####
  fontsize = 12
  
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
  main_plot = ggplot()+
    geom_tile(data = df_m, aes(x = date, y = con,fill = cuts))+
    #scale_fill_viridis(option = 'C',discrete = T)+
    scale_fill_manual(values = mycolors)+
    legend_set+
    scale_y_discrete(position = 'right')+
    theme(
      panel.background = element_rect(fill = "transparent",color = 'transparent'),
      legend.position = 'none',
      axis.text = element_text(face='bold',colour='black',size=fontsize,hjust=.5),
      axis.text.x = element_text(face='bold',colour='black',size=fontsize-3,vjust=0.5,angle = 90),
      axis.title.x = element_text(face='bold',colour='black',size=fontsize,hjust=.5),
      axis.title.y = element_blank(),
      legend.direction = c('horizontal')
    )
  
  
  png('E:\\Desktop\\trial_mutation.png',
      height = 25,
      width = 25,
      units = 'cm',
      res = 1000)
  print(main_plot)
  dev.off()
  
  df1_1 = df
  df2_1 = df2
  
  
  #df1_1_m_origin = melt(df1_1,c('date','type'))
  #df2_1_m_origin = melt(df2_1,c('date','type'))
  # calculation of diff
  #####
  df1_1$IEI[g1][2:length(g1)] = df1_1$IEI[g1][2:length(g1)] - df1_1$IEI[g1][1:(length(g1)-1)]
  df1_1$IEI[g1][1] = 0
  df1_1$IEI[g2][2:length(g2)] = df1_1$IEI[g2][2:length(g2)] - df1_1$IEI[g2][1:(length(g2)-1)]
  df1_1$IEI[g2][1] = 0
  df1_1$IEI[g3][2:length(g3)] = df1_1$IEI[g3][2:length(g3)] - df1_1$IEI[g3][1:(length(g3)-1)]
  df1_1$IEI[g3][1] = 0
  df1_1$IEI[g4][2:length(g4)] = df1_1$IEI[g4][2:length(g4)] - df1_1$IEI[g4][1:(length(g4)-1)]
  df1_1$IEI[g4][1] = 0
  df1_1$IEI[g5][2:length(g5)] = df1_1$IEI[g5][2:length(g5)] - df1_1$IEI[g5][1:(length(g5)-1)]
  df1_1$IEI[g5][1] = 0
  df1_1$IEI[g6][2:length(g6)] = df1_1$IEI[g6][2:length(g6)] - df1_1$IEI[g6][1:(length(g6)-1)]
  df1_1$IEI[g6][1] = 0
  df1_1$IEI[g7][2:length(g7)] = df1_1$IEI[g7][2:length(g7)] - df1_1$IEI[g7][1:(length(g7)-1)]
  df1_1$IEI[g7][1] = 0
  
  df2_1$gg[g1][2:length(g1)] = df2_1$gg[g1][2:length(g1)] - df2_1$gg[g1][1:(length(g1)-1)]
  df2_1$gg[g1][1] = 0
  df2_1$gg[g2][2:length(g2)] = df2_1$gg[g2][2:length(g2)] - df2_1$gg[g2][1:(length(g2)-1)]
  df2_1$gg[g2][1] = 0
  df2_1$gg[g3][2:length(g3)] = df2_1$gg[g3][2:length(g3)] - df2_1$gg[g3][1:(length(g3)-1)]
  df2_1$gg[g3][1] = 0
  df2_1$gg[g4][2:length(g4)] = df2_1$gg[g4][2:length(g4)] - df2_1$gg[g4][1:(length(g4)-1)]
  df2_1$gg[g4][1] = 0
  df2_1$gg[g5][2:length(g5)] = df2_1$gg[g5][2:length(g5)] - df2_1$gg[g5][1:(length(g5)-1)]
  df2_1$gg[g5][1] = 0
  df2_1$gg[g6][2:length(g6)] = df2_1$gg[g6][2:length(g6)] - df2_1$gg[g6][1:(length(g6)-1)]
  df2_1$gg[g6][1] = 0
  df2_1$gg[g7][2:length(g7)] = df2_1$gg[g7][2:length(g7)] - df2_1$gg[g7][1:(length(g7)-1)]
  df2_1$gg[g7][1] = 0
  
  #####
  
  df1_1m = melt(df1_1,c('date','type'))
  df2_1m = melt(df2_1,c('date','type'))
  
  #df1_1m$value = df1_1m$value / df1_1_m_origin$value
  #df2_1m$value = df2_1m$value / df2_1_m_origin$value
  
  df1_1m$con = con_iei[1]
  df1_1m$con[g2] = con_iei[2]
  df1_1m$con[g3] = con_iei[3]
  df1_1m$con[g4] = con_iei[4]
  df1_1m$con[g5] = con_iei[5]
  df1_1m$con[g6] = con_iei[6]
  df1_1m$con[g7] = con_iei[7]
  
  df2_1m$con = con_gg[1]
  df2_1m$con[g2] = con_gg[2]
  df2_1m$con[g3] = con_gg[3]
  df2_1m$con[g4] = con_gg[4]
  df2_1m$con[g5] = con_gg[5]
  df2_1m$con[g6] = con_gg[6]
  df2_1m$con[g7] = con_gg[7]
  
  df1_1m$con_name = con_name[1]
  df1_1m$con_name[g2] = con_name[2]
  df1_1m$con_name[g3] = con_name[3]
  df1_1m$con_name[g4] = con_name[4]
  df1_1m$con_name[g5] = con_name[5]
  df1_1m$con_name[g6] = con_name[6]
  df1_1m$con_name[g7] = con_name[7]
  
  df2_1m$con_name = con_name[1]
  df2_1m$con_name[g2] = con_name[2]
  df2_1m$con_name[g3] = con_name[3]
  df2_1m$con_name[g4] = con_name[4]
  df2_1m$con_name[g5] = con_name[5]
  df2_1m$con_name[g6] = con_name[6]
  df2_1m$con_name[g7] = con_name[7]
  
  
  df_m_diff = rbind(df1_1m,df2_1m)
  df_m_diff$con = factor(df_m_diff$con,levels = rev(con_name2))
  df_m_diff$con_name = factor(df_m_diff$con_name, levels = rev(con_name))
  
  
  df_m_diff$diff_zone = 'Negative-IEI'
  pos_index  = which(df_m_diff$value >0 &df_m_diff$variable == 'IEI')
  df_m_diff$diff_zone[pos_index] = "Positive-IEI"
  
  pos_index2  = which(df_m_diff$value >0 &df_m_diff$variable == 'gg')
  df_m_diff$diff_zone[pos_index2] = "Positive-GGRACE"
  
  pos_index3  = which(df_m_diff$value <0 &df_m_diff$variable == 'gg')
  df_m_diff$diff_zone[pos_index3] = "Negative-GGRACE"
  
  
  df_m_diff$diff_zone = factor(df_m_diff$diff_zone, levels = c("Negative-IEI",
                                                               "Negative-GGRACE",
                                                               "Positive-IEI",
                                                               "Positive-GGRACE"))
  
  #label_df = data.frame(x = df_m_diff$date,y = df_m_diff$value,
  #                      label = df_m_diff$con_name)
  
  
  mycolor2 = colorRampPalette(brewer.pal(10,'Spectral'))(4)
  
  top_plot = ggplot()+
    geom_bar(data = df_m_diff,aes(x = date, y = round(value,2),
                                  fill = diff_zone),
             stat = 'identity',position = 'stack')+
    #geom_text(data = df_m_diff,aes(x = date,y = round(value,2), label = con_name),
    #                    size = 4,fontface = 'bold')+
    geom_hline(data = df_m_diff, aes(yintercept = 0),linetype = 1,color = 'black',size = 0.5)+
    geom_hline(data = df_m_diff, aes(yintercept = 0.5),linetype = 'dashed',color = 'black',size = 0.5)+
    geom_hline(data = df_m_diff, aes(yintercept = -0.5),linetype = 'dashed',color = 'black',size = 0.5)+
    geom_hline(data = df_m_diff, aes(yintercept = 1),linetype = 'dashed',color = 'black',size = 0.5)+
    geom_hline(data = df_m_diff, aes(yintercept = -1),linetype = 'dashed',color = 'black',size = 0.5)+
    geom_hline(data = df_m_diff, aes(yintercept = 2),linetype = 'dashed',color = 'black',size = 0.5)+
    geom_hline(data = df_m_diff, aes(yintercept = -2),linetype = 'dashed',color = 'black',size = 0.5)+
    scale_fill_manual(values = mycolor2)+
    legend_set+
    theme(
      panel.background = element_rect(fill = "transparent",color = 'transparent'),
      legend.position = 'none',
      axis.text = element_text(face='bold',colour='black',size=fontsize,hjust=.5),
      #axis.text.x = element_text(face='bold',colour='black',size=fontsize,vjust=0.5,angle = 90),
      axis.text.x = element_blank(),
      axis.title.y = element_text(face='bold',colour='black',size=fontsize,hjust=.5),
      axis.title.x = element_blank(),
      legend.direction = c('horizontal'),
      axis.line.y = element_line(color = 'black',size = 1)
    )+ylab('Amplitude of Variation')
  
  
  ### left plot
  
  df2arrow <- expand.grid(
    lineend = c('round', 'butt', 'square'),
    linejoin = c('round', 'mitre', 'bevel'),
    stringsAsFactors = FALSE
  )
  

  
  
  
  #clip part
  ######
  con_name5  = con_name
  con_high = paste0(con_name5,'-High')
  con_low = paste0(con_name5,'-Low')
  
  con_name4 = rep(con_name,each = 2)
  index41 = seq(1,14,2)
  index42 = seq(2,14,2)
  con_name4[index41] = paste0(con_name4[index41],'-High')
  con_name4[index42] = paste0(con_name4[index42],'-Low')
  
  
  high_low_tell <- function(x,temp_con_high,temp_con_low){
    x1 = x[1]
    hl_tell = 'no'
    for(i in 1:(length(x)-1)){
      if(x[i]<x[i+1]){
        hl_tell = c(hl_tell,temp_con_low)
      }else {
        hl_tell = c(hl_tell,temp_con_high)
      }
      
    }
    hl_tell = hl_tell[-1]
    
    if(x[length(x)]>x[length(x)-1]){
      hl_tell = c(hl_tell,temp_con_high)
    }else{
      hl_tell = c(hl_tell,temp_con_low)
    }
    return(hl_tell)
  }
  
  df3 =  df
  df4 = df2
  
  df3$value_zone = con_high[1]
  
  df3$value_zone[g1] = high_low_tell(df3$IEI[g1],con_high[1],con_low[1])
  df3$value_zone[g2] = high_low_tell(df3$IEI[g2],con_high[2],con_low[2])
  df3$value_zone[g3] = high_low_tell(df3$IEI[g3],con_high[3],con_low[3])
  df3$value_zone[g4] = high_low_tell(df3$IEI[g4],con_high[4],con_low[4])
  df3$value_zone[g5] = high_low_tell(df3$IEI[g5],con_high[5],con_low[5])
  df3$value_zone[g6] = high_low_tell(df3$IEI[g6],con_high[6],con_low[6])
  df3$value_zone[g7] = high_low_tell(df3$IEI[g7],con_high[7],con_low[7])
  
  df4$value_zone = con_high[1]
  
  df4$value_zone[g1] = high_low_tell(df4$gg[g1],con_high[1],con_low[1])
  df4$value_zone[g2] = high_low_tell(df4$gg[g2],con_high[2],con_low[2])
  df4$value_zone[g3] = high_low_tell(df4$gg[g3],con_high[3],con_low[3])
  df4$value_zone[g4] = high_low_tell(df4$gg[g4],con_high[4],con_low[4])
  df4$value_zone[g5] = high_low_tell(df4$gg[g5],con_high[5],con_low[5])
  df4$value_zone[g6] = high_low_tell(df4$gg[g6],con_high[6],con_low[6])
  df4$value_zone[g7] = high_low_tell(df4$gg[g7],con_high[7],con_low[7])
  
  
  df3_m = melt(df3,c('value_zone','date','type'))
  df4_m = melt(df4,c('value_zone','type','date'))
  
  df3_m$value_zone = factor(df3_m$value_zone,
                            levels = rev(con_name4))
  df4_m$value_zone = factor(df4_m$value_zone,
                            levels = rev(con_name4))
  
  df_m_left = rbind(df3_m,df4_m)
  df_m_left$variable = factor(df_m_left$variable,
                              labels = c('IEI','GGRACE'))
  ######
  
  df3 =  df
  df4 = df2
  
  g21 = g2 - g1[length(g1)]
  g31 = g3 - g2[length(g2)]
  g41 = g4 - g3[length(g3)]
  g51 = g5 - g4[length(g4)]
  g61 = g6 - g5[length(g5)]
  g71 = g7 - g6[length(g6)]
  
  gnew = rep(c(g1,g21,g31,g41,g51,g61,g71),2)
  
  
  con_name_class1 = paste0(con_name[1],'-',g1)
  con_name_class2 = paste0(con_name[2],'-',g21)
  con_name_class3 = paste0(con_name[3],'-',g31)
  con_name_class4 = paste0(con_name[4],'-',g41)
  con_name_class5 = paste0(con_name[5],'-',g51)
  con_name_class6 = paste0(con_name[6],'-',g61)
  con_name_class7 = paste0(con_name[7],'-',g71)
 
  
  df_m3 = melt(df3,c('date','type'))
  df_m4 = melt(df4,c('date','type'))

  df_m4$variable = factor(df_m4$variable,
                          labels = 'GGRACE')
  
  df_m34 = rbind(df_m3,df_m4)
  
  gnew_group = unique(letters[gnew])
  
  df_m34$con2 = letters[gnew]
  df_m34$con2 = factor(df_m34$con2,
                       levels = rev(gnew_group)) #x
  df_m34$value_zone = df_m_left$value_zone # y
  
  df_m34$cuts = cut(df_m34$value,breaks = c(-1,seq(round(min(df_m$value),1),round(max(df_m$value),1),0.1),1))
  
  pl_h_line1 = data.frame(
    xmin = df_m34$con2[-c(
                        c(g1[length(g1)],
                          g2[length(g2)],
                          g3[length(g3)],
                          g4[length(g4)],
                          g5[length(g5)],
                          g6[length(g6)],
                          g7[length(g7)]),
                        c(g1[length(g1)],
                          g2[length(g2)],
                          g3[length(g3)],
                          g4[length(g4)],
                          g5[length(g5)],
                          g6[length(g6)],
                          g7[length(g7)])+56)],
    xend = df_m34$con2[-c(c(g1[1],
                         g2[1],
                         g3[1],
                         g4[1],
                         g5[1],
                         g6[1],
                         g7[1]),
                         c(g1[1],
                           g2[1],
                           g3[1],
                           g4[1],
                           g5[1],
                           g6[1],
                           g7[1])+56)],
    ymin = df_m34$value_zone[-c(
      c(g1[length(g1)],
        g2[length(g2)],
        g3[length(g3)],
        g4[length(g4)],
        g5[length(g5)],
        g6[length(g6)],
        g7[length(g7)]),
      c(g1[length(g1)],
        g2[length(g2)],
        g3[length(g3)],
        g4[length(g4)],
        g5[length(g5)],
        g6[length(g6)],
        g7[length(g7)])+56)],
    yend = df_m34$value_zone[-c(c(g1[1],
                                     g2[1],
                                     g3[1],
                                     g4[1],
                                     g5[1],
                                     g6[1],
                                     g7[1]),
                                   c(g1[1],
                                     g2[1],
                                     g3[1],
                                     g4[1],
                                     g5[1],
                                     g6[1],
                                     g7[1])+56)],
    variable = df_m34$variable[-c(c(g1[1],
                                       g2[1],
                                       g3[1],
                                       g4[1],
                                       g5[1],
                                       g6[1],
                                       g7[1]),
                                     c(g1[1],
                                       g2[1],
                                       g3[1],
                                       g4[1],
                                       g5[1],
                                       g6[1],
                                       g7[1])+56)]
    
  )
  
  pl_text = data.frame(
     x = df_m34$value,
    y = df_m34$value_zone,
    text = as.character(c(g1,g2,g3,g4,g5,g6,g7))
  )
  
  
  pl_h_line1$tell = pl_h_line1$xmin - pl_h_line1$xend
  
  index_low = which(pl_h_line1$tell < 0 )
  index_high = which(pl_h_line1$tell > 0 )
  
  #pl_h_line1$y[index_neg] = paste0(pl_h_line1$y[index_neg],'-Neg')
  #pl_h_line1$y[index_pos] = paste0(pl_h_line1$y[index_pos],'-Pos')
  
  
  pl_h_line2 = pl_h_line1[index_low,]
  pl_h_line3 = pl_h_line1[index_high,]
  
  
  # 
  # con_name3 = paste0(rep(con_name2,each = 2),'-Neg')
  # index3 = seq(1,28,2)
  # index4 = seq(2,28,2)
  # con_name3[index4] = paste0(con_name3[index4],'-Pos')
  # 
  # sec_label = factor(unique(pl_h_line1$y),
  #                    levels = )
  
  pl_h_line_iei = pl_h_line1[which(pl_h_line1$variable =='IEI'),]
  pl_h_line_gg = pl_h_line1[which(pl_h_line1$variable =='GGRACE'),]
  
  #pl_h_line1$variable = factor(pl_h_line1$variable,levels = c('GGRACE','IEI'))
  
  left_plot = ggplot()+
    geom_point(data = df_m34, aes(x = con2, 
                                y = value_zone,
                                color = variable),shape = 1,size = 4)+
    geom_segment(data = pl_h_line1,aes(x = xmin,xend = xend,y = ymin,yend = yend,
                                       color = variable,
                                       linetype = variable),size = 1,
                 lineend = df2arrow$lineend[2],linejoin = df2arrow$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    #geom_segment(data = pl_h_line_gg,aes(x = xmin,xend = xend,y = ymin,yend = yend),
    #             color = 'blue',size = 1,
    #             lineend = df2arrow$lineend[2],linejoin = df2arrow$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    
    #geom_segment(data = pl_h_line1,aes(x = xmin,xend = xend,y = ymin,yend = yend),color = 'blue',size = 1,
    #             lineend = df2arrow$lineend[2],linejoin = df2arrow$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    #geom_text(data = pl_text,aes(x = x,y = y, label = text),
    #          size = 4,fontface = 'bold')+
    scale_y_discrete(position = 'right')+
    scale_x_discrete(position = 'top')+
    scale_fill_manual(values = mycolors)+
    scale_linetype_manual(values= c(1,2))+
    scale_color_manual(values = c('#8FBC8F','#1E90FF'))+
    legend_set+
    theme(
      panel.background = element_rect(fill = "transparent",color = 'transparent'),
      legend.position = 'none',
      axis.line.y = element_line(colour = 'black',size = 1),
      axis.text = element_text(face='bold',colour='black',size=fontsize,hjust=.5),
      axis.text.x = element_text(face='bold',colour='black',size=fontsize,vjust=0.5,angle = 0),
      axis.title = element_text(face='bold',colour='black',size=fontsize,hjust=.5),
      axis.title.y = element_blank(),
      legend.direction = c('horizontal')
    )+
    xlab('Abstrup Changing Point')
    
  
  ### plot arrange 
  library(cowplot)
  
  #extraction of legend
  top_legend = as_ggplot(get_legend(top_plot+theme(legend.position = 'bottom')))
  left_legend = as_ggplot(get_legend(left_plot+theme(legend.position = 'bottom')))
  main_legend = as_ggplot(get_legend(main_plot+theme(legend.position = 'bottom')))
  
  black_p = ggplot()+theme(panel.background = element_rect(fill = 'transparent',color = 'black'))
    #theme(plot.background = element_rect(colour = 'black',size = 1),
    #      plot.margin = unit(c(-1,0,-1,0),'cm'))
  
  p_final= plot_grid(top_plot,main_plot,
                     axis = c('lr'),
                     align = c('v'),rel_widths = c(4,4),
                     rel_heights = c(1,3),greedy = T,
                     ncol = 1)
  
  p2 = plot_grid(left_plot,p_final,
                 axis = c("bt"),align = 'h',rel_heights = c(1,1),greedy = F,
                 rel_widths = c(2,4))
  
  p3= plot_grid(p2,top_legend,main_legend,left_legend,ncol = 1,
                    align = 'v',axis = c('lr'),
                    rel_heights = c(10,1,1,1))
  output_png = 'D:\\Mining_project\\global\\plot\\mutation_analysis'
  dir.create(output_png)
  output_png1 = paste0(output_png,'\\mutation_analysis_neg.png')
  png(output_png1,
      height = 25,
      width = 30,
      units = 'cm',
      res = 1000)
  print(p3)
  dev.off()
  
  ### Pos
  df = pos_box_iei[,1:3]
  df_m = melt(df,c('date','type'))
  df2 = pos_box_iei[,c(1,4,3)]
  df_m2 = melt(df2,c('date','type'))
  
  g1 = which(df_m$type == 1)
  g2 = which(df_m$type == 3)
  g3 = which(df_m$type == 5)
  g4 = which(df_m$type == 7)
  g5 = which(df_m$type == 9)
  g6 = which(df_m$type == 11)
  g7 = which(df_m$type == 13)
  
  df_m$con = con_iei[1]
  df_m$con[g2] = con_iei[2]
  df_m$con[g3] = con_iei[3]
  df_m$con[g4] = con_iei[4]
  df_m$con[g5] = con_iei[5]
  df_m$con[g6] = con_iei[6]
  df_m$con[g7] = con_iei[7]
  
  df_m2$con = con_gg[1]
  df_m2$con[g2] = con_gg[2]
  df_m2$con[g3] = con_gg[3]
  df_m2$con[g4] = con_gg[4]
  df_m2$con[g5] = con_gg[5]
  df_m2$con[g6] = con_gg[6]
  df_m2$con[g7] = con_gg[7]
  
  
  df_m = rbind(df_m,df_m2)
  
  df_m$con = factor(df_m$con,levels = rev(con_name2))
  df_m$cuts = cut(df_m$value, breaks = c(-1,seq(round(min(df_m$value),1),round(max(df_m$value),1),0.1),1))  
  
  
  
  library(viridis)
  len = length(unique(df_m$cuts))
  mycolors = colorRampPalette(brewer.pal(10,'Spectral'))(len)
  
  #theme_set
  #####
  #theme_set_evolution<-theme_set_evolution_cor_map(12,
  #                                                 width = 3)
  fontsize = 12
  
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
  main_plot = ggplot()+
    geom_tile(data = df_m, aes(x = date, y = con,fill = cuts))+
    #scale_fill_viridis(option = 'C',discrete = T)+
    scale_fill_manual(values = mycolors)+
    legend_set+
    scale_y_discrete(position = 'right')+
    theme(
      panel.background = element_rect(fill = "transparent",color = 'transparent'),
      legend.position = 'none',
      axis.text = element_text(face='bold',colour='black',size=fontsize,hjust=.5),
      axis.text.x = element_text(face='bold',colour='black',size=fontsize-3,vjust=0.5,angle = 90),
      axis.title.x = element_text(face='bold',colour='black',size=fontsize,hjust=.5),
      axis.title.y = element_blank(),
      legend.direction = c('horizontal')
    )
  
  
  png('E:\\Desktop\\trial_mutation.png',
      height = 25,
      width = 25,
      units = 'cm',
      res = 1000)
  print(main_plot)
  dev.off()
  
  df1_1 = df
  df2_1 = df2
  
  
  #df1_1_m_origin = melt(df1_1,c('date','type'))
  #df2_1_m_origin = melt(df2_1,c('date','type'))
  # calculation of diff
  #####
  df1_1$IEI[g1][2:length(g1)] = df1_1$IEI[g1][2:length(g1)] - df1_1$IEI[g1][1:(length(g1)-1)]
  df1_1$IEI[g1][1] = 0
  df1_1$IEI[g2][2:length(g2)] = df1_1$IEI[g2][2:length(g2)] - df1_1$IEI[g2][1:(length(g2)-1)]
  df1_1$IEI[g2][1] = 0
  df1_1$IEI[g3][2:length(g3)] = df1_1$IEI[g3][2:length(g3)] - df1_1$IEI[g3][1:(length(g3)-1)]
  df1_1$IEI[g3][1] = 0
  df1_1$IEI[g4][2:length(g4)] = df1_1$IEI[g4][2:length(g4)] - df1_1$IEI[g4][1:(length(g4)-1)]
  df1_1$IEI[g4][1] = 0
  df1_1$IEI[g5][2:length(g5)] = df1_1$IEI[g5][2:length(g5)] - df1_1$IEI[g5][1:(length(g5)-1)]
  df1_1$IEI[g5][1] = 0
  df1_1$IEI[g6][2:length(g6)] = df1_1$IEI[g6][2:length(g6)] - df1_1$IEI[g6][1:(length(g6)-1)]
  df1_1$IEI[g6][1] = 0
  df1_1$IEI[g7][2:length(g7)] = df1_1$IEI[g7][2:length(g7)] - df1_1$IEI[g7][1:(length(g7)-1)]
  df1_1$IEI[g7][1] = 0
  
  df2_1$gg[g1][2:length(g1)] = df2_1$gg[g1][2:length(g1)] - df2_1$gg[g1][1:(length(g1)-1)]
  df2_1$gg[g1][1] = 0
  df2_1$gg[g2][2:length(g2)] = df2_1$gg[g2][2:length(g2)] - df2_1$gg[g2][1:(length(g2)-1)]
  df2_1$gg[g2][1] = 0
  df2_1$gg[g3][2:length(g3)] = df2_1$gg[g3][2:length(g3)] - df2_1$gg[g3][1:(length(g3)-1)]
  df2_1$gg[g3][1] = 0
  df2_1$gg[g4][2:length(g4)] = df2_1$gg[g4][2:length(g4)] - df2_1$gg[g4][1:(length(g4)-1)]
  df2_1$gg[g4][1] = 0
  df2_1$gg[g5][2:length(g5)] = df2_1$gg[g5][2:length(g5)] - df2_1$gg[g5][1:(length(g5)-1)]
  df2_1$gg[g5][1] = 0
  df2_1$gg[g6][2:length(g6)] = df2_1$gg[g6][2:length(g6)] - df2_1$gg[g6][1:(length(g6)-1)]
  df2_1$gg[g6][1] = 0
  df2_1$gg[g7][2:length(g7)] = df2_1$gg[g7][2:length(g7)] - df2_1$gg[g7][1:(length(g7)-1)]
  df2_1$gg[g7][1] = 0
  
  #####
  
  df1_1m = melt(df1_1,c('date','type'))
  df2_1m = melt(df2_1,c('date','type'))
  
  #df1_1m$value = df1_1m$value / df1_1_m_origin$value
  #df2_1m$value = df2_1m$value / df2_1_m_origin$value
  
  df1_1m$con = con_iei[1]
  df1_1m$con[g2] = con_iei[2]
  df1_1m$con[g3] = con_iei[3]
  df1_1m$con[g4] = con_iei[4]
  df1_1m$con[g5] = con_iei[5]
  df1_1m$con[g6] = con_iei[6]
  df1_1m$con[g7] = con_iei[7]
  
  df2_1m$con = con_gg[1]
  df2_1m$con[g2] = con_gg[2]
  df2_1m$con[g3] = con_gg[3]
  df2_1m$con[g4] = con_gg[4]
  df2_1m$con[g5] = con_gg[5]
  df2_1m$con[g6] = con_gg[6]
  df2_1m$con[g7] = con_gg[7]
  
  df1_1m$con_name = con_name[1]
  df1_1m$con_name[g2] = con_name[2]
  df1_1m$con_name[g3] = con_name[3]
  df1_1m$con_name[g4] = con_name[4]
  df1_1m$con_name[g5] = con_name[5]
  df1_1m$con_name[g6] = con_name[6]
  df1_1m$con_name[g7] = con_name[7]
  
  df2_1m$con_name = con_name[1]
  df2_1m$con_name[g2] = con_name[2]
  df2_1m$con_name[g3] = con_name[3]
  df2_1m$con_name[g4] = con_name[4]
  df2_1m$con_name[g5] = con_name[5]
  df2_1m$con_name[g6] = con_name[6]
  df2_1m$con_name[g7] = con_name[7]
  
  
  df_m_diff = rbind(df1_1m,df2_1m)
  df_m_diff$con = factor(df_m_diff$con,levels = rev(con_name2))
  df_m_diff$con_name = factor(df_m_diff$con_name, levels = rev(con_name))
  
  
  df_m_diff$diff_zone = 'Negative-IEI'
  pos_index  = which(df_m_diff$value >0 &df_m_diff$variable == 'IEI')
  df_m_diff$diff_zone[pos_index] = "Positive-IEI"
  
  pos_index2  = which(df_m_diff$value >0 &df_m_diff$variable == 'gg')
  df_m_diff$diff_zone[pos_index2] = "Positive-GGRACE"
  
  pos_index3  = which(df_m_diff$value <0 &df_m_diff$variable == 'gg')
  df_m_diff$diff_zone[pos_index3] = "Negative-GGRACE"
  
  
  df_m_diff$diff_zone = factor(df_m_diff$diff_zone, levels = c("Negative-IEI",
                                                               "Negative-GGRACE",
                                                               "Positive-IEI",
                                                               "Positive-GGRACE"))
  
  #label_df = data.frame(x = df_m_diff$date,y = df_m_diff$value,
  #                      label = df_m_diff$con_name)
  
  
  mycolor2 = colorRampPalette(brewer.pal(10,'Spectral'))(4)
  
  top_plot = ggplot()+
    geom_bar(data = df_m_diff,aes(x = date, y = round(value,2),
                                  fill = diff_zone),
             stat = 'identity',position = 'stack')+
    #geom_text(data = df_m_diff,aes(x = date,y = round(value,2), label = con_name),
    #                    size = 4,fontface = 'bold')+
    geom_hline(data = df_m_diff, aes(yintercept = 0),linetype = 1,color = 'black',size = 0.5)+
    geom_hline(data = df_m_diff, aes(yintercept = 0.5),linetype = 'dashed',color = 'black',size = 0.5)+
    geom_hline(data = df_m_diff, aes(yintercept = -0.5),linetype = 'dashed',color = 'black',size = 0.5)+
    geom_hline(data = df_m_diff, aes(yintercept = 1),linetype = 'dashed',color = 'black',size = 0.5)+
    geom_hline(data = df_m_diff, aes(yintercept = -1),linetype = 'dashed',color = 'black',size = 0.5)+
    geom_hline(data = df_m_diff, aes(yintercept = 2),linetype = 'dashed',color = 'black',size = 0.5)+
    geom_hline(data = df_m_diff, aes(yintercept = -2),linetype = 'dashed',color = 'black',size = 0.5)+
    scale_fill_manual(values = mycolor2)+
    legend_set+
    theme(
      panel.background = element_rect(fill = "transparent",color = 'transparent'),
      legend.position = 'none',
      axis.text = element_text(face='bold',colour='black',size=fontsize,hjust=.5),
      #axis.text.x = element_text(face='bold',colour='black',size=fontsize,vjust=0.5,angle = 90),
      axis.text.x = element_blank(),
      axis.title.y = element_text(face='bold',colour='black',size=fontsize,hjust=.5),
      axis.title.x = element_blank(),
      legend.direction = c('horizontal'),
      axis.line.y = element_line(color = 'black',size = 1)
    )+ylab('Amplitude of Variation')
  
  
  ### left plot
  
  df2arrow <- expand.grid(
    lineend = c('round', 'butt', 'square'),
    linejoin = c('round', 'mitre', 'bevel'),
    stringsAsFactors = FALSE
  )
  
  
  
  
  
  #clip part
  ######
  con_name5  = con_name
  con_high = paste0(con_name5,'-High')
  con_low = paste0(con_name5,'-Low')
  
  con_name4 = rep(con_name,each = 2)
  index41 = seq(1,14,2)
  index42 = seq(2,14,2)
  con_name4[index41] = paste0(con_name4[index41],'-High')
  con_name4[index42] = paste0(con_name4[index42],'-Low')
  
  
  high_low_tell <- function(x,temp_con_high,temp_con_low){
    x1 = x[1]
    hl_tell = 'no'
    for(i in 1:(length(x)-1)){
      if(x[i]<x[i+1]){
        hl_tell = c(hl_tell,temp_con_low)
      }else {
        hl_tell = c(hl_tell,temp_con_high)
      }
      
    }
    hl_tell = hl_tell[-1]
    
    if(x[length(x)]>x[length(x)-1]){
      hl_tell = c(hl_tell,temp_con_high)
    }else{
      hl_tell = c(hl_tell,temp_con_low)
    }
    return(hl_tell)
  }
  
  df3 =  df
  df4 = df2
  
  df3$value_zone = con_high[1]
  
  df3$value_zone[g1] = high_low_tell(df3$IEI[g1],con_high[1],con_low[1])
  df3$value_zone[g2] = high_low_tell(df3$IEI[g2],con_high[2],con_low[2])
  df3$value_zone[g3] = high_low_tell(df3$IEI[g3],con_high[3],con_low[3])
  df3$value_zone[g4] = high_low_tell(df3$IEI[g4],con_high[4],con_low[4])
  df3$value_zone[g5] = high_low_tell(df3$IEI[g5],con_high[5],con_low[5])
  df3$value_zone[g6] = high_low_tell(df3$IEI[g6],con_high[6],con_low[6])
  df3$value_zone[g7] = high_low_tell(df3$IEI[g7],con_high[7],con_low[7])
  
  df4$value_zone = con_high[1]
  
  df4$value_zone[g1] = high_low_tell(df4$gg[g1],con_high[1],con_low[1])
  df4$value_zone[g2] = high_low_tell(df4$gg[g2],con_high[2],con_low[2])
  df4$value_zone[g3] = high_low_tell(df4$gg[g3],con_high[3],con_low[3])
  df4$value_zone[g4] = high_low_tell(df4$gg[g4],con_high[4],con_low[4])
  df4$value_zone[g5] = high_low_tell(df4$gg[g5],con_high[5],con_low[5])
  df4$value_zone[g6] = high_low_tell(df4$gg[g6],con_high[6],con_low[6])
  df4$value_zone[g7] = high_low_tell(df4$gg[g7],con_high[7],con_low[7])
  
  
  df3_m = melt(df3,c('value_zone','date','type'))
  df4_m = melt(df4,c('value_zone','type','date'))
  
  df3_m$value_zone = factor(df3_m$value_zone,
                            levels = rev(con_name4))
  df4_m$value_zone = factor(df4_m$value_zone,
                            levels = rev(con_name4))
  
  df_m_left = rbind(df3_m,df4_m)
  df_m_left$variable = factor(df_m_left$variable,
                              labels = c('IEI','GGRACE'))
  ######
  
  df3 =  df
  df4 = df2
  
  g21 = g2 - g1[length(g1)]
  g31 = g3 - g2[length(g2)]
  g41 = g4 - g3[length(g3)]
  g51 = g5 - g4[length(g4)]
  g61 = g6 - g5[length(g5)]
  g71 = g7 - g6[length(g6)]
  
  gnew = rep(c(g1,g21,g31,g41,g51,g61,g71),2)
  
  
  con_name_class1 = paste0(con_name[1],'-',g1)
  con_name_class2 = paste0(con_name[2],'-',g21)
  con_name_class3 = paste0(con_name[3],'-',g31)
  con_name_class4 = paste0(con_name[4],'-',g41)
  con_name_class5 = paste0(con_name[5],'-',g51)
  con_name_class6 = paste0(con_name[6],'-',g61)
  con_name_class7 = paste0(con_name[7],'-',g71)
  
  df_m3 = melt(df3,c('date','type'))
  df_m4 = melt(df4,c('date','type'))
  
  df_m4$variable = factor(df_m4$variable,
                          labels = 'GGRACE')
  
  df_m34 = rbind(df_m3,df_m4)
  
  gnew_group = unique(letters[gnew])
  
  df_m34$con2 = letters[gnew]
  df_m34$con2 = factor(df_m34$con2,
                       levels = rev(gnew_group)) #x
  df_m34$value_zone = df_m_left$value_zone # y
  
  df_m34$cuts = cut(df_m34$value,breaks = c(-1,seq(round(min(df_m$value),1),round(max(df_m$value),1),0.1),1))
  
  pl_h_line1 = data.frame(
    xmin = df_m34$con2[-c(
      c(g1[length(g1)],
        g2[length(g2)],
        g3[length(g3)],
        g4[length(g4)],
        g5[length(g5)],
        g6[length(g6)],
        g7[length(g7)]),
      c(g1[length(g1)],
        g2[length(g2)],
        g3[length(g3)],
        g4[length(g4)],
        g5[length(g5)],
        g6[length(g6)],
        g7[length(g7)])+54)],
    xend = df_m34$con2[-c(c(g1[1],
                            g2[1],
                            g3[1],
                            g4[1],
                            g5[1],
                            g6[1],
                            g7[1]),
                          c(g1[1],
                            g2[1],
                            g3[1],
                            g4[1],
                            g5[1],
                            g6[1],
                            g7[1])+54)],
    ymin = df_m34$value_zone[-c(
      c(g1[length(g1)],
        g2[length(g2)],
        g3[length(g3)],
        g4[length(g4)],
        g5[length(g5)],
        g6[length(g6)],
        g7[length(g7)]),
      c(g1[length(g1)],
        g2[length(g2)],
        g3[length(g3)],
        g4[length(g4)],
        g5[length(g5)],
        g6[length(g6)],
        g7[length(g7)])+54)],
    yend = df_m34$value_zone[-c(c(g1[1],
                                  g2[1],
                                  g3[1],
                                  g4[1],
                                  g5[1],
                                  g6[1],
                                  g7[1]),
                                c(g1[1],
                                  g2[1],
                                  g3[1],
                                  g4[1],
                                  g5[1],
                                  g6[1],
                                  g7[1])+54)],
    variable = df_m34$variable[-c(c(g1[1],
                                    g2[1],
                                    g3[1],
                                    g4[1],
                                    g5[1],
                                    g6[1],
                                    g7[1]),
                                  c(g1[1],
                                    g2[1],
                                    g3[1],
                                    g4[1],
                                    g5[1],
                                    g6[1],
                                    g7[1])+54)]
    
  )
  
  pl_text = data.frame(
    x = df_m34$value,
    y = df_m34$value_zone,
    text = as.character(c(g1,g2,g3,g4,g5,g6,g7))
  )
  
  
  pl_h_line1$tell = pl_h_line1$xmin - pl_h_line1$xend
  
  index_low = which(pl_h_line1$tell < 0 )
  index_high = which(pl_h_line1$tell > 0 )
  
  #pl_h_line1$y[index_neg] = paste0(pl_h_line1$y[index_neg],'-Neg')
  #pl_h_line1$y[index_pos] = paste0(pl_h_line1$y[index_pos],'-Pos')
  
  
  pl_h_line2 = pl_h_line1[index_low,]
  pl_h_line3 = pl_h_line1[index_high,]
  
  
  # 
  # con_name3 = paste0(rep(con_name2,each = 2),'-Neg')
  # index3 = seq(1,28,2)
  # index4 = seq(2,28,2)
  # con_name3[index4] = paste0(con_name3[index4],'-Pos')
  # 
  # sec_label = factor(unique(pl_h_line1$y),
  #                    levels = )
  
  pl_h_line_iei = pl_h_line1[which(pl_h_line1$variable =='IEI'),]
  pl_h_line_gg = pl_h_line1[which(pl_h_line1$variable =='GGRACE'),]
  
  #pl_h_line1$variable = factor(pl_h_line1$variable,levels = c('GGRACE','IEI'))
  #df_m34$variable = factor(df_m34$variable,levels = c('GGRACE','IEI'))
  left_plot = ggplot()+
    geom_point(data = df_m34, aes(x = con2, 
                                  y = value_zone,
                                  color = variable),shape = 1,size = 4)+
    geom_segment(data = pl_h_line1,aes(x = xmin,xend = xend,y = ymin,yend = yend,
                                       color = variable,
                                       linetype = variable),size = 1,
                 lineend = df2arrow$lineend[2],linejoin = df2arrow$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    #geom_segment(data = pl_h_line_gg,aes(x = xmin,xend = xend,y = ymin,yend = yend),
    #             color = 'blue',size = 1,
    #             lineend = df2arrow$lineend[2],linejoin = df2arrow$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    
    #geom_segment(data = pl_h_line1,aes(x = xmin,xend = xend,y = ymin,yend = yend),color = 'blue',size = 1,
    #             lineend = df2arrow$lineend[2],linejoin = df2arrow$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    #geom_text(data = pl_text,aes(x = x,y = y, label = text),
    #          size = 4,fontface = 'bold')+
    scale_color_manual(values = c('#8FBC8F','#1E90FF'))+
    scale_y_discrete(position = 'right')+
    scale_x_discrete(position = 'top')+
    scale_linetype_manual(values = c(1,2))+
    #scale_fill_manual(values = mycolors)+
    legend_set+
    theme(
      panel.background = element_rect(fill = "transparent",color = 'transparent'),
      legend.position = 'none',
      axis.line.y = element_line(colour = 'black',size = 1),
      axis.text = element_text(face='bold',colour='black',size=fontsize,hjust=.5),
      axis.text.x = element_text(face='bold',colour='black',size=fontsize,vjust=0.5,angle = 0),
      axis.title = element_text(face='bold',colour='black',size=fontsize,hjust=.5),
      axis.title.y = element_blank(),
      legend.direction = c('horizontal')
    )+
    xlab('Abstrup Changing Point')
  
  
  ### plot arrange 
  library(cowplot)
  library(ggpubr)
  #extraction of legend
  top_legend = as_ggplot(get_legend(top_plot+theme(legend.position = 'bottom')))
  left_legend = as_ggplot(get_legend(left_plot+theme(legend.position = 'bottom')))
  main_legend = as_ggplot(get_legend(main_plot+theme(legend.position = 'bottom')))
  
  black_p = ggplot()+theme(panel.background = element_rect(fill = 'transparent',color = 'black'))
  #theme(plot.background = element_rect(colour = 'black',size = 1),
  #      plot.margin = unit(c(-1,0,-1,0),'cm'))
  
  p_final= plot_grid(top_plot,main_plot,
                     axis = c('lr'),
                     align = c('v'),rel_widths = c(4,4),
                     rel_heights = c(1,3),greedy = T,
                     ncol = 1)
  
  p2 = plot_grid(left_plot,p_final,
                 axis = c("bt"),align = 'h',rel_heights = c(1,1),greedy = F,
                 rel_widths = c(2,4))
  
  p3= plot_grid(p2,top_legend,main_legend,left_legend,ncol = 1,
                align = 'v',axis = c('lr'),
                rel_heights = c(10,1,1,1))
  output_png = 'D:\\Mining_project\\global\\plot\\mutation_analysis'
  dir.create(output_png)
  output_png1 = paste0(output_png,'\\mutation_analysis_pos.png')
  png(output_png1,
      height = 25,
      width = 30,
      units = 'cm',
      res = 1000)
  print(p3)
  dev.off()
  
  
  
}




























