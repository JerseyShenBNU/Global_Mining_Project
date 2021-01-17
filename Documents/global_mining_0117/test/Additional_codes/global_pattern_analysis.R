global_pattern_analysis<-function(
  
){
  
  
  # cor analysis
  source('D:/Mining_project/global/R/cor_analysis_global.R')
  
  ret_box = cor_analysis_global()
  #return(ret_box)
  library(reshape2)
  
  ret_m = melt(ret_box,c('long','lat'))
  ret_m$long = ret_m$long - 180
  
  index_neg = which(ret_m$value <=0)
  index_pos = which(ret_m$value >0)
  
  ret_pos = ret_m[index_pos,]
  ret_neg = ret_m[index_neg,]
  
  ret_pos$cuts = cut(ret_pos$value, breaks = c(0,0.25,0.50,0.75,1.00))

  ret_neg$cuts = cut(ret_neg$value, breaks = c(-1.00,-0.75,-0.5,-0.25,0))
  
  shape_neg = rep(25,length(index_neg))
  shape_pos = rep(24,length(index_pos))
  
  ret_pos$shape = shape_pos
  ret_neg$shape = shape_neg
  
  
  ret_box = rbind(ret_neg,ret_pos)
  ret_box$type = c(rep(2,nrow(ret_neg)),rep(3,nrow(ret_pos)))
  
  ret_box2 = ret_box
  ret_box2$type = 1
  
  ret2_size = 1
  for(i in 1:nrow(ret_box2)){
    temp = abs(ret_box2$value[i])
    if(temp <= 0.25 & temp >0){
      ret2_size = c(ret2_size,10)
    }else if(temp <= 0.5 & temp >0.25){
      ret2_size = c(ret2_size,4)
    }else if(temp <= 0.75 & temp >0.50){
      ret2_size = c(ret2_size,6)
    }else if(temp <= 1 & temp >0.75){
      ret2_size = c(ret2_size,8)
    }
  }
  ret2_size = ret2_size[-1]
  ret_box2$size = ret2_size
  
  neg_index = which(ret_box2$value <0)
  pos_index = which(ret_box2$value >0)
  
  ret_box2$fill = 'Positive Relation Mines'
  ret_box2$fill[neg_index] = 'Negative Relation Mines'
  
  ret_box_size = 1
  for(i in 1:nrow(ret_box)){
    temp = abs(ret_box$value)
    if(temp <= 0.25){
      ret_box_size = c(ret_box_size,1)
      
    }else if(temp <= 0.5){
      ret_box_size = c(ret_box_size,2)
    }else if(temp <= 0.75){
      ret_box_size = c(ret_box_size,3)
    }else if(temp <= 1){
      ret_box_size = c(ret_box_size,4)
    }
  }
  ret_box_size = ret_box_size[-1]
  
  ret_box$size = ret_box_size
  

  #return(ret_box2)  
  
  a = raster('D:\\Mining_project\\global\\Data\\new\\climate_zone\\climate.tif')
  a_df = as.data.frame(a,xy = T)  
  a_df_m = melt(a_df,c('x','y'))
  
  index = which(a_df_m$value == 8 |
                  a_df_m$value == 7 |
                  a_df_m$value == 6 |
                  a_df_m$value == 5)
  
  a_df_m = a_df_m[index,]
  #a_df_m[,1] = a_df_m[,1] - 180
  a_df_m$cuts = cut(a_df_m$value,breaks = c(4,5,6,7,8))
  
  
  # map attributes
  ########
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
  #color_map = c('#ACCD64','#F9F477','#E8010C','#FCD287')
  
  color_map = c('#013F87','#2A4FA0','#177ABD','#6CBDE3',
                '#FE2725','#FE5B34','#FEB666','#E8002D')
  color_map = rep(color_map,2)
  point_color = c('blue')
  ########
  
  
  world = shapefile('D:\\Mining_project\\global\\Data\\new\\climate_zone\\world_continent2.shp')
  #world_df = as.data.frame(world,xy = T)
  #world_df[,1] = world_df[,1] + 180
  world = crop(world, extent(-180,180,-60,90))
  
  prov_map = fortify(world,region = "Region2")
  prov_map2 = rbind(prov_map,prov_map)
  prov_map2$type = rep(c(2,3),each = nrow(prov_map))
  
  df2 <- expand.grid(
    lineend = c('round', 'butt', 'square'),
    linejoin = c('round', 'mitre', 'bevel'),
    stringsAsFactors = FALSE
  )
  source('E:/Desktop/global_mining_extended/global_growth_analysis_mine.R')
  
  df_growth_box = global_growth_analysis_mine()
  df_grace = df_growth_box[[1]]
  df_iei = df_growth_box[[2]]
  df_grace2 = df_growth_box[[3]]
  df_iei2 = df_growth_box[[4]]
  
  fills_manual = c('Negative Relation Mines'='#177ABD',
                   "Positive Relation Mines"='#FE5B34',
                   "Negative Growth Mines"='#3C5488FF',
                   "Positive Growth Mines"='#E64B35FF',
                   "Negative Growth Mines"='#3C5488FF',
                   "Positive Growth Mines"='#E64B35FF')
  
  sizes_manual = c('(-1,-0.75]'= 8,
                   '(-0.75,-0.5]' = 6,
                   '(-0.5,-0.25]' = 4,
                   '(-0.25,0]' = 2,
                   '(0,0.25]' = 2,
                   '(0.25,0.5]' = 4,
                   '(0.5,0.75]' = 6,
                   '(0.75,1]' = 8,
                   '(-75,-50]' = 6,
                   '(-50,-25]'= 4,
                   '(-25,0]' = 2,
                   '(0,25]' = 2,
                   '(25,50]'=4,
                   '(50,75]' = 6)
  
  color_manual = c('(-1,-0.75]'= '#013F87',
                   '(-0.75,-0.5]' = '#2A4FA0',
                   '(-0.5,-0.25]' = '#177ABD',
                   '(-0.25,0]' = '#6CBDE3',
                   '(0,0.25]' = '#FE2725',
                   '(0.25,0.5]' = '#FE5B34',
                   '(0.5,0.75]' = '#FEB666',
                   '(0.75,1]' = '#E8002D',
                   '(-75,-50]' = '#2A4FA0',
                   '(-50,-25]'= '#177ABD',
                   '(-25,0]' ='#6CBDE3',
                   '(0,25]' = '#FE2725',
                   '(25,50]'='#FE5B34',
                   '(50,75]' = '#FEB666')
  shape_manual = c('(-1,-0.75]'= 16,
                   '(-0.75,-0.5]' = 16,
                   '(-0.5,-0.25]' = 16,
                   '(-0.25,0]' = 16,
                   '(0,0.25]' = 16,
                   '(0.25,0.5]' =16,
                   '(0.5,0.75]' = 16,
                   '(0.75,1]' = 16,
                   '(-75,-50]' = 25,
                   '(-50,-25]'= 25,
                   '(-25,0]' =25,
                   '(0,25]' = 24,
                   '(25,50]'=24,
                   '(50,75]' = 24)
  ret_box3 = ret_box[,-c(6,8)]
  point_box = rbind(ret_box3,df_grace,df_iei)
  label_df1 = data.frame(
    x = df_grace2$x,
    y = df_grace2$value*1.2,
    label = as.character(abs(df_grace2$value)),
    type = df_grace2$type
  )
  label_df1$y[3] = label_df1$y[3]-10
  label_df1$y[9] = label_df1$y[9]+10
  label_df2 = data.frame(
    x = df_iei2$x,
    y = df_iei2$value*1.2,
    label = as.character(abs(df_iei2$value)),
    type = df_iei2$type
  )
  label_df2$y[3] = label_df2$y[3]-10
  label_df2$y[9] = label_df2$y[9]+10
  
  
  df_grace2$fill[which(df_grace2$fill=='Negative Growth Number')] = 'Negative Growth Mines'
  df_grace2$fill[which(df_grace2$fill=='Positive Growth Number')] = 'Positive Growth Mines'
  df_iei2$fill[which(df_iei2$fill=='Negative Growth Number')] = 'Negative Growth Mines'
  df_iei2$fill[which(df_iei2$fill=='Positive Growth Number')] = 'Positive Growth Mines'
  
  
  a = ggplot()+
    geom_histogram(data = ret_box2, aes(x = value,fill = fill),binwidth = 0.25,center = 0.125,color = 'white')+
    #geom_histogram(data = df_grace2, aes(x = value,fill = fill),binwidth = 25,center = 12.5,color = 'white')+
    #geom_histogram(data = df_iei2, aes(x = value,fill = fill),binwidth = 25,center = 12.5,color = 'white')+
    
    #geom_bar(data = df_grace2,aes(x = x, y= value,fill =fill),position = 'stack',stat = 'identity')+
    #geom_bar(data = df_iei2,aes(x =  x,y = value,fill= fill),position = 'stack',stat = 'identity')+
    
    geom_vline(data = ret_box2, aes(xintercept = 0),linetype = 'dashed',color = 'black',size = 1)+
    #geom_vline(data = df_grace2, aes(xintercept = 0),linetype = 'dashed',color = 'black',size = 1)+
    #geom_vline(data = df_iei2, aes(xintercept = 0),linetype = 'dashed',color = 'black',size = 1)+
    
    geom_segment(data = ret_box2,aes(x = -1, xend = 0, y = 280,yend = 280),color = 'black',size = 1,
                 lineend = df2$lineend[2],linejoin = df2$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    geom_segment(data = ret_box2,aes(x = 0, xend = 1, y = 280,yend = 280),color = 'black',size = 1,
                 lineend = df2$lineend[2],linejoin = df2$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    geom_segment(data = ret_box2,aes(x = 0, xend = -1, y = 280,yend = 280),color = 'black',size = 1,
                 lineend = df2$lineend[2],linejoin = df2$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    geom_segment(data = ret_box2,aes(x = 1, xend = 0, y = 280,yend = 280),color = 'black',size = 1,
                 lineend = df2$lineend[2],linejoin = df2$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    geom_segment(data = ret_box2,aes(x = -1, xend = -1, y = 270,yend = 300),color = 'black',size = 1)+
    geom_segment(data = ret_box2,aes(x = 1, xend = 1, y = 270,yend = 300),color = 'black',size = 1)+
    geom_text(data = ret_box2,aes(x = -0.5, y = 295, label = 'Negative: 622 mines'),size = 4)+
    geom_text(data = ret_box2,aes(x = 0.5, y = 295, label = 'Positive: 112 mines'),size = 4)+
    #scale_fill_manual(values = fill)+
    geom_polygon(data=prov_map2,aes(x=long, y= lat,group = group),
                 colour='black',fill = '#CFCFCF',size = 0.5,alpha = 0.5
                 #'#CFCFCF'
    )+
    
    #coord_flip()+
    geom_point(data = ret_box,aes(x = long, y = lat,color = cuts,size = cuts,shape = cuts), alpha = 0.5)+
    #geom_point(data = df_grace,aes(x = long, y = lat,color = cuts,size = cuts,shape= cuts), alpha = 0.5)+
    #geom_point(data = df_iei,aes(x = long, y = lat,color = cuts,size = cuts,shape = cuts), alpha = 0.5)+
    #geom_point(data = point_box,aes(x = long, y = lat,color = cuts,
    #                                size = cuts,shape = cuts), alpha = 0.5)+
    #geom_text(data = label_df1,aes(x = x,y = y,label = label),color = 'black',size = 4)+
    #geom_text(data = label_df2,aes(x = x,y = y,label = label),color = 'black',size = 4)+
    scale_color_manual(values = color_manual)+
    scale_size_manual(values = sizes_manual)+
    scale_shape_manual(values = shape_manual)+
    scale_fill_manual(values = fills_manual)+
    facet_wrap(~type, nrow = 3,scales = 'free')+
    theme_set_evolution+legend_set+
    guides(fill = guide_legend(nrow = 1),
           color = guide_legend(nrow = 2))+
    xlab('Longitude')+
    ylab('Latitude')
    
    #scale_fill_manual(values = color_map,labels = c('Hot-Arid-Steppe','Cold-Arid-Steppe','Hot-Arid-Desrt','Cold-Arid-Desert'))
  
  png('D:\\Mining_project\\global\\plot\\pattern\\pattern6.png',
      width = 20,
      height = 26,
      units = 'cm',
      res = 1000)
  print(a)
    
  dev.off()  
    
    
  return(ret_box2)
    
    
    
  
  
  
  
  
  
  
  
  
  
  
}















