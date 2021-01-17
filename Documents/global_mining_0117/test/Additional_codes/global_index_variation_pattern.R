global_index_variation_pattern <- function(
  
){
  library(raster)
  library(ncdf4)
  library(ggplot2)
  library(ggnewscale)
  library(RColorBrewer)
  library(scales)
  library(reshape2)
  source('E:/Desktop/global_mining_extended/global_growth_analysis_mine.R')
  
  df_growth_box = global_growth_analysis_mine()
  df_grace = df_growth_box[[1]]
  df_iei = df_growth_box[[2]]
  df_grace2 = df_growth_box[[3]]
  df_iei2 = df_growth_box[[4]]
  
  df_grace$type = 2
  df_iei$type = 3
  
  point_box = rbind(df_grace,df_iei)
  
  # import line box
  
  line_box = read.csv('output_analysis/global_mean_index.csv',
                      header = T)
  line_box = line_box[,-1]
  
  line_box = as.data.frame(line_box)
  
  line_box$type = 1
  line_box$x = 1:29
  
  naid = which(is.na(line_box$value))
  line_box = line_box[-naid,]
  
  line_box1 = line_box[1:14,]
  line_box2 = line_box[15:28,]
  line_box3 = line_box[29:57,]
  
 
  
  fills_manual = c('Negative Relation Mines'='#177ABD',
                   "Positive Relation Mines"='#FE5B34',
                   "Negative Growth Mines"='#3C5488FF',
                   "Positive Growth Mines"='#E64B35FF',
                   "Negative Growth Mines"='#3C5488FF',
                   "Positive Growth Mines"='#E64B35FF')
  
  sizes_manual = c('Mineral Production' = 2,
                   'NGGRACE'= 2,
                   'NIEI' = 2,
                   '(-75,-50]' = 6,
                   '(-50,-25]'= 4,
                   '(-25,0]' = 2,
                   '(0,25]' = 2,
                   '(25,50]'=4,
                   '(50,75]' = 6)
  
  color_manual = c(
                   '(-75,-50]' = '#2A4FA0',
                   '(-50,-25]'= '#177ABD',
                   '(-25,0]' ='#6CBDE3',
                   '(0,25]' = '#FE2725',
                   '(25,50]'='#FE5B34',
                   '(50,75]' = '#FEB666')
  color2 = c('Mineral Production' = 'red',
             'NGGRACE'= '#2453A9',
             'NIEI' = '#24A994')
  shape_manual = c('(-75,-50]' = 25,
                   '(-50,-25]'= 25,
                   '(-25,0]' =25,
                   '(0,25]' = 24,
                   '(25,50]'=24,
                   '(50,75]' = 24)
  
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
  
  
  library(ggnewscale)
  
  breaks_fun <- function(x){
    if(max(x)<50){
      breaks = c(1,13,21,26,29)
    }else{
      breaks = c(-150,0,150)
    }
  }
  
  year = 1990:2018
  year = as.character(year[c(1,13,21,26,29)])
  labels_fun <- function(x){
    if(max(x)<50){
      labels = year
    }else{
      labels = c(-150,0,150)
    }
  }
  
  label_df_b = data.frame(
    y = 0.75,
    x = c(7,17,23.5,27.5),
    label = c('History Period',
              'Accending Period',
              'SIP','SP'),#Slowly Increasing Period #Steady Period
    type= 1
    
  )
  
  # Mineral Production Ratio
  line_box31 = line_box3$value[c(1,13,21,26)]
  line_box32 = line_box3$value[c(13,21,26,29)]
  line_box3r = round((line_box32 - line_box31)/2*100) #18 24  7  1 %
  # IEI ratio
  line_box11 = line_box1$value[c(1,9)]
  line_box12 = line_box1$value[c(9,14)]
  line_box1r = round((line_box12 - line_box11)/2*100) # 37 -25 %
  
  # GGRACE ratio
  line_box21 = line_box2$value[c(1,9)]
  line_box22 = line_box2$value[c(9,14)]
  line_box2r = round((line_box22 - line_box21)/2*100) # -22 23 %
  
  
  b = ggplot()+
    geom_polygon(data=prov_map2,aes(x=long, y= lat,group = group),
                 colour='black',fill = '#CFCFCF',size = 0.5
                 #'#CFCFCF'
    )+
    geom_point(data = point_box,aes(x = long, y = lat,color = cuts,
                                    size = cuts,shape = cuts))+
   
    scale_color_manual(values = color_manual)+
    scale_size_manual(values = sizes_manual)+
    scale_shape_manual(values = shape_manual)+
    new_scale_color()+
    
    geom_line(data = line_box1,aes(x = x,y = value,color=color),
              size = 2)+
    geom_line(data = line_box2,aes(x = x,y = value,color=color),
              size = 2)+
    geom_line(data = line_box3,aes(x = x,y = value,color=color),
              size = 2)+
    geom_vline(data = line_box,aes(xintercept = 1),color = 'black',
               linetype = 'dashed',size = 1)+ 
    geom_vline(data = line_box,aes(xintercept = 13),color = 'black',
               linetype = 'dashed',size = 1)+ 
    
    geom_vline(data = line_box,aes(xintercept = 21),color = 'black',
                linetype = 'dashed',size = 1)+
    geom_vline(data = line_box,aes(xintercept = 26),color = 'black',
                linetype = 'dashed',size = 1)+
    geom_vline(data = line_box,aes(xintercept = 29),color = 'black',
               linetype = 'dashed',size = 1)+
    
    geom_segment(data = line_box,aes(x = 1, xend = 13, y = 0.7,yend = 0.7),color = 'black',size = 1,
                 lineend = df2$lineend[2],linejoin = df2$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    geom_segment(data = line_box,aes(x = 13, xend = 1, y = 0.7,yend = 0.7),color = 'black',size = 1,
                 lineend = df2$lineend[2],linejoin = df2$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    geom_segment(data = line_box,aes(x = 13, xend = 21, y = 0.7,yend = 0.7),color = 'black',size = 1,
                 lineend = df2$lineend[2],linejoin = df2$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    geom_segment(data = line_box,aes(x = 21, xend = 13, y = 0.7,yend = 0.7),color = 'black',size = 1,
                 lineend = df2$lineend[2],linejoin = df2$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    geom_segment(data = line_box,aes(x = 21, xend = 26, y = 0.7,yend = 0.7),color = 'black',size = 1,
                 lineend = df2$lineend[2],linejoin = df2$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    geom_segment(data = line_box,aes(x = 26, xend = 21, y = 0.7,yend = 0.7),color = 'black',size = 1,
                 lineend = df2$lineend[2],linejoin = df2$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    geom_segment(data = line_box,aes(x = 26, xend = 29, y = 0.7,yend = 0.7),color = 'black',size = 1,
                 lineend = df2$lineend[2],linejoin = df2$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    geom_segment(data = line_box,aes(x = 29, xend = 26, y = 0.7,yend = 0.7),color = 'black',size = 1,
                 lineend = df2$lineend[2],linejoin = df2$linejoin[2],arrow = arrow(length = unit(0.1, "inches")))+
    
    geom_segment(data = line_box,aes(x = 1, xend = 1, y = 0.6,yend = 0.8),color = 'black',size = 1)+
    geom_segment(data = line_box,aes(x = 13, xend = 13, y = 0.6,yend = 0.8),color = 'black',size = 1)+
    
    geom_segment(data = line_box,aes(x = 21, xend = 21, y = 0.6,yend = 0.8),color = 'black',size = 1)+
    geom_segment(data = line_box,aes(x = 26, xend = 26, y = 0.6,yend = 0.8),color = 'black',size = 1)+
    geom_segment(data = line_box,aes(x = 29, xend = 29, y = 0.6,yend = 0.8),color = 'black',size = 1)+
    
    geom_text(data = label_df_b,aes(x = x,y =y,label = label),color = 'black',size = 5)+
    scale_color_manual(values = color2)+
    #scale_linetype_manual(values = linetype_manual)+
    facet_wrap(~type, ncol = 1,scales = 'free')+
    scale_x_continuous(breaks = breaks_fun,labels = labels_fun)+
    theme_set_evolution+legend_set+
    theme(legend.position = 'none')+
    #guides(color = guide_legend(order = 1),
    #       shape = guide_legend(order = 1),
    #       size = guide_legend(order = 1))+
    xlab('Longitude')+
    ylab('Latitude')
  
  dir.create('plot')
  png('plot/pattern6.png',
      width = 18,
      height = 27,
      units = 'cm',
      res = 1000)
  print(b)
  dev.off()  
  
  label_df1 = data.frame(
    x = df_grace2$x,
    y = df_grace2$value*1.45,
    label = as.character(abs(df_grace2$value)),
    type = df_grace2$type
  )
  
   label_df1$y[7] = 280
   label_df1$y[9] = 30
   label_df1$y[3] = -30
   label_df1$y[2] = -110
   label_df1$y[4] = -110
   label_df1$y[11] = 50
   label_df1$y[5] = -50
  # bar plot 
   p12 = ggplot()+
     geom_bar(data = df_grace2,aes(x = con_name, y= value,fill =fill),
            position = 'stack',stat = 'identity')+
     
     geom_text(data = label_df1,aes(x = x,y = y,label = label),
               color = 'black',size = 5)+
     scale_fill_manual(values = fills_manual)+
     theme_set_evolution+legend_set+
     guides(fill = guide_legend(nrow = 2),
            color = guide_legend(nrow = 4))+
     theme_void()+
     theme(legend.position = 'none',
           #axis.title = element_text(color = "black",size = 8),
           axis.text.x = element_text(color = "black",size = 12),
           axis.text.y = element_blank(),
           axis.title = element_blank(),
           axis.ticks.x = element_line(color = 'black',size = 0.5,linetype = 'solid'),
           axis.line.x = element_line(color = 'black',size = 0.5,linetype = 'solid'),
           plot.margin = unit(c(0,0,0,0),'cm')
     )+
     scale_y_continuous(breaks = c(-100,0,100,200),labels = c(100,0,100,200))
  
  label_df2 = data.frame(
    x = df_iei2$x,
    y = df_iei2$value*1.4,
    label = as.character(abs(df_iei2$value)),
    type = df_iei2$type
  )
  label_df2$y[1] = -220
  label_df2$y[3] = -15
  label_df2$y[7] = 150
  label_df2$y[5] = -15
  label_df2$y[6] = -20
  
  label_df2$y[9] = 20
  label_df2$y[10] = 110
  
  p13 = ggplot()+
    geom_bar(data = df_iei2,aes(x =  con_name,y = value,fill= fill),position = 'stack',stat = 'identity')+
    geom_text(data = label_df2,aes(x = x,y = y,label = label),
              color = 'black',size = 5)+
    scale_fill_manual(values = fills_manual)+
    theme_set_evolution+legend_set+
    guides(fill = guide_legend(nrow = 2),
           color = guide_legend(nrow = 4))+
    theme_void()+
    theme(
          #legend.position = 'none',
          #axis.title = element_text(color = "black",size = 8),
          axis.text.x = element_text(color = "black",size = 12),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          axis.ticks.x = element_line(color = 'black',size = 0.5,linetype = 'solid'),
          axis.line.x = element_line(color = 'black',size = 0.5,linetype = 'solid'),
          plot.margin = unit(c(0,0,0,0),'cm')
    )+
    scale_y_continuous(breaks = c(-200,-100,0,100),labels = c(200,100,0,100))+
    xlab('Continent')+
    ylab('Mine Numbers')
  library(ggplot2)
  library(cowplot)
  library(data.table)
  library(RColorBrewer)
  library(forcats)
  
  integrated_plot = ggdraw()+
    draw_plot(b)+
    draw_plot(p12, x = 0.10, y = 0.372,
              width = 0.26, height = 0.2)+
    draw_plot(p13, x = 0.10, y = 0.048,
              width = 0.26, height = 0.16)
   
    png('plot/pattern5.png',
        width = 22 ,
        height = 30,
        units = 'cm',
        res = 1000)
    print(integrated_plot)
    dev.off() 
    
    
    p14 = ggplot()+
      geom_bar(data = df_iei2,aes(x =  con_name,y = value,fill= fill),position = 'stack',stat = 'identity')+
      geom_text(data = label_df2,aes(x = x,y = y,label = label),
                color = 'black',size = 5)+
      scale_fill_manual(values = fills_manual)+
      theme_set_evolution+legend_set+
      guides(fill = guide_legend(nrow = 2),
             color = guide_legend(nrow = 4))+
      #theme_void()+
      theme(
        legend.position = 'bottom',
        
        #axis.title = element_text(color = "black",size = 8),
        axis.text.x = element_text(color = "black",size = 12),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_line(color = 'black',size = 0.5,linetype = 'solid'),
        axis.line.x = element_line(color = 'black',size = 0.5,linetype = 'solid')
        #plot.margin = unit(c(0,0,0,0),'cm')
      )+
      guides(fill = guide_legend(nrow = 2))+
      scale_y_continuous(breaks = c(-200,-100,0,100),labels = c(200,100,0,100))+
      xlab('Continent')+
      ylab('Mine Numbers')
    
    png('plot/pattern5-14_legend.png',
        width = 22 ,
        height = 30,
        units = 'cm',
        res = 1000)
    print(p14)
    dev.off() 
    
    b2 = b+theme(legend.position = 'bottom')+
      guides(color = guide_legend(nrow = 1),
                    fill = guide_legend(nrow = 1))+
    
    png('plot/pattern5_legend2.png',
        width = 22 ,
        height = 30,
        units = 'cm',
        res = 1000)
    print(b2)
    dev.off() 
    
}









































