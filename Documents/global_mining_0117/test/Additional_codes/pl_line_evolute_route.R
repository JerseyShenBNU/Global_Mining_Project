pl_line_evolute_route<-function(
  temp_df,
  chos_loc_cor_neg,
  chos_loc_cor_pos,
  date_start = '2002-10-01',
  date_end = '2015-06-01',
  fy = '60 month',
  fontsize = 12,
  y_step = 10,
  x_step = 10
){
  
  
  cor_neg = chos_loc_cor_neg
  cor_pos = chos_loc_cor_pos
  cor_neg = as.data.frame(cor_neg)
  cor_pos = as.data.frame(cor_pos)
  
  
  # export route data for abrupt point analysis
  # Structure neg: IEI GGRACE, pos:IEI GGRACE
  # date 2002-10-01 to 2015-06-01
  write.csv(temp_df, 'D:\\Mining_project\\global\\Data\\new\\route_analysis\\data_for_ap_analysis.csv')
  
  library(reshape2)
  
  # line data prepare
  datebreak<-seq(as.Date(date_start),as.Date(date_end),by=fy)
  # set the data frame
  date<-seq(as.Date.numeric(0,origin = date_start),as.Date.numeric(0,origin = date_end),by = '1 month')
  #date = as.character(date)
  #date = as.numeric(substr(date,1,4))
  date <- 1:nrow(temp_df)
  evolution_df_l = data.frame(date,temp_df)
  evolution_melt_l = melt(evolution_df_l,'date')
  line_color = rep(c("IEI",'GGRACE'),each = nrow(temp_df))
  line_color = rep(line_color,14)
  evolution_melt_l$line_color = line_color
    
  type = rep(5:18,each = (nrow(temp_df)*2))
  
  evolution_melt_l$type = type
  
  # map data prepare 
  world = shapefile('D:\\Mining_project\\global\\Data\\new\\climate_zone\\world_continent2.shp')
  #world_df = as.data.frame(world,xy = T)
  #world_df[,1] = world_df[,1] + 180
  world = crop(world, extent(-180,180,-60,90))
  
  prov_map = fortify(world,region = "Region2")
  prov_map2 = rbind(prov_map,prov_map,prov_map,prov_map)
  prov_map2$type = rep(c(1,2,3,4),each = nrow(prov_map))
  
  cor_neg = melt(cor_neg,c('long','lat'))
  cor_pos = melt(cor_pos,c('long','lat'))
  
  cor_neg$type = rep(1,nrow(cor_neg))
  cor_pos$type = rep(2,nrow(cor_pos))
  
  
  
  cor_neg$color = rep('Mines with Maximum AV of NCCs in each Continent',nrow(cor_neg))
  cor_pos$color = rep('Mines with Maximum AV of PCCs in each Continent',nrow(cor_pos))
  
  cor_neg$label = c('e','g','i','k',
                      'm','o','q')
  cor_pos$label = c('f','h','j','l',
                      'n','p','r')
  
  group1 = cor_neg[c(1,3,5,7),]
  group2 = cor_pos[c(1,3,5,7),]
  group3 = cor_neg[c(2,4,6),]
  group4 = cor_pos[c(2,4,6),]
  
  group1$type = rep(1,4)
  group2$type = rep(2,4)
  group3$type = rep(3,3)
  group4$type = rep(4,3)
  
  ret_box = rbind(group1,group2,group3,group4)
  
  # a, b, c, d
  # 1, 2, 3, 4
  # e, f, g, h
  # i, j, k, l
  # m, n, o, p
  # q, r 
  
  
  # map attributes
  ########
  source('D:/Mining_project/global/R/theme_set_evolution_cor_map.R')
  print('start')
  theme_set_evolution<-theme_set_evolution_cor_map(12,
                                                   width = 3)
  fontsize = 25
  
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
  #color_map = c('#ACCD64','#F9F477','#E8010C','#FCD287')
  
  color_map = c('#6CBDE3','#177ABD','#2A4FA0','#013F87',
                '#FE2725','#FE5B34','#FEB666','#E8002D')
  point_color = c('blue')
  
  label_pic = c("a","b","c","d",
            "e","f","g","h",
            "i","j","k","l",
            "m","n","o","p",
            "q","r")
  buffer = c(rep(80,4),0.5,0.55,0.4,0.5,
             0.5,0.5,0.4,0.5,0.5,0.5,0.55,0.4,
             0.5,0.6)
  label_pic = paste0("(",label_pic,")")
  label = data.frame(
    x = c(-160,-160,-160,-160,rep(0.2,14)),
    y = c(rep(80,4),rep(0.7,14)),
    label_pic = label_pic,
    type = 1:18
  )
  ########
  
    
  a = ggplot()+
    #scale_fill_manual(values = fill)+
    geom_polygon(data=prov_map2,aes(x=long, y= lat,group = group),
                 colour='black',fill = '#CFCFCF',size = 0.5,alpha = 0.5
                 #'#CFCFCF'
    )+
    geom_point(data = ret_box,aes(x = long, y = lat,fill = color), 
               color = 'transparent',size = 8, alpha = 1,shape = 25)+
    scale_fill_manual(values = c('#177ABD','#FE5B34'))+
    #coord_fixed(1.3)+
    # line plot
    geom_line(data = evolution_melt_l,aes(x = date,y = value,color = line_color),lwd = 2)+
    scale_color_manual(values = c('#1E90FF','#8FBC8F'))+
    
    geom_label(data = ret_box,aes(x = long, y = lat+16, label = label),size = 8)+
    #geom_text(data = label,aes(x = x, y = y,label = label_pic,fontface = 2),size = 8)+
    facet_wrap(~type, ncol = 4, scales = 'free')+
    theme_set_evolution+legend_set+
    theme(axis.text = element_text(face='bold',colour='black',size=fontsize,hjust=.5),
          
          axis.title=element_text(face='bold',colour='black',size=fontsize,hjust=.5)
          
            )+
    #guides(shape=guide_legend(nrow=1))+
    xlab('Longitude')+
    ylab('Latitude')+
    guides(colour = guide_legend(nrow = 5))
    
    
  png('D:\\Mining_project\\global\\plot\\route_analysis\\route_analysis5.png',
      width = 48,
      height = 36,
      units = 'cm',
      res = 1000)
  print(a)
  
  dev.off()  
  
    
  
}









































