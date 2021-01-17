gloabl_index_evolution<-function(x)
{
  ggrace = read.csv(input_ggrace,header = T)
  iei = read.csv(input_iei,header = T)
  
  ggrace = ggrace[,-1]
  iei = iei[,-1]
  
  
  
  source('E:/Desktop/global_mining_extended/crop_by_continent_index.R')
  
  index_cont <- crop_by_continent_index()
  
  
  gg_con_box = 1
  iei_con_box = 1
  for(i in 1:length(index_cont)){
    tmpid = index_cont[[i]]
    
    
    tmpgg = ggrace[,tmpid]
    tmpiei = iei[,tmpid]
    
    tmpgg = apply(tmpgg,1,mean)
    tmpiei = apply(tmpiei,1,mean)
    
    gg_con_box = cbind(gg_con_box,tmpgg)
    iei_con_box = cbind(iei_con_box,tmpiei)
  }
 
  gg_con_box = gg_con_box[,-1]
  iei_con_box = iei_con_box[,-1]
  
  
  colnames(iei_con_box) = c('AF',"AS", "EU",  "NA", "OC", "SA")
  colnames(gg_con_box) = c('AF',"AS", "EU",  "NA", "OC", "SA")
  
  # import mine 
  
  date = seq(as.Date('2002-10-01'),
             as.Date('2015-06-01'),
             '1 month')
  
  
  
  df2 = data.frame(date,gg_con_box)
  df3 = data.frame(date,iei_con_box)
  
 
  df2 = melt(df2,'date')
  df3 = melt(df3,'date')
  
  mycolor = colorRampPalette(brewer.pal(11,'Spectral'))(12)
  color_group = paste0('a',1:12)
  
  df2$color = 'NGGRACE'
  df3$color = 'NIEI'
  
  mycolor = c('NGGRACE'= '#2453A9',
              'NIEI' = '#24A994')
  df = rbind(df2,df3)
  
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
    facet_wrap(~variable,nrow = 2)+
    scale_color_manual(values = mycolor)+
    theme_bw()+
    theme_set_evolution+legend_set+
    guides(color = guide_legend(nrow = 1))+
    xlab('Date')+
    ylab("Indices")
    
  p 
  
  png('plot/add_line_plot.png',
      width = 40,
      height = 15,
      units = 'cm',
      res = 1000)
  print(p)
  dev.off()
}