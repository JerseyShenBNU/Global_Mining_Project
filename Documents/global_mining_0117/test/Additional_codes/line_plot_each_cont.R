line_plot_each_cont<-function(
  
){
  
  iei_con_an = read.csv('output_analysis/iei_con_an.csv',
                        header = T)
  gg_con_an = read.csv("output_analysis/gg_con_an.csv",
                       header = T)
  
  mine_an = read.csv('output_analysis/mine_box_s.csv',header = T)
  mine_an = mine_an[,-1]
  
  
  iei_con_an = iei_con_an[,-1]
  gg_con_an = gg_con_an[,-1]
  
  
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
    geom_hline(yintercept = 0,color = 'black',linetype = 'solid',size = 1)+
    facet_wrap(~variable,nrow = 2,scales = 'free')+
    scale_color_manual(values = mycolor)+
    theme_bw()+
    theme_set_evolution+legend_set+
    theme(legend.position = 'none')+
    guides(color = guide_legend(nrow = 1))+
    xlab('Date')+
    ylab("Indices")
  
  png('plot/line_annual_cont.png',
      width =  32.38,
      height =  13,
      units = 'cm',
      res = 1000)
  print(p)
  dev.off()
  
  
  
  p1 = p+ theme(legend.position = 'bottom')
  png('plot/line_annual_cont2.png',
      width =  32.38,
      height =  13,
      units = 'cm',
      res = 1000)
  print(p1)
  dev.off()
  
}