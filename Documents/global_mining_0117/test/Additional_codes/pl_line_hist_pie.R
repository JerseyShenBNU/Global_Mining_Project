pl_line_hist_pie <- function(
  
){
  
  
  data = read.csv('output/arid_coun_mine_zengfu2.csv',
                  header = T)
  growth = data$zengfu
  
  Group1 = which(growth<=0)
  Group2 = which(growth<=700)
  Group3 = which(growth<=6000)
  Group4 = which(growth>6000)
  
  
  len1 = length(Group1)
  len2 = length(Group2)
  len3 = length(Group3)
  len4 = length(Group4)
  lent = len1+len2+len3+len4
  
  df = data.frame(
    Growth = c('(-6308,0]',
               '(0,700]',
               '(700,6000]',
               '(6000,529346]'),
    Number = c(len1,len2,len3,len4)
  )
  df$label = paste0(round(df$Number/lent*100,2),'%')
  df$Growth = factor(df$Growth,levels =c('(-6308,0]',
                                         '(0,700]',
                                         '(700,6000]',
                                         '(6000,529346]') )
  
  mycolor = c('#4575B5','#C0CCBE','#FAB984','#D62F27')
  p1 = ggplot()+
    geom_bar(data = df,aes(x = "",y = Number,fill = Growth),
             width = 1,stat = 'identity')+
    coord_polar('y',start = 0)+
    scale_fill_manual(values = mycolor)+
    #geom_text(data = df,
    #          aes(x="",y=Number/2+c(0, cumsum(Number)[-length(Number)])-5),
    #         label = df$label,size = 5)+
    theme_void()+
    theme(
      legend.position = 'none'
    )
  
  png('plot/pie1.png',
      width = 8.53,
      height = 8.53,
      units = 'cm',
      res = 1000)
  print(p1)
  dev.off()
  
  df2 = data.frame(
    group1 = as.character(1:4),
    Number = c(103,332,321,273)
  )
  mycolor2 = c('#FFD37F','#E60000','#F5F57A','#ABCD66')
  
  p2 = ggplot()+
    geom_bar(data = df2,aes(x = "",y = Number,fill = group1),
             width = 1,stat = 'identity')+
    coord_polar('y',start = 0)+
    scale_fill_manual(values = mycolor2)+
    theme_void()+
    theme(legend.position = 'none')
  
  png('plot/pie2.png',
      width = 8.53,
      height = 8.53,
      units = 'cm',
      res = 1000)
  print(p)
  dev.off()
  
  data3 = read.csv('output/arid_coun_mine.csv',
                   header = T)
  
  data3 = data3[,-1]
  data3 = data3[,-c(1,31)]
  
  data3_sum = apply(data3,2,sum)
  
  df3 = data.frame(
    date = seq(as.Date('1990-01-01'),
               as.Date('2018-01-01'),
               '12 month'),
    value = as.vector(data3_sum)
  )
  
  p3 = ggplot()+
    geom_line(data = df3,aes(x = date,y = value/100000000),
              color = '#E60000',size = 3)+
    theme_bw()+
    theme(
      axis.text.x =  element_text(face = 'bold',color = 'black',size = 14,angle = 0,hjust = 0.5,vjust = 0.5),
      axis.text.y =  element_text(face = 'bold',color = 'black',size = 14,angle = 90,vjust = 0.5,hjust = 0.5),
      axis.title =  element_text(face = 'bold',color = 'black',size = 14, hjust = .5),
      legend.text = element_text(face = 'bold',color = 'black',size = 14),
      legend.title = element_blank(),
      legend.position = 'bottom',
      legend.direction = 'horizontal',
      panel.border = element_blank(),
      axis.line = element_line(colour ='black',size = 1,arrow = arrow(length= unit(0.15,'inches')))
    )+
    xlab("Date")+
    ylab('Global Minearl Annual Production \n')
  
  png('plot/line_plot.png',
      height = 8.86,
      width = 27.82,
      units = 'cm',
      res = 1000)
  print(p3)
  dev.off()
  
}