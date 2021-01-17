global_index_annual_evo<-function(
  
){
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
  
  iei_con_an = apply(iei_con_box,2,index2annual)
  gg_con_an = apply(gg_con_box,2,index2annual)
  
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
    facet_wrap(~variable,nrow = 2)+
    scale_color_manual(values = mycolor)+
    theme_bw()+
    theme_set_evolution+legend_set+
    guides(color = guide_legend(nrow = 1))+
    xlab('Date')+
    ylab("Indices")
  
  p 
  
  return(df)
}