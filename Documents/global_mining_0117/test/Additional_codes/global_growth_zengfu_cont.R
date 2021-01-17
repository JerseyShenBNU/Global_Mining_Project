global_growth_zengfu_cont<-function(
  
){
  zengfu_calc <- function(x){
    len = length(x)
    tmp = (x[len]-x[1])/2*100
    return(tmp)
  }
  
  iei_con_an = read.csv('output_analysis/iei_con_an.csv',
                        header = T)
  gg_con_an = read.csv("output_analysis/gg_con_an.csv",
                       header = T)
  
  mine_an = read.csv('output_analysis/mine_box_s.csv',header = T)
  mine_an = mine_an[,-1]
  
  iei_con_an = iei_con_an[,-1]
  gg_con_an = gg_con_an[,-1]
  
  zengfu_calc <- function(x){
    len = length(x)
    tmp = (x[len]-x[1])/2*100
    return(tmp)
  }
  
  
  
  iei_con_z = apply(iei_con_an,2,zengfu_calc)
  gg_con_z = apply(gg_con_an,2,zengfu_calc)
  mine_con_z = apply(mine_an,2,zengfu_calc)
  
  
  con_name = c('AF',"AS", "EU",  "NA", "OC", "SA")
  
  df_zengfu = data.frame(con_name,
                         Growth_NNGRACE = gg_con_z,
                         Growth_NIEI = iei_con_z,
                         Growth_Minearl_Production = mine_con_z)
  
  
  dfm = melt(df_zengfu,'con_name')
  
  mycolor = c('Growth_NNGRACE'='#2453A9',
              'Growth_NIEI' = '#24A994',
              "Growth_Minearl_Production" = 'red')
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
             stat = 'identity',position = 'dodge',width= 0.7)+
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
  
  png('plot/statistic_bar3.png',
      width = 29.97,
      height = 10.76,
      units = 'cm',
      res = 1000)
  print(p1)
  dev.off()
  
  p1l = p1+theme(legend.position = 'bottom')+guides(fill = guide_legend(nrow = 2))
  png('plot/statistic_bar3_l.png',
      width = 29.97,
      height = 10.76,
      units = 'cm',
      res = 1000)
  print(p1l)
  dev.off()
  
  30.42
  11
  
}