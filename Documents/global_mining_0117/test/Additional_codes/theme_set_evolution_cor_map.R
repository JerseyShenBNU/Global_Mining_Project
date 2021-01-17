theme_set_evolution_cor_map<-function(fontsize,height=0.5,width=5,
                                       fontangle = 0){
  theme_set_detail<-theme(
    
    legend.background = element_rect(fill='white'),
    legend.key = element_blank(),
    #legend.key.size = unit(0.5,'lines'),
    legend.key.height=unit(height,"cm"),
    legend.key.width=unit(width,"cm"),
    legend.text=element_text(colour="black",size=fontsize,face = 'bold'),
    legend.text.align=0,
    legend.title=element_text(colour="black",size=fontsize,face='bold'),
    legend.title.align=0,
    legend.position=c('bottom'),#"none","left","right","bottom","top",or 
    # two-element numeric vector,(0,0)-(1,1)
    legend.direction='horizontal',#horizontal
    legend.justification=c('center'),#"center" or two-element numeric vector
    legend.box="horizontal",
    legend.box.just="top"
    ,plot.background = element_rect(color='transparent'),
    ###################### axis theme set############
    axis.title=element_text(face='bold',colour='black',size=fontsize,hjust=.5),
    axis.ticks = element_line(colour='black',size=.5,linetype = 1,lineend = 1),
    axis.line = element_line(colour = 'black',linetype = 1,lineend = 1),
    axis.text = element_text(face='bold',colour='black',size=fontsize,hjust=.5),
    axis.text.x = element_text(angle = fontangle,vjust=0.5),
    panel.background = element_rect(fill='white',color = 'black',linetype = 'solid',size=1),
    #change the property of labels for each facet
    strip.text = element_blank()
    #strip.text = element_text(face="bold", size=10,lineheight=5.0),
    #strip.background = element_rect(fill="white", colour="transparent",
    #size=1)
  )
  
  return(theme_set_detail)
  
}