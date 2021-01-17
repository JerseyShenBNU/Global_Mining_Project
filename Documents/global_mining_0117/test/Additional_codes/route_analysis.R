route_analysis <-function(
  
){
  
  # 1. Sub continents and choose mines with highest absoulute value of correlation coefficients 
  # 1.1 import continent shp files
  source('D:/Mining_project/global/R/import_continent_shp.R')
  # saving structure 
  # africa,asia, europe, north_america, oceania, south_america
  continent_shp = import_continent_shp() # list
  # 1.2 import points with cor value and longitude and latitude 
  print("Start Selecting")
  loc_cor = read.csv('D:\\Mining_project\\global\\Data\\new\\pattern_analysis\\loc_cor_index.csv',
                     header = T)
  
  loc_cor = loc_cor[,-1]
  
  loc_cor[,1] = loc_cor[,1] - 180 # extent -180, 180
  
  loc_df = data.frame(long = loc_cor[,1],lat = loc_cor[,2])
  coordinates(loc_df) = ~ long + lat
  proj4string(loc_df) = crs(continent_shp[[1]])
  
  #sps = SpatialPoints(loc_cor[,1:2]) # spatial point points 
  
  
  con_africa = 1
  con_asia = 1
  con_europe = 1
  con_north_ame = 1
  con_oceania = 1
  con_south_ame = 1
  
  for(i in 1:length(continent_shp)){
    temp = over(loc_df,continent_shp[[i]])
    temp_index = which(!is.na(temp))
    #print(temp_index)
    temp = loc_df[temp_index,]
    temp = as.data.frame(temp,xy = T)    
    
    if(i == 1){
      con_africa = cbind(con_africa,temp)
    }else if(i == 2){
      con_asia = cbind(con_asia,temp)
    }else if(i == 3){
      con_europe = cbind(con_europe,temp)
    }else if(i == 4){
      con_north_ame = cbind(con_north_ame,temp)      
    }else if(i == 5){
      con_oceania = cbind(con_oceania,temp)
    }else if(i == 6){
      con_south_ame = cbind(con_south_ame,temp)
    }
  }
  
  con_africa = con_africa[,-1]
  
  con_north_afr = con_africa[which(con_africa[,2]>0),]
  con_south_afr = con_africa[which(con_africa[,2]<0),]
  
  con_asia = con_asia[,-1]
  con_europe = con_europe[,-1]
  con_north_ame = con_north_ame[,-1]
  con_oceania = con_oceania[,-1]
  con_south_ame = con_south_ame[,-1]
  
  #plot(continent_shp[[1]])
  #points(con_africa);break
  
  con_list = list(con_north_ame,con_south_ame,con_north_afr,
                  con_south_afr,con_asia,con_europe,con_oceania
                  )
  source('D:/Mining_project/global/R/cor_match.R')
  
  
 
  chos_loc_cor_neg = 1
  chos_loc_cor_pos = 1
  for(i in 1:length(con_list)){
    
    temp_con = con_list[[i]]
    temp_out = cor_match(temp_con,loc_cor)  # name check for first front 5
    
    temp_neg_out = temp_out[[1]]
    temp_pos_out = temp_out[[2]]
    
    chos_loc_cor_neg = rbind(chos_loc_cor_neg,
                                  temp_neg_out)
    chos_loc_cor_pos = rbind(chos_loc_cor_pos,
                                  temp_pos_out)
    
  }
  chos_loc_cor_neg = chos_loc_cor_neg[-1,]
  chos_loc_cor_pos = chos_loc_cor_pos[-1,]
  #break
  #break
  #print(chos_loc_cor_neg)
  #print(chos_loc_cor_pos)
  #;break
  
  row_names = c('North_America','South_America','North_Africa','South_Africa','Asia','Europe',
                'Oceania')
  
  rownames(chos_loc_cor_neg) = row_names
  rownames(chos_loc_cor_pos) = row_names
  
  chos_loc_cor = rbind(chos_loc_cor_neg,
                       chos_loc_cor_pos)
  
  write.csv(chos_loc_cor,'D:\\Mining_project\\global\\Data\\new\\chos_loc\\chos_loc.csv')
  
  print("Finish Choosing loc with maximum absoulte value of coorrelation coefficients")
  # 2. import IEI and GGRACE
  print("Start Route Analysis")
  input_ggrace = 'D:\\Mining_project\\global\\Data\\new\\ts_index\\ggrace_mat_ts.csv'
  input_iei = 'D:\\Mining_project\\global\\Data\\new\\ts_index\\output_iei_ts.csv'
  iei = read.csv(input_iei,header = T)
  ggrace = read.csv(input_ggrace,header= T)
  
  iei = iei[,-1]
  ggrace = ggrace[,-1]
  
  #return(list(chos_loc_cor_neg,));break
  neg_index = 1
  pos_index = 1
  for(i in 1:nrow(chos_loc_cor_neg)){
    temp_neg_long = chos_loc_cor_neg[i,1]
    temp_pos_long = chos_loc_cor_pos[i,1]
    
    temp_neg_index = which(loc_cor[,1] == temp_neg_long)
    temp_pos_index = which(loc_cor[,1] == temp_pos_long)
    
    #print(temp_neg_index)
    neg_index = c(neg_index,temp_neg_index)
    pos_index = c(pos_index,temp_pos_index)
    
    
  }
  neg_index = neg_index[-1]
  pos_index = pos_index[-1]
  
  neg_index = neg_index[-5]
  print(neg_index)
  print(pos_index) ;
  temp_df = 1

  for(i in 1:length(neg_index)){
    
    temp_df = cbind(temp_df, 
                    iei[,neg_index[i]],
                    ggrace[,neg_index[i]],
                    iei[,pos_index[i]],
                    ggrace[,pos_index[i]])
  }
  temp_df = temp_df[,-1]
  temp_df =as.data.frame(temp_df)
  #print(temp_df);break
  #return(temp_df)
  
  source('D:/Mining_project/global/R/pl_line_evolute_route.R')
  print("Start Plot")
  pl_line_evolute_route(temp_df,
                        chos_loc_cor_neg,
                        chos_loc_cor_pos,
                        date_start = '2002-10-01',
                        date_end = '2015-06-01',
                        fy = '60 month',
                        fontsize = 12,
                        y_step = 10,
                        x_step = 10)
  
  print('End of Plot')
  
  
  
}















































































