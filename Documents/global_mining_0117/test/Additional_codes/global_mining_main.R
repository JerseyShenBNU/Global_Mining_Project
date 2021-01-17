global_mining_main<-function(
  
){
  ### Important Notes ###
  # The path for sub functions need to be altered based on local environment before applying in R 
  ### Important Notes ###

  # 1. Develope GGRACE and IEI indices 
  source('D:/Mining_project/global/R/main_extract_global.R')
  main_extract_global()
  print("Pass Extraction by point")

  # 2.  Spatiotemporal variation of groundwater storage (NGGRACE), vegetation living and growth condition (NIEI) 
  # and annual mineral productions (AMP) for the global dryland mining sites on both global and continental scales.
  source('~/Additional_codes/global_index_variation_pattern.R')
  global_index_variation_pattern()
  source('~/Additional_codes/global_growth_zengfu_cont.R')
  global_growth_zengfu_cont()
  source('~/Additional_codes/line_plot_each_cont.R')
  line_plot_each_cont()

  # 3. Global Pattern Analysis-mineral productions, GGRACE and IEI over global dryland mine sites
  source('D:/Mining_project/global/R/global_pattern_analysis.R')
  global_pattern_analysis()
  print("Pass Global_pattern_analysis")

  # 4. Attribution analysis of soil moisture and Conjoint analysis between water source types of soil moisture and 
  # correlation patterns between vegetation conditions and groundwater storages over global mines under arid and semi-arid climatic conditions
  source('~/Additional_codes/attr_analysis.R')
  attr_analysis()
  source('~/Additional_codes/coanalyze_cor_attr.R')
  coanalyze_cor_attr()
  
  # 5. Route Analysis at typical mines with maximum absoulte values of negative 
  #    (positive) correlations in each continent
  source('D:/Mining_project/global/R/route_analysis.R')
  source('D:/Mining_project/global/R/pl_line_evolute_route.R')
  route_analysis()
  print("Pass Route analysis")
  # 6. AP analysis at typical mines with maximum absoulte values of negative 
  #    (positive) correlations in each continent
  source('D:/Mining_project/global/R/abrupt_analysis.R')
  abrupt_ponit_analysis()
  print("Pass AP analysis")
 
  
 
}