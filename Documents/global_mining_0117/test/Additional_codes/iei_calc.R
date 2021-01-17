iei_calc<-function(
  gpp_box,
  vci_box,
  sm_box
){
  library(psych)
  temp_gpp = gpp_box
  temp_vci = vci_box
  temp_sm = sm_box
  
  # computation of iei
  
  w2 = principal(data.frame(temp_gpp,temp_vci,temp_sm),
                 nfactors = 1,
                 rotate = 'varimax')$weights
  p2 = w2[1]*temp_gpp + w2[2]*temp_vci + w2[3]*temp_sm
  
  return(p2)
  
}