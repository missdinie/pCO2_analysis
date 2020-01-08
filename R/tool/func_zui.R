###########################################################
# Multi regression method to calculate pCO2 from
# temperature and chl
# Reference: Zhui et al., 2012
#
# p_CO2 = partial pressure of CO2 [atm]
# SST = sea surface temperature [deg C]
# chl = chlorophyl-a [mg m-3]
# The function returns NA if SST or chl is not available
# 
# Author:
# Date: 2019-09-17
############################################################

func_zui <- function (SST, chl){
  
  if(SST < 12) {
    
    p_CO2 <- (-1.55 * SST) + (0.081 * (SST)^2) - (40.85 * chl) + 
      (7.27 * chl^2) + 396.49 
    
  } else if( SST >= 12 & SST < 23) {
    
    p_CO2 <- (-22.625 * SST) + (0.79 * SST^2) + (2.77 * chl) - 
      (7.53 * chl^2) + 526.58
    
  } else if (SST >= 23){
    
    p_CO2 <- (-156.8 * SST) + (3.31 * SST^2) - (31.06 * chl) - 
      (3.21 * chl^2) + 2293.25
    
  } else {
    
    p_CO2 <- NA
    
  }
  return(p_CO2)
}




