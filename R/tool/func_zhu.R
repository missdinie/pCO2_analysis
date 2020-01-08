##########################################################
# Zhu algorithm method to calculate pCO2 from
# temperature and chlorophyll
# Reference: Zhu et al., 2009

# p_CO2 = partial pressure of CO2 [atm]
# SST = sea surface temperature [deg C]
# chl = chlorophyl-a [mg m-3]
# The function returns NA if SST or chl is not available
# 
# Author: Nurdinie bt Raizulasmadi
# Date: 2019-09-17
############################################################

func_zhu <- function (SST, chl){
  
  p_CO2_zhu <- (6.31*SST^2) + (61.9*chl^2) - (365.85*SST) - 
    (94.41*chl) + 5715.94
  

  return(p_CO2_zhu)
}



