###########################################################
# Multi regression method to calculate pCO2 from
# temperature and solubility
# Reference: Lefèvre, 2002

# p_CO2 = partial pressure of CO2 [atm]
# SST = sea surface temperature [deg C]
# S = Solubility [%]
# The function returns NA if SST or S is not available
# 
# Author: Nur
# Date: 2019-09-17
############################################################

func_lef <- function (SST){
  
  if(SST < 19.5) {
    
    p_CO2 <- -13682 + (416.1*((0.0269*SST) + 35.065))-
      ( 18.78*SST)
    
  } else if( SST >= 19.5 ) {
    
    p_CO2 <- -13682 + (416.1*((0.101*SST) + 32.561)) - 
      (18.78*SST)
    
    
  } else {
    
    p_CO2 <- NA
    
  }
  return(p_CO2)
}




