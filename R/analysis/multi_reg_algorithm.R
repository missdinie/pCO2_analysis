

monthly_data <- read.csv('DATA/monthly_data_latest_withoutfunc.csv')
  
SST <- monthly_data$SEA_SURFACE
chl <- monthly_data$chlor_a



plot(SST)
# To init the variable
p_CO2_zhui <- as.numeric(length(SST))

# To run the data through the algorithm one-by-one.
#ic()
for (i in 1:length(SST)){
  p_CO2_zhui[i] <- multi_reg(SST[i],chl[i])
}
#toc()

monthly_data <- cbind(monthly_data, p_CO2_zhui)

#deltaP in microatm
delta_p_co2_zhui <- NA
delta_p_co2_zhui = monthly_data$p_CO2_zhui - monthly_data$PP_air
monthly_data <- cbind(monthly_data, delta_p_co2_zhui)














monthly_data <- cbind(monthly_data, p_CO2_zhui)



