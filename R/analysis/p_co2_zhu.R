
monthly_data <- read.csv('DATA/monthly_data_latest_withoutfunc.csv')

SST <- monthly_data$SEA_SURFACE
chl <- monthly_data$chlor_a


plot(SST)
# To init the variable
p_CO2_zhu <- as.numeric(length(SST))

# To run the data through the algorithm one-by-one.
for (i in 1:length(SST)){
  p_CO2_zhu[i] <- zhu_algorithm(SST[i],chl[i])
}

monthly_data <- cbind(monthly_data, p_CO2_zhu)

#deltaP in microatm
delta_p_co2_zhu <- NA
delta_p_co2_zhu = monthly_data$p_CO2_zhu - monthly_data$PP_air
monthly_data <- cbind(monthly_data, delta_p_co2_zhu)




