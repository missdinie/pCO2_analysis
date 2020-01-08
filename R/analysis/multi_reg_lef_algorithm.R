
monthly_data <- read.csv('DATA/monthly_data_latest_withoutfunc.csv')

SST <- monthly_data$SEA_SURFACE


plot(SST)
# To init the variable
p_CO2_lef <- as.numeric(length(SST))

# To run the data through the algorithm one-by-one.

for (i in 1:length(SST)){
  p_CO2_lef[i] <- multi_reg_lef(SST[i])
}


monthly_data <- cbind(monthly_data, p_CO2_lef)

#deltaP in microatm
delta_p_p_lef <- NA
delta_p_p_lef = monthly_data$p_CO2_lef - monthly_data$PP_air
monthly_data <- cbind(monthly_data, delta_p_p_lef)


