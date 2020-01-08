
#### data rearrange from eddypro processed data ####

require(stringr)
muka <- read.csv('Data/rearrange data from eddy.csv')

t <- str_pad(muka$time, width=5, side="left", pad="0")


date <- paste(muka$date,t)
muka$date<- as.POSIXct(date, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
muka <- muka[,-2]

muka$wind_speed[muka$wind_speed >5]<-NA
plot(muka$wind_speed)


muka$dir_90 = muka$wind_dir
muka$dir_90[muka$dir_90 > 90 ]<- NA
muka$radian_wd = muka$dir_90 
muka$radian_wd <- (muka$dir_90*pi)/180

#Remove qc=2, wind direction > 90, rain >0
muka$co2_flux[muka$wind_dir > 90 ]<- NA
muka$co2_flux[muka$qc_co2_flux == 2] <- NA
muka$co2_flux[muka$HourlyPrecipMM >0] <- NA
muka$co2_flux[muka$co2_flux >0.5] <- NA
muka$co2_flux[muka$co2_flux < -0.5] <- NA
muka$u.[muka$wind_dir > 90] <- NA
muka$co2_mole_fraction[muka$wind_dir>90] <- NA
muka$co2_mixing_ratio[muka$wind_dir> 90] <- NA
muka$co2_mixing_ratio[muka$co2_mixing_ratio < 0] <- NA
muka$co2_mole_fraction[muka$co2_mole_fraction < 0] <-NA
muka$air_pressure[muka$wind_dir> 90] <- NA
muka$x_90.[muka$wind_dir> 90] <- NA

plot(muka$co2_flux)
which.max(muka$co2_flux)
which.min(muka$co2_flux)

plot(muka$co2_mole_fraction)
which.max(muka$co2_mole_fraction)
which.min(muka$co2_mole_fraction)





#### Grouping data monthly ####
library(dplyr)
by_monthly_grp_data_mean <- muka %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%Y %m')) %>%
  summarise(co2_flux=mean(co2_flux,na.rm=TRUE),
            co2_mixing_ratio=mean(co2_mixing_ratio,na.rm=TRUE),
            co2_mole_fraction=mean(co2_mole_fraction,na.rm=TRUE),
            air_pressure=mean(air_pressure,na.rm = TRUE),
            U_insitu =mean(wind_speed,na.rm=TRUE),
            u. =mean(u.,na.rm = TRUE),
            x_90. = mean(x_90.,na.rm = TRUE))
by_monthly_grp_data_sd <- muka %>% 
  mutate(time_stamp=as.POSIXct(date)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%Y %m')) %>%
  summarise(co2_flux_sd=sd(co2_flux,na.rm=TRUE),
            co2_mixing_ratio_sd=sd(co2_mixing_ratio,na.rm=TRUE),
            co2_mole_fraction_sd=sd(co2_mole_fraction,na.rm=TRUE),
            air_pressure_sd=sd(air_pressure,na.rm = TRUE),
            U_insitu_sd =sd(wind_speed,na.rm=TRUE),
            radian_wd_sd =sd(radian_wd, na.rm = TRUE),
            u._sd =sd(u.,na.rm = TRUE),
            x_90._sd = sd(x_90.,na.rm = TRUE)) 

by_monthly_grp_data <- merge(by_monthly_grp_data_mean, by_monthly_grp_data_sd,by='month')
rm(by_monthly_grp_data_mean,by_monthly_grp_data_sd)
month <- paste(by_monthly_grp_data$month,"15"," 00:00:00")
by_monthly_grp_data <- cbind(month,by_monthly_grp_data)
month <- as.POSIXct(month, format = "%Y %m %d %H:%M:%S", tz = 'Asia/Kuala_Lumpur')
by_monthly_grp_data <- cbind(month,by_monthly_grp_data)
by_monthly_grp_data <- by_monthly_grp_data[,-2]

monthly_data<- by_monthly_grp_data

lat_site <-NA
lat_site = 5.468333
monthly_data<-cbind(monthly_data,lat_site)

lon_site <-NA
lon_site = 100.2002778
monthly_data<-cbind(monthly_data,lon_site)



PP_air <- NA

co2_mF <- monthly_data$co2_mole_fraction*10^-6  #convert into mol/mol
PP_air= co2_mF*monthly_data$air_pressure
PP_air <- PP_air * 0.001 #convert to kPa
#convert PP_air into micro atm
PP_air = (PP_air/101.325)*10^6
monthly_data <- cbind(monthly_data, PP_air)



####quality check for SST####

sst <- read.csv('DATA/huiyin_SST_Aqua_latest_old.csv')

#Remove  sst > 100, sst< -253

sst$SST[sst$SST > 100 ] <- NA
sst$SST[sst$SST < -253 ] <- NA


plot(sst$SST)
which.max(sst$SST)
which.min(sst$SST)


#### Grouping data monthly for SST ####
library(dplyr)
by_monthly_grp_data_mean_sst <- sst %>% 
  mutate(time_stamp=as.POSIXct(DATE)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%Y %m')) %>%
  summarise(LAT=mean(LAT,na.rm=TRUE),
            LON=mean(LON,na.rm=TRUE),
            SST=mean(SST,na.rm=TRUE))

by_monthly_grp_data_sd_sst <- sst %>% 
  mutate(time_stamp=as.POSIXct(DATE)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%Y %m')) %>%
  summarise(LAT_sd=sd(LAT,na.rm=TRUE),
            LON_sd=sd(LON,na.rm=TRUE),
            SST_sd=sd(SST,na.rm=TRUE))


by_monthly_grp_data_sst <- merge(by_monthly_grp_data_mean_sst, by_monthly_grp_data_sd_sst,by='month')
rm(by_monthly_grp_data_mean_sst,by_monthly_grp_data_sd_sst)
month <- paste(by_monthly_grp_data_sst$month,"15"," 00:00:00")
by_monthly_grp_data_sst <- cbind(month,by_monthly_grp_data_sst)
month <- as.POSIXct(month, format = "%Y %m %d %H:%M:%S", tz = 'Asia/Kuala_Lumpur')
by_monthly_grp_data_sst <- cbind(month,by_monthly_grp_data_sst)
by_monthly_grp_data_sst <- by_monthly_grp_data_sst[,-2]

monthly_data_sst <- by_monthly_grp_data_sst

TS <-monthly_data_sst 
TS <- TS[-c(40, 41, 42, 43, 44), ] 

SEA_SURFACE <- NA
SEA_SURFACE <- TS$SST
monthly_data <- cbind(monthly_data, SEA_SURFACE)


####quality check for chlor####

chlor <- read.csv('DATA/huiyin_chlor_Aqua_latest_old.csv')

#Remove  negative value of chl

chlor$CHLOR[chlor$CHLOR < 0.00 ]  <- NA
chlor$CHLOR[chlor$CHLOR > 20.00 ]  <- NA



plot(chlor$CHLOR)
which.max(chlor$CHLOR)
which.min(chlor$CHLOR)


#### Grouping data monthly for chlor ####
library(dplyr)
by_monthly_grp_data_mean_chl <- chlor %>% 
  mutate(time_stamp=as.POSIXct(DATE)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%Y %m')) %>%
  summarise(LAT=mean(LAT,na.rm=TRUE),
            LON=mean(LON,na.rm=TRUE),
            CHLOR=mean(CHLOR,na.rm=TRUE))

by_monthly_grp_data_sd_chl <- chlor %>% 
  mutate(time_stamp=as.POSIXct(DATE)) %>%
  group_by(month=format(as.POSIXlt(cut(time_stamp,breaks='month')),'%Y %m')) %>%
  summarise(LAT_sd=sd(LAT,na.rm=TRUE),
            LON_sd=sd(LON,na.rm=TRUE),
            CHLOR_sd=sd(CHLOR,na.rm=TRUE))


by_monthly_grp_data_chl <- merge(by_monthly_grp_data_mean_chl, by_monthly_grp_data_sd_chl,by='month')
rm(by_monthly_grp_data_mean_chl,by_monthly_grp_data_sd_chl)
month <- paste(by_monthly_grp_data_chl$month,"15"," 00:00:00")
by_monthly_grp_data_chl <- cbind(month,by_monthly_grp_data_chl)
month <- as.POSIXct(month, format = "%Y %m %d %H:%M:%S", tz = 'Asia/Kuala_Lumpur')
by_monthly_grp_data_chl <- cbind(month,by_monthly_grp_data_chl)
by_monthly_grp_data_chl <- by_monthly_grp_data_chl[,-2]

chl <- by_monthly_grp_data_chl
chl<- chl[-c(40, 41, 42, 43, 44), ] 
chlor_a <- chl$CHLOR
monthly_data <- cbind(monthly_data, chlor_a)


#### Add first algorithm ####

source("R/tool/func_zhu.R")

SST <- monthly_data$SEA_SURFACE
chl <- monthly_data$chlor_a


plot(SST)
# To init the variable
p_CO2_zhu <- as.numeric(length(SST))

# To run the data through the algorithm one-by-one.
for (i in 1:length(SST)){
  p_CO2_zhu[i] <- func_zhu(SST[i],chl[i])
}

monthly_data <- cbind(monthly_data, p_CO2_zhu)

#deltaP in microatm
delta_p_co2_zhu <- NA
delta_p_co2_zhu = monthly_data$p_CO2_zhu - monthly_data$PP_air
monthly_data <- cbind(monthly_data, delta_p_co2_zhu)

#### Add second algorithm ####

source("R/tool/func_zui.R")
SST <- monthly_data$SEA_SURFACE
chl <- monthly_data$chlor_a


plot(SST)
# To init the variable
p_CO2_zhui <- as.numeric(length(SST))

# To run the data through the algorithm one-by-one.
#ic()
for (i in 1:length(SST)){
  p_CO2_zhui[i] <- func_zui(SST[i],chl[i])
}
#toc()

monthly_data <- cbind(monthly_data, p_CO2_zhui)

#deltaP in microatm
delta_p_co2_zhui <- NA
delta_p_co2_zhui = monthly_data$p_CO2_zhui - monthly_data$PP_air
monthly_data <- cbind(monthly_data, delta_p_co2_zhui)

#### Add third algorithm ####

source("R/tool/func_lef.R")

SST <- monthly_data$SEA_SURFACE


plot(SST)
# To init the variable
p_CO2_lef <- as.numeric(length(SST))

# To run the data through the algorithm one-by-one.

for (i in 1:length(SST)){
  p_CO2_lef[i] <- func_lef(SST[i])
}


monthly_data <- cbind(monthly_data, p_CO2_lef)

#deltaP in microatm
delta_p_p_lef <- NA
delta_p_p_lef = monthly_data$p_CO2_lef - monthly_data$PP_air
monthly_data <- cbind(monthly_data, delta_p_p_lef)



#### Calculate k value (gas transfer velocity) with the bulk formula ####
#Fco2 = kSdelta_p_p
#Fco2 in μmol per m^2 per s
#S = solubility of Carbon dioxide in water
#S = 0.10615 mol per litre per atm
#delta_p_p in μatm
#k in m per second


#### k value calculate using theory ####
#k = 1/β * 1/(Sc^n) * u.
#β =16 -11  #16 is high turbulence, 11 is low turbulence
#n 0.67 -0.4   # 0.67 for smooth surface
#Sc = 2116.8 + (-136.25*t)+4.7353*(t^2) + (-0.092307)*(t^3) + 0.0007555*(t^4)
#t = sea surface temperature

t = monthly_data$SEA_SURFACE
S = 0.10615 
Sc = 2116.8 + (-136.25*t)+4.7353*(t^2) + (-0.092307)*(t^3) + 0.0007555*(t^4)
n = 0.67
β = 11
u. = monthly_data$u.
k_theory <-NA
k_theory = 1/β * 1/(Sc^n) * u.
k_theory <- k_theory*100 * 3600  ## unit in cm/hr
monthly_data<- cbind(monthly_data,k_theory)


#### k value calculate from literature's equations ####
#k_N2000 based on Nightingale
#k_N2000 = ((660/Sc)^0.5)*(0.212*(U^2) + 0.318U)
U = monthly_data$U_insitu
k_N2000 <- NA
k_N2000 = ((660/Sc)^0.5)*(0.212*(U^2) + 0.318*U)  ## unit in cm/hr
monthly_data <- cbind(monthly_data,k_N2000)

#k_W2014 based on Wanniknof 2014
# k = ((660/Sc)^0.5)*(0.251*(U^2))
k_W2014<- NA
k_W2014= ((660/Sc)^0.5)*(0.251*(U^2))   ## unit in cm/hr
monthly_data <- cbind(monthly_data, k_W2014)

#k_MG2001 based on McGillis 2001
k_MG2001 <- NA
k_MG2001 = ((660/Sc)^0.5)*(0.026*(U^3) + 3.3)  ## unit in cm/hr
monthly_data <- cbind(monthly_data,k_MG2001)

#k_WMG1999 based on Wanniknof n McGillis 1999
k_WMG1999 <- NA
k_WMG1999 = ((660/Sc)^0.5)*(0.0283*(U^3))  ## unit in cm/hr
monthly_data<- cbind(monthly_data,k_WMG1999)



#### Use the k_MG2001 to calculate the co2_flux of zhu####

k_MG2001_co2_zhu <- NA
k_MG2001_co2_zhu = (k_MG2001/3600/100) * S*1000 * monthly_data$delta_p_co2_zhu
monthly_data <- cbind(monthly_data, k_MG2001_co2_zhu)

#### Use the k_MG2001 to calculate the co2_flux of zhui ####

k_MG2001_co2_zhui <- NA
k_MG2001_co2_zhui = (k_MG2001/3600/100) * S*1000 * monthly_data$delta_p_co2_zhui
monthly_data <- cbind(monthly_data, k_MG2001_co2_zhui)

#### Use the k_MG2001 to calculate the co2_flux of lef ####

k_MG2001_co2_lef <- NA
k_MG2001_co2_lef = (k_MG2001/3600/100) * S*1000 * monthly_data$delta_p_p_lef
monthly_data <- cbind(monthly_data, k_MG2001_co2_lef)



#### Use the k_theory to calculate the co2_flux of zhu ####

k_theory_co2_zhu <- NA
k_theory_co2_zhu = (k_theory/3600/100) * S*1000 * monthly_data$delta_p_co2_zhu
monthly_data <- cbind(monthly_data, k_theory_co2_zhu)

#### Use the k_theory to calculate the co2_flux of zhui ####

k_theory_co2_zhui <- NA
k_theory_co2_zhui = (k_theory/3600/100) * S*1000 * monthly_data$delta_p_co2_zhui
monthly_data <- cbind(monthly_data, k_theory_co2_zhui)

#### Use the k_theory to calculate the co2_flux of lef ####

k_theory_co2_lef <- NA
k_theory_co2_lef = (k_theory/3600/100) * S*1000 * monthly_data$delta_p_p_lef
monthly_data <- cbind(monthly_data, k_theory_co2_lef)


#### Use the k_WMG1999 to calculate the co2_flux of zhu####

k_WMG1999_co2_zhu <- NA
k_WMG1999_co2_zhu = (k_WMG1999/3600/100) * S*1000 * monthly_data$delta_p_co2_zhu
monthly_data <- cbind(monthly_data, k_WMG1999_co2_zhu)

#### Use the k_WMG1999 to calculate the co2_flux of zhui ####

k_WMG1999_co2_zhui <- NA
k_WMG1999_co2_zhui = (k_WMG1999/3600/100) * S*1000 * monthly_data$delta_p_co2_zhui
monthly_data <- cbind(monthly_data, k_WMG1999_co2_zhui)

#### Use the k_WMG1999 to calculate the co2_flux of lef ####

k_WMG1999_co2_lef <- NA
k_WMG1999_co2_lef = (k_WMG1999/3600/100) * S*1000 * monthly_data$delta_p_p_lef
monthly_data <- cbind(monthly_data, k_WMG1999_co2_lef)



#### Use the k_N2000 to calculate the co2_flux of zhu####

k_N2000_co2_zhu <- NA
k_N2000_co2_zhu = (k_N2000/3600/100) * S*1000 * monthly_data$delta_p_co2_zhu
monthly_data <- cbind(monthly_data, k_N2000_co2_zhu)

#### Use the k_N2000 to calculate the co2_flux of zhui ####

k_N2000_co2_zhui <- NA
k_N2000_co2_zhui = (k_N2000/3600/100) * S*1000 * monthly_data$delta_p_co2_zhui
monthly_data <- cbind(monthly_data, k_N2000_co2_zhui)

#### Use the k_N2000 to calculate the co2_flux of lef ####

k_N2000_co2_lef <- NA
k_N2000_co2_lef = (k_N2000/3600/100) * S*1000 * monthly_data$delta_p_p_lef
monthly_data <- cbind(monthly_data, k_N2000_co2_lef)



#### Use the k_W2014 to calculate the co2_flux of zhu####

k_W2014_co2_zhu <- NA
k_W2014_co2_zhu = (k_W2014/3600/100) * S*1000 * monthly_data$delta_p_co2_zhu
monthly_data <- cbind(monthly_data, k_W2014_co2_zhu)

#### Use the k_W2014 to calculate the co2_flux of zhui ####

k_W2014_co2_zhui <- NA
k_W2014_co2_zhui = (k_W2014/3600/100) * S*1000 * monthly_data$delta_p_co2_zhui
monthly_data <- cbind(monthly_data, k_W2014_co2_zhui)

#### Use the k_W2014 to calculate the co2_flux of lef ####

k_W2014_co2_lef <- NA
k_W2014_co2_lef = (k_W2014/3600/100) * S*1000 * monthly_data$delta_p_p_lef
monthly_data <- cbind(monthly_data, k_W2014_co2_lef)

### relationship among parameters of field data ##

#correlation of pco air and co2 flux#


library("ggpubr")

cor(monthly_data$PP_air,monthly_data$co2_flux,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "PP_air", y = "co2_flux", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PP_air(atm)", ylab = "co2 flux")

#correlation of u* and co2 flux#

cor(monthly_data$u.,monthly_data$co2_flux,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "u.", y = "co2_flux", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "u (ms-1)", ylab = "co2 flux")

#correlation of Uinsitu and co2 flux#

cor(monthly_data$U_insitu,monthly_data$co2_flux,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "U_insitu", y = "co2_flux", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Uinsitu (ms-1)", ylab = "co2 flux")



#Correlation of SST and chlor_a

install.packages("ggpubr")

library("ggpubr")

cor(monthly_data$SEA_SURFACE,monthly_data$chlor_a,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "SEA_SURFACE", y = "chlor_a", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "SEA_SURFACE(deg C)", ylab = "CHL(mg m-3)")


#Correlation of SST and PP_air

cor(monthly_data$SEA_SURFACE,monthly_data$PP_air,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "SEA_SURFACE", y = "PP_air", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "SEA_SURFACE(deg C)", ylab = "pCO2 air(atm)")


#Correlation of SST and CO2 FLUX
cor(monthly_data$SEA_SURFACE,monthly_data$co2_flux,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "SEA_SURFACE", y = "co2_flux", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "SEA_SURFACE(deg C)", ylab = "co2 flux")


#Correlation of CO2 flux & pCO2 air

cor(monthly_data$co2_flux,monthly_data$PP_air,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "co2_flux", y = "PP_air", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "co2 flux", ylab = "PP_air(atm)")




#Correlation of CO2 flux & SST

cor(monthly_data$co2_flux,monthly_data$SEA_SURFACE,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "co2_flux", y = "SEA_SURFACE", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "co2 flux", ylab = "SST(deg celcius)")



#Correlation of CO2 flux & CHL

cor(monthly_data$co2_flux,monthly_data$chlor_a,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "co2_flux", y = "chlor_a", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "co2 flux", ylab = "CHL(mg m-3)")



#Correlation of PP air & SST 

cor(monthly_data$PP_air,monthly_data$SEA_SURFACE,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "PP_air", y = "SEA_SURFACE", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PP_air(atm)", ylab = "SEA SURFACE (deg celcius)")

plot(monthly_data$PP_air,monthly_data$SEA_SURFACE)


monthly_data_cor <- cor(monthly_data[,c(21,23)]) #combine the data we need
monthly_data_cor<- round(monthly_data_cor, 4) #roundoff data

library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)

monthly_data_cor_hmisc <- rcorr(as.matrix(monthly_data_finalize[,c(21,23)]), type ="pearson")
monthly_data_cor_hmisc$n #number of observation
monthly_data_cor_hmisc_r <- monthly_data_cor_hmisc$r #correlation
monthly_data_cor_hmisc_p <- monthly_data_cor_hmisc$P #P- values

#Correlation of PP air & CHL

cor(monthly_data$PP_air,monthly_data$chlor_a,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "PP_air", y = "chlor_a", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PP_air(atm)", ylab = "CHL(mg m-3)")

plot(monthly_data$PP_air,monthly_data$chlor_a)


monthly_data_cor <- cor(monthly_data[,c(21,22)]) #combine the data we need
monthly_data_cor<- round(monthly_data_cor, 4) #roundoff data

library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)

monthly_data_cor_hmisc <- rcorr(as.matrix(monthly_data[,c(21,22)]), type ="pearson")
monthly_data_cor_hmisc$n #number of observation
monthly_data_cor_hmisc_r <- monthly_data_cor_hmisc$r #correlation
monthly_data_cor_hmisc_p <- monthly_data_cor_hmisc$P #P- values

#Correlation of PP air & CO2 flux

cor(monthly_data$PP_air,monthly_data$co2_flux,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "PP_air", y = "co2_flux", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PP_air(atm)", ylab = "co2_flux")

#Correlation of CHL & CO2 FLUX

cor(monthly_data$chlor_a,monthly_data$co2_flux,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "chlor_a", y = "co2_flux", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "CHL(mg m-3)", ylab = "co2_flux")

#Correlation of CHL & SST

cor(monthly_data$chlor_a,monthly_data$SEA_SURFACE,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "chlor_a", y = "SEA_SURFACE", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "CHL(mg m-3)", ylab = "SST (DEG CELCIUS)")

#Correlation of CHL & PP air
cor(monthly_data$chlor_a,monthly_data$PP_air,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "chlor_a", y = "PP_air", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "CHL(mg m-3)", ylab = "PP_air(atm)")


#Correlation of pco2 zhu & CO2 FLUX

cor(monthly_data$p_CO2_zhu,monthly_data$co2_flux,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "p_CO2_zhu", y = "co2_flux", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "pCO2 zhu(atm)", ylab = "co2_flux")

#Correlation of delta pco2 zhu & CO2 FLUX

cor(monthly_data$delta_p_co2_zhu,monthly_data$co2_flux,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "delta_p_co2_zhu", y = "co2_flux", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "delta pCO2 zhu(atm)", ylab = "co2_flux")



#Correlation of pco2 zhui & CO2 FLUX

cor(monthly_data$p_CO2_zhui,monthly_data$co2_flux,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "p_CO2_zhui", y = "co2_flux", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "pCO2 zhui(atm)", ylab = "co2_flux")

#Correlation of delta pco2 zhui & CO2 FLUX

cor(monthly_data$delta_p_co2_zhui,monthly_data$co2_flux,  method = "pearson", use = "complete.obs")
ggscatter(monthly_data, x = "delta_p_co2_zhui", y = "co2_flux", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "delta pCO2 zhui(atm)", ylab = "co2_flux")


write.csv(monthly_data,'monthly_data.csv')

