Calculate.ETo <-
function(Elevation, MaxTemperature, MinTemperature, MeanTemperature, Precipitation, VP, MaxRH, MinRH, Wind, SolarRad, Filename){
  # Inputs are file names of the input raster files (daily weather files, elevation, etc.)
  
  ### I. Load CONSTANTS
  LHV <- 2.45  # latent heat of vaporization [MJ/kg]
  Cp <- 1.013*10^-3 # specific heat at constant pressure
  E <- 0.622 # P from above (1 millibar = 100 pascals = 0.1 kilopascals) 
  Alpha <- 0.23 # albedo or canopy reflection coefficient, which is 0.23 for the hypothetical grass reference crop [dimensionless],
  G <-0 #### 10. Soil heat flux (G) # G [MJ/m^2*day], "may be ignored for daily timesteps".
  Gsc  <- 0.0820 # Solar constant (MJ/m^2*min)
  As0 <- 0.75 # as+bs fraction of extraterrestrial radiation reaching the earth on clear days (n = N).
  Bs0 <- 2*10^-5
  Sigma <- 4.903*10^-9 # Stefan-Boltzmann constant [ 4.903 10^-9 MJ K^-4 m^-2 day^-1],
  
  ### II. Load & Mask INPUT files:
  
  # In the Daymet algorithm, spatially and temporally explicit empirical analyses of the relationships of temperature and precipitation to elevation are performed.
  Elev <- raster(Elevation) # elevation (meters)
  MaxTemp <- brick(MaxTemperature) # max temperature. degrees Celsius
  MinTemp <- brick(MinTemperature) # min temperature. degrees Celsius
  MeanTemp <- brick(MeanTemperature) # mean temperature. degrees Celsius
  Precip <- brick(Precipitation) # precipitation (mm)

  ### ADDED vapor pressure - DayMet units for VP are Pascal - need to convert to [kPa]
  EaPascal <- brick(VP) # "daily average vapor pressure" - Vapor Pressure (Ea)
  Ea <- EaPascal/1000
  
  MxRH <- brick(MaxRH) # max relative humidity. 0-100 (%) (verified) 
  MnRH <- brick(MinRH) # min relative humidity. 0-100 (%) (verified)
  
  ### Wind speed is commonly measured at 10 m height above the ground surface. 
  U2 <- brick(Wind) # wind speed @ 2 meters height, assuming conversion from 10 meters. in m/s
  
  Sol_watts <- brick(SolarRad) # length(Sol_watts[is.na(Sol_watts)]) # [1] 55339
  # Temp <- mask(Sol_watts, ETo); all.equal(Temp, Sol_watts); rm(Temp)
  Solar <- Sol_watts/11.6 # Rs - solar or shortwave radiation [MJ m-2 day-1]
  
  #### aeaMapping, Lat/Long rasters - see Appendix 1
  Lat <- raster('Lat.values.grd')
  Long <- raster('Long.values.grd')
  Julian <- brick('Julian.values.grd')
  Rasters <- list(MaxTemp, MinTemp, MeanTemp, MxRH, MnRH, U2, Solar, Ea, Julian)
  print('do the rasters match?')
  print(sapply(Rasters, function(x) compareRaster(x, Elev)))
  
  # "Julian" is the rasterBrick with the values being the Julian day - see Appendix 2
  
  ##### III. Calculate Solar Parameters
  E0Max <- calc(MaxTemp, fun=function(x) {0.6108*exp(17.27*x/(x+273.3))})
  Dr <- calc(Julian, fun=function(x) {1 + 0.033*cos(2*pi/365*x)}) # Inverse relative earth-sun distance
  Theta <- calc(Julian, fun=function(x) {0.409*sin((2*pi/365*x)-1.39)}) # Solar declination
  Lrad <- Lat*pi/180 # Latitude (radians)
  b <- calc(Julian, fun=function(x) {2*pi*(x-81)/364}) # 
  Sc <- 0.1645*sin(2*b)-0.1255*cos(b)-0.025*sin(b) # seasonal correction for solar time [hour]
  Ws <- acos(-1*tan(Lrad)*tan(Theta)) # Sunset hour angle (radians) -- equation 25

  Gsc  <- 0.0820 # Solar constant (MJ/m^2*min)
  N <- 24/pi*Ws # Daylight hours (N)
  Multiply.Day <- calc(Dr, fun=function(x) {(24*60)/pi*Gsc*x}) # first part of equation 28
  Ra <- Multiply.Day*(Ws*sin(Lrad)*sin(Theta)+cos(Lrad)*cos(Theta)*sin(Ws))
  Rs0 <- (As0+Bs0*Elev)*Ra   ###### Clear-sky radiation (Rso)
  
  #### Net longwave radiation - (detailed explanation on page 51): Rnl (MJ/meter^2*day) (Equation 39)
  Sigma <- 4.903*10^-9 # Stefan-Boltzmann constant [ 4.903 10^-9 MJ K^-4 m^-2 day^-1],
  KTmax <- MaxTemp + 273.16 # Max/Min daily temp in Kelvin:
  KTmin <- MinTemp + 273.16
  
  ###### Clear-sky radiation (Rs0)
  Bs0 <- Elev*10^-5 
  Rs0 <- (As0+Bs0*Elev)*Ra
  
  # IV. Calculate Intermediates:
  P <- calc(Elev, fun=function(x) {101.3*((293-0.0065*(x))/293)^5.26})
  gamma <- Cp*P/E*LHV
  
  E0Max <- calc(MaxTemp, fun=function(x) {0.6108*exp(17.27*x/(x+273.3))}) # as does this one
  E0Min <- calc(MinTemp, fun=function(x) {0.6108*exp(17.27*x/(x+273.3))})
  Es <- (E0Min+E0Max)/2
  E0Mean <- calc(MeanTemp, fun=function(x) {0.6108*exp(17.27*x/(x+273.3))})
  Delta <- 4098*(E0Mean)/((MeanTemp+273.3)^2)
  
  ### Use Raster of VP - need units of kPa
  Ea1 <- (E0Min*MxRH/100+E0Max*MnRH/100)/2 # Actual Vapor Pressure [kPa]
  # writeRaster(Ea1, 'Derived.VP.kPa.NCAR.min.max.RH.grd', overwrite = TRUE)
  VPD <- Es - Ea   # Vapor Pressure Deficit [kPa]

  #### Net longwave radiation - (detailed explanation on page 51): Rnl (MJ/meter^2*day) (Equation 39)
  Sigma <- 4.903*10^-9 # Stefan-Boltzmann constant [ 4.903 10^-9 MJ K^-4 m^-2 day^-1],
  KTmax <- MaxTemp + 273.16 # Max/Min daily temp in Kelvin:
  KTmin <- MinTemp + 273.16
  Rnl <- Sigma*((KTmax^4+KTmin^4)/2)*(0.34-0.14*Ea^0.5)*(1.35*(Solar/Rs0)-0.35)
  # Final product: Rnl (net longwave Radiation)
  
  # Solar radiation (Rs)
  Rns <- calc(Solar, fun=function(x) {(1-Alpha)*x}) ##### Net solar or net shortwave radiation: Rns (MJ/meter^2*day)
  Rn <- Rns - Rnl # Net longwave Radiation
  Rn <- dropLayer(Rn, 366)
  # writeRaster(Rn, 'Rn.2008.grd', overwrite = TRUE)

  ### V. Final ETo calculation steps:
  Numerator <- 0.408*Delta*(Rn-G)+gamma*900/(MeanTemp+273)*U2*(Es-Ea)
  Denominator <- Delta+gamma*(1+0.34*U2)
  ETo <- Numerator/Denominator

  ####### 13. Clean out negative ETo values:
  ETo[ETo < 0] <- 0.001 
  ETo <- mask(ETo, U2)
  writeRaster(ETo, filename = Filename, overwrite = TRUE)
  # YearAve.ETo <- calc(s, fun = mean, filename = paste0('Annual.average.ETo_2008.grd'), overwrite = TRUE)
  # YearTotal.ETo <- calc(s, fun = sum, filename = paste0('Year.total.ETo_2008.grd'), overwrite = TRUE)
}
