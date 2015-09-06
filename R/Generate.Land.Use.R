Generate.Land.Use <-
function(Croplayer, Type){
  # Types are "Total" and 'Irr.multiplier' (the later keeps the split b/w irrigated and rainfed)

  load('Vars.Rdata')
  if (Croplayer %in% Vars){
    LU.csv <- read.csv(paste0(Croplayer, '.Master.DF.2008.BAU.csv'))
    LU <- LU.csv[,c(1,2, grep("Rainfed", names(LU.csv)), grep("Irrigated", names(LU.csv)))]
    if (Type == 'Total'){
      if (length(LU) == 3) names(LU)[3] <- Croplayer
      if (length(LU) == 4){
        LU$Final <- rowSums(cbind(LU.csv[,c(grep("Rainfed", names(LU.csv)), grep("Irrigated", names(LU.csv)))]), na.rm = TRUE)
        LU <- LU[,c(1,2, grep("Final", names(LU)))]
        names(LU)[3] <- Croplayer
      }   
    }
  }   
  
  coordinates(LU) <- ~x+y
  proj4string(LU) <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  gridded(LU) = TRUE
  LU.brick <- brick(LU)
  projection(LU.brick) <- ("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  Crops.brick.2008 <- brick('cdl_10k_2008_albers.grd')
  
  # print('likely if there is an error that Crops.brick.2008 is not an object in the workspace')
  LU.brick <- extend(LU.brick, Crops.brick.2008)
  plot(LU.brick, main = Croplayer)
  LU.brick[LU.brick == 0] <- NA
  if (Type == 'Total') writeRaster(LU.brick, filename = paste0(Intermediates, Croplayer, '.grd'), overwrite = TRUE)
}
