KcMAX <-
function(Croplayer){
  
  
  # Load wind speed (meters/hr at 2 meters height)
  load(paste0(Intermediates, paste('Growing.Season', Croplayer, 'U2.final_', 'Rdata', sep = '.'))); U2 <- Growing.Season; rm(Growing.Season)
  # load minimum relative humidity
  load(paste0(Intermediates, paste('Growing.Season', Croplayer, 'MNRH_', 'Rdata', sep = '.'))); RHmin <- Growing.Season; rm(Growing.Season)
  # load scaled crop parameters
  load(paste0(Intermediates, paste('CropsList', Croplayer, 'Rdata', sep = '.')))
  
  # check x, y coordinates listing of the two files:
  all.equal(lapply(U2, function(x) dim(x)), lapply(RHmin, function(x) dim(x)))
  all.equal(lapply(U2, function(x) x[,c('x','y')]), lapply(RHmin, function(x) x[,c('x','y')]))
  all.equal(names(U2), names(Daily.Crops.list))
  
  GS.U2 <- lapply(U2, function(x) x[,(grep('layer', names(x)))])
  GS.RHmin <- lapply(RHmin, function(x) x[,(grep('layer', names(x)))])
  all.equal(lapply(GS.U2, function(x) dim(x)), lapply(GS.RHmin, function(x) dim(x)))
  
  Wind_term <- lapply(GS.U2, function(x) 0.04*(x[]-2))
  RH_term <- lapply(GS.RHmin, function(x) 0.004*(x[]-45))
  all.equal(lapply(Wind_term, function(x) dim(x)), lapply(RH_term, function(x) dim(x)))
  Max.season.heights <- lapply(Daily.Crops.list, function(x) tapply(x$day_height, x$season.ID, max))
  Season.IDs <- lapply(Daily.Crops.list, function(x) x$season.ID); Plant_heights <- Season.IDs
  
  for (i in 1:length(Max.season.heights)){
    Plant_heights[[i]] <- Max.season.heights[[i]][match(Season.IDs[[i]], names(Max.season.heights[[i]]))]
  }
  
  height_term <- lapply(Plant_heights, function(x) (x[]/3)^0.3)
  
  Term1 <- Wind_term
  
  for (i in 1:length(Wind_term)){
    for (j in 1:length(height_term[[i]])){
      Term1[[i]][,j] <- 1.2 + (Wind_term[[i]][,j] - RH_term[[i]][,j]) * height_term[[i]][j]
    }
  }
  
  Kcb <- lapply(Daily.Crops.list, function(x) x$daily_Kcb)
  
  KcMax <- Term1
  
  for (i in 1:length(Term1)){
    for (j in 1:length(Kcb[[i]])){
      KcMax[[i]][,j] <- max(Term1[[i]][,j], Kcb[[i]][j] + 0.05)
    }
  }
  
  for (i in 1:length(Term1)){
    KcMax[[i]] <- cbind(KcMax[[i]], U2[[i]]$x, U2[[i]]$y)
    names(KcMax[[i]])[c(length(KcMax[[i]])-1,length(KcMax[[i]]))] <- c('x', 'y')
  }
  
  save(KcMax, file = paste0(Intermediates, paste('KcMax', Croplayer,'Rdata', sep = '.')))
}
