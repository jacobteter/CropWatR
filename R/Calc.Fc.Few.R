Calc.Fc.Few <-
function(Croplayer){  
  load(paste0(Intermediates, paste('KcMax', Croplayer, 'Rdata', sep = '.')))
  GR.KcMax <- lapply(KcMax, function(x) x[,(grep('layer', names(x)))])   # clip off the coordinates for analysis
  load(paste0(Intermediates, paste('CropsList', Croplayer, 'Rdata', sep = '.')))
  # str(Daily.Crops.list[[1]])
  DayHeight <- lapply(Daily.Crops.list, function(x) x$day_height)
  load(paste0(Intermediates, paste('Kcb.corrected', Croplayer, 'Rdata', sep = '.'))) # Kcb.corrected
  
  KcMin <- lapply(Kcb.corrected, function(x) c(rep(min(x)-0.01, times = length(x)))); Fc <- GR.KcMax
  for (i in 1:length(Fc)){
    for (j in 1:length(DayHeight[[i]])){
      Fc[[i]][,j] <- ((Kcb.corrected[[i]][,j] - KcMin[[i]][j])/(GR.KcMax[[i]][,j] - KcMin[[i]][j]))^(1+0.5*DayHeight[[i]][j])
    }
  }
  Few <- Fc
  Few <- lapply(Fc, function(x) 1-x[]) # This suffices for RAINFED crops
  save(Few, file = paste0(Intermediates, paste('Few', Croplayer, 'Rdata', sep = '.')))
}
