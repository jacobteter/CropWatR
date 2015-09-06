Calc.Basal.Crop.Coeff <-
function(Croplayer){
  load(paste0(Intermediates, paste('Growing.Season', Croplayer, 'U2.final_', 'Rdata', sep = '.'))); U2 <- Growing.Season; rm(Growing.Season)
  load(paste0(Intermediates, paste('Growing.Season', Croplayer, 'MNRH_', 'Rdata', sep = '.'))); RHmin <- Growing.Season; rm(Growing.Season)
  load(paste0(Intermediates,paste('CropsList', Croplayer, 'Rdata', sep = '.')))
  # check x, y coordinates listing of the two files: # all.equal(lapply(U2, function(x) dim(x)), lapply(RHmin, function(x) dim(x)))
  all.equal(lapply(U2, function(x) x[,c('x','y')]), lapply(RHmin, function(x) x[,c('x','y')]))
  all.equal(names(U2), names(Daily.Crops.list))
  
  GS.U2 <- lapply(U2, function(x) x[,(grep('layer', names(x)))])
  GS.RHmin <- lapply(RHmin, function(x) x[,(grep('layer', names(x)))])
  
  sapply(GS.U2, dim)
  sapply(GS.RHmin, dim)
  all.equal(sapply(GS.U2, dim), sapply(GS.RHmin, dim))
  
  ### Subset only mid and late growing season
  Wind_term <- lapply(GS.U2, function(x) 0.04*(x[]-2))
  RH_term <- lapply(GS.RHmin, function(x) 0.004*(x[]-45))
  all.equal(lapply(Wind_term, function(x) c(x$x, x$y)), lapply(RH_term, function(x) c(x$x, x$y)))
  
  Max.season.heights <- lapply(Daily.Crops.list, function(x) tapply(x$day_height, x$season.ID, max))
  Season.IDs <- lapply(Daily.Crops.list, function(x) x$season.ID); Plant_heights <- Season.IDs
  for (i in 1:length(Max.season.heights)){
    Plant_heights[[i]] <- Max.season.heights[[i]][match(Season.IDs[[i]], names(Max.season.heights[[i]]))]
  }
  Kcb <- lapply(Daily.Crops.list, function(x) x$daily_Kcb) # dim(Kcb[[1]]); length(Kcb[[1]])
  print('done plant heights')
  height_term <- lapply(Plant_heights, function(x) (x[]/3)^0.3)
  Kcb.corrected <- Wind_term
  summary(Kcb.corrected[[1]])
  for (i in 1:length(Wind_term)){
    for (j in 1:length(height_term[[i]])){
      Kcb.corrected[[i]][,j] <- Kcb[[i]][j] + (Wind_term[[i]][,j] - RH_term[[i]][,j]) * height_term[[i]][j]
    }
  }
  print('done correction term')
  Mid.Late.Season.cuts <- lapply(Daily.Crops.list, function(x) which(x$season.ID == 3 | x$season.ID == 4))
  
  Before <- Kcb.corrected
  for (i in 1:length(Kcb.corrected)){ # lots of looping, so it's slow. It takes: ~30 seconds
    for (j in 1:length(Kcb.corrected[[i]])){
      Kcb.corrected[[i]][,j][which(GS.RHmin[[i]][,j] > 20 & GS.RHmin[[i]][,j] < 80)] <- Kcb[[i]][j]
      Kcb.corrected[[i]][,j][which(GS.U2[[i]][,j] > 1 & GS.U2[[i]][,j] < 6)] <- Kcb[[i]][j]
    }
  }
  all.equal(Before, Kcb.corrected) # not equal, but minor differences, as is should be.
  all.equal(lapply(Before, function(x) dim(x)), lapply(Kcb.corrected, function(x) dim(x)))
  all.equal(names(Before), names(Kcb.corrected))
  save(Kcb.corrected, file = paste0(Intermediates, paste('Kcb.corrected', Croplayer, 'Rdata', sep = '.'))) 
}
