KcMAX.fallow <-
function(Croplayer){
  load(paste0(Intermediates, paste('Fallow.Season', Croplayer, 'U2.final_','Rdata', sep = '.'))); U2 <- Fallow.Season; rm(Fallow.Season)
  load(paste0(Intermediates, paste('Fallow.Season', Croplayer, 'MNRH_','Rdata', sep = '.'))); RHmin <- Fallow.Season; rm(Fallow.Season)
  load(paste0(Intermediates, paste('CropsList', Croplayer, 'Rdata', sep = '.')))
  
  # check x, y coordinates listing of the two files:
  all.equal(lapply(U2, function(x) dim(x)), lapply(RHmin, function(x) dim(x)))
  all.equal(lapply(U2, function(x) x[,c('x','y')]), lapply(RHmin, function(x) x[,c('x','y')]))
  
  FS.U2 <- lapply(U2, function(x) x[,(grep('layer', names(x)))])
  FS.RHmin <- lapply(RHmin, function(x) x[,(grep('layer', names(x)))])
  Wind_term <- lapply(FS.U2, function(x) 0.04*(x[]-2))
  RH_term <- lapply(FS.RHmin, function(x) 0.004*(x[]-45))
  all.equal(lapply(Wind_term, function(x) c(x$x, x$y)), lapply(RH_term, function(x) c(x$x, x$y)))  
  # if (tilled == TRUE){
  Max.season.heights <- lapply(Daily.Crops.list, function(x) tapply(x$day_height, x$season.ID, function(x) max(x)*.08))
  
  Kcb <- lapply(RH_term, function(x) x[1,])
  Kcb <- lapply(Kcb, function(x) replace(x[], 1:length(x[]), 0))  
  DaysRow <- lapply(RH_term, function(x) as.numeric(gsub('layer.', '', names(x))))
  Cuts <- lapply(DaysRow, function(x) which(diff(x) > 1))
  
  Season.IDs <- lapply(Kcb, function(x) replace(x[], 1:length(x[]), 4)); Plant_heights <- Season.IDs
  for (i in 1:length(Cuts)){
    if (length(Cuts[[i]]) > 0){
      Season.IDs[[i]][,1: round(Cuts[[i]]*3/4) ] <- 3
      Season.IDs[[i]][,1: round(Cuts[[i]]*1/2) ] <- 2
      Season.IDs[[i]][,1: round(Cuts[[i]]*1/4) ] <- 1
      
      Season.IDs[[i]][, Cuts[[i]]: floor(((length(Season.IDs[[i]]) - Cuts[[i]])*3/4) + Cuts[[i]]) ] <- 3
      Season.IDs[[i]][, Cuts[[i]]: floor(((length(Season.IDs[[i]]) - Cuts[[i]])*1/2) + Cuts[[i]]) ] <- 2
      Season.IDs[[i]][, Cuts[[i]]: floor(((length(Season.IDs[[i]]) - Cuts[[i]])*1/4) + Cuts[[i]]) ] <- 1
      
      Plant_heights[[i]] <- Max.season.heights[[i]][match(Season.IDs[[i]], names(Max.season.heights[[i]]))]
      
    }
    if (length(Cuts[[i]]) == 0){
      Season.IDs[[i]][,1: round(length(Season.IDs[[i]])*3/4) ] <- 3
      Season.IDs[[i]][,1: round(length(Season.IDs[[i]])*1/2) ] <- 2
      Season.IDs[[i]][,1: round(length(Season.IDs[[i]])*1/4) ] <- 1
    }
  }
  
  ### For tilled & untilled land,...what should Kcb be?
  if (length(Season.IDs[[i]]) > 3){
    # Kcb.values <- lapply(Max.season.heights, function(x) replace(x[], c(1, 2, 3, 4), c(0.1, 0.2, .7, .7))) 
    Kcb.values <- lapply(Max.season.heights, function(x) replace(x[], c(1, 2, 3, 4), c(0.1, 0.2, .3, .2))) 
    Kcb[[i]] <- Kcb[[i]][match(Season.IDs[[i]], names(Kcb.values[[i]]))]
  }
  if (length(Season.IDs[[i]]) <= 3){
    Kcb[[i]] <- as.data.frame(t(rep(.1, times = length(Season.IDs[[i]]))))
    names(Kcb[[i]]) <- names(Season.IDs[[i]])     }
  
  height_term <- lapply(Plant_heights, function(x) (x[]/3)^0.3)
  Term1 <- Wind_term
  summary(Term1[[1]])
  ## The loop is slow in R: this takes about 10 seconds
  for (i in 1:length(Wind_term)){
    for (j in 1:length(height_term[[i]])){
      Off.season.vars <- c('winter_wheat', 'durum_wheat', 'fall_barley', 'fall_oats')
      if (Croplayer %in% Off.season.vars){
        Term1[[i]][,j] <- 1 + (Wind_term[[i]][,j] - RH_term[[i]][,j]) * height_term[[i]][j]
      }
      if (!(Croplayer %in% Off.season.vars)){
        # 'weed' Kcb set to 0.15, not 1.2
        Term1[[i]][,j] <- 0.15 + (Wind_term[[i]][,j] - RH_term[[i]][,j]) * height_term[[i]][j] 
      }      
    }
  }
  
  KcMax <- Term1
  for (i in 1:length(Term1)){
    for (j in 1:length(Kcb[[i]])){
      KcMax[[i]][,j] <- pmax.int(Term1[[i]][,j], Kcb[[i]][,j] + 0.05) ## NOTE THAT THE CORRECT FUNCTION pmax.int
    }
  }
  for (i in 1:length(Term1)){
    KcMax[[i]] <- cbind(KcMax[[i]], U2[[i]]$x, U2[[i]]$y)
    names(KcMax[[i]])[c(length(KcMax[[i]])-1,length(KcMax[[i]]))] <- c('x', 'y')
  }
  summary(KcMax[[1]])
  save(KcMax, file = paste0(Intermediates, paste('KcMax.Fallow', Croplayer, 'Rdata', sep = '.')))
}
