Fallow.Few.Calc <-
function(Croplayer){
  # Fc - the effective fraction of soil surface covered by vegetation [0 - 0.99],
  load(paste0(Intermediates, paste('Fallow.Season', Croplayer, 'MNRH_', 'Rdata', sep = '.'))); Template <- Fallow.Season; rm(Fallow.Season)
  Template <- lapply(Template, function(x) x[,(grep('layer', names(x)))]); Fc <- Template
  
  load(paste0(Intermediates, paste('KcMax.Fallow', Croplayer, 'Rdata', sep = '.')))
  KcMax.fallow <- lapply(KcMax, function(x) x[,(grep('layer', names(x)))])
  all.equal(sapply(Template, dim), sapply(KcMax.fallow, dim))
  
  Off.season.vars <- c('winter_wheat', 'durum_wheat', 'fall_barley', 'fall_oats')
  
  if (Croplayer %in% Off.season.vars){
    KcMin <- lapply(Template, function(x) c(rep(.15, times = (length(x)))))
    DayHeight <- lapply(Template, function(x) c(rep(.15, times = (length(x)))))
    Kcb <- lapply(Template, function(x) c(rep(1, times = (length(x)))))
  }
  if (!(Croplayer %in% Off.season.vars)){
    KcMin <- lapply(Template, function(x) c(rep(.03, times = (length(x)))))
    DayHeight <- lapply(Template, function(x) c(rep(.05, times = (length(x)))))
    Kcb <- lapply(Template, function(x) c(rep(.07, times = (length(x)))))
  }      
  
  # GR.KcMax <- lapply(Template, function(x) c(rep(.16, times = (length(x)))))
  
  for (i in 1:length(Fc)){
    for (j in 1:length(DayHeight[[i]])){
      Fc[[i]][,j] <- ((Kcb[[i]][j] - KcMin[[i]][j])/(KcMax.fallow[[i]][j] - KcMin[[i]][j]))^(1+0.5*DayHeight[[i]][j])
    }
  }
  Fallow.Few <- Fc
  Fallow.Few <- lapply(Fc, function(x) 1-x[]) # This suffices for RAINFED crops  
  save(Fallow.Few, file = paste0(Intermediates, paste('Fallow.Few', Croplayer, 'Rdata', sep = '.')))
}
