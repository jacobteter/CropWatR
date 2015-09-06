Plot.Check.Variety.Annual.Water.Balances <-
function(Variety){
  if (Variety != 'barley' && Variety != 'oats' && Variety != 'wheat'){
    Irr <- raster(brick(paste0('Base.WBs.2008/Irrigated.mm.', Variety,'.grd')), layer = 5)    
  }
  if (Variety == 'fall_barley' || Variety == 'fall_oats'){
    stop(paste(Variety, "isn't irrigated"))
  }
  if (Variety == 'spring_wheat' || Variety == 'winter_wheat'){
    SW.Irr <- raster(brick('Base.WBs.2008/Irrigated.mm.spring_wheat.grd'), layer = 5)  
    WW.Irr <- raster(brick('Base.WBs.2008/Irrigated.mm.winter_wheat.grd'), layer = 5)
    Irr <- mosaic(SW.Irr, WW.Irr, fun = mean)
  }
  
  ## Irrigation checks:
  # 1 acre millimeter = 0.0032808399 acre foot
  Irr <- Irr*0.0032808399
  survey <- read.csv('acre-feet.per.acre.csv')
  
  Crop <- Variety
  if (Variety == 'spring_barley') Crop <- 'barley'
  if (Variety == 'spring_oats') Crop <- 'oats'
  if (Variety == 'spring_wheat' || Variety == 'winter_wheat') Crop <- 'wheat'
  if (Variety == 'silage') Crop <- 'corn'
  
  aeaStates <- shapefile('aeaStates.shp')
  head(aeaStates@data)
  
  # Robert's spatial merge:
  setMethod('merge', signature(x='Spatial', y='data.frame'), function(x, y, by=intersect(names(x), names(y)), by.x=by, by.y=by, all.x=TRUE, suffixes = c(".x",".y"), incomparables = NULL, ...) {
    if (!'data' %in% slotNames(x)) {
      stop('x has no data.frame')
    }
    d <- x@data
    d$donotusethisvariablename976 <- 1:nrow(d)
    
    y <- unique(y)
    i <- apply(y[, by.y, drop=FALSE], 1, paste) %in% apply(x@data[, by.x, drop=FALSE], 1, paste)
    y <- y[i, ,drop=FALSE]
    if (isTRUE(any(table(y[, by.y]) > 1))) {
      stop("'y' has multiple records for one or more 'by.y' key(s)")
    }
    
    if (!all.x) {
      y$donotusethisvariablename679 <- 1
    }
    
    d <- merge(d, y, by=by, by.x=by.x, by.y=by.y, suffixes=suffixes, incomparables=incomparables, all.x=TRUE, all.y=FALSE)
    d <- d[order(d$donotusethisvariablename976), ]
    d$donotusethisvariablename976 <- NULL
    rownames(d) <- row.names(x)
    x@data <- d
    
    if (! all.x ) {
      x <- x[!is.na(x@data$donotusethisvariablename679),  ,drop=FALSE] 
      x@data$donotusethisvariablename679 <- NULL
    }
    x
  } 
  )
  
  Surveyed <- survey[,c(1, which(names(survey) == Crop))]
  aeaStates <- merge(aeaStates, Surveyed, by.x = 'ATLAS_NAME', by.y = 'State')
  
  MeanModelled <- extract(Irr, aeaStates, fun = function(x) mean(x, na.rm = TRUE))
  aeaStates$Modelled <- round(MeanModelled, digits = 1)
  aeaStates$Modelled[is.nan(aeaStates$Modelled)] <- NA
  aeaStates@data[,10][is.na(aeaStates$Modelled)] <- NA
    
  # pdf(file = paste0("Irr.Map.", Variety, ".pdf"), width = 11, height = 7)
  par(mar = c(0.2, 0.2, 0.2, 0.2))
  # plot(Irr, axes = FALSE, box = FALSE, col = rev(heat.colors(255)), alpha = 0.75) # , main = paste('\n\nAverage acre-feet per acre, by state, for', Variety, '\nRed values are modelled, Blue are FRIS survey values'), cex.main = 0.85)
  plot(Irr, axes = FALSE, box = FALSE, col = rev(heat.colors(255)), alpha = 0.75)
  plot(aeaStates, add = TRUE)
  text(aeaStates, labels = Crop, cex = 1, col = 'blue', adj = c(0.5, 0))
  text(aeaStates, labels = 'Modelled', cex = 1, col = 'red', pos = 1,  adj = c(0, -0.5))
  # dev.off()
  
  d <- density(na.omit((survey[,(names(survey) == Crop)])))
  f <- density(na.omit(getValues(Irr)))
  e <- density(na.omit(MeanModelled))
  # png(paste0(Variety, '_irrigation.png'), width = 900, height = 600)
  plot(e, xlim = c(0, 6.2), ylab = '', xlab = '', main = "") # , xlim = c(0, 5), main = paste('Statistical vs. modelled distribution of irrigation for', Variety, "in acre-feet per acre")
  mtext('acre-feet per acre', side = 1, line = 2)
  polygon(d, col="transparent", border="blue")
  polygon(e, col="transparent", border="red2")
  # polygon(f, col="transparent", border="green")
  
  Lbls <- c("State survey", "Modelled State averages")   # Lbls <- c("Modelled State averages", "Modelled Full Distribution", "State survey")
  colfill<-c('blue', 'red2')   # colfill<-c('red', 'red2','blue')
  # legend(2.9, 1.1, Lbls, fill=colfill, cex = 1.5)
  # dev.off()  
}
