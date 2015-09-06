Plot.Base.Land.Use <-
function(Aggregate = FALSE){
  base <- brick('Base.Crops.LU.2008.grd')
  if (Aggregate == FALSE) Agg <- '.Sep'  

  if (Aggregate == TRUE){
    Sum <- function(x,...) {sum(x, na.rm = TRUE)}
    Base <- calc(base, Sum)
    Base[Base == 0] <- NA
    plot(Base, axes = FALSE, box = FALSE)
    base <- Base
    Agg <- '.Agg'
  }
  
  Mil.Acres <- round(cellStats(base, sum)/10^6, digits = 2)
  Names <- gsub("_", " ", names(base), fixed = TRUE)
  
  Names <- paste(Names, paste(Mil.Acres, 'million acres', sep = " "), sep = " - ")  
  Per <- base/24710.5*100 # 24710.5 acres per 100 square kilometers
  
  Subtitle <- paste0("Percentage of land cropped (million acres cropped total shown for each crop)")
  my.ckey <- list(labels = list(cex = 1.25), col=GnYlRdTheme$regions$col, space = "right")  
  MyScheme <- GnYlRdTheme
  Layout <- c(2,7)
  
  pdf(file = paste0(Intermediates, "RasterVis.rowcrops.Base", Agg, ".pdf"), width = 7, height = 14)  
  par(mar = c(0.1,0.1,0.1,0.1))
  
  p <- levelplot(Per, scales=list(draw=FALSE), contour = FALSE, sub = "", sub.cex = 1.25, par.settings=MyScheme, zscaleLog = 10, colorkey=my.ckey, 
                 layout = Layout, names.attr = Names, main = "", side=1, outer=TRUE, line=1, cex = 1.5)
  
  p <- p + layer(sp.lines(SL.aeaCRDs, lwd=0.01, col='gray'))
  p <- p + layer(sp.lines(SL.aeaStates, lwd=0.05, col='darkgrey'))
  # p <- p + layer(sp.lines(SL.aeaHuc2, lwd=0.03, col='black'))
  plot(p) 
  dev.off()
}
