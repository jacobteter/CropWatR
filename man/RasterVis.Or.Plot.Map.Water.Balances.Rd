\name{RasterVis.Or.Plot.Map.Water.Balances}
\alias{RasterVis.Or.Plot.Map.Water.Balances}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RasterVis.Or.Plot.Map.Water.Balances(Crop, mm = FALSE, rainfed = FALSE, type = c("annual", "seasonal"), Raster.Vis = TRUE, metric = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Crop}{
%%     ~~Describe \code{Crop} here~~
}
  \item{mm}{
%%     ~~Describe \code{mm} here~~
}
  \item{rainfed}{
%%     ~~Describe \code{rainfed} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{Raster.Vis}{
%%     ~~Describe \code{Raster.Vis} here~~
}
  \item{metric}{
%%     ~~Describe \code{metric} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Crop, mm = FALSE, rainfed = FALSE, type = c("annual", 
    "seasonal"), Raster.Vis = TRUE, metric = FALSE) 
{
    if (mm == TRUE) 
        Pat <- "mm"
    if (mm == FALSE) 
        Pat <- "Total"
    if (rainfed == TRUE) 
        Irr <- "rainfed"
    if (rainfed == FALSE) 
        Irr <- "irrigated"
    Final <- brick(paste0(Intermediates, Pat, ".", type, ".WB.", 
        Irr, ".", Crop, ".grd"))
    Final[Final == 0] <- NA
    print("Final stats:")
    print(cellStats(Final, summary))
    plot(Final)
    names(Final)[which(names(Final) == "GW.Infiltration")] <- "Groundwater Infiltration"
    if (mm == FALSE) {
        if (metric == FALSE) {
            Final <- Final * 0.0032808399
            Final <- Final/10^3
            Subtitle <- "water balances in thousand acre-feet"
            Type <- "Acre-feet"
        }
        if (metric == TRUE) {
            Final <- Final * 0.000404685642
            Final <- Final/10^3
            Subtitle <- "water balances in thousand hectare-meters"
            Type <- "Hectare-meters"
        }
    }
    if (mm == TRUE) {
        Subtitle <- "Water balances in mm"
        Type <- "mm"
    }
    setwd(paste0(Path, "/CropWatR/Intermediates/"))
    if (Raster.Vis == TRUE) {
        if (mm == FALSE) {
            my.ckey <- list(labels = list(cex = 1.25), col = GnYlRdTheme$regions$col, 
                space = "left")
            Type <- "Total.WB"
        }
        if (mm == TRUE) {
            my.ckey <- list(labels = list(cex = 1.25), col = GnYlRdTheme$regions$col, 
                space = "right")
            Subtitle <- "water balances in mm"
            Type <- "mm"
        }
        png(filename = paste("Transpiration", Crop, Type, type, 
            Irr, "png", sep = "."), width = 300, height = 200)
        par(mai = c(0.1, 0.1, 0.1, 0.1), mar = c(0.1, 0.1, 0.1, 
            0.1), oma = c(0, 0, 0, 0))
        p <- levelplot(raster(Final, layer = 1), cex = 1.15, 
            border = "transparent", scales = list(draw = FALSE), 
            margin = FALSE, contour = FALSE, par.settings = GnYlRdTheme, 
            colorkey = my.ckey)
        p <- p + layer(sp.lines(SL.aeaCounties, lwd = 0.05, col = "gray"))
        p <- p + layer(sp.lines(SL.aeaStates, lwd = 0.08, col = "darkgray"))
        p <- p + layer(sp.lines(SL.aeaHuc2, lwd = 0.15, col = "black"))
        plot(p)
        dev.off()
        png(filename = paste("Evaporation", Crop, Type, type, 
            Irr, "png", sep = "."), width = 300, height = 200)
        par(mai = c(0.1, 0.1, 0.1, 0.1), mar = c(0.1, 0.1, 0.1, 
            0.1), oma = c(0, 0, 0, 0))
        p <- levelplot(raster(Final, layer = 2), cex = 1.15, 
            border = "transparent", scales = list(draw = FALSE), 
            margin = FALSE, contour = FALSE, par.settings = GnYlRdTheme, 
            colorkey = my.ckey)
        p <- p + layer(sp.lines(SL.aeaCounties, lwd = 0.05, col = "gray"))
        p <- p + layer(sp.lines(SL.aeaStates, lwd = 0.08, col = "darkgray"))
        p <- p + layer(sp.lines(SL.aeaHuc2, lwd = 0.15, col = "black"))
        plot(p)
        dev.off()
        png(filename = paste("Runoff", Crop, Type, type, Irr, 
            "png", sep = "."), width = 300, height = 200)
        par(mai = c(0.1, 0.1, 0.1, 0.1), mar = c(0.1, 0.1, 0.1, 
            0.1), oma = c(0, 0, 0, 0))
        p <- levelplot(raster(Final, layer = 3), cex = 1.15, 
            border = "transparent", scales = list(draw = FALSE), 
            margin = FALSE, contour = FALSE, par.settings = GnYlRdTheme, 
            colorkey = my.ckey)
        p <- p + layer(sp.lines(SL.aeaCounties, lwd = 0.05, col = "gray"))
        p <- p + layer(sp.lines(SL.aeaStates, lwd = 0.08, col = "darkgray"))
        p <- p + layer(sp.lines(SL.aeaHuc2, lwd = 0.15, col = "black"))
        plot(p)
        dev.off()
        png(filename = paste("GW.Infiltration", Crop, Type, type, 
            Irr, "png", sep = "."), width = 300, height = 200)
        par(mai = c(0.1, 0.1, 0.1, 0.1), mar = c(0.1, 0.1, 0.1, 
            0.1), oma = c(0, 0, 0, 0))
        p <- levelplot(raster(Final, layer = 4), cex = 1.15, 
            border = "transparent", scales = list(draw = FALSE), 
            margin = FALSE, contour = FALSE, par.settings = GnYlRdTheme, 
            colorkey = my.ckey)
        p <- p + layer(sp.lines(SL.aeaCounties, lwd = 0.05, col = "gray"))
        p <- p + layer(sp.lines(SL.aeaStates, lwd = 0.08, col = "darkgray"))
        p <- p + layer(sp.lines(SL.aeaHuc2, lwd = 0.15, col = "black"))
        plot(p)
        dev.off()
        if (rainfed == FALSE) {
            png(filename = paste("Irrigation", Crop, Type, type, 
                Irr, "png", sep = "."), width = 300, height = 200)
            par(mai = c(0.1, 0.1, 0.1, 0.1), mar = c(0.1, 0.1, 
                0.1, 0.1), oma = c(0, 0, 0, 0))
            p <- levelplot(raster(Final, layer = 5), cex = 1.15, 
                border = "transparent", scales = list(draw = FALSE), 
                margin = FALSE, contour = FALSE, par.settings = GnYlRdTheme, 
                colorkey = my.ckey)
            p <- p + layer(sp.lines(SL.aeaCounties, lwd = 0.05, 
                col = "gray"))
            p <- p + layer(sp.lines(SL.aeaStates, lwd = 0.08, 
                col = "darkgray"))
            p <- p + layer(sp.lines(SL.aeaHuc2, lwd = 0.15, col = "black"))
            plot(p)
            dev.off()
        }
    }
    if (Raster.Vis == FALSE) {
        aeaHuc2 <- shapefile("aea.HUC2.bounds.shp")
        aeaHuc2$REG_NAME <- strsplit(aeaHuc2$REG_NAME, " Region")
        pdf(filename = paste("BrickPlot", Type, type, Irr, Crop, 
            "pdf"), width = 8, height = 2)
        par(mfrow = c(1, 5), mai = c(0, 0.1, 0, 0.8), mar = c(0, 
            0.1, 0, 5.5))
        plot(raster(Final, layer = 1), axes = FALSE, box = FALSE, 
            main = "Transpiration")
        par(bg = "transparent")
        plot(aeaHuc2, border = "black", col = "transparent", 
            lwd = 0.25, add = TRUE)
        text(aeaHuc2, labels = "REG_NAME", col = "black", font = 2, 
            cex = 0.7)
        plot(raster(Final, layer = 2), axes = FALSE, box = FALSE, 
            main = "Evaporation")
        par(bg = "transparent")
        plot(aeaHuc2, border = "black", col = "transparent", 
            lwd = 0.25, add = TRUE)
        text(aeaHuc2, labels = "REG_NAME", col = "black", font = 2, 
            cex = 0.7)
        plot(raster(Final, layer = 3), axes = FALSE, box = FALSE, 
            main = "Runoff")
        par(bg = "transparent")
        plot(aeaHuc2, border = "black", col = "transparent", 
            lwd = 0.25, add = TRUE)
        text(aeaHuc2, labels = "REG_NAME", col = "black", font = 2, 
            cex = 0.7)
        plot(raster(Final, layer = 4), axes = FALSE, box = FALSE, 
            main = "Groundwater Infiltration")
        par(bg = "transparent")
        plot(aeaHuc2, border = "black", col = "transparent", 
            lwd = 0.25, add = TRUE)
        text(aeaHuc2, labels = "REG_NAME", col = "black", font = 2, 
            cex = 0.7)
        plot(raster(Final, layer = 5), axes = FALSE, box = FALSE, 
            main = "Irrigation")
        par(bg = "transparent")
        plot(aeaHuc2, border = "black", col = "transparent", 
            lwd = 0.25, add = TRUE)
        text(aeaHuc2, labels = "REG_NAME", col = "black", font = 2, 
            cex = 0.7)
        mtext(Subtitle, side = 1, outer = TRUE, line = -3, cex = 1.25)
        dev.off()
    }
    setwd(paste0(Path, "/CropWatR/Data"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
