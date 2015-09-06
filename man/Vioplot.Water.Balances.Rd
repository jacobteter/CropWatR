\name{Vioplot.Water.Balances}
\alias{Vioplot.Water.Balances}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Vioplot.Water.Balances(Crop, mm = TRUE, rainfed = FALSE, type = c("annual", "seasonal"), Agg.Level = "HUC2", metric = FALSE)
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
  \item{Agg.Level}{
%%     ~~Describe \code{Agg.Level} here~~
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
function (Crop, mm = TRUE, rainfed = FALSE, type = c("annual", 
    "seasonal"), Agg.Level = "HUC2", metric = FALSE) 
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
    Final[Final == 0] <- NA
    names(Final)[2] <- "Evaporation"
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
    AggShp <- shapefile(paste0("aea", Agg.Level, ".shp"))
    if (Agg.Level == "HUC2") 
        Labels <- as.character(gsub(" Region", "", AggShp$REG_NAME, 
            fixed = TRUE))
    if (Agg.Level == "States") 
        Labels <- as.character(AggShp$ATLAS_NAME)
    aea.Loc.IDs <- read.csv("aea.Loc.IDs.csv")
    d <- as.data.frame(Final)
    xy <- as.data.frame(xyFromCell(Final, 1:ncell(Final)))
    DF <- cbind(xy, d)
    DF[DF == 0] <- NA
    summary(na.omit(DF))
    print(table(cbind(DF$x, DF$y) \%in\% cbind(aea.Loc.IDs$x, aea.Loc.IDs$y)))
    print(table(cbind(aea.Loc.IDs$x, aea.Loc.IDs$y) \%in\% cbind(DF$x, 
        DF$y)))
    DF <- merge(DF, aea.Loc.IDs, by.x = c("x", "y"), by.y = c("x", 
        "y"), all.x = TRUE)
    require(vioplot)
    library(plotrix)
    Identifiers <- c("x", "y", "CountyFIPS", "STATE_FIPS", "HUC2", 
        "Abbreviation", "State_name", "Ers.region", "CRD")
    Identifiers <- Identifiers[-(which(Identifiers == Agg.Level))]
    Average <- DF[, -(which(names(DF) \%in\% Identifiers))]
    str(Average)
    Average <- Average[!is.na(Average$HUC2), ]
    summary(Average)
    Average[Average$HUC2 == "16", c(1, 3, 4)] <- Average[Average$HUC2 == 
        "16", c(1, 3, 4)] * 0.55
    Average[Average$HUC2 == "17", c(1, 3, 4)] <- Average[Average$HUC2 == 
        "17", c(1, 3, 4)] * 0.7
    Average[Average$HUC2 == "18", c(1, 3, 4)] <- Average[Average$HUC2 == 
        "18", c(1, 3, 4)] * 1.3
    Average[Average$HUC2 == "16", 5] <- Average[Average$HUC2 == 
        "16", 5] * 0.5
    Average[Average$HUC2 == "17", 5] <- Average[Average$HUC2 == 
        "17", 5] * 0.35
    Average[Average$HUC2 == "18", c(4, 5)] <- Average[Average$HUC2 == 
        "18", c(4, 5)] * 1.15
    Transp.by.HUC <- split(Average$Transpiration, as.factor(Average$HUC2))
    Tr.by.HUC <- lapply(Transp.by.HUC, function(x) na.omit(x))
    Tr.Means <- unlist(lapply(Transp.by.HUC, function(x) mean(x, 
        na.rm = TRUE) + 3 * sd(x, na.rm = TRUE)))
    Tr.Labs <- unlist(lapply(Transp.by.HUC, function(x) mean(x, 
        na.rm = TRUE) + 2 * sd(x, na.rm = TRUE)))
    Evap.by.HUC <- split(Average$Evaporation, as.factor(Average$HUC2))
    Ev.by.HUC <- lapply(Evap.by.HUC, function(x) na.omit(x))
    Runoff.by.HUC <- split(Average$Runoff, as.factor(Average$HUC2))
    Ro.by.HUC <- lapply(Runoff.by.HUC, function(x) na.omit(x))
    GW.Inf.by.HUC <- split(Average$Groundwater.Infiltration, 
        as.factor(Average$HUC2))
    GW.by.HUC <- lapply(GW.Inf.by.HUC, function(x) na.omit(x))
    Irr.Inf.by.HUC <- split(Average$Irrigation, as.factor(Average$HUC2))
    Irr.by.HUC <- lapply(Irr.Inf.by.HUC, function(x) na.omit(x))
    Irr.Means <- unlist(lapply(Irr.by.HUC, function(x) mean(x, 
        na.rm = TRUE) + 3 * sd(x, na.rm = TRUE)))
    addAlpha <- function(colors, alpha = 1) {
        r <- col2rgb(colors, alpha = T)
        r[4, ] <- alpha * 255
        r <- r/255
        return(rgb(r[1, ], r[2, ], r[3, ], r[4, ]))
    }
    BlueT <- addAlpha("blue", alpha = 0.5)
    GreenT <- addAlpha("green", alpha = 0.5)
    RedT <- addAlpha("red", alpha = 0.75)
    x <- c(0:18)
    y <- c(0:18)
    Max <- max(Tr.Means) + 150
    target.scale <- c(0, Max, na.rm = TRUE)
    y <- rescale(y, target.scale)
    setwd(paste0(Path, "/CropWatR/Intermediates/"))
    bmp(filename = paste("ViolinPlot.Water.Regions", Crop, type, 
        Irr, Type, "WB.bmp", sep = "."), width = 1300, height = 700)
    par(mai = c(3, 0.5, 0.2, 0.2), mar = c(3, 3, 2, 2), oma = c(1, 
        1, 1, 1))
    plot(x, y, col = "transparent", frame.plot = FALSE, xlab = "", 
        xaxt = "n")
    vioplot(GW.by.HUC[[1]], GW.by.HUC[[2]], GW.by.HUC[[3]], GW.by.HUC[[4]], 
        GW.by.HUC[[5]], GW.by.HUC[[6]], GW.by.HUC[[7]], GW.by.HUC[[8]], 
        GW.by.HUC[[9]], GW.by.HUC[[10]], GW.by.HUC[[11]], GW.by.HUC[[12]], 
        GW.by.HUC[[13]], GW.by.HUC[[14]], GW.by.HUC[[15]], GW.by.HUC[[16]], 
        GW.by.HUC[[17]], GW.by.HUC[[18]], col = "transparent", 
        border = rgb(8, 160, 255, max = 255), rectCol = "black", 
        ylim = c(0, 6), add = TRUE)
    vioplot(Ev.by.HUC[[1]], Ev.by.HUC[[2]], Ev.by.HUC[[3]], Ev.by.HUC[[4]], 
        Ev.by.HUC[[5]], Ev.by.HUC[[6]], Ev.by.HUC[[7]], Ev.by.HUC[[8]], 
        Ev.by.HUC[[9]], Ev.by.HUC[[10]], Ev.by.HUC[[11]], Ev.by.HUC[[12]], 
        Ev.by.HUC[[13]], Ev.by.HUC[[14]], Ev.by.HUC[[15]], Ev.by.HUC[[16]], 
        Ev.by.HUC[[17]], Ev.by.HUC[[18]], col = RedT, rectCol = "black", 
        ylim = c(0, 6), add = TRUE)
    vioplot(Irr.by.HUC[[1]], Irr.by.HUC[[2]], Irr.by.HUC[[3]], 
        Irr.by.HUC[[4]], Irr.by.HUC[[5]], Irr.by.HUC[[6]], Irr.by.HUC[[7]], 
        Irr.by.HUC[[8]], Irr.by.HUC[[9]], Irr.by.HUC[[10]], Irr.by.HUC[[11]], 
        Irr.by.HUC[[12]], Irr.by.HUC[[13]], Irr.by.HUC[[14]], 
        Irr.by.HUC[[15]], Irr.by.HUC[[16]], Irr.by.HUC[[17]], 
        Irr.by.HUC[[18]], col = BlueT, border = rgb(180, 16, 
            25, max = 255), rectCol = "black", ylim = c(0, 6), 
        add = TRUE)
    vioplot(Ro.by.HUC[[1]], Ro.by.HUC[[2]], Ro.by.HUC[[3]], Ro.by.HUC[[4]], 
        Ro.by.HUC[[5]], Ro.by.HUC[[6]], Ro.by.HUC[[7]], Ro.by.HUC[[8]], 
        Ro.by.HUC[[9]], Ro.by.HUC[[10]], Ro.by.HUC[[11]], Ro.by.HUC[[12]], 
        Ro.by.HUC[[13]], Ro.by.HUC[[14]], Ro.by.HUC[[15]], Ro.by.HUC[[16]], 
        Ro.by.HUC[[17]], Ro.by.HUC[[18]], col = "transparent", 
        border = "brown", rectCol = "black", ylim = c(0, 6), 
        add = TRUE)
    vioplot(Tr.by.HUC[[1]], Tr.by.HUC[[2]], Tr.by.HUC[[3]], Tr.by.HUC[[4]], 
        Tr.by.HUC[[5]], Tr.by.HUC[[6]], Tr.by.HUC[[7]], Tr.by.HUC[[8]], 
        Tr.by.HUC[[9]], Tr.by.HUC[[10]], Tr.by.HUC[[11]], Tr.by.HUC[[12]], 
        Tr.by.HUC[[13]], Tr.by.HUC[[14]], Tr.by.HUC[[15]], Tr.by.HUC[[16]], 
        Tr.by.HUC[[17]], Tr.by.HUC[[18]], col = GreenT, rectCol = "red", 
        ylim = c(0, 6), add = TRUE)
    colfill <- c(GreenT, RedT, "transparent", "transparent", 
        BlueT)
    colbord <- c("black", "black", "brown", rgb(8, 160, 255, 
        max = 255), rgb(180, 16, 25, max = 255))
    Labs <- c("transpiration", "evaporation", "runoff", "groundwater infiltration", 
        "irrigation")
    par(srt = 0)
    title(paste("Annual water balances (mm) for", Crop, sep = " "), 
        cex.main = 1.75)
    par(srt = 30)
    text(x = c(1:18), y = c(Tr.Labs) + 100, labels = c(Labels), 
        cex = 1.5)
    dev.off()
    setwd(paste0(Path, "/CropWatR/Data"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
