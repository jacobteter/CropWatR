\name{Sum.Save.Water.Balances}
\alias{Sum.Save.Water.Balances}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Sum.Save.Water.Balances(Croplayer, rainfed = FALSE, type = c("seasonal", "annual"), BW.GW = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Croplayer}{
%%     ~~Describe \code{Croplayer} here~~
}
  \item{rainfed}{
%%     ~~Describe \code{rainfed} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{BW.GW}{
%%     ~~Describe \code{BW.GW} here~~
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
function (Croplayer, rainfed = FALSE, type = c("seasonal", "annual"), 
    BW.GW = FALSE) 
{
    setwd(paste0(Path, "/CropWatR/Intermediates/"))
    if (rainfed == FALSE && type == "annual") {
        load(paste("Preseason_Deep.Percolation", Croplayer, "Rdata", 
            sep = "."))
        load(paste("Growing.Season_Deep.Percolation", Croplayer, 
            "Rdata", sep = "."))
        Post.DP <- local(get(load(paste("Postseason_Deep.Percolation", 
            Croplayer, "Rdata", sep = "."))))
        Pre.GW.Infiltration <- unlist(lapply(Pre.DP, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        GS.GW.Infiltration <- unlist(lapply(DP, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Post.GW.Infiltration <- unlist(lapply(Post.DP, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        GW.Infiltration <- rowSums(as.data.frame(cbind(Pre.GW.Infiltration, 
            GS.GW.Infiltration, Post.GW.Infiltration)))
        print(paste("Infiltration Summary for", Croplayer))
        print(c(summary(Pre.GW.Infiltration), summary(GS.GW.Infiltration), 
            summary(Post.GW.Infiltration), summary(GW.Infiltration)))
        load(paste("Preseason_Soil.Evaporation", Croplayer, "Rdata", 
            sep = "."))
        load(paste("Growing.Season_Soil.Evaporation", Croplayer, 
            "Rdata", sep = "."))
        Post.KeETo <- local(get(load(paste("Postseason_Soil.Evaporation", 
            Croplayer, "Rdata", sep = "."))))
        Pre.Evap <- unlist(lapply(Pre.KeETo, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Evap <- unlist(lapply(E, function(x) rowSums(x[, (grep("layer", 
            names(x)))])))
        Post.Evap <- unlist(lapply(Post.KeETo, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Evaporation <- rowSums(as.data.frame(cbind(Pre.Evap, 
            Evap, Post.Evap)))
        print(paste("Evaporation Summary for", Croplayer))
        load(paste("Preseason_Weed.Transpiration", Croplayer, 
            "Rdata", sep = "."))
        Pre.weed.Kcb <- unlist(lapply(Pre.Kcb.tot, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Post.Kcb.tot <- local(get(load(paste("Postseason_Weed.Transpiration", 
            Croplayer, "Rdata", sep = "."))))
        Post.weed.Kcb <- unlist(lapply(Post.Kcb.tot, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Evapor.Fallow.Transpir <- rowSums(as.data.frame(cbind(Pre.Evap, 
            Pre.weed.Kcb, Evap, Post.Evap, Post.weed.Kcb)))
        print(c(summary(Pre.Evap), summary(Evap), summary(Post.Evap), 
            summary(Evaporation)))
        load(paste("Preseason_Runoff", Croplayer, "Rdata", sep = "."))
        load(paste("Growing.Season_Runoff", Croplayer, "Rdata", 
            sep = "."))
        Post.ROi <- local(get(load(paste("Postseason_Runoff", 
            Croplayer, "Rdata", sep = "."))))
        Pre.runoff <- unlist(lapply(Pre.ROi, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        runoff <- unlist(lapply(ROi, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Post.runoff <- unlist(lapply(Post.ROi, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Runoff <- rowSums(as.data.frame(cbind(Pre.runoff, runoff, 
            Post.runoff)))
        print(paste("Runoff Summary for", Croplayer))
        print(c(summary(Pre.runoff), summary(runoff), summary(Post.runoff), 
            summary(Runoff)))
        load(file = paste("Growing.Season_Transpiration", Croplayer, 
            "Rdata", sep = "."))
        Transpiration <- unlist(lapply(Transp.final, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        print(paste("Transpiration Summary for", Croplayer))
        print(summary(Transpiration))
        load(paste("Preseason_Weed.Transpiration", Croplayer, 
            "Rdata", sep = "."))
        Post.Kcb.tot <- local(get(load(paste("Postseason_Weed.Transpiration", 
            Croplayer, "Rdata", sep = "."))))
        Pre.weed.Kcb <- unlist(lapply(Pre.Kcb.tot, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Post.weed.Kcb <- unlist(lapply(Post.Kcb.tot, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Fallow.Transpiration <- rowSums(as.data.frame(cbind(Pre.weed.Kcb, 
            Post.weed.Kcb)))
        print(paste("Weed Evaporation Summary for", Croplayer))
        print(c(summary(Pre.weed.Kcb), summary(Post.weed.Kcb), 
            summary(Fallow.Transpiration)))
        load(file = paste("Growing.Season_Irrigation", Croplayer, 
            "Rdata", sep = "."))
        print(paste("Irrigation Summary for", Croplayer))
        Irrigation <- unlist(lapply(Irr, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Irrigate <- Irrigation
        Irrigate[Irrigate == 0] <- NA
        print(summary(Irrigate))
        load(paste("BASE", Croplayer, "MNRH_", "MasterDF2", sep = "."))
        IDs.1 <- as.numeric(rownames(DF2))
        Coords <- cbind(DF2$x, DF2$y)
        Coords <- as.data.frame(cbind(IDs.1, Coords))
        IDs.2 <- as.numeric(unlist(lapply(Pre.DP, function(x) rownames(x))))
        table(IDs.2 \%in\% IDs.1)
        Water.Balance <- as.data.frame(cbind(IDs.2, Transpiration, 
            Evapor.Fallow.Transpir, Runoff, GW.Infiltration, 
            Irrigation))
    }
    if (rainfed == TRUE && type == "annual") {
        load(paste("Preseason_Deep.Percolation", Croplayer, "Rdata", 
            sep = "."))
        load(paste("Growing.Season.Rainfed_Deep.Percolation", 
            Croplayer, "Rdata", sep = "."))
        Post.DP <- local(get(load(paste("Postseason_Deep.Percolation", 
            Croplayer, "Rdata", sep = "."))))
        Pre.GW.Infiltration <- unlist(lapply(Pre.DP, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        GS.GW.Infiltration <- unlist(lapply(DP, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Post.GW.Infiltration <- unlist(lapply(Post.DP, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        GW.Infiltration <- rowSums(as.data.frame(cbind(Pre.GW.Infiltration, 
            GS.GW.Infiltration, Post.GW.Infiltration)))
        print(paste("Infiltration Summary for", Croplayer))
        print(c(summary(Pre.GW.Infiltration), summary(GS.GW.Infiltration), 
            summary(Post.GW.Infiltration), summary(GW.Infiltration)))
        load(paste("Preseason_Soil.Evaporation", Croplayer, "Rdata", 
            sep = "."))
        load(paste("Growing.Season.Rainfed_Soil.Evaporation", 
            Croplayer, "Rdata", sep = "."))
        Post.KeETo <- local(get(load(paste("Postseason_Soil.Evaporation", 
            Croplayer, "Rdata", sep = "."))))
        Pre.Evap <- unlist(lapply(Pre.KeETo, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Evap <- unlist(lapply(E, function(x) rowSums(x[, (grep("layer", 
            names(x)))])))
        Post.Evap <- unlist(lapply(Post.KeETo, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        load(paste("Preseason_Weed.Transpiration", Croplayer, 
            "Rdata", sep = "."))
        Pre.weed.Kcb <- unlist(lapply(Pre.Kcb.tot, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Post.Kcb.tot <- local(get(load(paste("Postseason_Weed.Transpiration", 
            Croplayer, "Rdata", sep = "."))))
        Post.weed.Kcb <- unlist(lapply(Post.Kcb.tot, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Evapor.Fallow.Transpir <- rowSums(as.data.frame(cbind(Pre.Evap, 
            Pre.weed.Kcb, Evap, Post.Evap, Post.weed.Kcb)))
        Evaporation <- rowSums(as.data.frame(cbind(Pre.Evap, 
            Evap, Post.Evap)))
        print(paste("Evaporation Summary for", Croplayer))
        print(c(summary(Pre.Evap), summary(Evap), summary(Post.Evap), 
            summary(Evaporation)))
        load(paste("Preseason_Runoff", Croplayer, "Rdata", sep = "."))
        load(paste("Growing.Season.Rainfed_Runoff", Croplayer, 
            "Rdata", sep = "."))
        Post.ROi <- local(get(load(paste("Postseason_Runoff", 
            Croplayer, "Rdata", sep = "."))))
        Pre.runoff <- unlist(lapply(Pre.ROi, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        runoff <- unlist(lapply(ROi, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Post.runoff <- unlist(lapply(Post.ROi, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Runoff <- rowSums(as.data.frame(cbind(Pre.runoff, runoff, 
            Post.runoff)))
        print(paste("Runoff Summary for", Croplayer))
        print(c(summary(Pre.runoff), summary(runoff), summary(Post.runoff), 
            summary(Runoff)))
        load(file = paste("Growing.Season.Rainfed_Transpiration", 
            Croplayer, "Rdata", sep = "."))
        Transpiration <- unlist(lapply(Transp.final, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        print(paste("Transpiration Summary for", Croplayer))
        print(summary(Transpiration))
        load(paste("Preseason_Weed.Transpiration", Croplayer, 
            "Rdata", sep = "."))
        Post.Kcb.tot <- local(get(load(paste("Postseason_Weed.Transpiration", 
            Croplayer, "Rdata", sep = "."))))
        Pre.weed.Kcb <- unlist(lapply(Pre.Kcb.tot, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Post.weed.Kcb <- unlist(lapply(Post.Kcb.tot, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Fallow.Transpiration <- rowSums(as.data.frame(cbind(Pre.weed.Kcb, 
            Post.weed.Kcb)))
        print(paste("Weed Transpiration Summary for", Croplayer))
        print(c(summary(Pre.weed.Kcb), summary(Post.weed.Kcb), 
            summary(Fallow.Transpiration)))
        load(paste("BASE", Croplayer, "MNRH_", "MasterDF2", sep = "."))
        IDs.1 <- as.numeric(rownames(DF2))
        Coords <- cbind(DF2$x, DF2$y)
        Coords <- as.data.frame(cbind(IDs.1, Coords))
        IDs.2 <- as.numeric(unlist(lapply(Pre.DP, function(x) rownames(x))))
        table(IDs.2 \%in\% IDs.1)
        Water.Balance <- as.data.frame(cbind(IDs.2, Transpiration, 
            Evapor.Fallow.Transpir, Runoff, GW.Infiltration))
    }
    if (rainfed == FALSE && type == "seasonal") {
        load(paste("Growing.Season_Deep.Percolation", Croplayer, 
            "Rdata", sep = "."))
        GS.GW.Infiltration <- unlist(lapply(DP, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        print(paste("Infiltration Summary for", Croplayer))
        print(summary(GS.GW.Infiltration))
        load(paste("Growing.Season_Soil.Evaporation", Croplayer, 
            "Rdata", sep = "."))
        Evap <- unlist(lapply(E, function(x) rowSums(x[, (grep("layer", 
            names(x)))])))
        print(paste("Evaporation Summary for", Croplayer))
        print(summary(Evap))
        load(paste("Growing.Season_Runoff", Croplayer, "Rdata", 
            sep = "."))
        runoff <- unlist(lapply(ROi, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        print(paste("Runoff Summary for", Croplayer))
        print(summary(runoff))
        load(file = paste("Growing.Season_Transpiration", Croplayer, 
            "Rdata", sep = "."))
        Transpiration <- unlist(lapply(Transp.final, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        print(paste("Transpiration Summary for", Croplayer))
        print(summary(Transpiration))
        load(file = paste("Growing.Season_Irrigation", Croplayer, 
            "Rdata", sep = "."))
        print(paste("Irrigation Summary for", Croplayer))
        Irrigation <- unlist(lapply(Irr, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        Irrigate <- Irrigation
        Irrigate[Irrigate == 0] <- NA
        print(summary(Irrigate))
        load(paste("BASE", Croplayer, "MNRH_", "MasterDF2", sep = "."))
        IDs.1 <- as.numeric(rownames(DF2))
        Coords <- cbind(DF2$x, DF2$y)
        Coords <- as.data.frame(cbind(IDs.1, Coords))
        IDs.2 <- as.numeric(unlist(lapply(DP, function(x) rownames(x))))
        table(IDs.2 \%in\% IDs.1)
        if (BW.GW == FALSE) 
            Water.Balance <- as.data.frame(cbind(IDs.2, Transpiration, 
                Evap, runoff, GS.GW.Infiltration, Irrigation))
        if (BW.GW == TRUE) {
            GreenWater <- Transpiration + Evap
            BlueWater <- Irrigation
            Water.Balance <- as.data.frame(cbind(IDs.2, GreenWater, 
                BlueWater))
        }
    }
    if (rainfed == TRUE && type == "seasonal") {
        load(paste("Growing.Season.Rainfed_Deep.Percolation", 
            Croplayer, "Rdata", sep = "."))
        GS.GW.Infiltration <- unlist(lapply(DP, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        print(paste("Infiltration Summary for", Croplayer))
        print(summary(GS.GW.Infiltration))
        load(paste("Growing.Season.Rainfed_Soil.Evaporation", 
            Croplayer, "Rdata", sep = "."))
        Evap <- unlist(lapply(E, function(x) rowSums(x[, (grep("layer", 
            names(x)))])))
        print(paste("Evaporation Summary for", Croplayer))
        print(summary(Evap))
        load(paste("Growing.Season.Rainfed_Runoff", Croplayer, 
            "Rdata", sep = "."))
        runoff <- unlist(lapply(ROi, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        print(paste("Runoff Summary for", Croplayer))
        print(summary(runoff))
        load(file = paste("Growing.Season.Rainfed_Transpiration", 
            Croplayer, "Rdata", sep = "."))
        Transpiration <- unlist(lapply(Transp.final, function(x) rowSums(x[, 
            (grep("layer", names(x)))])))
        print(paste("Transpiration Summary for", Croplayer))
        print(summary(Transpiration))
        load(paste("BASE", Croplayer, "MNRH_", "MasterDF2", sep = "."))
        IDs.1 <- as.numeric(rownames(DF2))
        Coords <- cbind(DF2$x, DF2$y)
        Coords <- as.data.frame(cbind(IDs.1, Coords))
        IDs.2 <- as.numeric(unlist(lapply(DP, function(x) rownames(x))))
        table(IDs.2 \%in\% IDs.1)
        if (BW.GW == FALSE) 
            Water.Balance <- as.data.frame(cbind(IDs.2, Transpiration, 
                Evap, runoff, GS.GW.Infiltration))
        if (BW.GW == TRUE) {
            GreenWater <- Transpiration + Evap
            BlueWater <- Irrigation
            Water.Balance <- as.data.frame(cbind(IDs.2, GreenWater, 
                BlueWater))
        }
    }
    print(table(Coords$IDs.1 \%in\% Water.Balance$IDs.2))
    Water.Balance <- merge(Coords, Water.Balance, by.x = "IDs.1", 
        by.y = "IDs.2")
    names(Water.Balance)[1:3] <- c("IDs", "x", "y")
    Water.Balance[Water.Balance == 0] <- NA
    Water.Balance <- Water.Balance[, -1]
    coordinates(Water.Balance) <- ~x + y
    proj4string(Water.Balance) <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    gridded(Water.Balance) = TRUE
    WB.brick <- brick(Water.Balance)
    projection(WB.brick) <- ("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    Crops.brick.2008 <- brick("../Data/cdl_10k_2008_albers.grd")
    WB.brick <- extend(WB.brick, Crops.brick.2008)
    plot(WB.brick)
    print(cellStats(WB.brick, summary))
    if (rainfed == FALSE && type == "annual") 
        writeRaster(WB.brick, filename = paste(Croplayer, "Basic.WB.grd", 
            sep = "."), overwrite = TRUE)
    if (rainfed == FALSE && type == "seasonal") 
        writeRaster(WB.brick, filename = paste(Croplayer, "Growing.Season.WB.grd", 
            sep = "."), overwrite = TRUE)
    if (rainfed == TRUE && type == "annual") 
        writeRaster(WB.brick, filename = paste(Croplayer, "Basic.Rainfed.WB.grd", 
            sep = "."), overwrite = TRUE)
    if (rainfed == TRUE && type == "seasonal") 
        writeRaster(WB.brick, filename = paste(Croplayer, "Growing.Season.Rainfed.WB.grd", 
            sep = "."), overwrite = TRUE)
    if (BW.GW == FALSE && type == "annual") 
        writeRaster(WB.brick, filename = paste(Croplayer, "Growing.Season.WB.grd", 
            sep = "."), overwrite = TRUE)
    if (BW.GW == TRUE && type == "seasonal") 
        writeRaster(WB.brick, filename = paste(Croplayer, "Growing.Season.GW.BW.WB.grd", 
            sep = "."), overwrite = TRUE)
    if (BW.GW == TRUE && type != "seasonal") 
        print("Blue / Green water volumes only relevant for the growing season")
    setwd(paste0(Path, "/CropWatR/Data"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
