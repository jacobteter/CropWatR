\name{SuperImpose.WB.on.LU}
\alias{SuperImpose.WB.on.LU}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SuperImpose.WB.on.LU(Croplayer, rainfed = FALSE, type = c("seasonal", "annual"), Growing.Season.GW.BW = FALSE)
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
  \item{Growing.Season.GW.BW}{
%%     ~~Describe \code{Growing.Season.GW.BW} here~~
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
    Growing.Season.GW.BW = FALSE) 
{
    load("Vars.Rdata")
    Irr.Vars <- Vars[-c(3, 6, 8, 14, 15)]
    if (Growing.Season.GW.BW == TRUE) 
        class <- "BW.GW"
    if (Growing.Season.GW.BW == FALSE) 
        class <- "WB"
    if (rainfed == FALSE) 
        Irr <- "irrigated"
    if (rainfed == TRUE) 
        Irr <- "rainfed"
    if (Croplayer \%in\% Irr.Vars) {
        LU.brick <- raster(paste0(Intermediates, Croplayer, ".grd"))
        LU.brick[LU.brick == 0] <- NA
        if (rainfed == FALSE && type == "annual") 
            WB.brick <- brick(paste0(Intermediates, Croplayer, 
                ".Basic.WB.grd"))
        if (rainfed == FALSE && type == "seasonal") 
            WB.brick <- brick(paste0(Intermediates, Croplayer, 
                ".Growing.Season.WB.grd"))
        if (rainfed == TRUE && type == "annual") 
            WB.brick <- brick(paste0(Intermediates, Croplayer, 
                ".Basic.Rainfed.WB.grd"))
        if (rainfed == TRUE && type == "seasonal") 
            WB.brick <- brick(paste0(Intermediates, Croplayer, 
                ".Growing.Season.Rainfed.WB.grd"))
        if (Growing.Season.GW.BW == FALSE && type == "annual") 
            WB.brick <- brick(paste0(Intermediates, Croplayer, 
                ".Basic.WB.grd"))
        if (Growing.Season.GW.BW == TRUE && type == "seasonal") 
            WB.brick <- brick(paste0(Intermediates, Croplayer, 
                ".Growing.Season.GW.BW.WB.grd"))
        if (Growing.Season.GW.BW == TRUE && type == "seasonal" && 
            rainfed == FALSE) 
            WB.brick <- brick(paste0(Intermediates, Croplayer, 
                ".Growing.Season.BW.GW.WB.grd"))
        Names <- names(WB.brick)
        WB.brick <- extend(WB.brick, LU.brick)
        LU.brick <- extend(LU.brick, WB.brick)
        LU.brick <- extend(LU.brick, WB.brick)
        WB.brick <- calc(WB.brick, fun = function(x) replace(x, 
            x < 0, 0.001))
        WB.total <- overlay(WB.brick, LU.brick, fun = prod)
        names(WB.total) <- names(WB.brick)
        WB.total[WB.total == 0] <- NA
        LU.mm <- LU.brick
        LU.mm[LU.mm > 0] <- 1
        WB.mm <- overlay(WB.brick, LU.mm, fun = prod)
        names(WB.mm) <- names(WB.brick)
        print(paste("saving", Croplayer))
        print(cellStats(WB.total, summary))
        print(cellStats(WB.mm, summary))
        writeRaster(WB.total, filename = paste0(Intermediates, 
            "Total.", type, ".", class, ".", Irr, ".", Croplayer, 
            ".grd"), overwrite = TRUE)
        writeRaster(WB.mm, filename = paste0(Intermediates, "mm.", 
            type, ".", class, ".", Irr, ".", Croplayer, ".grd"), 
            overwrite = TRUE)
    }
    if (!(Croplayer \%in\% Irr.Vars)) {
        LU.brick <- raster(paste0(Intermediates, Croplayer, ".grd"))
        LU.brick[LU.brick == 0] <- NA
        if (rainfed == FALSE && type == "annual") 
            WB.brick <- brick(paste0(Intermediates, Croplayer, 
                ".Basic.WB.grd"))
        if (rainfed == FALSE && type == "seasonal") 
            WB.brick <- brick(paste0(Intermediates, Croplayer, 
                ".Growing.Season.WB.grd"))
        if (rainfed == TRUE && type == "annual") 
            WB.brick <- brick(paste0(Intermediates, Croplayer, 
                ".Basic.Rainfed.WB.grd"))
        if (rainfed == TRUE && type == "seasonal") 
            WB.brick <- brick(paste0(Intermediates, Croplayer, 
                ".Growing.Season.Rainfed.WB.grd"))
        if (Growing.Season.GW.BW == FALSE && type == "annual") 
            WB.brick <- brick(paste0(Intermediates, Croplayer, 
                ".Basic.WB.grd"))
        if (Growing.Season.GW.BW == TRUE && type == "seasonal") 
            WB.brick <- brick(paste0(Intermediates, Croplayer, 
                ".Growing.Season.GW.BW.WB.grd"))
        if (Growing.Season.GW.BW == TRUE && type == "seasonal" && 
            rainfed == FALSE) 
            WB.brick <- brick(paste0(Intermediates, Croplayer, 
                ".Growing.Season.BW.GW.WB.grd"))
        Names <- names(WB.brick)
        WB.brick <- extend(WB.brick, LU.brick)
        LU.brick <- extend(LU.brick, WB.brick)
        WB.brick <- calc(WB.brick, fun = function(x) replace(x, 
            x < 0, 0.001))
        WB.total <- overlay(WB.brick, LU.brick, fun = prod)
        names(WB.total) <- names(WB.brick)
        WB.total[WB.total == 0] <- NA
        LU.mm <- LU.brick
        LU.mm[LU.mm > 0] <- 1
        WB.mm <- overlay(WB.brick, LU.mm, fun = prod)
        names(WB.mm) <- names(WB.brick)
        print(paste("saving", Croplayer))
        print(cellStats(WB.total, summary))
        print(cellStats(WB.mm, summary))
        writeRaster(WB.total, filename = paste0(Intermediates, 
            "Total.", type, ".", class, ".", Irr, ".", Croplayer, 
            ".grd"), overwrite = TRUE)
        writeRaster(WB.mm, filename = paste0(Intermediates, "mm.", 
            type, ".", class, ".", Irr, ".", Croplayer, ".grd"), 
            overwrite = TRUE)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
