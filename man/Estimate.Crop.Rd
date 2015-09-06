\name{Estimate.Crop}
\alias{Estimate.Crop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Estimate.Crop(crop)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{crop}{
%%     ~~Describe \code{crop} here~~
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
function (crop) 
{
    Subset.Growth.Season(crop)
    print(paste("Seasons subsetted and rescaled for", crop))
    Daily.Crop.Parameters(crop)
    print(paste("Daily crop parameters estimated for", crop))
    Final.Daily.ET.Calc(crop)
    print(paste("Daily crop water balance estimated for", crop))
    Sum.Save.Daily.Evapotranspiration(crop, rainfed = TRUE)
    Sum.Save.Daily.Evapotranspiration(crop, rainfed = FALSE)
    print(paste("Daily ET rasters made for", crop))
    Sum.Save.Water.Balances(crop, rainfed = FALSE, type = "seasonal", 
        BW.GW = FALSE)
    Sum.Save.Water.Balances(crop, rainfed = FALSE, type = "annual", 
        BW.GW = FALSE)
    Sum.Save.Water.Balances(crop, rainfed = TRUE, type = "seasonal", 
        BW.GW = FALSE)
    Sum.Save.Water.Balances(crop, rainfed = TRUE, type = "annual", 
        BW.GW = FALSE)
    Sum.Save.Water.Balances(crop, rainfed = FALSE, type = "seasonal", 
        BW.GW = TRUE)
    Generate.Land.Use(crop)
    print(paste("land use raster generated for", crop))
    print(paste("Annual and seasonal water balance rasters saved for", 
        crop))
    SuperImpose.WB.on.LU(crop, rainfed = FALSE, type = "seasonal", 
        Growing.Season.GW.BW = FALSE)
    SuperImpose.WB.on.LU(crop, rainfed = FALSE, type = "annual", 
        Growing.Season.GW.BW = FALSE)
    SuperImpose.WB.on.LU(crop, rainfed = TRUE, type = "seasonal", 
        Growing.Season.GW.BW = FALSE)
    SuperImpose.WB.on.LU(crop, rainfed = TRUE, type = "annual", 
        Growing.Season.GW.BW = FALSE)
    SuperImpose.WB.on.LU(crop, rainfed = FALSE, type = "seasonal", 
        Growing.Season.GW.BW = TRUE)
    print(paste("Water balances superimposed on land use for", 
        crop))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
