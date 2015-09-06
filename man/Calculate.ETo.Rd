\name{Calculate.ETo}
\alias{Calculate.ETo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Calculate.ETo(Elevation, MaxTemperature, MinTemperature, MeanTemperature, Precipitation, VP, MaxRH, MinRH, Wind, SolarRad, Filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Elevation}{
%%     ~~Describe \code{Elevation} here~~
}
  \item{MaxTemperature}{
%%     ~~Describe \code{MaxTemperature} here~~
}
  \item{MinTemperature}{
%%     ~~Describe \code{MinTemperature} here~~
}
  \item{MeanTemperature}{
%%     ~~Describe \code{MeanTemperature} here~~
}
  \item{Precipitation}{
%%     ~~Describe \code{Precipitation} here~~
}
  \item{VP}{
%%     ~~Describe \code{VP} here~~
}
  \item{MaxRH}{
%%     ~~Describe \code{MaxRH} here~~
}
  \item{MinRH}{
%%     ~~Describe \code{MinRH} here~~
}
  \item{Wind}{
%%     ~~Describe \code{Wind} here~~
}
  \item{SolarRad}{
%%     ~~Describe \code{SolarRad} here~~
}
  \item{Filename}{
%%     ~~Describe \code{Filename} here~~
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
function (Elevation, MaxTemperature, MinTemperature, MeanTemperature, 
    Precipitation, VP, MaxRH, MinRH, Wind, SolarRad, Filename) 
{
    LHV <- 2.45
    Cp <- 1.013 * 10^-3
    E <- 0.622
    Alpha <- 0.23
    G <- 0
    Gsc <- 0.082
    As0 <- 0.75
    Bs0 <- 2 * 10^-5
    Sigma <- 4.903 * 10^-9
    Elev <- raster(Elevation)
    MaxTemp <- brick(MaxTemperature)
    MinTemp <- brick(MinTemperature)
    MeanTemp <- brick(MeanTemperature)
    Precip <- brick(Precipitation)
    EaPascal <- brick(VP)
    Ea <- EaPascal/1000
    MxRH <- brick(MaxRH)
    MnRH <- brick(MinRH)
    U2 <- brick(Wind)
    Sol_watts <- brick(SolarRad)
    Solar <- Sol_watts/11.6
    Lat <- raster("Lat.values.grd")
    Long <- raster("Long.values.grd")
    Julian <- brick("Julian.values.grd")
    Rasters <- list(MaxTemp, MinTemp, MeanTemp, MxRH, MnRH, U2, 
        Solar, Ea, Julian)
    print("do the rasters match?")
    print(sapply(Rasters, function(x) compareRaster(x, Elev)))
    E0Max <- calc(MaxTemp, fun = function(x) {
        0.6108 * exp(17.27 * x/(x + 273.3))
    })
    Dr <- calc(Julian, fun = function(x) {
        1 + 0.033 * cos(2 * pi/365 * x)
    })
    Theta <- calc(Julian, fun = function(x) {
        0.409 * sin((2 * pi/365 * x) - 1.39)
    })
    Lrad <- Lat * pi/180
    b <- calc(Julian, fun = function(x) {
        2 * pi * (x - 81)/364
    })
    Sc <- 0.1645 * sin(2 * b) - 0.1255 * cos(b) - 0.025 * sin(b)
    Ws <- acos(-1 * tan(Lrad) * tan(Theta))
    Gsc <- 0.082
    N <- 24/pi * Ws
    Multiply.Day <- calc(Dr, fun = function(x) {
        (24 * 60)/pi * Gsc * x
    })
    Ra <- Multiply.Day * (Ws * sin(Lrad) * sin(Theta) + cos(Lrad) * 
        cos(Theta) * sin(Ws))
    Rs0 <- (As0 + Bs0 * Elev) * Ra
    Sigma <- 4.903 * 10^-9
    KTmax <- MaxTemp + 273.16
    KTmin <- MinTemp + 273.16
    Bs0 <- Elev * 10^-5
    Rs0 <- (As0 + Bs0 * Elev) * Ra
    P <- calc(Elev, fun = function(x) {
        101.3 * ((293 - 0.0065 * (x))/293)^5.26
    })
    gamma <- Cp * P/E * LHV
    E0Max <- calc(MaxTemp, fun = function(x) {
        0.6108 * exp(17.27 * x/(x + 273.3))
    })
    E0Min <- calc(MinTemp, fun = function(x) {
        0.6108 * exp(17.27 * x/(x + 273.3))
    })
    Es <- (E0Min + E0Max)/2
    E0Mean <- calc(MeanTemp, fun = function(x) {
        0.6108 * exp(17.27 * x/(x + 273.3))
    })
    Delta <- 4098 * (E0Mean)/((MeanTemp + 273.3)^2)
    Ea1 <- (E0Min * MxRH/100 + E0Max * MnRH/100)/2
    VPD <- Es - Ea
    Sigma <- 4.903 * 10^-9
    KTmax <- MaxTemp + 273.16
    KTmin <- MinTemp + 273.16
    Rnl <- Sigma * ((KTmax^4 + KTmin^4)/2) * (0.34 - 0.14 * Ea^0.5) * 
        (1.35 * (Solar/Rs0) - 0.35)
    Rns <- calc(Solar, fun = function(x) {
        (1 - Alpha) * x
    })
    Rn <- Rns - Rnl
    Rn <- dropLayer(Rn, 366)
    Numerator <- 0.408 * Delta * (Rn - G) + gamma * 900/(MeanTemp + 
        273) * U2 * (Es - Ea)
    Denominator <- Delta + gamma * (1 + 0.34 * U2)
    ETo <- Numerator/Denominator
    ETo[ETo < 0] <- 0.001
    ETo <- mask(ETo, U2)
    writeRaster(ETo, filename = Filename, overwrite = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
