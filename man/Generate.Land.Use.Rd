\name{Generate.Land.Use}
\alias{Generate.Land.Use}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Generate.Land.Use(Croplayer, Type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Croplayer}{
%%     ~~Describe \code{Croplayer} here~~
}
  \item{Type}{
%%     ~~Describe \code{Type} here~~
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
function (Croplayer, Type) 
{
    load("Vars.Rdata")
    if (Croplayer \%in\% Vars) {
        LU.csv <- read.csv(paste0(Croplayer, ".Master.DF.2008.BAU.csv"))
        LU <- LU.csv[, c(1, 2, grep("Rainfed", names(LU.csv)), 
            grep("Irrigated", names(LU.csv)))]
        if (Type == "Total") {
            if (length(LU) == 3) 
                names(LU)[3] <- Croplayer
            if (length(LU) == 4) {
                LU$Final <- rowSums(cbind(LU.csv[, c(grep("Rainfed", 
                  names(LU.csv)), grep("Irrigated", names(LU.csv)))]), 
                  na.rm = TRUE)
                LU <- LU[, c(1, 2, grep("Final", names(LU)))]
                names(LU)[3] <- Croplayer
            }
        }
    }
    coordinates(LU) <- ~x + y
    proj4string(LU) <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    gridded(LU) = TRUE
    LU.brick <- brick(LU)
    projection(LU.brick) <- ("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    Crops.brick.2008 <- brick("cdl_10k_2008_albers.grd")
    LU.brick <- extend(LU.brick, Crops.brick.2008)
    plot(LU.brick, main = Croplayer)
    LU.brick[LU.brick == 0] <- NA
    if (Type == "Total") 
        writeRaster(LU.brick, filename = paste0(Intermediates, 
            Croplayer, ".grd"), overwrite = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
