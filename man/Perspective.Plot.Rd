\name{Perspective.Plot}
\alias{Perspective.Plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Perspective.Plot(Raster, Country, ColorScheme = list("terrain", "heat", "topo", "cm"), log = TRUE, Save = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Raster}{
%%     ~~Describe \code{Raster} here~~
}
  \item{Country}{
%%     ~~Describe \code{Country} here~~
}
  \item{ColorScheme}{
%%     ~~Describe \code{ColorScheme} here~~
}
  \item{log}{
%%     ~~Describe \code{log} here~~
}
  \item{Save}{
%%     ~~Describe \code{Save} here~~
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
function (Raster, Country, ColorScheme = list("terrain", "heat", 
    "topo", "cm"), log = TRUE, Save = TRUE) 
{
    Pop <- Raster
    if (log == TRUE) {
        Log <- "log.scale"
        setBasePop <- function(x) {
            x[x > 0] <- x[x > 0] + 1
            x[x <= 0] <- NA
            x[x > 0] <- log2(x)
            return(x)
        }
    }
    if (log == FALSE) {
        Log <- "linear.scale"
        setBasePop <- function(x) {
            x[x <= 0] <- NA
            x[x > 0] <- x[x > 0] * 0.001 + 0.1
            return(x)
        }
    }
    Pop3D <- calc(Pop, setBasePop)
    Pop3D[is.na(Pop3D)] <- 0
    zData <- round(as.matrix(Pop3D), 1)
    x <- 1:nrow(zData)
    y <- 1:ncol(zData)
    nrz <- nrow(zData)
    ncz <- ncol(zData)
    if (Save == FALSE) 
        quartz(width = 12, height = 9)
    DissDir <- "/Users/jacobteter/Desktop/Dissertation/"
    png(filename = paste0(DissDir, paste(Country, ColorScheme, 
        Log, "png", sep = ".")), width = 1400, height = 600, 
        bg = "white")
    par(bg = "transparent", mar = c(4, 0, 0, 0), mai = c(0.1, 
        0.1, 0.5, 0.1))
    nbcol <- 120
    if (ColorScheme == "heat") 
        Pal <- rev(heat.colors(nbcol))[20:120]
    if (ColorScheme == "terrain") 
        Pal <- terrain.colors(nbcol)
    if (ColorScheme == "topo") 
        Pal <- topo.colors(nbcol)
    if (ColorScheme == "cm") 
        Pal <- cm.colors(nbcol)
    color <- c("transparent", Pal)
    zfacet <- zData[-1, -1] + zData[-1, -ncz] + zData[-nrz, -1] + 
        zData[-nrz, -ncz]
    facetcol <- cut(zfacet, nbcol + 1)
    persp(x, y, z = zData, theta = 90, phi = 30, col = color[facetcol], 
        scale = FALSE, expand = 0.75, ltheta = 75, shade = 0.05, 
        border = NA, box = F, ticktype = "detailed")
    Scale <- gsub(".", " ", Log, fixed = TRUE)
    if (Save == TRUE) 
        dev.off()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
