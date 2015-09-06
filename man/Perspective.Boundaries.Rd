\name{Perspective.Boundaries}
\alias{Perspective.Boundaries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Perspective.Boundaries(Raster, Country, Save = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Raster}{
%%     ~~Describe \code{Raster} here~~
}
  \item{Country}{
%%     ~~Describe \code{Country} here~~
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
function (Raster, Country, Save = TRUE) 
{
    Pop3D <- Raster
    Pop3D[is.na(Pop3D)] <- 0
    zData <- round(as.matrix(Pop3D), 1)
    x <- 1:nrow(zData)
    y <- 1:ncol(zData)
    nrz <- nrow(zData)
    ncz <- ncol(zData)
    if (Save == FALSE) 
        quartz(width = 12, height = 9)
    png(filename = paste(Country, "border.png", sep = "."), width = 1400, 
        height = 600, bg = "white")
    par(bg = "transparent", mar = c(4, 0, 0, 0), mai = c(0.1, 
        0.1, 0.5, 0.1))
    nbcol <- 2
    color <- c("transparent", "black")
    zfacet <- zData[-1, -1] + zData[-1, -ncz] + zData[-nrz, -1] + 
        zData[-nrz, -ncz]
    facetcol <- cut(zfacet, nbcol + 1)
    persp(x, y, z = zData, theta = 90, phi = 30, col = color[facetcol], 
        scale = FALSE, expand = 0.75, ltheta = 75, shade = 0.05, 
        border = NA, box = F, ticktype = "detailed")
    if (Save == TRUE) 
        dev.off()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
