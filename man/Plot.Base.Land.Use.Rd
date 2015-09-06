\name{Plot.Base.Land.Use}
\alias{Plot.Base.Land.Use}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Plot.Base.Land.Use(Aggregate = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Aggregate}{
%%     ~~Describe \code{Aggregate} here~~
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
function (Aggregate = FALSE) 
{
    base <- brick("Base.Crops.LU.2008.grd")
    if (Aggregate == FALSE) 
        Agg <- ".Sep"
    if (Aggregate == TRUE) {
        Sum <- function(x, ...) {
            sum(x, na.rm = TRUE)
        }
        Base <- calc(base, Sum)
        Base[Base == 0] <- NA
        plot(Base, axes = FALSE, box = FALSE)
        base <- Base
        Agg <- ".Agg"
    }
    Mil.Acres <- round(cellStats(base, sum)/10^6, digits = 2)
    Names <- gsub("_", " ", names(base), fixed = TRUE)
    Names <- paste(Names, paste(Mil.Acres, "million acres", sep = " "), 
        sep = " - ")
    Per <- base/24710.5 * 100
    Subtitle <- paste0("Percentage of land cropped (million acres cropped total shown for each crop)")
    my.ckey <- list(labels = list(cex = 1.25), col = GnYlRdTheme$regions$col, 
        space = "right")
    MyScheme <- GnYlRdTheme
    Layout <- c(2, 7)
    pdf(file = paste0(Intermediates, "RasterVis.rowcrops.Base", 
        Agg, ".pdf"), width = 7, height = 14)
    par(mar = c(0.1, 0.1, 0.1, 0.1))
    p <- levelplot(Per, scales = list(draw = FALSE), contour = FALSE, 
        sub = "", sub.cex = 1.25, par.settings = MyScheme, zscaleLog = 10, 
        colorkey = my.ckey, layout = Layout, names.attr = Names, 
        main = "", side = 1, outer = TRUE, line = 1, cex = 1.5)
    p <- p + layer(sp.lines(SL.aeaCRDs, lwd = 0.01, col = "gray"))
    p <- p + layer(sp.lines(SL.aeaStates, lwd = 0.05, col = "darkgrey"))
    plot(p)
    dev.off()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
