\name{Calc.Fc.Few}
\alias{Calc.Fc.Few}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Calc.Fc.Few(Croplayer)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Croplayer}{
%%     ~~Describe \code{Croplayer} here~~
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
function (Croplayer) 
{
    load(paste0(Intermediates, paste("KcMax", Croplayer, "Rdata", 
        sep = ".")))
    GR.KcMax <- lapply(KcMax, function(x) x[, (grep("layer", 
        names(x)))])
    load(paste0(Intermediates, paste("CropsList", Croplayer, 
        "Rdata", sep = ".")))
    DayHeight <- lapply(Daily.Crops.list, function(x) x$day_height)
    load(paste0(Intermediates, paste("Kcb.corrected", Croplayer, 
        "Rdata", sep = ".")))
    KcMin <- lapply(Kcb.corrected, function(x) c(rep(min(x) - 
        0.01, times = length(x))))
    Fc <- GR.KcMax
    for (i in 1:length(Fc)) {
        for (j in 1:length(DayHeight[[i]])) {
            Fc[[i]][, j] <- ((Kcb.corrected[[i]][, j] - KcMin[[i]][j])/(GR.KcMax[[i]][, 
                j] - KcMin[[i]][j]))^(1 + 0.5 * DayHeight[[i]][j])
        }
    }
    Few <- Fc
    Few <- lapply(Fc, function(x) 1 - x[])
    save(Few, file = paste0(Intermediates, paste("Few", Croplayer, 
        "Rdata", sep = ".")))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
