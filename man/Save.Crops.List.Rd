\name{Save.Crops.List}
\alias{Save.Crops.List}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Save.Crops.List(PH, Croplayer, Kcb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{PH}{
%%     ~~Describe \code{PH} here~~
}
  \item{Croplayer}{
%%     ~~Describe \code{Croplayer} here~~
}
  \item{Kcb}{
%%     ~~Describe \code{Kcb} here~~
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
function (PH, Croplayer, Kcb) 
{
    PH.season.breaks <- subset(PH, select = Initial:Late)
    PH[, which(names(PH) == "Initial"):which(names(PH) == "Late")] <- Rescale.Season(PH.season.breaks, 
        PH$Growing_Season)
    deleteCols <- c("Crop", "Total", "Region", "Plant_Date")
    PH <- PH[, -(which(names(PH) \%in\% deleteCols))]
    stages <- PH[, which(names(PH) == "Initial"):which(names(PH) == 
        "Late")]
    Daily.Crops.list <- Daily.Crop.Curves(Croplayer, PH$State_Fips, 
        stages, Kcb[, 2:4], Kcb[, 5])
    Daily.Crops.list <- Daily.Crops.list[order(names(Daily.Crops.list))]
    save(Daily.Crops.list, file = paste0(Intermediates, paste("CropsList", 
        Croplayer, "Rdata", sep = ".")))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
