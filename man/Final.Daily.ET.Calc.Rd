\name{Final.Daily.ET.Calc}
\alias{Final.Daily.ET.Calc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Final.Daily.ET.Calc(Croplayer)
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
    if (file.exists(paste0(Intermediates, paste("Preseason_Weed.Transpiration", 
        Croplayer, "Rdata", sep = "."))) == FALSE) {
        Fallow.Preseason.Daily.ET.Calc(Croplayer)
    }
    if (file.exists(paste0(Intermediates, paste("Postseason_Weed.Transpiration", 
        Croplayer, "Rdata", sep = "."))) && file.exists(paste0(Intermediates, 
        paste("Growing.Season_Runoff", Croplayer, "Rdata", sep = ".")))) {
        print(paste("Daily ETo calculation completed for", Croplayer))
    }
    if ((file.exists(paste0(Intermediates, paste("Postseason_Weed.Transpiration", 
        Croplayer, "Rdata", sep = "."))) && file.exists(paste0(Intermediates, 
        paste("Growing.Season_Runoff", Croplayer, "Rdata", sep = ".")))) == 
        FALSE) {
        if (file.exists(paste0(Intermediates, paste("Growing.Season_Transpiration", 
            Croplayer, "Rdata", sep = "."))) == FALSE) {
            load("Vars.Rdata")
            if (Croplayer \%in\% Vars || Croplayer == "silage") {
                Main.Growing.Season.Daily.ET.Calc(Croplayer)
                Main.Rainfed.Growing.Season.Daily.ET.Calc(Croplayer)
            }
            Others <- c("switchgrass", "miscanthus", "idle_cropland", 
                "pasture_grass")
            if (Croplayer \%in\% Others) {
                Main.Growing.Season.Daily.ET.Calc(Croplayer)
            }
        }
        if (file.exists(paste0(Intermediates, paste("Postseason_Soil.Water.Balance", 
            Croplayer, "Rdata", sep = "."))) == FALSE) {
            Fallow.Postseason.Daily.ET.Calc(Croplayer)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
