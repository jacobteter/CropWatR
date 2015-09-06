\name{Rescale.And.Save}
\alias{Rescale.And.Save}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Rescale.And.Save(Variable, PH, DataList, Croplayer, Kcb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Variable}{
%%     ~~Describe \code{Variable} here~~
}
  \item{PH}{
%%     ~~Describe \code{PH} here~~
}
  \item{DataList}{
%%     ~~Describe \code{DataList} here~~
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
function (Variable, PH, DataList, Croplayer, Kcb) 
{
    PH.season.breaks <- subset(PH, select = Initial:Late)
    PH[, which(names(PH) == "Initial"):which(names(PH) == "Late")] <- Rescale.Season(PH.season.breaks, 
        PH$Growing_Season)
    deleteCols <- c("Crop", "Total", "Region", "Plant_Date")
    PH <- PH[, -(which(names(PH) \%in\% deleteCols))]
    Daily.Crops.list <- Daily.Crop.Curves(Croplayer, PH$State_Fips, 
        PH[, which(names(PH) == "Initial"):which(names(PH) == 
            "Late")], Kcb[, 2:4], Kcb[, 5])
    Daily.Crops.list <- Daily.Crops.list[order(names(Daily.Crops.list))]
    Growing.Season <- DataList$Growing.Season
    Fallow.Season <- DataList$Fallow.Season
    Growing.Season <- Growing.Season[order(names(Growing.Season))]
    Fallow.Season <- Fallow.Season[order(names(Fallow.Season))]
    print("names equal?:")
    print(all.equal(names(Growing.Season), names(Daily.Crops.list)))
    print(all.equal(names(Growing.Season), names(Fallow.Season)))
    print("layer lengths equal?: (expect a \"no\"")
    print(all.equal(lapply(Growing.Season, function(x) length(grep("layer", 
        names(x)))), lapply(Daily.Crops.list, nrow)))
    print(cbind(as.numeric(names(Growing.Season)), sapply(Growing.Season, 
        function(x) length(grep("layer", names(x)))), sapply(Daily.Crops.list, 
        nrow)))
    for (i in 1:length(Growing.Season)) {
        while (length(grep("layer", names(Growing.Season[[i]]))) > 
            nrow(Daily.Crops.list[[i]])) {
            Growing.Season[[i]] <- Growing.Season[[i]][, -1]
            Fallow.Season[[i]] <- cbind(Growing.Season[[i]][, 
                1], Fallow.Season[[i]])
        }
    }
    print("layer lengths equal?:")
    print(all.equal(lapply(Growing.Season, function(x) length(grep("layer", 
        names(x)))), lapply(Daily.Crops.list, nrow)))
    save(Growing.Season, file = paste0(Intermediates, paste("Growing.Season", 
        Croplayer, Variable, "Rdata", sep = ".")))
    save(Fallow.Season, file = paste0(Intermediates, paste("Fallow.Season", 
        Croplayer, Variable, "Rdata", sep = ".")))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
