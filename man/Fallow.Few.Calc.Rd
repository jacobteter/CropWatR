\name{Fallow.Few.Calc}
\alias{Fallow.Few.Calc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Fallow.Few.Calc(Croplayer)
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
    load(paste0(Intermediates, paste("Fallow.Season", Croplayer, 
        "MNRH_", "Rdata", sep = ".")))
    Template <- Fallow.Season
    rm(Fallow.Season)
    Template <- lapply(Template, function(x) x[, (grep("layer", 
        names(x)))])
    Fc <- Template
    load(paste0(Intermediates, paste("KcMax.Fallow", Croplayer, 
        "Rdata", sep = ".")))
    KcMax.fallow <- lapply(KcMax, function(x) x[, (grep("layer", 
        names(x)))])
    all.equal(sapply(Template, dim), sapply(KcMax.fallow, dim))
    Off.season.vars <- c("winter_wheat", "durum_wheat", "fall_barley", 
        "fall_oats")
    if (Croplayer \%in\% Off.season.vars) {
        KcMin <- lapply(Template, function(x) c(rep(0.15, times = (length(x)))))
        DayHeight <- lapply(Template, function(x) c(rep(0.15, 
            times = (length(x)))))
        Kcb <- lapply(Template, function(x) c(rep(1, times = (length(x)))))
    }
    if (!(Croplayer \%in\% Off.season.vars)) {
        KcMin <- lapply(Template, function(x) c(rep(0.03, times = (length(x)))))
        DayHeight <- lapply(Template, function(x) c(rep(0.05, 
            times = (length(x)))))
        Kcb <- lapply(Template, function(x) c(rep(0.07, times = (length(x)))))
    }
    for (i in 1:length(Fc)) {
        for (j in 1:length(DayHeight[[i]])) {
            Fc[[i]][, j] <- ((Kcb[[i]][j] - KcMin[[i]][j])/(KcMax.fallow[[i]][j] - 
                KcMin[[i]][j]))^(1 + 0.5 * DayHeight[[i]][j])
        }
    }
    Fallow.Few <- Fc
    Fallow.Few <- lapply(Fc, function(x) 1 - x[])
    save(Fallow.Few, file = paste0(Intermediates, paste("Fallow.Few", 
        Croplayer, "Rdata", sep = ".")))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
