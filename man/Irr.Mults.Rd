\name{Irr.Mults}
\alias{Irr.Mults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Irr.Mults(Crop)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Crop}{
%%     ~~Describe \code{Crop} here~~
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
function (Crop) 
{
    survey <- read.csv("acre-feet.per.acre.csv")
    Sub <- survey[c(1, which(names(survey) \%in\% Crop))]
    Sub <- Sub[complete.cases(Sub), ]
    Mean <- mean(Sub[, 2])
    Sub$Mult <- round(Sub[, 2]/Mean, digits = 3)
    Sub$State <- factor(Sub$State)
    return(Sub)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
