\name{Rescale.Season}
\alias{Rescale.Season}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Rescale.Season(Stages, Season.length)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Stages}{
%%     ~~Describe \code{Stages} here~~
}
  \item{Season.length}{
%%     ~~Describe \code{Season.length} here~~
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
function (Stages, Season.length) 
{
    Season <- rowSums(Stages)
    Scalor <- Season.length/Season
    Rescale <- c(floor(Stages[, 1] * Scalor), ceiling(Stages[, 
        2] * Scalor), floor(Stages[, 3] * Scalor), ceiling(Stages[, 
        4] * Scalor))
    Rescale <- matrix(Rescale, ncol = 4)
    return(Rescale)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
