\name{Daily.Crop.Curves}
\alias{Daily.Crop.Curves}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Daily.Crop.Curves(Croplayer, StateNames, Stages, Kcb_tab, MaxHeight)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Croplayer}{
%%     ~~Describe \code{Croplayer} here~~
}
  \item{StateNames}{
%%     ~~Describe \code{StateNames} here~~
}
  \item{Stages}{
%%     ~~Describe \code{Stages} here~~
}
  \item{Kcb_tab}{
%%     ~~Describe \code{Kcb_tab} here~~
}
  \item{MaxHeight}{
%%     ~~Describe \code{MaxHeight} here~~
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
function (Croplayer, StateNames, Stages, Kcb_tab, MaxHeight) 
{
    root.depth <- read.csv("crop.roots.csv")
    Crop <- Croplayer
    if (Croplayer == "spring_barley" | Croplayer == "fall_barley") {
        Crop <- "barley"
    }
    if (Croplayer == "spring_oats" | Croplayer == "fall_oats") {
        Crop <- "oats"
    }
    if (Croplayer == "durum_wheat") {
        Crop <- "spring_wheat"
    }
    root.depth <- subset(root.depth, crop == Crop, select = c(min_rooting_depth, 
        max_rooting_depth))
    Season <- rowSums(Stages)
    Base <- lapply(Season, function(x) x = c(1:x))
    Next <- Base
    Kci <- Base
    Height.Kci <- Base
    DailyKcb <- Base
    Growth_split <- Base
    season.ID <- Base
    day_height <- Base
    MAX.Height <- Base
    One.Minus.Fc <- Base
    Roots <- Base
    Day_Roots <- Base
    for (i in 1:length(Next)) {
        B <- c(Stages[i, 1], sum(c(Stages[i, 1], Stages[i, 2])), 
            sum(c(Stages[i, 1], Stages[i, 2], Stages[i, 3])), 
            sum(Stages[i, ]))
        Xs <- c(1, B[1], mean(c(B[1], B[1], B[2])), mean(c(B[1], 
            B[2])), mean(c(B[2], B[2], B[3])), B[3], mean(c(B[3], 
            B[4])), B[4])
        Ys <- c(0.01, 0.02, Kcb_tab[[1]], Kcb_tab[[2]], sum(c(Kcb_tab[[1]], 
            Kcb_tab[[2]], Kcb_tab[[3]])), Kcb_tab[[2]], mean(c(Kcb_tab[[2]], 
            Kcb_tab[[3]])), Kcb_tab[[3]])
        P <- data.frame(bezierCurve(Xs, Ys, 500))
        Q <- P[!duplicated(round(P$x)), ]
        Q$x <- round(Q$x)
        Kci[[i]] <- round(Q$y, digits = 2)
        Growth_split[[i]] <- unlist(c(0.01, 0.015, rep(NA, times = ceiling(Stages[i, 
            1]/2) - 2), Kcb_tab[1], rep(NA, times = floor(Stages[i, 
            1]/2) - 1), rep(NA, times = Stages[i, 2] - 1), Kcb_tab[2], 
            rep(Kcb_tab[2], times = Stages[i, 3]), rep(NA, times = (Stages[i, 
                4] - 1)), Kcb_tab[3]))
        Height.Kci[[i]] <- spline(Base[[i]], Growth_split[[i]], 
            xout = Base[[i]], method = "natural", ties = mean)$y
        MAX.Height[[i]] <- max(Height.Kci[[i]])
        day_height[[i]] <- round(Height.Kci[[i]] * MaxHeight/MAX.Height[[i]], 
            2)
        Zs <- c(0.1, 0.15, mean(c(0.15, root.depth[[1]])), root.depth[[1]], 
            root.depth[[2]], sum(c(mean(c(0.15, root.depth[[1]])), 
                root.depth[[2]])), root.depth[[2]], root.depth[[1]])
        R <- data.frame(bezierCurve(Xs, Zs, 500))
        S <- R[!duplicated(round(R$x)), ]
        S$x <- round(S$x)
        Day_Roots[[i]] <- round(S$y, digits = 2)
        season.ID[[i]] <- as.factor(c(rep(1, times = Stages[i, 
            1]), rep(2, times = Stages[i, 2]), rep(3, times = Stages[i, 
            3]), rep(4, times = Stages[i, 4])))
        DailyKcb[[i]] <- as.data.frame(cbind(Base[[i]], round(Kci[[i]], 
            2), round(Day_Roots[[i]], 2), day_height[[i]], as.factor(season.ID[[i]])))
        names(DailyKcb[[i]]) <- c("Season_day", "daily_Kcb", 
            "daily_root.depth", "day_height", "season.ID")
    }
    names(DailyKcb) <- StateNames
    save(DailyKcb, file = paste0(Intermediates, "Daily.Crop.Profile.", 
        Croplayer, ".Rdata"))
    return(DailyKcb)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
