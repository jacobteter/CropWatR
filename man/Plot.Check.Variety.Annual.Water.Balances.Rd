\name{Plot.Check.Variety.Annual.Water.Balances}
\alias{Plot.Check.Variety.Annual.Water.Balances}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Plot.Check.Variety.Annual.Water.Balances(Variety)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Variety}{
%%     ~~Describe \code{Variety} here~~
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
function (Variety) 
{
    if (Variety != "barley" && Variety != "oats" && Variety != 
        "wheat") {
        Irr <- raster(brick(paste0("Base.WBs.2008/Irrigated.mm.", 
            Variety, ".grd")), layer = 5)
    }
    if (Variety == "fall_barley" || Variety == "fall_oats") {
        stop(paste(Variety, "isn't irrigated"))
    }
    if (Variety == "spring_wheat" || Variety == "winter_wheat") {
        SW.Irr <- raster(brick("Base.WBs.2008/Irrigated.mm.spring_wheat.grd"), 
            layer = 5)
        WW.Irr <- raster(brick("Base.WBs.2008/Irrigated.mm.winter_wheat.grd"), 
            layer = 5)
        Irr <- mosaic(SW.Irr, WW.Irr, fun = mean)
    }
    Irr <- Irr * 0.0032808399
    survey <- read.csv("acre-feet.per.acre.csv")
    Crop <- Variety
    if (Variety == "spring_barley") 
        Crop <- "barley"
    if (Variety == "spring_oats") 
        Crop <- "oats"
    if (Variety == "spring_wheat" || Variety == "winter_wheat") 
        Crop <- "wheat"
    if (Variety == "silage") 
        Crop <- "corn"
    aeaStates <- shapefile("aeaStates.shp")
    head(aeaStates@data)
    setMethod("merge", signature(x = "Spatial", y = "data.frame"), 
        function(x, y, by = intersect(names(x), names(y)), by.x = by, 
            by.y = by, all.x = TRUE, suffixes = c(".x", ".y"), 
            incomparables = NULL, ...) {
            if (!"data" \%in\% slotNames(x)) {
                stop("x has no data.frame")
            }
            d <- x@data
            d$donotusethisvariablename976 <- 1:nrow(d)
            y <- unique(y)
            i <- apply(y[, by.y, drop = FALSE], 1, paste) \%in\% 
                apply(x@data[, by.x, drop = FALSE], 1, paste)
            y <- y[i, , drop = FALSE]
            if (isTRUE(any(table(y[, by.y]) > 1))) {
                stop("'y' has multiple records for one or more 'by.y' key(s)")
            }
            if (!all.x) {
                y$donotusethisvariablename679 <- 1
            }
            d <- merge(d, y, by = by, by.x = by.x, by.y = by.y, 
                suffixes = suffixes, incomparables = incomparables, 
                all.x = TRUE, all.y = FALSE)
            d <- d[order(d$donotusethisvariablename976), ]
            d$donotusethisvariablename976 <- NULL
            rownames(d) <- row.names(x)
            x@data <- d
            if (!all.x) {
                x <- x[!is.na(x@data$donotusethisvariablename679), 
                  , drop = FALSE]
                x@data$donotusethisvariablename679 <- NULL
            }
            x
        })
    Surveyed <- survey[, c(1, which(names(survey) == Crop))]
    aeaStates <- merge(aeaStates, Surveyed, by.x = "ATLAS_NAME", 
        by.y = "State")
    MeanModelled <- extract(Irr, aeaStates, fun = function(x) mean(x, 
        na.rm = TRUE))
    aeaStates$Modelled <- round(MeanModelled, digits = 1)
    aeaStates$Modelled[is.nan(aeaStates$Modelled)] <- NA
    aeaStates@data[, 10][is.na(aeaStates$Modelled)] <- NA
    par(mar = c(0.2, 0.2, 0.2, 0.2))
    plot(Irr, axes = FALSE, box = FALSE, col = rev(heat.colors(255)), 
        alpha = 0.75)
    plot(aeaStates, add = TRUE)
    text(aeaStates, labels = Crop, cex = 1, col = "blue", adj = c(0.5, 
        0))
    text(aeaStates, labels = "Modelled", cex = 1, col = "red", 
        pos = 1, adj = c(0, -0.5))
    d <- density(na.omit((survey[, (names(survey) == Crop)])))
    f <- density(na.omit(getValues(Irr)))
    e <- density(na.omit(MeanModelled))
    plot(e, xlim = c(0, 6.2), ylab = "", xlab = "", main = "")
    mtext("acre-feet per acre", side = 1, line = 2)
    polygon(d, col = "transparent", border = "blue")
    polygon(e, col = "transparent", border = "red2")
    Lbls <- c("State survey", "Modelled State averages")
    colfill <- c("blue", "red2")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
