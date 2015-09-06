\name{Split.Seasons}
\alias{Split.Seasons}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Split.Seasons(Crop, Variable, Lat.long, TopSoil, Crop.Layers, PH)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Crop}{
%%     ~~Describe \code{Crop} here~~
}
  \item{Variable}{
%%     ~~Describe \code{Variable} here~~
}
  \item{Lat.long}{
%%     ~~Describe \code{Lat.long} here~~
}
  \item{TopSoil}{
%%     ~~Describe \code{TopSoil} here~~
}
  \item{Crop.Layers}{
%%     ~~Describe \code{Crop.Layers} here~~
}
  \item{PH}{
%%     ~~Describe \code{PH} here~~
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
function (Crop, Variable, Lat.long, TopSoil, Crop.Layers, PH) 
{
    if (Variable != "Precip_") 
        RasterBrick <- brick(paste0(Variable, "2008.grd"))
    aea.Loc.IDs <- read.csv("aea.Loc.IDs.csv")
    if (Variable == "Precip_") 
        RasterBrick <- brick("Prism.ppt.10km.aea.grd")
    DF <- as.data.frame(getValues(RasterBrick))
    DF <- cbind(DF, Lat.long)
    DF <- na.omit(DF)
    print("BEPAM growing pixels in aea.Loc.IDs:")
    print(table(c(DF$x, DF$y) \%in\% c(aea.Loc.IDs$x, aea.Loc.IDs$y)))
    DF <- merge(DF, aea.Loc.IDs, by.x = c("x", "y"), by.y = c("x", 
        "y"), all = TRUE)
    print("BEPAM growing pixels in TopSoil:")
    print(table(c(DF$x, DF$y) \%in\% c(TopSoil$x, TopSoil$y)))
    DF <- merge(DF, TopSoil, by.x = c("x", "y"), by.y = c("x", 
        "y"), all = TRUE)
    print(table(DF$STATE_FIPS \%in\% PH$State_Fips))
    print(unique(DF$State_name[which(!(DF$STATE_FIPS \%in\% PH$State_Fips))]))
    DF <- merge(DF, PH, by.x = "STATE_FIPS", by.y = "State_Fips", 
        all.x = TRUE)
    print(unique(DF$State_name[which(!(DF$STATE_FIPS \%in\% Crop.Layers$STATE_FIPS))]))
    Droppers <- c("CountyFIPS", "HUC2", "Abbreviation", "State_name", 
        "Ers.region", "CRD")
    Crop.Layers <- Crop.Layers[, -which(names(Crop.Layers) \%in\% 
        Droppers)]
    DF <- merge(DF, Crop.Layers, by.x = c("x", "y", "STATE_FIPS"), 
        by.y = c("x", "y", "STATE_FIPS"), all.x = TRUE)
    DF <- cbind(DF[4:ncol(DF)], DF[, 1:3])
    DF <- DF[!is.na(DF$Planting.Main), ]
    DF <- DF[!is.na(DF$Harvesting.Main), ]
    DF <- DF[!is.na(DF$STATE_FIPS), ]
    DF <- DF[!is.na(DF$layer.1), ]
    DF$STATE_FIPS <- as.factor(DF$STATE_FIPS)
    if (Variable == "MNRH_") {
        DF2 <- DF
        save(DF2, file = paste0(Intermediates, paste("BASE", 
            Crop, Variable, "MasterDF2", sep = ".")))
    }
    OverWinter <- max(DF$Harvesting.Main)
    if (OverWinter > 365) {
        DF <- as.data.frame(cbind(DF[, 1:365], DF[, 1:length(DF)]))
        names(DF)[366:730] <- paste0(rep("layer."), 366:730)
    }
    Split.DF <- split(DF, DF$STATE_FIPS, drop = FALSE)
    print("number of states growing crop:")
    print(length(Split.DF))
    if (Crop != "sugarcane" & Crop != "switchgrass" & Crop != 
        "miscanthus" & Crop != "idle_cropland" & Crop != "pasture_grass" & 
        Crop != "rep_cropland") {
        Split.DF <- lapply(Split.DF, drop.levels)
    }
    Growing.Season <- lapply(Split.DF, function(x) x[, c(x$Planting.Main[1]:x$Harvesting.Main[1], 
        (which(names(x) == "CountyFIPS")):(which(names(x) == 
            "STATE_FIPS")))])
    Fallow.Season <- lapply(Split.DF, function(x) x[, c(1:(x$Planting.Main[1] - 
        1), (x$Harvesting.Main[1] + 1):ncol(x))])
    if (OverWinter > 365) {
        GS.dates <- lapply(Growing.Season, function(x) names(x[grep("layer", 
            names(x))]))
        GS.dates <- lapply(GS.dates, function(x) as.numeric(substr(x, 
            7, 9)))
        GS.dates.1 <- lapply(GS.dates, function(x) paste0("layer.", 
            x - 365))
        GS.dates.2 <- lapply(GS.dates, function(x) paste0("layer.", 
            x + 365))
        Dups <- c(paste0("layer.", 365:730))
        for (i in 1:length(Fallow.Season)) {
            Fallow.Season[[i]] <- Fallow.Season[[i]][, -(which(names(Fallow.Season[[i]]) \%in\% 
                Dups))]
            FS.check <- ncol(Fallow.Season[[i]][, grep("layer", 
                names(Fallow.Season[[i]]))]) + ncol(Growing.Season[[i]][, 
                grep("layer", names(Growing.Season[[i]]))])
            if (FS.check > 365) {
                Fallow.Season[[i]] <- Fallow.Season[[i]][, -(which(names(Fallow.Season[[i]]) \%in\% 
                  GS.dates.1[[i]]))]
            }
        }
    }
    GS.length <- unlist(lapply(Growing.Season, function(x) length(x[grep("layer", 
        names(x))])))
    FS.length <- unlist(lapply(Fallow.Season, function(x) length(x[grep("layer", 
        names(x))])))
    print(GS.length + FS.length)
    DF <- list(Variable = Variable, Growing.Season = Growing.Season, 
        Fallow.Season = Fallow.Season)
    save(DF, file = paste0(Intermediates, paste("Base", Crop, 
        Variable, "MasterDF", sep = ".")))
    return(DF)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
