\name{Fallow.Postseason.Daily.ET.Calc}
\alias{Fallow.Postseason.Daily.ET.Calc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Fallow.Postseason.Daily.ET.Calc(Croplayer, Overwrite = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Croplayer}{
%%     ~~Describe \code{Croplayer} here~~
}
  \item{Overwrite}{
%%     ~~Describe \code{Overwrite} here~~
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
function (Croplayer, Overwrite = FALSE) 
{
    load(paste0(Intermediates, paste("Fallow.Saved", Croplayer, 
        "Rdata", sep = ".")))
    Post.ETo <- Fallow.File[[2]]
    Post.Precip <- Fallow.File[[4]]
    Post.Few <- Fallow.File[[6]]
    Post.ROi <- Fallow.File[[8]]
    Qfc.minus.Qwp <- Fallow.File[[9]]
    Post.Dei <- Fallow.File[[11]]
    TAW <- Fallow.File[[12]]
    TEW <- Fallow.File[[13]]
    REW <- Fallow.File[[14]]
    Post.Kr <- Post.Precip
    Post.Ke <- Post.Precip
    Post.Dei <- Post.Precip
    Post.DPei <- Post.Precip
    Post.Kcb.tot <- Post.Precip
    Post.E <- Post.Precip
    Post.Fw <- Post.Precip
    Post.Dr <- Post.Precip
    Post.DP <- Post.Precip
    Post.Ks <- Post.Precip
    Post.Kcb.tot <- Post.Precip
    Post.Pval <- Post.Precip
    Post.TAW <- Post.Precip
    Post.RAW <- Post.Precip
    Post.Kcb <- Post.Precip
    if (file.exists(paste0(Intermediates, paste("KcMax.Fallow", 
        Croplayer, "Rdata", sep = "."))) == FALSE) {
        KcMAX.fallow(Croplayer)
    }
    load(paste0(Intermediates, paste("KcMax.Fallow", Croplayer, 
        "Rdata", sep = ".")))
    KcMax <- lapply(KcMax, function(x) x[, (grep("layer", names(x)))])
    DaysRow <- lapply(Post.Precip, function(x) as.numeric(gsub("layer.", 
        "", names(x))))
    Cuts <- lapply(DaysRow, function(x) which(diff(x) > 1))
    Cuts <- sapply(Cuts, function(x) replace(x, length(x) == 
        0, 0))
    LengthCheck <- unlist(lapply(DaysRow, length))
    CutCheck <- unlist(Cuts)
    for (i in 1:length(KcMax)) {
        if (length(CutCheck) == 0) {
            KcMax[[i]] <- KcMax[[i]][1:length(Post.Precip[[i]])]
        }
        if (length(Cuts[[i]]) == 0) {
            KcMax[[i]] <- KcMax[[i]][1:length(KcMax[[i]])]
        }
        else {
            KcMax[[i]] <- KcMax[[i]][, Cuts[[i]]:length(KcMax[[i]])]
        }
        while (length(KcMax[[i]]) > length(Post.Precip[[i]])) {
            KcMax[[i]] <- KcMax[[i]][, 1:length(KcMax[[i]]) - 
                1]
        }
    }
    print("Post Season KcMax layer lengths equal?:")
    print(all.equal(lapply(KcMax, length), lapply(Post.Precip, 
        length)))
    Kcb <- 0.55
    load(paste0(Intermediates, paste("Growing.Season", Croplayer, 
        "Precip_", "Rdata", sep = ".")))
    Precip <- Growing.Season
    rm(Growing.Season)
    Qfc.minus.Qwp <- lapply(Precip, function(x) x$Qfc.minus.Qwp)
    root.depth <- 0.1
    TAW <- lapply(Qfc.minus.Qwp, function(x) 1000 * (x[] * root.depth))
    TEW <- lapply(Precip, function(x) x$ave_TEW)
    Dei <- TEW
    REW <- lapply(Precip, function(x) x$ave_REW)
    if (!file.exists(paste0(Intermediates, paste("Postseason_Deep.Percolation", 
        Croplayer, "Rdata", sep = "."))) | Overwrite == TRUE) {
        Others <- c("switchgrass", "miscanthus", "idle_cropland", 
            "pasture_grass", "silage")
        load("Vars.Rdata")
        if (Croplayer \%in\% Vars) {
            setwd(paste0(Path, "/CropWatR/Intermediates/"))
            load(paste("Growing.Season_Root.Zone.Depletion", 
                Croplayer, "Rdata", sep = "."))
            load(paste("Growing.Season_Deep.Percolation", Croplayer, 
                "Rdata", sep = "."))
            load(paste("Growing.Season_Runoff", Croplayer, "Rdata", 
                sep = "."))
            load(paste("Growing.Season_Soil.Evaporation", Croplayer, 
                "Rdata", sep = "."))
            load(paste("Growing.Saved", Croplayer, "Rdata", sep = "."))
            load(paste("Growing.Season_Soil.Water.Balance", Croplayer, 
                "Rdata", sep = "."))
            DPe <- local(get(load(file = paste("Growing.Season.Root.Zone.Percolation.Loss", 
                Croplayer, "Rdata", sep = "."))))
            load(file = paste("Growing.Season.Evaporation.Fractions", 
                Croplayer, "Rdata", sep = "."))
            setwd(paste0(Path, "/CropWatR/Data"))
        }
        if (Croplayer \%in\% Others) {
            setwd(paste0(Path, "/CropWatR/Intermediates/"))
            load(paste("Growing.Season_Root.Zone.Depletion", 
                Croplayer, "Rdata", sep = "."))
            load(paste("Growing.Season_Deep.Percolation", Croplayer, 
                "Rdata", sep = "."))
            load(paste("Growing.Season_Runoff", Croplayer, "Rdata", 
                sep = "."))
            load(paste("Growing.Season_Soil.Evaporation", Croplayer, 
                "Rdata", sep = "."))
            load(paste("Growing.Saved", Croplayer, "Rdata", sep = "."))
            load(paste("Growing.Season_Soil.Water.Balance", Croplayer, 
                "Rdata", sep = "."))
            load(file = paste("Growing.Season.Root.Zone.Percolation.Loss", 
                Croplayer, "Rdata", sep = "."))
            load(file = paste("Growing.Season.Evaporation.Fractions", 
                Croplayer, "Rdata", sep = "."))
            setwd(paste0(Path, "/CropWatR/Data"))
        }
        ETo <- Growing.Files[[1]]
        Precip <- Growing.Files[[2]]
        ROi <- Growing.Files[[3]]
        Irr <- Growing.Files[[4]]
        Fw <- Growing.Files[[5]]
        print("starting calculation of post season")
        for (i in 1:length(Post.Precip)) {
            for (j in 1:length(Post.Precip[[i]])) {
                Kcb <- 0.75
                if (j == 1) {
                  Post.Fw[[i]][, j] <- Few[[i]][, length(Few[[i]])]
                  Post.Kr[[i]][, j][De[[i]][, length(De[[i]])] > 
                    REW[[i]]] <- (TEW[[i]][De[[i]][, length(De[[i]])] > 
                    REW[[i]]] - De[[i]][, length(De[[i]])][De[[i]][, 
                    length(De[[i]])] > REW[[i]]])/(TEW[[i]][De[[i]][, 
                    length(De[[i]])] > REW[[i]]] - REW[[i]][De[[i]][, 
                    length(De[[i]])] > REW[[i]]])
                  Post.Kr[[i]][, j][De[[i]][, length(De[[i]])] <= 
                    REW[[i]]] <- 1
                  Post.Kr[[i]][, j][Post.Kr[[i]][, j] < 0] <- 0
                  Post.Ke[[i]][, j] <- pmin.int(Post.Kr[[i]][, 
                    j] * (KcMax[[i]][, j] - Kcb), Post.Few[[i]][, 
                    j] * KcMax[[i]][, j])
                  Post.Ke[[i]][, j][Post.Ke[[i]][, j] < 0] <- 0
                  Post.E[[i]][, j] <- Post.Ke[[i]][, j] * Post.ETo[[i]][, 
                    j]
                  Post.DPei[[i]][, j] <- (Post.Precip[[i]][, 
                    j] - Post.ROi[[i]][, j]) - De[[i]][, length(De[[i]])]
                  Post.DPei[[i]][, j][Post.DPei[[i]][, j] < 0] <- 0
                  Post.Dei[[i]][, j] <- De[[i]][, length(De[[i]])] - 
                    (Post.Precip[[i]][, j] - Post.ROi[[i]][, 
                      j]) + (Post.E[[i]][, j]/Post.Few[[i]][, 
                    j]) + DPe[[i]][, length(DPe[[i]])]
                  Post.Dei[[i]][, j][Post.Dei[[i]][, j] < 0] <- 0
                  Post.Dei[[i]][, j][Post.Dei[[i]][, j] > TEW[[i]]] <- TEW[[i]][Post.Dei[[i]][, 
                    j] > TEW[[i]]]
                  Post.Kcb[[i]][, j] <- (Kcb + Post.Ke[[i]][, 
                    j]) * Post.ETo[[i]][, j]
                  Post.Kcb.tot[[i]][, j] <- (Kcb) * Post.ETo[[i]][, 
                    j]
                  P.value <- 0.1
                  Post.Pval[[i]][, j] <- P.value + 0.02 * (5 - 
                    (Post.Kcb.tot[[i]][, j]))
                  Post.Pval[[i]][, j][Post.Pval[[i]][, j] < 0.1] <- 0.1
                  Post.Pval[[i]][, j][Post.Pval[[i]][, j] > 0.8] <- 0.8
                  Root.depth <- 0.1 + 0.002 * j
                  Post.TAW[[i]][, j] <- TAW[[i]] * Root.depth
                  Post.RAW[[i]][, j] <- Post.Pval[[i]][, j] * 
                    Post.TAW[[i]][, j]
                  Per.of.field.capacity <- 0.2
                  Post.Dr[[i]][, j] <- Post.TAW[[i]][, j] * Per.of.field.capacity
                  Post.Dr[[i]][, j] <- Post.Dr[[i]][, j] - (Post.Precip[[i]][, 
                    j] - Post.ROi[[i]][, j]) + Post.Kcb.tot[[i]][, 
                    j] + Post.DP[[i]][, j]
                  Post.Dr[[i]][, j][Post.Dr[[i]][, j] < 0] <- 0
                  Post.Dr[[i]][, j][Post.Dr[[i]][, j] > Post.TAW[[i]][, 
                    j]] <- Post.TAW[[i]][, j][Post.Dr[[i]][, 
                    j] > Post.TAW[[i]][, j]]
                  Post.Ks[[i]][, j][Post.Dr[[i]][, j] > Post.RAW[[i]][, 
                    j]] <- ((Post.TAW[[i]][, j] - Post.Dr[[i]][, 
                    j])[Post.Dr[[i]][, j] > Post.RAW[[i]][, j]])/((1 - 
                    Post.Pval[[i]][, j][Post.Dr[[i]][, j] > Post.RAW[[i]][, 
                      j]]) * Post.TAW[[i]][, j][Post.Dr[[i]][, 
                    j] > Post.RAW[[i]][, j]])
                  Post.Ks[[i]][, j][Post.Dr[[i]][, j] <= Post.RAW[[i]][, 
                    j]] <- 1
                  Post.DP[[i]][, j] <- (Post.Precip[[i]][, j] - 
                    Post.ROi[[i]][, j]) - Post.Kcb.tot[[i]][, 
                    j] - Dr[[i]][, length(Dr[[i]])]
                  Post.DP[[i]][, j][Post.Dr[[i]][, j] > 0] <- 0
                  Post.DP[[i]][, j][Post.DP[[i]][, j] < 0] <- 0
                  Post.Kcb.tot[[i]][, j] <- (Post.Ks[[i]][, j] * 
                    Post.Kcb.tot[[i]][, j]) * Post.ETo[[i]][, 
                    j]
                  Post.Kcb[[i]][, j] <- (Post.Ks[[i]][, j] * 
                    Post.Kcb[[i]][, j] + Post.Ke[[i]][, j]) * 
                    Post.ETo[[i]][, j]
                  Post.DPei[[i]][, j] <- (Post.Precip[[i]][, 
                    j] - Post.ROi[[i]][, j]) - De[[i]][, length(De[[i]])]
                  Post.DPei[[i]][, j][Post.DPei[[i]][, j] < 0] <- 0
                }
                else {
                  Kcb <- Kcb - 0.003 * j
                  Kcb[Kcb < 0.005] <- 0.005
                  Post.Fw[[i]][, j] <- Post.Few[[i]][, j - 1]
                  Post.Few[[i]][, j] <- pmin.int(Post.Few[[i]][, 
                    j], Post.Fw[[i]][, j])
                  Post.Kr[[i]][, j][Post.Dei[[i]][, (j - 1)] > 
                    REW[[i]]] <- (TEW[[i]][Post.Dei[[i]][, (j - 
                    1)] > REW[[i]]] - Post.Dei[[i]][, (j - 1)][Post.Dei[[i]][, 
                    (j - 1)] > REW[[i]]])/(TEW[[i]][Post.Dei[[i]][, 
                    (j - 1)] > REW[[i]]] - REW[[i]][Post.Dei[[i]][, 
                    (j - 1)] > REW[[i]]])
                  Post.Kr[[i]][, j][Post.Dei[[i]][, (j - 1)] <= 
                    REW[[i]]] <- 1
                  Post.Kr[[i]][, j][Post.Kr[[i]][, j] < 0] <- 0
                  Post.Ke[[i]][, j] <- pmin.int(Post.Kr[[i]][, 
                    j] * (KcMax[[i]][, j] - Kcb), Post.Few[[i]][, 
                    j] * KcMax[[i]][, j])
                  Post.Ke[[i]][, j][Post.Ke[[i]][, j] < 0] <- 0
                  Post.E[[i]][, j] <- Post.Ke[[i]][, j] * Post.ETo[[i]][, 
                    j]
                  if (length(Post.E[[i]][, j][Post.E[[i]][, j] > 
                    5]) > 0) {
                    print("Evaporation triggered:")
                    print("day col:")
                    print(j)
                    print("State code")
                    print(names(Post.Precip[i]))
                    print("Evap profile")
                    print(Post.E[[i]][, j][Post.E[[i]][, j] > 
                      5])
                    print("ETo profile")
                    print(Post.ETo[[i]][, j][Post.E[[i]][, j] > 
                      5])
                    print("Ke profile")
                    print(Post.Ke[[i]][, j][Post.E[[i]][, j] > 
                      5])
                  }
                  Post.DPei[[i]][, j] <- (Post.Precip[[i]][, 
                    j] - Post.ROi[[i]][, j]) - Post.Dei[[i]][, 
                    (j - 1)]
                  Post.DPei[[i]][, j][Post.DPei[[i]][, j] < 0] <- 0
                  Post.Dei[[i]][, j] <- Post.Dei[[i]][, (j - 
                    1)] - (Post.Precip[[i]][, j] - Post.ROi[[i]][, 
                    j]) + (Post.E[[i]][, j]/Post.Few[[i]][, j]) + 
                    Post.DPei[[i]][, j]
                  Post.Dei[[i]][, j][Post.Dei[[i]][, j] < 0] <- 0
                  Post.Dei[[i]][, j][Post.Dei[[i]][, j] > TEW[[i]]] <- TEW[[i]][Post.Dei[[i]][, 
                    j] > TEW[[i]]]
                  Post.Kcb[[i]][, j] <- (Kcb + Post.Ke[[i]][, 
                    j]) * Post.ETo[[i]][, j]
                  Post.Kcb.tot[[i]][, j] <- (Kcb) * Post.ETo[[i]][, 
                    j]
                  P.value <- 0.05
                  Post.Pval[[i]][, j] <- P.value + 0.04 * (5 - 
                    (Post.Kcb.tot[[i]][, j]))
                  Post.Pval[[i]][, j][Post.Pval[[i]][, j] < 0.1] <- 0.1
                  Post.Pval[[i]][, j][Post.Pval[[i]][, j] > 0.8] <- 0.8
                  Root.depth <- 0.05 + 0.002 * j
                  Post.TAW[[i]][, j] <- TAW[[i]] * Root.depth
                  Post.RAW[[i]][, j] <- Post.Pval[[i]][, j] * 
                    Post.TAW[[i]][, j]
                  Post.Dr[[i]][, j] <- Post.Dr[[i]][, (j - 1)] - 
                    (Post.Precip[[i]][, j] - Post.ROi[[i]][, 
                      j]) + Post.Kcb.tot[[i]][, j] + Post.DP[[i]][, 
                    (j - 1)]
                  Post.Dr[[i]][, j][Post.Dr[[i]][, j] < 0] <- 0
                  Post.Dr[[i]][, j][Post.Dr[[i]][, j] > Post.TAW[[i]][, 
                    j]] <- Post.TAW[[i]][, j][Post.Dr[[i]][, 
                    j] > Post.TAW[[i]][, j]]
                  Post.Ks[[i]][, j][Post.Dr[[i]][, j] > Post.RAW[[i]][, 
                    j]] <- ((Post.TAW[[i]][, j] - Post.Dr[[i]][, 
                    j])[Post.Dr[[i]][, j] > Post.RAW[[i]][, j]])/((1 - 
                    Post.Pval[[i]][, j][Post.Dr[[i]][, j] > Post.RAW[[i]][, 
                      j]]) * Post.TAW[[i]][, j][Post.Dr[[i]][, 
                    j] > Post.RAW[[i]][, j]])
                  Post.Ks[[i]][, j][Post.Dr[[i]][, j] <= Post.RAW[[i]][, 
                    j]] <- 1
                  Post.DP[[i]][, j] <- (Post.Precip[[i]][, j] - 
                    Post.ROi[[i]][, j]) - Post.Kcb.tot[[i]][, 
                    j] - Post.Dr[[i]][, j - 1]
                  Post.DP[[i]][, j][Post.Dr[[i]][, j] > 0] <- 0
                  Post.DP[[i]][, j][Post.DP[[i]][, j] < 0] <- 0
                  Post.Kcb.tot[[i]][, j] <- (Post.Ks[[i]][, j] * 
                    Post.Kcb.tot[[i]][, j]) * Post.ETo[[i]][, 
                    j]
                  Post.Kcb[[i]][, j] <- (Post.Ks[[i]][, j] * 
                    Post.Kcb[[i]][, j] + Post.Ke[[i]][, j]) * 
                    Post.ETo[[i]][, j]
                  Post.DPei[[i]][, j] <- (Post.Precip[[i]][, 
                    j] - Post.ROi[[i]][, j]) - Post.Dei[[i]][, 
                    j - 1]
                  Post.DPei[[i]][, j][Post.DPei[[i]][, j] < 0] <- 0
                  print(mean(Post.E[[i]][, j], na.rm = TRUE))
                  print(mean(Post.Kcb.tot[[i]][, j], na.rm = TRUE))
                }
            }
        }
        print("Calculation of Postseason daily soil water balance, deep percolation, and evaporation complete")
        setwd(paste0(Path, "/CropWatR/Intermediates/"))
        save(Post.Dei, file = paste("Postseason_Soil.Water.Balance", 
            Croplayer, "Rdata", sep = "."))
        save(Post.DP, file = paste("Postseason_Deep.Percolation", 
            Croplayer, "Rdata", sep = "."))
        save(Post.ROi, file = paste("Postseason_Runoff", Croplayer, 
            "Rdata", sep = "."))
        Post.KeETo <- Post.E
        save(Post.KeETo, file = paste("Postseason_Soil.Evaporation", 
            Croplayer, "Rdata", sep = "."))
        save(Post.Kcb.tot, file = paste("Postseason_Weed.Transpiration", 
            Croplayer, "Rdata", sep = "."))
        setwd(paste0(Path, "/CropWatR/Data"))
        print("Postseason files saved")
    }
    if (file.exists(paste0(Intermediates, paste("Postseason_Deep.Percolation", 
        Croplayer, "Rdata", sep = "."))) == TRUE && Overwrite == 
        FALSE) {
        print(paste("Post Season already estimated for", Croplayer))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
