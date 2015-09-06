\name{Main.Rainfed.Growing.Season.Daily.ET.Calc}
\alias{Main.Rainfed.Growing.Season.Daily.ET.Calc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Main.Rainfed.Growing.Season.Daily.ET.Calc(Croplayer, Auto = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Croplayer}{
%%     ~~Describe \code{Croplayer} here~~
}
  \item{Auto}{
%%     ~~Describe \code{Auto} here~~
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
function (Croplayer, Auto = TRUE) 
{
    load("Vars.Rdata")
    Irr.Vars <- Vars[-c(3, 6, 8, 14, 15)]
    if (!(Croplayer \%in\% Irr.Vars)) 
        stop("This function is for irrigated varieties only!")
    load(paste0(Intermediates, paste("Growing.Season", Croplayer, 
        "ETo_", "Rdata", sep = ".")))
    ETo <- Growing.Season
    rm(Growing.Season)
    load(paste0(Intermediates, paste("Growing.Season", Croplayer, 
        "Precip_", "Rdata", sep = ".")))
    Precip <- Growing.Season
    rm(Growing.Season)
    CROP <- Croplayer
    load(paste0("../Intermediates/Daily.Crop.Profile.", CROP, 
        ".Rdata"))
    Root.depth <- lapply(DailyKcb, function(x) x$daily_root.depth)
    Qfc.minus.Qwp <- lapply(Precip, function(x) x$Qfc.minus.Qwp)
    TEW <- lapply(Precip, function(x) x$ave_TEW)
    Dei <- TEW
    REW <- lapply(Precip, function(x) x$ave_REW)
    Precip <- lapply(Precip, function(x) x[, (grep("layer", names(x)))])
    load(paste0(Intermediates, paste("Few", Croplayer, "Rdata", 
        sep = ".")))
    load(paste0(Intermediates, paste("KcMax", Croplayer, "Rdata", 
        sep = ".")))
    KcMax <- lapply(KcMax, function(x) x[, (grep("layer", names(x)))])
    load(paste0(Intermediates, paste("Kcb.corrected", Croplayer, 
        "Rdata", sep = ".")))
    ETo <- lapply(ETo, function(x) x[, (grep("layer", names(x)))])
    sapply(ETo, function(x) length(x[x < 0]))
    if (file.exists(paste0(Intermediates, paste("Growing.Saved", 
        Croplayer, "Rdata", sep = "."))) == FALSE) {
        for (i in 1:length(ETo)) {
            ETo[[i]][ETo[[i]] < 0] <- 0
            ETo[[i]] <- round(ETo[[i]], 3)
            ETo[[i]][ETo[[i]] > 28] <- 1.655
            print("ETo high vals warning:")
            print(length(ETo[[i]][ETo[[i]] > 18]))
        }
        print("ETo data cleaned")
        ROi <- Precip
        for (i in 1:length(ROi)) {
            ROi[[i]] <- ROi[[i]] - TEW[[i]]
            ROi[[i]][ROi[[i]] < 0] <- 0
        }
        print("Growing season runoff estimated")
        Irr <- Precip
        for (i in 1:length(Irr)) {
            Irr[[i]][Irr[[i]] > 0] <- 0
        }
        Fw.table <- read.csv("Fw.table.csv")
        Irr.Eff <- Fw.table$fw[1]
        Fw <- Irr
        for (i in 1:length(Fw)) {
            Fw[[i]][Fw[[i]] == 0] <- Irr.Eff
        }
        Growing.Files <- list(ETo, Precip, ROi, Irr, Fw)
        save(Growing.Files, file = paste0(Intermediates, paste("Growing.Saved", 
            Croplayer, "Rdata", sep = ".")))
    }
    if (file.exists(paste0(Intermediates, paste("Growing.Saved", 
        Croplayer, "Rdata", sep = "."))) == TRUE) {
        load(paste0(Intermediates, paste("Growing.Saved", Croplayer, 
            "Rdata", sep = ".")))
        ETo <- Growing.Files[[1]]
        Precip <- Growing.Files[[2]]
        ROi <- Growing.Files[[3]]
        Irr <- Growing.Files[[4]]
        Fw <- Growing.Files[[5]]
    }
    Zr <- read.csv("crop.roots.csv")
    Zr <- Zr[Zr$crop == Croplayer, ]
    TAW.base <- lapply(Qfc.minus.Qwp, function(x) 1000 * (x[] * 
        Zr$root_depth))
    Kr <- Irr
    ETc <- Irr
    De <- Irr
    DPe <- Irr
    Transp <- Irr
    Ke <- Irr
    E <- Irr
    Transp <- Irr
    Pval <- Irr
    RAW <- Irr
    Ks <- Irr
    Transp.final <- Irr
    Dr <- Irr
    DP <- Irr
    TAW <- Irr
    setwd(paste0(Path, "/CropWatR/Intermediates/"))
    load(paste("Preseason_Root.Zone.Depletion", Croplayer, "Rdata", 
        sep = "."))
    load(paste("Preseason_Soil.Top.Layer.Depletion", Croplayer, 
        "Rdata", sep = "."))
    load(paste("Preseason_Deep.Percolation", Croplayer, "Rdata", 
        sep = "."))
    load(paste("Preseason_Soil.Evaporation", Croplayer, "Rdata", 
        sep = "."))
    load(paste("Preseason_Runoff", Croplayer, "Rdata", sep = "."))
    load(paste("Preseason_Weed.Transpiration", Croplayer, "Rdata", 
        sep = "."))
    load(paste("Fallow.Saved", Croplayer, "Rdata", sep = "."))
    Pre.Few <- Fallow.File[[5]]
    setwd(paste0(Path, "/CropWatR/Data"))
    if (file.exists(paste0(Intermediates, paste("Growing.Season.Rainfed_Transpiration", 
        Croplayer, "Rdata", sep = "."))) == TRUE & Auto == TRUE) {
        print(paste("Growing Season has been previously calculated for", 
            Croplayer))
    }
    if (file.exists(paste0(Intermediates, paste("Growing.Season.Rainfed_Transpiration", 
        Croplayer, "Rdata", sep = "."))) == FALSE) {
        Fw.table <- read.csv("Fw.table.csv")
        Irr.Eff <- Fw.table$fw[1]
        for (i in 1:length(Precip)) {
            for (j in 1:length(Precip[[i]])) {
                if (j == 1) {
                  Few[[i]][, j] <- pmin.int(Few[[i]][, j], Fw[[i]][, 
                    j])
                  Kr[[i]][, j][Pre.Dei[[i]][, length(Pre.Dei[[i]])] > 
                    REW[[i]]] <- (TEW[[i]][Pre.Dei[[i]][, length(Pre.Dei[[i]])] > 
                    REW[[i]]] - Pre.Dei[[i]][, length(Pre.Dei[[i]])][Pre.Dei[[i]][, 
                    length(Pre.Dei[[i]])] > REW[[i]]])/(TEW[[i]][Pre.Dei[[i]][, 
                    length(Pre.Dei[[i]])] > REW[[i]]] - REW[[i]][Pre.Dei[[i]][, 
                    length(Pre.Dei[[i]])] > REW[[i]]])
                  Kr[[i]][, j][Pre.Dei[[i]][, length(Pre.Dei[[i]])] <= 
                    REW[[i]]] <- 1
                  Kr[[i]][, j][Kr[[i]][, j] < 0] <- 0
                  Ke[[i]][, j] <- pmin.int(Kr[[i]][, j] * (KcMax[[i]][, 
                    j] - Kcb.corrected[[i]][, j]), Few[[i]][, 
                    j] * KcMax[[i]][, j])
                  Ke[[i]][, j][Ke[[i]][, j] < 0] <- 0
                  E[[i]][, j] <- Ke[[i]][, j] * ETo[[i]][, j]
                  DPe[[i]][, j] <- (Precip[[i]][, j] - ROi[[i]][, 
                    j]) + (Irr[[i]][, j]/Fw[[i]][, j]) - Pre.Dei[[i]][, 
                    length(Pre.Dei[[i]])]
                  DPe[[i]][, j][DPe[[i]][, j] < 0] <- 0
                  De[[i]][, j] <- Pre.Dei[[i]][, length(Pre.Dei[[i]])] - 
                    (Precip[[i]][, j] - ROi[[i]][, j]) + Irr[[i]][, 
                    j]/Fw[[i]][, j] + (E[[i]][, j]/Few[[i]][, 
                    j]) + DPe[[i]][, j]
                  De[[i]][, j][De[[i]][, j] < 0] <- 0
                  De[[i]][, j][De[[i]][, j] > TEW[[i]]] <- TEW[[i]][De[[i]][, 
                    j] > TEW[[i]]]
                  ETc[[i]][, j] <- (Kcb.corrected[[i]][, j] + 
                    Ke[[i]][, j]) * ETo[[i]][, j]
                  Pval[[i]][, j] <- Zr$p.value + 0.04 * (5 - 
                    (ETc[[i]][, j]))
                  Pval[[i]][, j][Pval[[i]][, j] < 0.1] <- 0.1
                  Pval[[i]][, j][Pval[[i]][, j] > 0.8] <- 0.8
                  if (is.na(Root.depth[[i]][j]/Zr$root_depth)) {
                    Frac <- Root.depth[[i]][length(Root.depth[[i]])]/Zr$root_depth
                  }
                  else Frac <- Root.depth[[i]][j]/Zr$root_depth
                  TAW[[i]][, j] <- TAW.base[[i]] * Frac
                  RAW[[i]][, j] <- Pval[[i]][, j] * TAW[[i]][, 
                    j]
                  Dr[[i]][, j] <- Pre.Dr[[i]][, length(Pre.Dr[[i]])] - 
                    (Precip[[i]][, j] - ROi[[i]][, j]) - Irr[[i]][, 
                    j] + ETc[[i]][, j] + Pre.DP[[i]][, length(Pre.DP[[i]])]
                  Dr[[i]][, j][Dr[[i]][, j] < 0] <- 0
                  Dr[[i]][, j][Dr[[i]][, j] > TAW[[i]][, j]] <- TAW[[i]][, 
                    j][Dr[[i]][, j] > TAW[[i]][, j]]
                  Ks[[i]][, j][Dr[[i]][, j] > RAW[[i]][, j]] <- ((TAW[[i]][, 
                    j] - Dr[[i]][, j])[Dr[[i]][, j] > RAW[[i]][, 
                    j]])/((1 - Pval[[i]][, j][Dr[[i]][, j] > 
                    RAW[[i]][, j]]) * TAW[[i]][, j][Dr[[i]][, 
                    j] > RAW[[i]][, j]])
                  Ks[[i]][, j][Dr[[i]][, j] <= RAW[[i]][, j]] <- 1
                  DP[[i]][, j] <- (Precip[[i]][, j] - ROi[[i]][, 
                    j]) + Irr[[i]][, j] - ETc[[i]][, j] - Pre.Dr[[i]][, 
                    length(Pre.Dr[[i]])]
                  DP[[i]][, j][Dr[[i]][, j] > 0] <- 0
                  DP[[i]][, j][DP[[i]][, j] < 0] <- 0
                  Transp[[i]][, j] <- (Ks[[i]][, j] * Kcb.corrected[[i]][, 
                    j] + Ke[[i]][, j]) * ETo[[i]][, j]
                  Transp.final[[i]][, j] <- (Ks[[i]][, j] * Kcb.corrected[[i]][, 
                    j]) * ETo[[i]][, j]
                  DPe[[i]][, j] <- (Precip[[i]][, j] - ROi[[i]][, 
                    j]) + (Irr[[i]][, j]/Fw[[i]][, j]) - Pre.Dei[[i]][, 
                    length(Pre.Dei[[i]])]
                  DPe[[i]][, j][DPe[[i]][, j] < 0] <- 0
                  De[[i]][, j] <- Pre.Dei[[i]][, length(Pre.Dei[[i]])] - 
                    (Precip[[i]][, j] - ROi[[i]][, j]) + Irr[[i]][, 
                    j]/Fw[[i]][, j] + (E[[i]][, j]/Few[[i]][, 
                    j]) + DPe[[i]][, j]
                  De[[i]][, j][De[[i]][, j] < 0] <- 0
                  De[[i]][, j][De[[i]][, j] > TEW[[i]]] <- TEW[[i]][De[[i]][, 
                    j] > TEW[[i]]]
                }
                else {
                  Fw[[i]][, j] <- Fw[[i]][, (j - 1)]
                  Few[[i]][, j] <- pmin.int(Few[[i]][, j], Fw[[i]][, 
                    j])
                  Kr[[i]][, j][De[[i]][, (j - 1)] > REW[[i]]] <- (TEW[[i]][De[[i]][, 
                    (j - 1)] > REW[[i]]] - De[[i]][, (j - 1)][De[[i]][, 
                    (j - 1)] > REW[[i]]])/(TEW[[i]][De[[i]][, 
                    (j - 1)] > REW[[i]]] - REW[[i]][De[[i]][, 
                    (j - 1)] > REW[[i]]])
                  Kr[[i]][, j][De[[i]][, (j - 1)] <= REW[[i]]] <- 1
                  Kr[[i]][, j][Kr[[i]][, j] < 0] <- 0
                  Ke[[i]][, j] <- pmin.int(Kr[[i]][, j] * (KcMax[[i]][, 
                    j] - Kcb.corrected[[i]][, j]), Few[[i]][, 
                    j] * KcMax[[i]][, j])
                  Ke[[i]][, j][Ke[[i]][, j] < 0] <- 0
                  ETo[[i]]
                  E[[i]][, j] <- Ke[[i]][, j] * ETo[[i]][, j]
                  DPe[[i]][, j] <- (Precip[[i]][, j] - ROi[[i]][, 
                    j]) + (Irr[[i]][, j]/Fw[[i]][, j]) - De[[i]][, 
                    j - 1]
                  DPe[[i]][, j][DPe[[i]][, j] < 0] <- 0
                  De[[i]][, j] <- De[[i]][, j - 1] - (Precip[[i]][, 
                    j] - ROi[[i]][, j]) + Irr[[i]][, j]/Fw[[i]][, 
                    j] + (E[[i]][, j]/Few[[i]][, j]) + DPe[[i]][, 
                    j]
                  De[[i]][, j][De[[i]][, j] < 0] <- 0
                  De[[i]][, j][De[[i]][, j] > TEW[[i]]] <- TEW[[i]][De[[i]][, 
                    j] > TEW[[i]]]
                  ETc[[i]][, j] <- (Kcb.corrected[[i]][, j] + 
                    Ke[[i]][, j]) * ETo[[i]][, j]
                  Pval[[i]][, j] <- Zr$p.value + 0.04 * (5 - 
                    (ETc[[i]][, j]))
                  Pval[[i]][, j][Pval[[i]][, j] < 0.1] <- 0.1
                  Pval[[i]][, j][Pval[[i]][, j] > 0.8] <- 0.8
                  if (is.na(Root.depth[[i]][j]/Zr$root_depth)) {
                    Frac <- Root.depth[[i]][length(Root.depth[[i]])]/Zr$root_depth
                  }
                  else Frac <- Root.depth[[i]][j]/Zr$root_depth
                  TAW[[i]][, j] <- TAW.base[[i]] * Frac
                  RAW[[i]][, j] <- Pval[[i]][, j] * TAW[[i]][, 
                    j]
                  Dr[[i]][, j] <- Dr[[i]][, j - 1] - (Precip[[i]][, 
                    j] - ROi[[i]][, j]) - Irr[[i]][, j] + ETc[[i]][, 
                    j] + DP[[i]][, j - 1]
                  Dr[[i]][, j][Dr[[i]][, j] < 0] <- 0
                  Dr[[i]][, j][Dr[[i]][, j] > TAW[[i]][, j]] <- TAW[[i]][, 
                    j][Dr[[i]][, j] > TAW[[i]][, j]]
                  Dr[[i]][, j] <- Dr[[i]][, j - 1] - (Precip[[i]][, 
                    j] - ROi[[i]][, j]) - Irr[[i]][, j] + ETc[[i]][, 
                    j] + DP[[i]][, j - 1]
                  Dr[[i]][, j][Dr[[i]][, j] < 0] <- 0
                  Dr[[i]][, j][Dr[[i]][, j] > TAW[[i]][, j]] <- TAW[[i]][, 
                    j][Dr[[i]][, j] > TAW[[i]][, j]]
                  Ks[[i]][, j][Dr[[i]][, j] > RAW[[i]][, j]] <- ((TAW[[i]][, 
                    j] - Dr[[i]][, j])[Dr[[i]][, j] > RAW[[i]][, 
                    j]])/((1 - Pval[[i]][, j][Dr[[i]][, j] > 
                    RAW[[i]][, j]]) * TAW[[i]][, j][Dr[[i]][, 
                    j] > RAW[[i]][, j]])
                  Ks[[i]][, j][Dr[[i]][, j] <= RAW[[i]][, j]] <- 1
                  DP[[i]][, j] <- (Precip[[i]][, j] - ROi[[i]][, 
                    j]) + Irr[[i]][, j] - ETc[[i]][, j] - Dr[[i]][, 
                    j - 1]
                  DP[[i]][, j][Dr[[i]][, j] > 0] <- 0
                  DP[[i]][, j][DP[[i]][, j] < 0] <- 0
                  Transp[[i]][, j] <- (Ks[[i]][, j] * Kcb.corrected[[i]][, 
                    j] + Ke[[i]][, j]) * ETo[[i]][, j]
                  Transp.final[[i]][, j] <- (Ks[[i]][, j] * Kcb.corrected[[i]][, 
                    j]) * ETo[[i]][, j]
                  DPe[[i]][, j] <- (Precip[[i]][, j] - ROi[[i]][, 
                    j]) + (Irr[[i]][, j]/Fw[[i]][, j]) - De[[i]][, 
                    j - 1]
                  DPe[[i]][, j][DPe[[i]][, j] < 0] <- 0
                  De[[i]][, j] <- De[[i]][, j - 1] - (Precip[[i]][, 
                    j] - ROi[[i]][, j]) + Irr[[i]][, j]/Fw[[i]][, 
                    j] + (E[[i]][, j]/Few[[i]][, j]) + DPe[[i]][, 
                    j]
                  De[[i]][, j][De[[i]][, j] < 0] <- 0
                  De[[i]][, j][De[[i]][, j] > TEW[[i]]] <- TEW[[i]][De[[i]][, 
                    j] > TEW[[i]]]
                }
            }
            Few[[i]][, 1] <- Few[[i]][, 2]
            Kr[[i]][, 1] <- Kr[[i]][, 2]
            Ke[[i]][, 1] <- Ke[[i]][, 2]
            E[[i]][, 1] <- E[[i]][, 2]
            DPe[[i]][, 1] <- DPe[[i]][, 2]
            De[[i]][, 1] <- De[[i]][, 2]
            ETc[[i]][, 1] <- ETc[[i]][, 2]
            Pval[[i]][, 1] <- Pval[[i]][, 2]
            TAW[[i]][, 1] <- TAW[[i]][, 2]
            RAW[[i]][, 1] <- RAW[[i]][, 2]
            Dr[[i]][, 1] <- Dr[[i]][, 2]
            Dr[[i]][, 1] <- Dr[[i]][, 2]
            Ks[[i]][, 1] <- Ks[[i]][, 2]
            DP[[i]][, 1] <- DP[[i]][, 2]
            Transp[[i]][, 1] <- Transp[[i]][, 2]
            Transp.final[[i]][, 1] <- Transp.final[[i]][, 2]
        }
    }
    print("Saving rainfed growing season SB files")
    setwd(paste0(Path, "/CropWatR/Intermediates/"))
    save(Few, file = paste("Growing.Season.Rainfed_Root.Zone.Depletion", 
        Croplayer, "Rdata", sep = "."))
    save(Kr, file = paste("Growing.Season.Rainfed_Kr", Croplayer, 
        "Rdata", sep = "."))
    save(Ks, file = paste("Growing.Season.Rainfed_Ks", Croplayer, 
        "Rdata", sep = "."))
    save(Pval, file = paste("Growing.Season.Rainfed_Pval", Croplayer, 
        "Rdata", sep = "."))
    save(Dr, file = paste("Growing.Season.Rainfed_Root.Zone.Depletion", 
        Croplayer, "Rdata", sep = "."))
    save(De, file = paste("Growing.Season.Rainfed_Soil.Water.Balance", 
        Croplayer, "Rdata", sep = "."))
    save(DP, file = paste("Growing.Season.Rainfed_Deep.Percolation", 
        Croplayer, "Rdata", sep = "."))
    save(ROi, file = paste("Growing.Season.Rainfed_Runoff", Croplayer, 
        "Rdata", sep = "."))
    save(E, file = paste("Growing.Season.Rainfed_Soil.Evaporation", 
        Croplayer, "Rdata", sep = "."))
    save(Transp.final, file = paste("Growing.Season.Rainfed_Transpiration", 
        Croplayer, "Rdata", sep = "."))
    save(DPe, file = paste("Growing.Season.Rainfed.Root.Zone.Percolation.Loss", 
        Croplayer, "Rdata", sep = "."))
    save(Few, file = paste("Growing.Season.Rainfed.Evaporation.Fractions", 
        Croplayer, "Rdata", sep = "."))
    setwd(paste0(Path, "/CropWatR/Data"))
    print("Calculation of Growing Season daily soil water balance, deep percolation, and evaporation complete")
    print("Growing Season initial run complete, on to post season")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
