\name{Main.Growing.Season.Daily.ET.Calc}
\alias{Main.Growing.Season.Daily.ET.Calc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Main.Growing.Season.Daily.ET.Calc(Croplayer)
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
    if (file.exists(paste0(Intermediates, paste("Growing.Season_Transpiration", 
        Croplayer, "Rdata", sep = "."))) == TRUE) {
        print(paste("Growing Season has been previously calculated for", 
            Croplayer))
    }
    if (file.exists(paste0(Intermediates, paste("Growing.Season_Transpiration", 
        Croplayer, "Rdata", sep = "."))) == FALSE) {
        print(paste("executing Growing Season calculations for", 
            Croplayer))
        Fw.table <- read.csv("Fw.table.csv")
        Irr.Eff <- Fw.table$fw[1]
        for (i in 1:length(Precip)) {
            Irrigated <- c("alfalfa", "cotton", "corn", "spring_barley", 
                "spring_oats", "rice", "soybeans", "sorghum", 
                "spring_wheat", "silage", "peanuts", "winter_wheat", 
                "silage")
            if (Croplayer \%in\% Irrigated) 
                irr <- TRUE
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
                  if (irr == TRUE & Frac > 0.5 & j < length(Irr[[i]]) * 
                    0.7) {
                    Irr[[i]][, j][Ks[[i]][, j] <= 0.9] <- 0.03 * 
                      RAW[[i]][, (j)][Ks[[i]][, j] <= 0.9]
                    Cum <- rowSums(Irr[[i]][, c(1:j)])
                    States <- read.csv("States.key.csv")
                    Crop <- Croplayer
                    if (Croplayer == "spring_wheat" || Croplayer == 
                      "winter_wheat") 
                      Crop <- "wheat"
                    if (Croplayer == "spring_barley") 
                      Crop <- "barley"
                    if (Croplayer == "spring_oats") 
                      Crop <- "oats"
                    if (Croplayer == "silage") 
                      Crop <- "corn"
                    Mults <- Irr.Mults(Crop)
                    Matched <- merge(States, Mults, by.x = "State_name", 
                      by.y = "State", all = TRUE)
                    Matched$Mult[is.na(Matched$Mult)] <- 1
                    m <- Matched$STATE_FIPS[as.character(Matched$STATE_FIPS) == 
                      names(Precip[i])]
                    Multiplier <- Matched$Mult[as.character(Matched$STATE_FIPS) == 
                      names(Precip[i])]
                    if (Croplayer == "alfalfa") {
                      if (m == 40 || m == 20) 
                        Multiplier <- 0.85
                      if (m == 46 || m == 38) 
                        Multiplier <- 1.75
                      if (m == 22 || m == 45) 
                        Multiplier <- 0.5
                      if (m == 48) 
                        Multiplier <- 1.1
                      if (m == 53 || m == 41 || m == 16) 
                        Multiplier <- Multiplier * 1.35
                      if (m == 6) 
                        Multiplier <- Multiplier * 1.25
                      if (m == 30) 
                        Multiplier <- Multiplier * 1.75
                      if (m == 4 || m == 55) 
                        Multiplier <- Multiplier * 2.25
                      if (m == 8 || m == 49) 
                        Multiplier <- Multiplier * 2.5
                      if (m == 56) 
                        Multiplier <- Multiplier * 3.2
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9] <- 0.035 * 
                        RAW[[i]][, (j)][Ks[[i]][, j] <= 0.9]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 300] <- 0.065 * RAW[[i]][, 
                        (j)][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 300]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 400] <- 0.1 * RAW[[i]][, 
                        (j)][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 400]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 600] <- 0.15 * RAW[[i]][, 
                        (j)][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 600]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 1000] <- 0.175 * RAW[[i]][, 
                        (j)][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 1000]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 2000] <- 0
                    }
                    if (Croplayer == "spring_barley") {
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9] <- 0.07 * 
                        RAW[[i]][, (j)][Ks[[i]][, j] <= 0.9]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 1000] <- 0
                    }
                    if (Croplayer == "corn" && j > 10) {
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9] <- 0.09 * 
                        RAW[[i]][, (j)][Ks[[i]][, j] <= 0.9]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 400] <- 0.11 * RAW[[i]][, 
                        (j)][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 400]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 800] <- 0.08 * RAW[[i]][, 
                        (j)][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 800]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 1000] <- 0
                      if (m == 6) {
                        Irr[[i]][, j][Ks[[i]][, j] <= 0.9] <- 0.12 * 
                          RAW[[i]][, (j)][Ks[[i]][, j] <= 0.9]
                        if (length(Irr[[i]][, j][Ks[[i]][, j] <= 
                          0.9 & Cum[Ks[[i]][, j] <= 0.9] >= 1400]) > 
                          0) {
                          print("irrigation max exceeded for...on day...")
                          print(length(Irr[[i]][, j][Ks[[i]][, 
                            j] <= 0.9 & Cum[Ks[[i]][, j] <= 0.9] >= 
                            1400]))
                          print(j)
                        }
                        Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                          j] <= 0.9] >= 1400] <- 0
                      }
                    }
                    if (Croplayer == "cotton") {
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9] <- 0.045 * 
                        RAW[[i]][, (j)][Ks[[i]][, j] <= 0.9]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 450] <- 0.025 * RAW[[i]][, 
                        (j)][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 450]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 650] <- 0.05 * RAW[[i]][, 
                        (j)][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 650]
                    }
                    if (Croplayer == "spring_oats") {
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9] <- 0.08 * 
                        RAW[[i]][, (j)][Ks[[i]][, j] <= 0.9]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 2200] <- 0
                    }
                    if (Croplayer == "peanuts") {
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9] <- 0.06 * 
                        RAW[[i]][, (j)][Ks[[i]][, j] <= 0.9]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 300] <- 0.08 * RAW[[i]][, 
                        (j)][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 300]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 1400] <- 0
                    }
                    if (Croplayer == "rice") {
                      Irr[[i]][, j][Dr[[i]][, (j)] >= 0.9 * (RAW[[i]][, 
                        (j)])] <- 1.75 * RAW[[i]][, (j)][Dr[[i]][, 
                        (j)] >= 0.9 * (RAW[[i]][, (j)])]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.6 & Cum[Ks[[i]][, 
                        j] <= 0.6] >= 200] <- 1 * RAW[[i]][, 
                        (j)][Ks[[i]][, j] <= 0.6 & Cum[Ks[[i]][, 
                        j] <= 0.6] >= 200]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.6 & Cum[Ks[[i]][, 
                        j] <= 0.6] >= 500] <- 0.5 * RAW[[i]][, 
                        (j)][Ks[[i]][, j] <= 0.6 & Cum[Ks[[i]][, 
                        j] <= 0.6] >= 500]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.6 & Cum[Ks[[i]][, 
                        j] <= 0.6] >= 850] <- 0.35 * RAW[[i]][, 
                        (j)][Ks[[i]][, j] <= 0.6 & Cum[Ks[[i]][, 
                        j] <= 0.6] >= 850]
                    }
                    if (Croplayer == "sorghum") {
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9] <- 0.05 * 
                        RAW[[i]][, (j)][Ks[[i]][, j] <= 0.9]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 400] <- 0.0275 * RAW[[i]][, 
                        (j)][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 100]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 4800] <- 0
                    }
                    if (Croplayer == "soybeans") {
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9] <- 0.095 * 
                        RAW[[i]][, (j)][Ks[[i]][, j] <= 0.9]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 350] <- 0.02 * RAW[[i]][, 
                        (j)][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 350]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 1200] <- 0
                    }
                    if (Croplayer == "spring_wheat") {
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9] <- 0.0075 * 
                        RAW[[i]][, (j)][Ks[[i]][, j] <= 0.9]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 1400] <- 0
                    }
                    if (Croplayer == "winter_wheat") {
                      if (m == 35 || m == 49 || m == 31 || m == 
                        20) 
                        Multiplier <- Multiplier * 0.5
                      if (m == 16) 
                        Multiplier <- Multiplier * 0.75
                      if (m == 41 || m == 8 || m == 32) 
                        Multiplier <- Multiplier * 1.5
                      if (m == 48 || m == 46 || m == 38 || m == 
                        45 || m == 37 || m == 51 || m == 40 || 
                        m == 5) 
                        Multiplier <- Multiplier * 2
                      if (m == 6 || m == 32) 
                        Multiplier <- Multiplier * 3
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9] <- 0.018 * 
                        RAW[[i]][, (j)][Ks[[i]][, j] <= 0.9]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 350] <- 0.045 * RAW[[i]][, 
                        (j)][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 350]
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 1200] <- 0
                    }
                    if (Croplayer == "silage") {
                      Irr[[i]][, j][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 400] <- 0.07 * RAW[[i]][, 
                        (j)][Ks[[i]][, j] <= 0.9 & Cum[Ks[[i]][, 
                        j] <= 0.9] >= 400]
                    }
                    Irr[[i]][, j] <- Irr[[i]][, j] * Multiplier
                  }
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
        }
    }
    print("Saving growing season SB files")
    setwd(paste0(Path, "/CropWatR/Intermediates/"))
    save(Few, file = paste("Growing.Season_Root.Zone.Depletion", 
        Croplayer, "Rdata", sep = "."))
    save(Kr, file = paste("Growing.Season_Kr", Croplayer, "Rdata", 
        sep = "."))
    save(Ks, file = paste("Growing.Season_Ks", Croplayer, "Rdata", 
        sep = "."))
    save(Pval, file = paste("Growing.Season_Pval", Croplayer, 
        "Rdata", sep = "."))
    save(Dr, file = paste("Growing.Season_Root.Zone.Depletion", 
        Croplayer, "Rdata", sep = "."))
    save(De, file = paste("Growing.Season_Soil.Water.Balance", 
        Croplayer, "Rdata", sep = "."))
    save(DP, file = paste("Growing.Season_Deep.Percolation", 
        Croplayer, "Rdata", sep = "."))
    save(ROi, file = paste("Growing.Season_Runoff", Croplayer, 
        "Rdata", sep = "."))
    save(E, file = paste("Growing.Season_Soil.Evaporation", Croplayer, 
        "Rdata", sep = "."))
    save(Irr, file = paste("Growing.Season_Irrigation", Croplayer, 
        "Rdata", sep = "."))
    save(Transp.final, file = paste("Growing.Season_Transpiration", 
        Croplayer, "Rdata", sep = "."))
    save(DPe, file = paste("Growing.Season.Root.Zone.Percolation.Loss", 
        Croplayer, "Rdata", sep = "."))
    save(Few, file = paste("Growing.Season.Evaporation.Fractions", 
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
