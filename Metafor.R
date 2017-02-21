#load main package for metanalysis


Master.spreadsheet <- read.csv("~/Metanalysis/Master spreadsheet.csv")


library(meta)

###########Examples

Abundance <- dplyr::filter(Master.spreadsheet, Fitness.measure == "abundance")



Treatment <- dplyr::filter(Abundance, AI != "control")
Control <-  dplyr::filter(Abundance, AI == "control")

colnames(Treatment) <- paste("T", colnames(Treatment), sep = "_")

Abundance <- cbind(Control, Treatment[,c(9:13,15, 19, 20, 21)])

library(meta)
m <- metacont(n.e = T_Individuals..n., mean.e = T_Mean , sd.e = T_SE , n.c = Individuals..n., mean.c = Mean, sd.c = SE, studlab= Authors,data=Abundance, sm="SMD", comb.random = TRUE, comb.fixed = FALSE)
forest(m, xlab="Abundance", comb.random = TRUE, comb.fixed = FALSE)

funnel(m)

Reproduction <- dplyr::filter(Master.spreadsheet, Fitness.measure == "reproduction")
Treatment <- dplyr::filter(Reproduction, AI != "control")
Control <-  dplyr::filter(Reproduction, AI == "control")

colnames(Treatment) <- paste("T", colnames(Treatment), sep = "_")
############

library(meta)


data2 <- read.csv("~/Metanalysis/examples/dataset02.csv")

m <- metacont(Ne, Me, Se, Nc, Mc, Sc, sm="SMD", data=data2)
forest(m, xlab="Mean abundance per effort")
