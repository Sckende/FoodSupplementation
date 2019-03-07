#---------------- GOOSE_MARK_supplemented_nests_VERSION 2 ----------------#
# SCript to analyse the DSR of supplemented nests
# 2 types of models
#     water vs. control (2005, 2015, 2016, 2017)
#     food vs. control (2015, 2016, 2017)

setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm(list = ls())
sup <- read.table("GOOSE_geese_with_WF.txt", sep = ",", h = T)
head(sup)
summary(sup)

sup <- sup[,-1]

utils::View(sup)
supW1 <- sup[which(sup$YEAR == 2015 | sup$YEAR == 2016 | sup$YEAR == 2017),]
supW1 <- supW1[which(supW1$SUPPL == "W" | supW1$SUPPL == "TEM"),]
supW1 <- droplevels(supW1)
summary(supW1)

supW1$YEAR <- as.factor(supW1$YEAR)
supW1$Fate <- as.factor(supW1$Fate)

#######################################################################################################
w05 <- read.table("GOOSE_Lecomte_supp_nests_2005.txt", h = T )
#Obtention de la variable AgeDay1 = correspond à l'âge du nid lors du premier jour du suivi de nids
w05$AgeDay1 <- (w05$AgeFound - w05$FirstFound)

supW1 <- sup[which(sup$YEAR == 2005 | sup$YEAR == 2015 | sup$YEAR == 2016 | sup$YEAR == 2017),]
supW1 <- supW1[which(supW1$SUPPL == "W" | supW1$SUPPL == "TEM"),]
supW1 <- droplevels(supW1)
supW11 <- supW1[, c(1:4, 7:10, 14, 16)]
head(supW11)

supW2 <- rbind(supW11, w05)

supW2$YEAR <- as.factor(supW2$YEAR)
supW2$Fate <- as.factor(supW2$Fate)
summary(supW2)