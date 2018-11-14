#### Script for figures production included in goose paper ####

setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()

#### Annual cumulative precipitation ####
# Trends between 1995 and 2017 -  Specific dates for each goose nidification period ***
rain <- read.table("PREC_precipitation_Bylot_1995-2017.txt", sep = "\t", dec = ",", h = T)
summary(rain)
rain <- na.omit(rain)
nidi <- read.table("GOOSE_jour_moy_ponte_eclos.txt", sep = "\t", dec = ".", h = T)
summary(nidi)

cum2 <- NULL
for (i in unique(rain$YEAR)) {
  YEAR <- i
  INI <- nidi$moy_ponte[nidi$year == i]
  ECLO <- nidi$moy_eclos[nidi$year == i]
  
  cumRAIN <- sum(rain$RAIN[rain$YEAR == i & rain$JJ >= INI & rain$JJ <= ECLO])
  
  TAB <- data.frame(YEAR, INI, ECLO, cumRAIN)
  
  cum2 <- rbind(cum2, TAB)
}
cum2$RAINFALL[cum2$cumRAIN <= mean(cum2$cumRAIN) - sd(cum2$cumRAIN)] <- "LOW"
cum2$RAINFALL[cum2$cumRAIN >= mean(cum2$cumRAIN) + sd(cum2$cumRAIN)] <- "HIGH"
cum2$RAINFALL[cum2$cumRAIN <= mean(cum2$cumRAIN) + sd(cum2$cumRAIN) & cum2$cumRAIN >= mean(cum2$cumRAIN) - sd(cum2$cumRAIN)] <- "INTER"
cum2$RAINFALL <- as.factor(cum2$RAINFALL)

# Check the duration of each nesting season
cum2$NEST_DURATION <- cum2$ECLO - cum2$INI

# cumRAIN/day to compensate the different length of nesing period
cum2$cumRAIN_DAY <- cum2$cumRAIN / cum2$NEST_DURATION

WEA <- read.table("GOOSE_PAPER_Annual_weather.txt", h = T)
head(WEA)
summary(WEA)


# Plot
png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 1 - Geese nesting success & supplemented nests/PAPER/Figures/GOOSE_prec_temp.tiff",
res=300,
width=20,
height=25,
pointsize=12,
unit="cm",
bg="transparent")

#x11()
par(mfrow = c(2, 1), mar=c(5,5,1,5)) # inner margin - default parameter is par("mar") <- 5.1 4.1 4.1 2.1
my_vector <- cum2$cumRAIN
names(my_vector) <- cum2$YEAR
cols <- c("seagreen4", "olivedrab3", "darkgoldenrod3")[as.numeric(cum2$RAINFALL)]

barplot(my_vector,
        col = cols,
        border = c("seagreen4", "olivedrab3", "darkgoldenrod3")[as.numeric(cum2$RAINFALL)],
        # main = "Trend for specific dates for each goose nesting period",
        ylab = "Cumulative precipitation (mm)",
        yaxt = "n",
        ylim = c(0, max(cum2$cumRAIN + 5)))
axis(side = 2,
     lwd = 1,
     las = 2)
abline(h = mean(cum2$cumRAIN),
       col = "olivedrab4",
       lwd = 2.5,
       lty = "solid")
abline(h = mean(cum2$cumRAIN) - sd(cum2$cumRAIN),
       col = "olivedrab4",
       lwd = 1.5,
       lty = "dotdash")
abline(h = mean(cum2$cumRAIN) + sd(cum2$cumRAIN),
       col = "olivedrab4",
       lwd = 1.5,
       lty = "dotdash")
legend(-0.5, 70, legend = c("high", "intermediate", "low"), col = c("seagreen4", "olivedrab3", "darkgoldenrod3"), pch = 15, bty = "n")
legend(26.5, 75, legend = "(a)", bty = "n")

#par(new = T)
#### Annual air temperature ####
WEA$type_temp[WEA$meanTEMP >= mean(WEA$meanTEMP) + sd(WEA$meanTEMP)] <- "WARM"
WEA$type_temp[WEA$meanTEMP <= mean(WEA$meanTEMP) - sd(WEA$meanTEMP)] <- "COLD"
WEA$type_temp[is.na(WEA$type_temp)] <- "INTER"
WEA$type_temp <- as.factor(WEA$type_temp)

pchs <- c(15:17)[as.numeric(WEA$type_temp)]
plot(WEA$YEAR,
     WEA$meanTEMP,
     xlab = "",
     ylab = "Mean air temperature (°C)",
     xaxp = c(1995, 2017, 22),
     ylim = c(3, 8),
     xlim = c(1995, 2017),
     bty = "n",
     #yaxp = c(min(WEA$meanTEMP) - 0.5, max(WEA$meanTEMP) + 0.5, 6),
     yaxt = "n",
     xaxt = "n",
     cex = c(2, 1, 2)[as.numeric(WEA$type_temp)],
     cex.lab = 1,
     col = "darkgoldenrod3",
     pch = pchs,
     lwd = 2,
     type = 'b'
)

lines(WEA$YEAR,
      rep(mean(WEA$meanTEMP), 23),
      col = "darkgoldenrod4",
      type = "l",
      lty = 1,
      lwd = 2.5)

lines(WEA$YEAR,
      rep(mean(WEA$meanTEMP) + sd(WEA$meanTEMP), 23),
      col = "darkgoldenrod4",
      type = "l",
      lty = "dotdash",
      lwd = 1.5)

lines(WEA$YEAR,
      rep(mean(WEA$meanTEMP) - sd(WEA$meanTEMP), 23),
      col = "darkgoldenrod4",
      type = "l",
      lty = "dotdash",
      lwd = 1.5)

axis(side = 2,
     lwd = 1,
     las = 2)

legend(1995, 8, legend = c("cold", "intermediate", "warm"), col = "darkgoldenrod3", pch = 15:17,bty = "n")
legend(2016, 8.3, legend = "(b)", bty = "n")


dev.off()

# Superposition des deux graphiques
png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 1 - Geese nesting success & supplemented nests/PAPER/Figures/GOOSE_prec_temp_superposition.tiff",
    res=300,
    width=25,
    height=20,
    pointsize=12,
    unit="cm",
    bg="transparent")
x11()
cols2 <- c("red4", "orangered", "orange")[as.numeric(WEA$type_temp)]
plot(WEA$YEAR, WEA$cumPREC, type = "h", col = cols, lwd = 20, xaxp = c(1995, 2017, 22), bty = "n")
lines(WEA$YEAR, WEA$meanTEMP*10, xaxt = "n", yaxt = "n", cex = c(2, 1, 2)[as.numeric(WEA$type_temp)], pch = pchs, type = "b", lwd = 3, col =cols2, bty = "n")

dev.off()
#### Extreme climate years impacts on DSR ####

setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
rm(list = ls())
geese <- read.table("GOOSE_geese_with_WF.txt", h = T, sep = ",")
summary(geese)
geese$RAINFALL <- relevel(geese$RAINFALL, "INTER")
geese$TYP_TEMP <- relevel(geese$TYP_TEMP, "INTER")
require(RMark)
nocc <- max(geese$LastChecked)

# Best model
m5 <-  mark(geese, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + NestAge + RAINFALL + TYP_TEMP)), groups = c("HAB", "RAINFALL", "TYP_TEMP"), delete = T)
m5$results$beta
m5$results$real

# MESIC HABITAT & CUMULATIVE PRECIPITATION
d <- 26 # nesting period less 1, in days

# Dataframe building

TAB <- rbind(m5$results$real[1:(1+d),], m5$results$real[79:(79+d),], m5$results$real[157:(157+d),], m5$results$real[40:(40+d),], m5$results$real[118:(118+d),], m5$results$real[196:(196+d),], m5$results$real[469:(469+d),], m5$results$real[235:(235+d),], m5$results$real[508:(508+d),], m5$results$real[274:(274+d),])
utils::View(TAB)

TAB2 <- as.data.frame(TAB)
hab <- c(rep("MES", 81), rep("WET", 81), rep("MES", 54), rep("WET", 54))
prec <- c(rep(c(rep("INTER", 27), rep("HIGH", 27), rep("LOW", 27)), 2), rep("INTER", 108))
temp <- c(rep("INTER", 162), rep( c(rep("WARM", 27), rep("COLD", 27)), 2))
day <- rep(1:27, 10)

TAB2 <- cbind(day, hab, prec, temp, TAB2)
TAB2 <- TAB2[,1:8]
summary(TAB2)
utils::View(TAB2)



#######################################################################################
# Graph parameters
mini <- min(TAB2$estimate[TAB2$temp == "INTER"] - TAB2$se[TAB2$temp == "INTER"])
maxi <- max(TAB2$estimate[TAB2$temp == "INTER"] + TAB2$se[TAB2$temp == "INTER"])

x11()

#png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 1 - Geese nesting success & supplemented nests/PAPER/Figures/GOOSE_EXTREM_PREC.tiff",
    #res=300,
    #width=30,
    #height=20,
    #pointsize=12,
    #unit="cm",
    #bg="transparent")

par(mfrow = c(1, 2),
    las = 1,# plot labels always horizontal
    bty = "n")
# GRAPHE 1 - MESIC HABITAT & CUMULATIVE PRECIPITATION
# INTERMEDIATE PREC
plot(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], type = "b", col = "springgreen4", bg = "springgreen4", pch = 21, bty = "n", yaxt = "n", ylim = c(0.965, 0.995), xaxt = "n", ylab = "DSR", xlab = "Days", main = "Mesic habitat & precipitation effect")
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"] - TAB2$se[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], col = "springgreen4", lty = 3, lwd = 1.5)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"] + TAB2$se[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], col = "springgreen4", lty = 3, lwd = 1.5)
# HIGH PREC
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], type = "b", col = "springgreen3", bg = "springgreen3", pch = 24, bty = "n")
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"] - TAB2$se[TAB2$hab == "MES" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], col = "springgreen3", lty = 3, lwd = 1.5)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"] + TAB2$se[TAB2$hab == "MES" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], col = "springgreen3", lty = 3, lwd = 1.5)
# LOW PREC
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "LOW" & TAB2$temp == "INTER"], type = "b", col = "olivedrab2", bg = "olivedrab2", pch = 25, bty = "n")
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "LOW" & TAB2$temp == "INTER"] - TAB2$se[TAB2$hab == "MES" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], col = "olivedrab2", lty = 3, lwd = 1.5)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "LOW" & TAB2$temp == "INTER"] + TAB2$se[TAB2$hab == "MES" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], col = "olivedrab2", lty = 3, lwd = 1.5)

legend("bottomright", col = c("springgreen4", "springgreen3", "olivedrab2"), pch = c(21, 24, 25), pt.bg = c("springgreen4", "springgreen3", "olivedrab2"), legend = c("inter", "high", "low"), bty = "n")
axis(1, at = seq(0, 28, by = 2))
axis(2, at = seq(0.965, 0.995, by = 0.01))
text(x = 26, y = 0.995, labels = "(a)", pos = 3)

# GRAPHE 2 - WETLAND HABITAT & CUMULATIVE PRECIPITATION
plot(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], type = "b", col = "springgreen4", bg = "springgreen4", pch = 21, bty = "n", ylim = c(0.965, 0.995), yaxt = "n", xaxt = "n", ylab = "DSR", xlab = "Days", main = "Wetland habitat & precipitation effect")
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"] - TAB2$se[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], col = "springgreen4", lty = 3, lwd = 1.5)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"] + TAB2$se[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], col = "springgreen4", lty = 3, lwd = 1.5)
# HIGH PREC
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], type = "b", col = "springgreen3", bg = "springgreen3", pch = 24, bty = "n")
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"] - TAB2$se[TAB2$hab == "WET" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], col = "springgreen3", lty = 3, lwd = 1.5)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"] + TAB2$se[TAB2$hab == "WET" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], col = "springgreen3", lty = 3, lwd = 1.5)
# LOW PREC
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "LOW" & TAB2$temp == "INTER"], type = "b", col = "olivedrab2", bg = "olivedrab2", pch = 25, bty = "n")
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "LOW" & TAB2$temp == "INTER"] - TAB2$se[TAB2$hab == "WET" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], col = "olivedrab2", lty = 3, lwd = 1.5)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "LOW" & TAB2$temp == "INTER"] + TAB2$se[TAB2$hab == "WET" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], col = "olivedrab2", lty = 3, lwd = 1.5)

axis(1, at = seq(0, 28, by = 2))
axis(2, at = seq(0.965, 0.995, by = 0.01))
text(x = 26, y = 0.995, labels = "(b)", pos = 3)
dev.off()
###############################################################################################
# Graph parameters
mini <- min(TAB2$estimate[TAB2$prec == "INTER"] - TAB2$se[TAB2$prec == "INTER"])
maxi <- max(TAB2$estimate[TAB2$prec == "INTER"] + TAB2$se[TAB2$prec == "INTER"])

png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 1 - Geese nesting success & supplemented nests/PAPER/Figures/GOOSE_EXTREM_TEMP.tiff",
    res=300,
    width=30,
    height=20,
    pointsize=12,
    unit="cm",
    bg="transparent")

par(mfrow = c(1, 2),
    las = 1,# plot labels always horizontal
    bty = "n")  
# GRAPHE 3 - MESIC HABITAT & AIR TEMPERATURE
# INTERMEDIATE AIR TEMPERATURE
plot(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], type = "b", col = "orangered", bg = "orangered", pch = 21, bty = "n", ylim = c(0.935, 0.995), yaxt = "n", ylab = "DSR", xlab = "Days", xaxt = "n", main = "Mesic habitat & temperature effect")
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"] - TAB2$se[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], col = "orangered",lty = 3, lwd = 1.5)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"] + TAB2$se[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], col = "orangered",lty =3, lwd = 1.5)

# WARM AIR TEMPERATURE
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "WARM"], type = "b", col = "red4", bg = "red4", pch = 24, bty = "n")
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "WARM"] - TAB2$se[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "WARM"], col = "red4",lty =3, lwd = 1.5)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "WARM"] + TAB2$se[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "WARM"], col = "red4",lty =3, lwd = 1.5)
# COLD AIR TEMPERATURE
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "COLD"], type = "b", col = "orange", bg = "orange", pch = 25, bty = "n")
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "COLD"] - TAB2$se[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "COLD"], col = "orange",lty =3, lwd = 1.5)

lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "COLD"] + TAB2$se[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "COLD"], col = "orange",lty =3, lwd = 1.5)

legend("bottomright", col = c("orangered", "red4", "orange"), pch = c(21, 24, 25), pt.bg = c("orangered", "red4", "orange"), legend = c("inter", "warm", "cold"), bty = "n")
axis(1, at = seq(0, 28, by = 2))
axis(2, at = seq(0.935, 0.995, by = 0.01))
text(x = 26, y = 0.995, labels = "(a)", pos = 3)

# GRAPHE 4 - WETLAND HABITAT & AIR TEMPERATURE
# INTERMEDIATE AIR TEMPERATURE
plot(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], type = "b", col = "orangered", bg = "orangered", pch = 21, bty = "n", yaxt = "n", ylim = c(0.935, 0.995), ylab = "DSR", xlab = "Days", xaxt = "n", main = "Wetland habitat & temperature effect")
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"] - TAB2$se[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], col = "orangered",lty =3, lwd = 1.5)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"] + TAB2$se[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], col = "orangered",lty =3, lwd = 1.5)
# WARM AIR TEMPERATURE
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "WARM"], type = "b", col = "red4", bg = "red4", pch = 24, bty = "n")
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "WARM"] - TAB2$se[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "WARM"], col = "red4",lty =3, lwd = 1.5)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "WARM"] + TAB2$se[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "WARM"], col = "red4",lty =3, lwd = 1.5)
# COLD AIR TEMPERATURE
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "COLD"], type = "b", col = "orange", bg = "orange", pch = 25, bty = "n")
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "COLD"] - TAB2$se[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "COLD"], col = "orange",lty =3, lwd = 1.5)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "COLD"] + TAB2$se[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "COLD"], col = "orange",lty =3, lwd = 1.5)


legend("bottomright", col = c("orangered", "red4", "orange"), pch = c(21, 24, 25), pt.bg = c("orangered", "red4", "orange"), legend = c("inter", "warm", "cold"), bty = "n")
axis(1, at = seq(0, 28, by = 2))
axis(2, at = seq(0.935, 0.995, by = 0.01))
text(x = 26, y = 0.995, labels = "(b)", pos = 3)

dev.off()

#### Histogram of NS depending on habitat and supplementation ####


setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")


#### Water models ####
# Concerning years : 2005, 2015, 2016 & 2017

sup <- read.table("GOOSE_geese_with_WF.txt", sep = ",", h = T)
head(sup)
summary(sup)

sup <- sup[,-1]
supW1 <- sup[which(sup$YEAR == 2015 | sup$YEAR == 2016 | sup$YEAR == 2017),]
supW1 <- supW1[which(supW1$SUPPL == "W" | supW1$SUPPL == "TEM"),]
supW1 <- droplevels(supW1)
summary(supW1)

supW1$YEAR <- as.factor(supW1$YEAR)
supW1$Fate <- as.factor(supW1$Fate)

# Add 2005 data
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

require(RMark)

nocc <- max(supW2$LastChecked)


wat9 <- mark(supW2, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + YEAR + NestAge)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)

wat9$results$beta
utils::View(wat9$results$real)

TAB <- as.data.frame(wat9$results$real)[,-c(5,6)]
TAB$YEAR <- c(rep(2005, 152), rep(2015, 152), rep(2016, 152), rep(2017, 152))
TAB$SUPPL <- c("TEM", "WAT")[rep(c(rep(1, 38), rep(2, 38)), times = 8)]
TAB$HAB <- c("MES", "WET")[rep(c(rep(1, 76), rep(2, 76)), times = 4)] 

lTAB <- split(TAB, list(TAB$YEAR, TAB$HAB, TAB$SUPPL))
NS <- NULL

for (i in 1:16){
  lTAB[[i]] <- lTAB[[i]][1:27,] 
  SR <- prod(lTAB[[i]]$estimate)
  year <- unique(lTAB[[i]]$YEAR)
  hab <- unique(lTAB[[i]]$HAB)
  suppl <- unique(lTAB[[i]]$SUPPL)
  
  r<- c(year, hab, suppl, SR)
  
  NS <- rbind(NS, r)
}

NS <- as.data.frame(NS)
names(NS) <- c("year", "hab", "suppl", "SR")
NS$SR <- as.numeric(as.character(NS$SR))
summary(NS)
utils::View(NS)

lNS <- split(NS, NS$year)
lNS <- lapply(lNS, function(i){
   i[order(i$hab),]
} )
x11()
par(mfrow = c(2,2))

lapply(lNS, function(i){
  barplot(i$SR, main = unique(i$year))
})

#### Plot of extreme weather model ####
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()
load("geeseEXTREM_1.rda")
summary(m5)
m5$results$beta
res <- as.data.frame(m5$results$beta)
dim(res)
summary(res)
str(res)
res$name <- c("intercept", "hab_WET", "NestAge", "rain_HIGH", "rain_LOW", "temp_COLD", "temp_WARM")
res

# Plot of modele estimates
x11()
plot(res$estimate, xaxt = "n", bty = "n")
abline(h = 0, lty = 3)

for (i in 2:7){
  arrows(i, res$estimate[i], i, res$ucl[i], length = 0)
  arrows(i, m5$results$beta$estimate[i], i, m5$results$beta$lcl[i], length = 0)
}

#### Plot of effects temperature and precipitation on DSR (quantitative variables) ####
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()
geese <- read.table("GOOSE_geese_with_WF.txt", h = T, sep = ",")
summary(geese)

# Exploration
X11()
par(mfrow = c(1, 2))
plot(geese$cumPREC[geese$TYP_TEMP == "COLD"], geese$Fate[geese$TYP_TEMP == "COLD"], col = "blue")
points(geese$cumPREC[geese$TYP_TEMP == "WARM"], geese$Fate[geese$TYP_TEMP == "WARM"], col = "red")
points(geese$cumPREC[geese$TYP_TEMP == "INTER"], geese$Fate[geese$TYP_TEMP == "INTER"], col = "grey")

# Exploration
plot(geese$cumPREC[geese$RAINFALL == "LOW"], geese$Fate[geese$RAINFALL == "LOW"], col = "blue")
points(geese$cumPREC[geese$RAINFALL == "HIGH"], geese$Fate[geese$RAINFALL == "HIGH"], col = "red")
points(geese$cumPREC[geese$RAINFALL == "INTER"], geese$Fate[geese$RAINFALL == "INTER"], col = "grey")

load("geesePAPER.rda") # geese.PAPER.results
geese.PAPER.results$t9$results$beta # modele t9 is the best modele, including interaction between precipitation and temperature
geese.PAPER.results$t9$results$real

# Obtain a slope for DSR vs. temp depending on 3 groups of cumulative precipitation (LOW, INTER, HIGH)

geeseHIGH <- geese[geese$RAINFALL == "HIGH",]
geeseINTER <- geese[geese$RAINFALL == "INTER",]
geeseLOW <- geese[geese$RAINFALL == "LOW",]

    # For HIGH precipitation seasons
require(RMark)
nocc <- max(geeseHIGH$LastChecked)

mark(geeseHIGH, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge + HAB + TEMP_Y)), groups = "HAB", delete = T) # TEMP_y estimate = 0.44 

    # For INTERMEDIATE precipitation seasons
require(RMark)
nocc <- max(geeseINTER$LastChecked)

mark(geeseINTER, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge + HAB + TEMP_Y)), groups = "HAB", delete = T) # TEMP_y estimate = 0

    # For LOW precipitation seasons
require(RMark)
nocc <- max(geeseLOW$LastChecked)

mark(geeseLOW, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge + HAB + TEMP_Y)), groups = "HAB", delete = T) # TEMP_y estimate = 0.79

# Obtain a slope for DSR vs. temp depending on 3 groups of MEAN TEMPERATURE (COLD, INTER, WARM)

geeseWARM <- geese[geese$TYP_TEMP == "WARM",]
geeseINTER <- geese[geese$TYP_TEMP == "INTER",]
geeseCOLD <- geese[geese$TYP_TEMP == "COLD",]

# For WARM seasons
require(RMark)
nocc <- max(geeseWARM$LastChecked)

mark(geeseWARM, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge + HAB + quad_prec)), groups = "HAB", delete = T) # PREC_y estimate = -0.02
geese$quad_prec <- (geese$PREC_Y^2)
# For INTERMEDIATE temperature seasons
require(RMark)
nocc <- max(geeseINTER$LastChecked)

mark(geeseINTER, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge + HAB + PREC_Y)), groups = "HAB", delete = T) # PREC_y estimate = -0.007

# For COLD seasons
require(RMark)
nocc <- max(geeseCOLD$LastChecked)

mark(geeseCOLD, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge + HAB + PREC_Y)), groups = "HAB", delete = T) # PREC_y estimate = 0
