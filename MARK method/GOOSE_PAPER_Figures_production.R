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
cols <- c("springgreen4", "springgreen3", "olivedrab2")[as.numeric(cum2$RAINFALL)]

barplot(my_vector,
        col = cols,
        border = cols,
        # main = "Trend for specific dates for each goose nesting period",
        #ylab = "Cumulative precipitation (mm)",
        yaxt = "n",
        ylim = c(0, max(cum2$cumRAIN + 5)))
axis(side = 2,
     lwd = 1,
     las = 2,
     cex.axis = 1.5)
abline(h = mean(cum2$cumRAIN),
       col = "olivedrab4",
       lwd = 3.5,
       lty = "solid")
abline(h = mean(cum2$cumRAIN) - sd(cum2$cumRAIN),
       col = "olivedrab4",
       lwd = 2.5,
       lty = "dotdash")
abline(h = mean(cum2$cumRAIN) + sd(cum2$cumRAIN),
       col = "olivedrab4",
       lwd = 2.5,
       lty = "dotdash")
legend(-0.5, 70, legend = c("high", "intermediate", "low"), col =c("springgreen4", "springgreen3", "olivedrab2"), pch = 15, bty = "n", cex = 2)
#legend(26.5, 75, legend = "(a)", bty = "n")

#par(new = T)
#### Annual air temperature ####
WEA$type_temp[WEA$meanTEMP >= mean(WEA$meanTEMP) + sd(WEA$meanTEMP)] <- "WARM"
WEA$type_temp[WEA$meanTEMP <= mean(WEA$meanTEMP) - sd(WEA$meanTEMP)] <- "COLD"
WEA$type_temp[is.na(WEA$type_temp)] <- "INTER"
WEA$type_temp <- as.factor(WEA$type_temp)

cols2 <- c("orange", "black", "red4")[as.numeric(WEA$type_temp)]
pchs <- c(25, 19, 24)[as.numeric(WEA$type_temp)]
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
     #xaxt = "n",
     cex = c(2, 1, 2)[as.numeric(WEA$type_temp)],
     cex.lab = 1,
     #col = cols2,
     col = "darkgoldenrod3",
     pch = pchs,
     bg = cols2,
     lwd = 3,
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

#### Figures SQEBC ####
# Cum prec plot
png("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ADMIN, COURSES & PRESENTATION/Colloques & congrès - Présentations orales & affiches/2018 SQEBC/2018_SQEBC_Presentation/Figures/GOOSE_prec.tiff",
    res=300,
    width=25,
    height=20,
    pointsize=12,
    unit="cm",
    bg="transparent")
my_vector <- cum2$cumRAIN
names(my_vector) <- cum2$YEAR
cols <- c("springgreen4", "springgreen3", "olivedrab2")[as.numeric(cum2$RAINFALL)]

barplot(my_vector,
        col = cols,
        border = cols,
        # main = "Trend for specific dates for each goose nesting period",
        #ylab = "Cumulative precipitation (mm)",
        yaxt = "n",
        ylim = c(0, max(cum2$cumRAIN + 5)))
axis(side = 2,
     lwd = 1,
     las = 2,
     cex.axis = 1.5)
abline(h = mean(cum2$cumRAIN),
       col = "olivedrab4",
       lwd = 3.5,
       lty = "solid")
abline(h = mean(cum2$cumRAIN) - sd(cum2$cumRAIN),
       col = "olivedrab4",
       lwd = 2.5,
       lty = "dotdash")
abline(h = mean(cum2$cumRAIN) + sd(cum2$cumRAIN),
       col = "olivedrab4",
       lwd = 2.5,
       lty = "dotdash")
legend(-0.5, 70, legend = c("high", "intermediate", "low"), col =c("springgreen4", "springgreen3", "olivedrab2"), pch = 15, bty = "n", cex = 2)
dev.off()
# Mean temp plot

WEA$type_temp[WEA$meanTEMP >= mean(WEA$meanTEMP) + sd(WEA$meanTEMP)] <- "WARM"
WEA$type_temp[WEA$meanTEMP <= mean(WEA$meanTEMP) - sd(WEA$meanTEMP)] <- "COLD"
WEA$type_temp[is.na(WEA$type_temp)] <- "INTER"
WEA$type_temp <- as.factor(WEA$type_temp)

png("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ADMIN, COURSES & PRESENTATION/Colloques & congrès - Présentations orales & affiches/2018 SQEBC/2018_SQEBC_Presentation/Figures/GOOSE_temp.tiff",
    res=300,
    width=25,
    height=20,
    pointsize=12,
    unit="cm",
    bg="transparent")

cols2 <- c("orange", "orangered", "red4")[as.numeric(WEA$type_temp)]
pchs <- c(25, 21, 24)[as.numeric(WEA$type_temp)]
plot(WEA$YEAR,
     WEA$meanTEMP,
     xlab = "",
     ylab = "",
     #ylab = "Mean air temperature (°C)",
     xaxp = c(1995, 2017, 22),
     ylim = c(3, 8),
     xlim = c(1995, 2017),
     bty = "n",
     #yaxp = c(min(WEA$meanTEMP) - 0.5, max(WEA$meanTEMP) + 0.5, 6),
     yaxt = "n",
     #xaxt = "n",
     cex = c(2, 1, 2)[as.numeric(WEA$type_temp)],
     cex.axis = 1.5,
     col = cols2,
     pch = pchs,
     bg = cols2,
     lwd = 3,
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
     las = 2,
     cex.axis = 1.5)

legend(1995, 8, legend = c("warm", "intermediate", "cold"), col = c("red4", "orangered", "orange"), pch =  c(24, 21, 25), pt.bg = c("red4", "orangered", "orange"), bty = "n", cex = 2)


dev.off()

#Superposition des deux graphiques -- Presentation for SQEBC #
png("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ADMIN, COURSES & PRESENTATION/Colloques & congrès - Présentations orales & affiches/2018 SQEBC/2018_SQEBC_Presentation/Figures/GOOSE_prec_temp_superposition.tiff",
    res=300,
    width=25,
    height=20,
    pointsize=12,
    unit="cm",
    bg="transparent")
#x11()
par(mar=c(5,5,1,5)) # inner margin - default parameter is par("mar") <- 5.1 4.1 4.1 2.1
cols <- c("springgreen3", "black", "olivedrab2")[as.numeric(cum2$RAINFALL)]
cols2 <- c("orange", "black", "red4")[as.numeric(WEA$type_temp)]
pchs <- c(25, 21, 24)[as.numeric(WEA$type_temp)]
r <- barplot(my_vector,
             col = cols,
             border = cols,
             # main = "Trend for specific dates for each goose nesting period",
             #ylab = "Cumulative precipitation (mm)",
             yaxt = "n",
             xaxt = "n",
             ylim = c(0, max(cum2$cumRAIN + 10)),
             cex.axis = 1.5)
lines(r, WEA$meanTEMP*10, cex = c(2, 1, 2)[as.numeric(WEA$type_temp)], pch = pchs, col = cols2, bg = cols2, type = "b", lwd = 3, bty = "n")
#points(r, WEA$meanTEMP*10, xaxt = "n", yaxt = "n", cex = c(2, 1, 2)[as.numeric(WEA$type_temp)], pch = pchs, bg = cols2, lwd = 3, col = cols2, bty = "n")
axis(side = 1, at = r, labels = cum2$YEAR, lwd = 1, las = 1, padj = 0, cex.axis = 1.5)
axis(side = 2, lwd = 1, las = 2, at = seq(0, 70, 10), labels = seq(0, 70, 10), padj = 0, cex.axis = 1.5)
axis(side = 4, lwd = 1, las = 2, at = seq(35, 75, 10), labels = (seq(35, 75, 10)/10), padj = 0, cex.axis = 1.5)

dev.off()
#### Extreme climate years impacts on DSR ####

setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
rm(list = ls())

load("geeseEXTREM.rda") # Object name = geese.EXTREM.results
geese.EXTREM.results

load("geeseEXTREM_1.rda") # modele name = m5
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

#png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 1 - Geese nesting success & supplemented nests/PAPER/Figures/GOOSE_EXTREM_TEMP.tiff",
 #   res=300,
  #  width=30,
   # height=20,
    #pointsize=12,
    #unit="cm",
    #bg="transparent")

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

#dev.off()

#### Superposition simplified plot of precipitation and temperature by habitat####

# Plot 1
mini <- min(TAB2$estimate)
maxi <- max(TAB2$estimate)

png("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ADMIN, COURSES & PRESENTATION/Colloques & congrès - Présentations orales & affiches/2018 SQEBC/2018_SQEBC_Presentation/Figures/GOOSE_DSR_prec_temp_superposition.tiff",
    res=300,
    width=30,
    height=20,
    pointsize=12,
    unit="cm",
    bg="transparent")
par(mfrow = c(1, 2))
# MESIC - Intermediaire - prec
plot(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], type = "l", col = "springgreen4", bty = "n", ylim = c(mini, maxi), xaxt = "n", ylab = "DSR", xlab = "Days", main = "Mesic habitat", las =1, lwd = 3, cex.lab = 1.5)
# MESIC - high = low - prec
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], type = "l", col = "springgreen3",bty = "n", lwd = 3)
# MESIC - Intermediate = warm - temperature
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "WARM"], type = "l", col = "red4", bty = "n", lwd = 3)
# MESIC - Cold - temperature
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "COLD"], type = "l", col = "orange", bty = "n", lwd = 3)

legend(x = 14, y = 0.95,  col = c("springgreen4", "springgreen3", "red4", "orange"), pch = 16, legend = c("inter. prec.", "high/low prec.", "inter./warm temp.", "cold temp."), bty = "n", cex = 1.5)
axis(1, at = seq(0, 28, by = 2))

# Plot 2

# WETLAND - Intermediaire - prec
plot(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], type = "l", col = "springgreen4", bty = "n", ylim = c(mini, maxi), xaxt = "n", ylab = "DSR", xlab = "Days", main = "Wetland habitat", las =1, lwd = 3, cex.lab = 1.5)
# WETLAND - high = low - prec
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], type = "l", col = "springgreen3",bty = "n", lwd = 3)
# WETLAND - Intermediate = warm - temperature
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "WARM"], type = "l", col = "red4", bty = "n", lwd = 3)
# WETLAND - Cold - temperature
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "COLD"], type = "l", col = "orange", bty = "n", lwd = 3)

legend(x = 14, y = 0.95, col = c("springgreen4", "springgreen3", "red4", "orange"), pch = 16, legend = c("inter. prec.", "high/low prec.", "inter./warm temp.", "cold temp."), bty = "n", cex = 1.5)
axis(1, at = seq(0, 28, by = 2))

dev.off()

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


setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
rm(list = ls())

load("geeseEXTREM.rda") # Object name = geese.EXTREM.results
geese.EXTREM.results

load("geeseEXTREM_1.rda") # modele name = m5
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

#x11()

png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 1 - Geese nesting success & supplemented nests/PAPER/Figures/GOOSE_EXTREM_PREC.tiff",
res=300,
width=30,
height=30,
pointsize=12,
unit="cm",
bg="transparent")

par(mfrow = c(2, 2),
    las = 1,# plot labels always horizontal
    bty = "n",
    mar = c(1, 4.5, 1, 1))
# GRAPHE 1 - MESIC HABITAT & CUMULATIVE PRECIPITATION
# INTERMEDIATE PREC
plot(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], type = "b", col = "springgreen4", bg = "springgreen4", pch = 21, bty = "n", yaxt = "n", ylim = c(0.965, 0.995), xaxt = "n", ylab = "", xlab = "", cex = 1, cex.axis = 1.5, lwd = 2)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"] - TAB2$se[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], col = "springgreen4", lty = 3, lwd = 2)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"] + TAB2$se[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], col = "springgreen4", lty = 3, lwd = 2)
# HIGH PREC
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], type = "b", col = "springgreen3", bg = "springgreen3", pch = 24, bty = "n", lwd = 2)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"] - TAB2$se[TAB2$hab == "MES" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], col = "springgreen3", lty = 3, lwd = 2)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"] + TAB2$se[TAB2$hab == "MES" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], col = "springgreen3", lty = 3, lwd = 2)
# LOW PREC
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "LOW" & TAB2$temp == "INTER"], type = "b", col = "olivedrab2", bg = "olivedrab2", pch = 25, bty = "n", lwd = 2)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "LOW" & TAB2$temp == "INTER"] - TAB2$se[TAB2$hab == "MES" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], col = "olivedrab2", lty = 3, lwd = 2)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "LOW" & TAB2$temp == "INTER"] + TAB2$se[TAB2$hab == "MES" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], col = "olivedrab2", lty = 3, lwd = 2)


#axis(1, at = seq(0, 28, by = 2))
axis(2, at = seq(0.965, 0.995, by = 0.01), cex = 1.5, lwd = 2, cex.axis = 1.5)
text(x = 26, y = 0.993, labels = "(a)", pos = 3, cex = 1.5)

# GRAPHE 2 - WETLAND HABITAT & CUMULATIVE PRECIPITATION
plot(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], type = "b", col = "springgreen4", bg = "springgreen4", pch = 21, bty = "n", ylim = c(0.965, 0.995), yaxt = "n", xaxt = "n", ylab = "", lwd = 2)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"] - TAB2$se[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], col = "springgreen4", lty = 3, lwd = 2)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"] + TAB2$se[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], col = "springgreen4", lty = 3, lwd = 2)
# HIGH PREC
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], type = "b", col = "springgreen3", bg = "springgreen3", pch = 24, bty = "n", lwd = 2)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"] - TAB2$se[TAB2$hab == "WET" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], col = "springgreen3", lty = 3, lwd = 2)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"] + TAB2$se[TAB2$hab == "WET" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], col = "springgreen3", lty = 3, lwd = 2)
# LOW PREC
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "LOW" & TAB2$temp == "INTER"], type = "b", col = "olivedrab2", bg = "olivedrab2", pch = 25, bty = "n", lwd = 2)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "LOW" & TAB2$temp == "INTER"] - TAB2$se[TAB2$hab == "WET" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], col = "olivedrab2", lty = 3, lwd = 2)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "LOW" & TAB2$temp == "INTER"] + TAB2$se[TAB2$hab == "WET" & TAB2$prec == "HIGH" & TAB2$temp == "INTER"], col = "olivedrab2", lty = 3, lwd = 2)

legend("bottomright", col = c("springgreen4", "springgreen3", "olivedrab2"), pch = c(21, 24, 25), pt.bg = c("springgreen4", "springgreen3", "olivedrab2"), legend = c("inter", "high", "low"), bty = "n", cex = 2)
#axis(1, at = seq(0, 28, by = 2))
axis(2, at = seq(0.965, 0.995, by = 0.01), cex = 1.5, cex.axis = 1.5, lwd = 2)
text(x = 26, y = 0.993, labels = "(b)", pos = 3, cex = 1.5)

# Graph parameters for graph 3 & 4
mini <- min(TAB2$estimate[TAB2$prec == "INTER"] - TAB2$se[TAB2$prec == "INTER"])
maxi <- max(TAB2$estimate[TAB2$prec == "INTER"] + TAB2$se[TAB2$prec == "INTER"])
par(mar = c(4, 4.5, 0.5, 1))
# GRAPHE 3 - MESIC HABITAT & AIR TEMPERATURE
# INTERMEDIATE AIR TEMPERATURE
plot(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], type = "b", col = "orangered", bg = "orangered", pch = 21, bty = "n", ylim = c(0.935, 0.995), yaxt = "n", ylab = "", xaxt = "n", xlab = "", lwd = 2)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"] - TAB2$se[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], col = "orangered",lty = 3, lwd = 2)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"] + TAB2$se[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], col = "orangered",lty =3, lwd = 2)

# WARM AIR TEMPERATURE
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "WARM"], type = "b", col = "red4", bg = "red4", pch = 24, bty = "n", lwd = 2)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "WARM"] - TAB2$se[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "WARM"], col = "red4",lty =3, lwd = 2)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "WARM"] + TAB2$se[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "WARM"], col = "red4",lty =3, lwd = 2)
# COLD AIR TEMPERATURE
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "COLD"], type = "b", col = "orange", bg = "orange", pch = 25, bty = "n", lwd = 2)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "COLD"] - TAB2$se[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "COLD"], col = "orange",lty =3, lwd = 2)
lines(TAB2$estimate[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "COLD"] + TAB2$se[TAB2$hab == "MES" & TAB2$prec == "INTER" & TAB2$temp == "COLD"], col = "orange",lty =3, lwd = 2)


axis(1, at = seq(0, 28, by = 2), cex = 1.5, cex.axis = 1.1, lwd =2)
axis(2, at = seq(0.935, 0.995, by = 0.01), cex = 1.5, cex.axis = 1.5, lwd = 2)
text(x = 26, y = 0.993, labels = "(c)", pos = 3, cex = 1.5)

# GRAPHE 4 - WETLAND HABITAT & AIR TEMPERATURE
# INTERMEDIATE AIR TEMPERATURE
plot(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], type = "b", col = "orangered", bg = "orangered", pch = 21, bty = "n", yaxt = "n", ylim = c(0.935, 0.995), ylab = "", xlab = "", xaxt = "n", lwd = 2)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"] - TAB2$se[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], col = "orangered",lty =3, lwd = 2)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"] + TAB2$se[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "INTER"], col = "orangered",lty =3, lwd = 2)
# WARM AIR TEMPERATURE
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "WARM"], type = "b", col = "red4", bg = "red4", pch = 24, bty = "n", lwd = 2)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "WARM"] - TAB2$se[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "WARM"], col = "red4",lty =3, lwd = 2)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "WARM"] + TAB2$se[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "WARM"], col = "red4",lty =3, lwd = 2)
# COLD AIR TEMPERATURE
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "COLD"], type = "b", col = "orange", bg = "orange", pch = 25, bty = "n", lwd = 2)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "COLD"] - TAB2$se[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "COLD"], col = "orange",lty =3, lwd = 2)
lines(TAB2$estimate[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "COLD"] + TAB2$se[TAB2$hab == "WET" & TAB2$prec == "INTER" & TAB2$temp == "COLD"], col = "orange",lty =3, lwd = 2)


legend("bottomright", col = c("orangered", "red4", "orange"), pch = c(21, 24, 25), pt.bg = c("orangered", "red4", "orange"), legend = c("inter", "warm", "cold"), bty = "n", cex = 2)

axis(1, at = seq(0, 28, by = 2), cex = 1.5, cex.axis = 1.1, lwd = 2)
axis(2, at = seq(0.935, 0.995, by = 0.01), cex = 1.5, cex.axis = 1.5, lwd = 2)
text(x = 26, y = 0.993, labels = "(d)", pos = 3, cex = 1.5)

dev.off()

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


#### Plot effect of supplementation ####

#### WATER SUPPLEMENTATION 2005 - 2017 ####
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()
load(file = "waterGEESE_2005-2017.rda") # water.2.results - Load the modeles comparison
load(file = "waterGEESE_2005-2017_1.rda") # wat8 - Best model #1
load(file = "waterGEESE_2005-2017_2.rda") # wat9 - Best model #2

wat9$results$beta
utils::View(wat9$results$real)

TAB <- as.data.frame(wat9$results$real)[,-c(5,6)]
TAB$YEAR <- c(rep(2005, 152), rep(2015, 152), rep(2016, 152), rep(2017, 152))
TAB$SUPPL <- c("TEM", "WAT")[rep(c(rep(1, 38), rep(2, 38)), times = 8)]
TAB$HAB <- c("MES", "WET")[rep(c(rep(1, 76), rep(2, 76)), times = 4)] 
utils::View(TAB)

lTAB <- split(TAB, list(TAB$YEAR, TAB$HAB, TAB$SUPPL))
NS <- NULL

for (i in 1:16){
  lTAB[[i]] <- lTAB[[i]][1:27,] 
  SR <- prod(lTAB[[i]]$estimate)
  year <- unique(lTAB[[i]]$YEAR)
  hab <- unique(lTAB[[i]]$HAB)
  suppl <- unique(lTAB[[i]]$SUPPL)
  se <- prod(lTAB[[i]]$se)
  
  r<- c(year, hab, suppl, SR, se)
  
  NS <- rbind(NS, r)
}

NS <- as.data.frame(NS)
names(NS) <- c("year", "hab", "suppl", "SR", "se")
NS$SR <- as.numeric(as.character(NS$SR))
NS$se <- as.numeric(as.character(NS$se))
summary(NS)

# Solution 1
NS <- NS[order(NS$year, NS$hab),]
utils::View(NS)

png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 1 - Geese nesting success & supplemented nests/PAPER/Figures/GOOSE_wat_suppl.tiff",
    res=300,
    width=25,
    height=20,
    pointsize=12,
    unit="cm",
    bg="transparent")
#x11()
par(mar=c(5,6,4,5)) # inner margin - default parameter is par("mar") <- 5.1 4.1 4.1 2.1
cols <- c("aquamarine3", "aquamarine4")[as.numeric(NS$hab)]
barCenters <- barplot(NS$SR,
                      width = 0.5,
                      col = cols,
                      #xlab = "Year",
                      #ylab = "Nesting success",
                      ylim = c(0, 1),
                      names.arg = rep(c("C", "WAT"), 8),
                      legend.text = TRUE,
                      space = c(0.2, rep(c(0,0.05,0,0.4), 3), c(0,0.05,0)),
                      las = 1,
                      cex.axis = 1.5,
                      cex = 1)
legend(#"topleft",
        x = 0,
        y = 0.95,
       legend = c("Mesic", "Wetland"), 
       fill = c("aquamarine3", "aquamarine4"),
       bty = "n",
       cex = 1.2)
text(c(1, 3.5, 5.5, 8), rep(0.98, 4), c(2005, 2015:2017), cex = 1.5)

#text(barCenters,0.2, labels = paste("(", as.character(prop$n), ")", sep = ""))

dev.off()

# Solution 2
lNS <- split(NS, NS$year)
lNS <- lapply(lNS, function(i){
  i[order(i$hab),]
} )

x11()
par(mfrow = c(2,2))
lapply(lNS, function(i){
  cols <- c("aquamarine3", "aquamarine4")[as.numeric(i$hab)]
  r <- barplot(i$SR, main = unique(i$year), names.arg = i$suppl ,col = cols)
  legend("topleft", legend = c("MESIC HAB.", "WETLAND"), col = cols)
})

barplot(NS$SR)


#### FOOD SUPPLEMENTATION 2005 - 2017 ####
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()
load(file = "foodGEESE.rda") # foodGEESE - Load the modeles comparison
load(file = "foodGEESE_1.rda") # foo14 - Best model #1 on 5 modeles
load(file = "foodGEESE_2.rda") # foo8 - Best model #2 on 5 modeles with supplementation effects

foo8$results$beta
utils::View(foo8$results$real)

TAB <- as.data.frame(foo8$results$real)[,-c(5,6)]
TAB$YEAR <- c(rep(2015, 152), rep(2016, 152), rep(2017, 152))
TAB$SUPPL <- c("TEM", "FOO")[rep(c(rep(1, 38), rep(2, 38)), times = 6)]
TAB$HAB <- c("MES", "WET")[rep(c(rep(1, 76), rep(2, 76)), times = 3)] 
utils::View(TAB)

# NS total
lTAB <- split(TAB, list(TAB$YEAR, TAB$HAB, TAB$SUPPL))
NS <- NULL

for (i in 1:12){
  lTAB[[i]] <- lTAB[[i]][1:27,] 
  SR <- prod(lTAB[[i]]$estimate)
  year <- unique(lTAB[[i]]$YEAR)
  hab <- unique(lTAB[[i]]$HAB)
  suppl <- unique(lTAB[[i]]$SUPPL)
  se <- prod(lTAB[[i]]$se)
  
  r<- c(year, hab, suppl, SR, se)
  
  NS <- rbind(NS, r)
}

NS <- as.data.frame(NS)
names(NS) <- c("year", "hab", "suppl", "SR", "se")
NS$SR <- as.numeric(as.character(NS$SR))
NS$se <- as.numeric(as.character(NS$se))
summary(NS)

utils::View(NS)

# Solution 1
NS <- NS[order(NS$year, NS$hab, - as.numeric(NS$suppl)),]
utils::View(NS)

png("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ADMIN, COURSES & PRESENTATION/Colloques & congrès - Présentations orales & affiches/2018 SQEBC/2018_SQEBC_Presentation/Figures/GOOSE_foo_suppl.tiff",
    res=300,
    width=25,
    height=20,
    pointsize=12,
    unit="cm",
    bg="transparent")
cols <- c("darkgoldenrod2", "darkgoldenrod3")[as.numeric(NS$hab)]
barCenters <- barplot(NS$SR,
                      #width = 0.5,
                      col = cols,
                      #xlab = "Year",
                      #ylab = "Nesting success",
                      ylim = c(0, 1),
                      names.arg = c("",2015, "", "", "", 2016, "", "", "", 2017, "", ""),
                      legend.text = TRUE,
                      space = c(0.2, rep(c(0,0.05,0,0.4), 2), c(0,0.05,0)),
                      las = 1,
                      cex.axis = 1.5)
legend("topleft",
       inset = c(0, - 0.05),
       legend = c("Mesic hab.", "Wetland"), 
       fill = c("darkgoldenrod2", "darkgoldenrod3"),
       bty = "n")

#text(barCenters,0.2, labels = paste("(", as.character(prop$n), ")", sep = ""))

dev.off()

# Solution 2
utils::View(TAB)
TAB$SUPPL <- as.factor(TAB$SUPPL)
TAB$HAB <- as.factor(TAB$HAB)
TAB$X <- rep(1:27)
summary(TAB)

llTAB <- split(TAB, TAB$YEAR)
# 2015 
summary(llTAB[[1]])
  # y lim
mini <- min(llTAB[[1]]$lcl)
maxi <- max(llTAB[[1]]$ucl)

  # Plot 1 - Food in mesic
plot(llTAB[[1]]$estimate[llTAB[[1]]$SUPPL == "TEM" & llTAB[[1]]$HAB == "MES"], ylim = c(mini, maxi), type = "l", bty = "n", col = "black", main = "2015 - Mesic habitat")
lines(llTAB[[1]]$ucl[llTAB[[1]]$SUPPL == "TEM" & llTAB[[1]]$HAB == "MES"], col = "black", lty = 3, lwd = 1.5)
lines(llTAB[[1]]$lcl[llTAB[[1]]$SUPPL == "TEM" & llTAB[[1]]$HAB == "MES"],  col = "black", lty = 3, lwd = 1.5)


lines(llTAB[[1]]$estimate[llTAB[[1]]$SUPPL == "FOO" & llTAB[[1]]$HAB == "MES"], type = "l", bty = "n", col = "darkgoldenrod3")
lines(llTAB[[1]]$ucl[llTAB[[1]]$SUPPL == "FOO" & llTAB[[1]]$HAB == "MES"], col = "darkgoldenrod3", lty = 3, lwd = 1.5)
lines(llTAB[[1]]$lcl[llTAB[[1]]$SUPPL == "FOO" & llTAB[[1]]$HAB == "MES"], col = "darkgoldenrod3", lty = 3, lwd = 1.5)

  # Plot 2 - Food in wetland
plot(llTAB[[1]]$estimate[llTAB[[1]]$SUPPL == "TEM" & llTAB[[1]]$HAB == "WET"], ylim = c(mini, maxi), type = "l", bty = "n", col = "black", main = "2015 - Wetland")
lines(llTAB[[1]]$ucl[llTAB[[1]]$SUPPL == "TEM" & llTAB[[1]]$HAB == "WET"], col = "black", lty = 3, lwd = 1.5)
lines(llTAB[[1]]$lcl[llTAB[[1]]$SUPPL == "TEM" & llTAB[[1]]$HAB == "WET"], col = "black", lty = 3, lwd = 1.5)


lines(llTAB[[1]]$estimate[llTAB[[1]]$SUPPL == "FOO" & llTAB[[1]]$HAB == "WET"], type = "l", bty = "n", col = "darkgoldenrod3")
lines(llTAB[[1]]$ucl[llTAB[[1]]$SUPPL == "FOO" & llTAB[[1]]$HAB == "WET"],  col = "darkgoldenrod3", lty = 3, lwd = 1.5)
lines(llTAB[[1]]$lcl[llTAB[[1]]$SUPPL == "FOO" & llTAB[[1]]$HAB == "WET"], col = "darkgoldenrod3", lty = 3, lwd = 1.5)

# 2016 
summary(llTAB[[2]])
# y lim
mini <- min(llTAB[[2]]$lcl)
maxi <- max(llTAB[[2]]$ucl)

# Plot 1 - Food in mesic
plot(llTAB[[2]]$estimate[llTAB[[2]]$SUPPL == "TEM" & llTAB[[2]]$HAB == "MES"], ylim = c(mini, maxi), type = "l", bty = "n", col = "black", main = "2016 - Mesic habitat")
lines(llTAB[[2]]$ucl[llTAB[[2]]$SUPPL == "TEM" & llTAB[[2]]$HAB == "MES"], col = "black", lty = 3, lwd = 1.5)
lines(llTAB[[2]]$lcl[llTAB[[2]]$SUPPL == "TEM" & llTAB[[2]]$HAB == "MES"],  col = "black", lty = 3, lwd = 1.5)


lines(llTAB[[2]]$estimate[llTAB[[2]]$SUPPL == "FOO" & llTAB[[2]]$HAB == "MES"], type = "l", bty = "n", col = "darkgoldenrod3")
lines(llTAB[[2]]$ucl[llTAB[[2]]$SUPPL == "FOO" & llTAB[[2]]$HAB == "MES"], col = "darkgoldenrod3", lty = 3, lwd = 1.5)
lines(llTAB[[2]]$lcl[llTAB[[2]]$SUPPL == "FOO" & llTAB[[2]]$HAB == "MES"], col = "darkgoldenrod3", lty = 3, lwd = 1.5)

# Plot 2 - Food in wetland
plot(llTAB[[2]]$estimate[llTAB[[2]]$SUPPL == "TEM" & llTAB[[2]]$HAB == "WET"], ylim = c(mini, maxi), type = "l", bty = "n", col = "black", main = "2016 - Wetland")
lines(llTAB[[2]]$ucl[llTAB[[2]]$SUPPL == "TEM" & llTAB[[2]]$HAB == "WET"], col = "black", lty = 3, lwd = 1.5)
lines(llTAB[[2]]$lcl[llTAB[[2]]$SUPPL == "TEM" & llTAB[[2]]$HAB == "WET"], col = "black", lty = 3, lwd = 1.5)


lines(llTAB[[2]]$estimate[llTAB[[2]]$SUPPL == "FOO" & llTAB[[2]]$HAB == "WET"], type = "l", bty = "n", col = "darkgoldenrod3")
lines(llTAB[[2]]$ucl[llTAB[[2]]$SUPPL == "FOO" & llTAB[[2]]$HAB == "WET"],  col = "darkgoldenrod3", lty = 3, lwd = 1.5)
lines(llTAB[[2]]$lcl[llTAB[[2]]$SUPPL == "FOO" & llTAB[[2]]$HAB == "WET"], col = "darkgoldenrod3", lty = 3, lwd = 1.5)

# 2017 
summary(llTAB[[3]])
  # y lim
mini <- min(llTAB[[3]]$lcl)
maxi <- max(llTAB[[3]]$ucl)

  # Plot 1 - Food in mesic
plot(llTAB[[3]]$estimate[llTAB[[3]]$SUPPL == "TEM" & llTAB[[3]]$HAB == "MES"], ylim = c(mini, maxi), type = "l", bty = "n", col = "black", main = "2017 - Mesic habitat")
lines(llTAB[[3]]$ucl[llTAB[[3]]$SUPPL == "TEM" & llTAB[[3]]$HAB == "MES"], col = "black", lty = 3, lwd = 1.5)
lines(llTAB[[3]]$lcl[llTAB[[3]]$SUPPL == "TEM" & llTAB[[3]]$HAB == "MES"],  col = "black", lty = 3, lwd = 1.5)


lines(llTAB[[3]]$estimate[llTAB[[3]]$SUPPL == "FOO" & llTAB[[3]]$HAB == "MES"], type = "l", bty = "n", col = "darkgoldenrod3")
lines(llTAB[[3]]$ucl[llTAB[[3]]$SUPPL == "FOO" & llTAB[[3]]$HAB == "MES"], col = "darkgoldenrod3", lty = 3, lwd = 1.5)
lines(llTAB[[3]]$lcl[llTAB[[3]]$SUPPL == "FOO" & llTAB[[3]]$HAB == "MES"], col = "darkgoldenrod3", lty = 3, lwd = 1.5)

# Plot 2 - Food in wetland
plot(llTAB[[3]]$estimate[llTAB[[3]]$SUPPL == "TEM" & llTAB[[3]]$HAB == "WET"], ylim = c(mini, maxi), type = "l", bty = "n", col = "black", main = "2017 - Wetland")
lines(llTAB[[3]]$ucl[llTAB[[3]]$SUPPL == "TEM" & llTAB[[3]]$HAB == "WET"], col = "black", lty = 3, lwd = 1.5)
lines(llTAB[[3]]$lcl[llTAB[[3]]$SUPPL == "TEM" & llTAB[[3]]$HAB == "WET"], col = "black", lty = 3, lwd = 1.5)


lines(llTAB[[3]]$estimate[llTAB[[3]]$SUPPL == "FOO" & llTAB[[3]]$HAB == "WET"], type = "l", bty = "n", col = "darkgoldenrod3")
lines(llTAB[[3]]$ucl[llTAB[[3]]$SUPPL == "FOO" & llTAB[[3]]$HAB == "WET"],  col = "darkgoldenrod3", lty = 3, lwd = 1.5)
lines(llTAB[[3]]$lcl[llTAB[[3]]$SUPPL == "FOO" & llTAB[[3]]$HAB == "WET"], col = "darkgoldenrod3", lty = 3, lwd = 1.5)

#### FOOD SUPPLEMENTATION 2017 ####
load(file = "foodGEESE.2017.rda") # FOOD.results.2017
load(file = "foodGEESE.2017_1.rda") #foo7

foo7$results$beta
utils::View(foo7$results$real)

TAB <- as.data.frame(foo7$results$real)[,-c(5,6)]
TAB$YEAR <- rep(2017, 148)
TAB$SUPPL <- c("TEM", "FOO")[rep(c(rep(1, 37), rep(2, 37)), times = 2)]
TAB$HAB <- rep(c(rep("MES", 74), rep("WET", 74)))
TAB$SUPPL <- factor(TAB$SUPPL)
TAB$HAB <- factor(TAB$HAB)
utils::View(TAB)

lTAB <- split(TAB, list(TAB$HAB))


# y lim
mini <- min(TAB$lcl)
maxi <- max(TAB$ucl)

# Plot 1 - No habitat effect !
png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 1 - Geese nesting success & supplemented nests/PAPER/Figures/GOOSE_foo_suppl_2017.tiff",
    res=300,
    width=25,
    height=20,
    pointsize=12,
    unit="cm",
    bg="transparent")
#x11()
plot(lTAB[[2]]$estimate[lTAB[[2]]$SUPPL == "TEM" ][1:27], ylim = c(mini, maxi), type = "b", pch = 16, bty = "n", col = "black", las = 1, cex = 1, cex.axis = 1.5, lwd = 2, xlab = "", ylab = "")
lines(lTAB[[2]]$ucl[lTAB[[2]]$SUPPL == "TEM"][1:27], col = "black", lty = 3, lwd = 2)
lines(lTAB[[2]]$lcl[lTAB[[2]]$SUPPL == "TEM"][1:27],  col = "black", lty = 3, lwd = 2)


lines(lTAB[[2]]$estimate[lTAB[[2]]$SUPPL == "FOO"][1:27], type = "b", pch = 17, bty = "n", col = "darkgoldenrod3", lwd = 2)
lines(lTAB[[2]]$ucl[lTAB[[2]]$SUPPL == "FOO"][1:27], col = "darkgoldenrod3", lty = 3, lwd = 2)
lines(lTAB[[2]]$lcl[lTAB[[2]]$SUPPL == "FOO"][1:27], col = "darkgoldenrod3", lty = 3, lwd = 2)

legend(20, 0.92, legend = c("control", "food"), col = c("black", "darkgoldenrod3"), pch = c(16, 17), bty = "n", cex = 2)

dev.off()

