#### Script for figures production included in goose paper ####

setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()

#### Yearly cumulative precipitation ####
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

#write.table(cum2, "PREC_cum2.txt", dec = ".")


# Plot
#png("GOOSE_prec.tiff",
#res=300,
#width=20,
#height=15,
#pointsize=12,
#unit="cm",
#bg="transparent")

x11()
par(mar=c(5,5,1,1)) # inner margin - default parameter is par("mar") <- 5.1 4.1 4.1 2.1
my_vector <- cum2$cumRAIN
names(my_vector) <- cum2$YEAR
barplot(my_vector,
        col = "olivedrab3",
        border = "olivedrab3",
        # main = "Trend for specific dates for each goose nesting period",
        # ylab = "Cumulative precipitation (mm)",
        yaxt = "n",
        ylim = c(0, 70))
axis(side = 2,
     lwd = 1,
     las = 2)
abline(h = mean(cum2$cumRAIN),
       col = "darkolivegreen",
       lwd = 3,
       lty = "solid")
abline(h = mean(cum2$cumRAIN) - sd(cum2$cumRAIN),
       col = "darkolivegreen",
       lwd = 1.5,
       lty = "dotdash")
abline(h = mean(cum2$cumRAIN) + sd(cum2$cumRAIN),
       col = "darkolivegreen",
       lwd = 1.5,
       lty = "dotdash")

dev.off()

#### Yearly mean temperature and p(H2O)

WEA <- read.table("GOOSE_PAPER_Annual_weather.txt", h = T)
head(WEA)
summary(WEA)

png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 1 - Geese nesting success & supplemented nests/PAPER/Figures/GOOSE_PAPER_meanTEMP_pH2O.tiff",
    res=300,
    width=20,
    height=15,
    pointsize=12,
    unit="cm",
    bg="transparent")

#x11()
#par(oma=c(0,0,0,3)) # outer margin
par(mar=c(5,5,1,5)) # inner margin - default parameter is par("mar") <- 5.1 4.1 4.1 2.1

plot(WEA$YEAR,
     WEA$meanTEMP,
     xlab = "Year",
     #     ylab = "Mean temperature (°C)",
     xaxp = c(1995, 2017, 12),
     ylim = c(2, 8),
     bty = "n",
     yaxt = "n",
     xaxt = "n",
     cex = 1,
     cex.lab = 1,
     col = "darkblue",
     pch = 19,
     lwd = 2,
     type = 'b'
     )

lines(WEA$YEAR,
      rep(mean(WEA$meanTEMP), 23),
      col = "darkblue",
      type = "l",
      lty = 4,
      lwd = 2)

axis(side = 2,
     lwd = 1,
     las = 2)

par(new = T)

plot(WEA$YEAR,
     WEA$GG_Ph2o,
     xlab = "",
     ylab = "",
     ylim = c(0.1, 0.13),
     bty = "n",
     yaxt = "n",
     xaxt = "n",
     cex = 1,
     cex.lab = 1,
     col = "chocolate",
     pch = 17,
     type = 'b',
     lwd = 2)

lines(WEA$YEAR,
      rep(mean(WEA$GG_Ph2o), 23),
      col = "chocolate",
      type = "l",
      lty = 4,
      lwd = 2)

axis(side = 1,
     at = 1995:2017,
     lwd = 1)

axis(side = 4,
     lwd = 1,
     las = 2)

# Add a legend
legend("topleft", 
       legend = c("Mean temperature", "p(H2O)"), 
       col = c("darkblue", "chocolate"), 
       pch = c(19,17), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       horiz = F )
dev.off()
