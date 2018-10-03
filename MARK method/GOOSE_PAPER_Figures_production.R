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
height=15,
pointsize=12,
unit="cm",
bg="transparent")

x11()
par(mfrow = c(2, 1), mar=c(5,5,1,5)) # inner margin - default parameter is par("mar") <- 5.1 4.1 4.1 2.1
my_vector <- cum2$cumRAIN
names(my_vector) <- cum2$YEAR
barplot(my_vector,
        col = "olivedrab3",
        border = "olivedrab3",
        # main = "Trend for specific dates for each goose nesting period",
        # ylab = "Cumulative precipitation (mm)",
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

#par(new = T)

plot(WEA$YEAR,
     WEA$meanTEMP,
     xlab = "",
     ylab = "",
     xaxp = c(1995, 2017, 22),
     ylim = c(3, 8),
     xlim = c(1995, 2017),
     bty = "n",
     #yaxp = c(min(WEA$meanTEMP) - 0.5, max(WEA$meanTEMP) + 0.5, 6),
     yaxt = "n",
    # xaxt = "n",
     cex = 1,
     cex.lab = 1,
     col = "darkgoldenrod2",
     pch = 19,
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
#axis(side = 4,
#     lwd = 1,
 #    las = 2)


dev.off()

