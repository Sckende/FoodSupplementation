############ Climate data for supplementation experiment ########
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()

#### Rainfall ####
rain <- read.table("PREC_precipitation_Bylot_1996-2017.txt", sep = "\t", dec = ",", h = T)

summary(rain)
rain <- na.omit(rain)

cum <- as.data.frame(tapply(rain$RAIN, rain$YEAR, sum))

cum <- cbind(1995:2017, cum)
names(cum) <- c("YEAR", "cumRAIN")
cum$meanCUMrain <- mean(cum$cumRAIN)

#### Trends between 1995 and 2017
plot(cum$YEAR, cum$cumRAIN, type = "h")
lines(cum$YEAR, cum$meanCUMrain)

#### Trends between 1995 and 2017
plot(cum$YEAR[cum$YEAR >= 2013], cum$cumRAIN[cum$YEAR >= 2013], type = "h")
mean2013 <- mean(cum$cumRAIN[cum$YEAR >= 2013])
lines(cum$YEAR[cum$YEAR >= 2013], rep(mean2013, 5))

      