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
my_vector <- cum$cumRAIN
names(my_vector) <- cum$YEAR
barplot(my_vector,
        col = "olivedrab3",
        border = "olivedrab3")
abline(h = mean(cum$cumRAIN),
       col = "darkgoldenrod2",
       lwd = 3,
       lty = "solid")
abline(h = mean(cum$cumRAIN) - sd(cum$cumRAIN),
       col = "darkgoldenrod2",
       lwd = 1.5,
       lty = "dotdash")
abline(h = mean(cum$cumRAIN) + sd(cum$cumRAIN),
       col = "darkgoldenrod2",
       lwd = 1.5,
       lty = "dotdash")

#### Trends between 2013 and 2017
my_vector2013 <- cum$cumRAIN[cum$YEAR >= 2013]
names(my_vector2013) <- cum$YEAR[cum$YEAR >= 2013]
barplot(my_vector2013,
        col = "olivedrab3",
        border = "olivedrab3")
abline(h = mean(cum$cumRAIN[cum$YEAR >= 2013]),
       col = "darkgoldenrod2",
       lwd = 3,
       lty = "dotdash")

#### Trends with 2005, 2015, 2016 & 2017
my_vectorSUPPL <- cum$cumRAIN[cum$YEAR == 2005 | cum$YEAR >= 2015]
names(my_vectorSUPPL) <- cum$YEAR[cum$YEAR == 2005 | cum$YEAR >= 2015]
barplot(my_vectorSUPPL,
        col = "olivedrab3",
        border = "olivedrab3")
abline(h = mean(cum$cumRAIN[cum$YEAR == 2005 | cum$YEAR >= 2015]),
       col = "darkgoldenrod2",
       lwd = 3,
       lty = "dotdash")

abline(h = mean(cum$cumRAIN[cum$YEAR == 2005 | cum$YEAR >= 2015]) - sd(cum$cumRAIN[cum$YEAR == 2005 | cum$YEAR >= 2015]),
       col = "darkgoldenrod2",
       lwd = 2,
       lty = "dotdash")
abline(h = mean(cum$cumRAIN[cum$YEAR == 2005 | cum$YEAR >= 2015]) + sd(cum$cumRAIN[cum$YEAR == 2005 | cum$YEAR >= 2015]),
       col = "darkgoldenrod2",
       lwd = 2,
       lty = "dotdash")

#### Trends between 2005 and 2017
my_vector <- cum$cumRAIN[cum$YEAR >= 2005]
names(my_vector) <- cum$YEAR[cum$YEAR >= 2005]
barplot(my_vector,
        col = "olivedrab3",
        border = "olivedrab3")
abline(h = mean(cum$cumRAIN[cum$YEAR >= 2005]),
       col = "darkgoldenrod2",
       lwd = 3,
       lty = "solid")
abline(h = mean(cum$cumRAIN[cum$YEAR >= 2005]) - sd(cum$cumRAIN[cum$YEAR >= 2005]),
       col = "darkgoldenrod2",
       lwd = 1.5,
       lty = "dotdash")
abline(h = mean(cum$cumRAIN[cum$YEAR >= 2005]) + sd(cum$cumRAIN[cum$YEAR >= 2005]),
       col = "darkgoldenrod2",
       lwd = 1.5,
       lty = "dotdash")


#### Partial water vapor pressure - p(H2O)  ####

# Buck equation
# p = 0.61121 exp((18.678 - T/234.5)(T/(257.14 + T)))
# T is the ambiant temperature in C
# P is in kPa
list.files()
temp <- read.table("TEMP_Tair moy 1989-2017 BYLCAMP.txt", sep = "\t", dec = ",", h = T)
summary(temp)
temp$pH2O <- 0.61121 * exp((18.678 - temp$TEMP / 234.5) * (temp$TEMP/(257.14 + temp$TEMP)))