####Library, working directory####
library(scales)
library(zoom)
library(lubridate)
library(AICcmodavg)
library(nlme)
library(lme4)
library(lattice)
library(MuMIn)
library(moments)

#Working directory + package
setwd("C:/Users/Laurent/Documents/Maîtrise/Claire")
load("~/Maîtrise/Claire/workspace_geese.RData")


####Datasets####

#Loading experiment dataset
inc_geese <- read.csv("ttagclaire.csv", sep = ";")

#Setting the first row as row names
rownames(inc_geese) <- inc_geese[, 1]
inc_geese[, 1] <- NULL
#Setting dates as dates
inc_geese$ttag_exp_start <-
  as.POSIXct(inc_geese$ttag_exp_start, tz = "America/Toronto", format = "%d-%m-%Y %H:%M:%S")
inc_geese$ttag_exp_end <-
  as.POSIXct(inc_geese$ttag_exp_end, tz = "America/Toronto", format = "%d-%m-%Y %H:%M:%S")


####Data extraction####

#Creating columns to be used later
#Number of recesses
inc_geese$nbrecess <- 0
#Total length of all recesses
inc_geese$lengthrecess <- 0

#Creating the function that smoothes out the incubation signal
mmed <- function(x, n = 1001) {
  runmed(x, n)
}

for (i in 1:nrow(inc_geese)) {
  #Loading all datasets
  assign(rownames(inc_geese)[i], read.csv(paste0(rownames(inc_geese)[i], ".csv"), sep =
                                            ";"))
  #Formatiing date and time columns (DatetimePOSIXct)
  DatetimePOSIXct <-
    as.POSIXct(get(rownames(inc_geese)[i])$Datetime, tz = "America/Toronto", format =
                 "%d-%m-%Y %H:%M:%S")
  DatetimePOSIXct <- format(DatetimePOSIXct, format = "%d-%m-%Y %H:%M")
  DatetimePOSIXct <-
    as.POSIXct(DatetimePOSIXct, tz = "America/Toronto", format = "%d-%m-%Y %H:%M")
  date <-
    as.Date(format(DatetimePOSIXct, format = "%d-%m-%Y"), format = "%d-%m-%Y")
  time <- as.factor(format(DatetimePOSIXct, format = "%H:%M"))
  year <- as.factor(format(DatetimePOSIXct, format = "%Y"))
  assign(rownames(inc_geese)[i],
         cbind(get(rownames(inc_geese)[i]), DatetimePOSIXct, date, time, year))
  #Restricting raw tinytag data to the period of interest
  bla1 <- inc_geese[rownames(inc_geese)[i], "ttag_exp_start"]
  bla2 <- inc_geese[rownames(inc_geese)[i], "ttag_exp_end"]
  assign(
    rownames(inc_geese)[i],
    subset(
      get(rownames(inc_geese)[i]),
      DatetimePOSIXct >= bla1 & DatetimePOSIXct <= bla2
    )
  )
  #Incubation (1 = incubating, 0 = not incubating)
  inc <- rep(1, nrow(get(rownames(inc_geese)[i])))
  med <- mmed(get(rownames(inc_geese)[i])$Temperature)
  
  #Setting 1s and 0s for incubation and recess (respectively)
  for (j in 1:nrow(get(rownames(inc_geese)[i]))) {
    if (get(rownames(inc_geese)[i])[j, "Temperature"] <= med[j] - 3) {
      inc[j] <- 0
      inc_geese[i, "lengthrecess"] <- inc_geese[i, "lengthrecess"] + 1
    }
  }
  
  #Merging columns for incubation
  assign(rownames(inc_geese)[i], cbind(get(rownames(inc_geese)[i]), inc))
  
  #Seperate loop for number of recesses (to handle j-1 not working for j=1)
  for (j in 2:nrow(get(rownames(inc_geese)[i]))) {
    if (inc[j] == 0 && inc[j - 1] == 1) {
      inc_geese[i, "nbrecess"] <- inc_geese[i, "nbrecess"] + 1
    }
  }
  
  #Calculating the percentage of daily time spent in the nest (nb inc minutes / total nb minutes)
  inc_geese[i, "inc.prop"] <-
    sum(get(rownames(inc_geese)[i])$inc) / nrow(get(rownames(inc_geese)[i]))
  
  #Calculating the mean length of recesses (nb recess minutes / nb recesses)
  inc_geese[i, "meanrecess"] <-
    inc_geese[i, "lengthrecess"] / inc_geese[i, "nbrecess"]
  
  #Calculating the average number of recesses per day (nb recesses / total nb minutes x 1440 minutes/day)
  inc_geese[i, "meanfreq"] <-
    (inc_geese[i, "nbrecess"] / nrow(get(rownames(inc_geese)[i]))) * 60 * 24
}


nestID <- nl38

####Graphs####
plot(c(1:nrow(nestID)),
     nestID$Temperature,
     type = "l",
     col = "grey")
lines(c(1:nrow(nestID)),
      mmed(nestID$Temperature) - 2,
      col = alpha("dark green", 0.7))=abline(
  h = median(nestID$Temperature) - 3,
  col = alpha("red", 0.7),
  lty = 2
)
#lines(c(1:nrow(nestID)), nestID$inc*5+34, col=alpha("cornflowerblue", 0.7))
reg <-
  summary(lm(Temperature ~ c(1:nrow(nestID)), data = nestID))
curve(
  x * reg$coefficients[2] + reg$coefficients[1] - 3,
  from = 1,
  to = nrow(nestID),
  add = TRUE,
  col = alpha("purple", 0.7),
  lwd = 1,
  lty = 2
)



####Exporting####
write.csv(inc_geese, "inc_geese.csv")
