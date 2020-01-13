setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
rm(list = ls())
#### Import data ####
data <- read.table("GOOSE_SHAFFER_database_all_nests_2005_2015-2017.csv", h = T, sep = " ")
summary(data)

data <- data[!data$YEAR == 2005,]
data$YEAR <- as.factor(data$YEAR)
data$SUPPL <- relevel(data$SUPPL, "TEM")
data$HAB2 <- relevel(data$HAB2, "MES")

# Supplementation date
# d.suppl <- data[!data$SUPPL == "TEM",]; d.suppl <- droplevels(d.suppl); summary(d.suppl)

# Creation of supplementation date variable in day number between initiation date and the first visit for the control nests and the supplementation date for the supplemented nests

data$SUPPL_JJ[data$SUPPL == "TEM"] <- data$FirstFound[data$SUPPL == "TEM"] - data$INITIATION[data$SUPPL == "TEM"]
data$SUPPL_JJ[data$SUPPL %in% c("F", "W")] <- data$SUPPL_DATE[data$SUPPL %in% c("F", "W")] - data$INITIATION[data$SUPPL %in% c("F", "W")]


food <- data[data$SUPPL == "F",]
water <- data[data$SUPPL == "W",]
temoin <- data[data$SUPPL == "TEM",]

treat <- data[data$SUPPL %in% c("W", "F"),]

#### TREATED NESTS ####
# Information extraction for food nest
t <- split(treat, paste(treat$YEAR, treat$ID))
tt <- lapply(t, function(x) {
  x <- x[1,]
  x
})

tt <- do.call("rbind", tt)

table(tt$SUPPL_DATE[tt$YEAR == 2015], useNA = "always")
table(tt$SUPPL_DATE[tt$YEAR == 2016], useNA = "always")
table(tt$SUPPL_DATE[tt$YEAR == 2017], useNA = "always")
#### FOOD NESTS ####
# Information extraction for food nest
f <- split(food, paste(food$YEAR, food$ID))
ff <- lapply(f, function(x) {
  x <- x[1,]
  x
})

ff <- do.call("rbind", ff)

# 2015 - MES
dim(ff[ff$YEAR == "2015" & ff$HAB2 == "MES",])
mean(ff$INITIATION[ff$YEAR == "2015" & ff$HAB2 == "MES"], na.rm = T)
sd(ff$INITIATION[ff$YEAR == "2015" & ff$HAB2 == "MES"], na.rm = T)

mean(ff$CLUTCH[ff$YEAR == "2015" & ff$HAB2 == "MES"], na.rm = T)
sd(ff$CLUTCH[ff$YEAR == "2015" & ff$HAB2 == "MES"], na.rm = T)

table(ff$SUPPL_DATE[ff$YEAR == "2015" & ff$HAB2 == "MES"], useNA = "always")

# 2015 - WET
dim(ff[ff$YEAR == "2015" & ff$HAB2 == "WET",])
mean(ff$INITIATION[ff$YEAR == "2015" & ff$HAB2 == "WET"], na.rm = T)
sd(ff$INITIATION[ff$YEAR == "2015" & ff$HAB2 == "WET"], na.rm = T)

mean(ff$CLUTCH[ff$YEAR == "2015" & ff$HAB2 == "WET"], na.rm = T)
sd(ff$CLUTCH[ff$YEAR == "2015" & ff$HAB2 == "WET"], na.rm = T)

table(ff$SUPPL_DATE[ff$YEAR == "2015" & ff$HAB2 == "WET"], useNA = "always")

# 2016 - MES
dim(ff[ff$YEAR == "2016" & ff$HAB2 == "MES",])
mean(ff$INITIATION[ff$YEAR == "2016" & ff$HAB2 == "MES"], na.rm = T)
sd(ff$INITIATION[ff$YEAR == "2016" & ff$HAB2 == "MES"], na.rm = T)

mean(ff$CLUTCH[ff$YEAR == "2016" & ff$HAB2 == "MES"], na.rm = T)
sd(ff$CLUTCH[ff$YEAR == "2016" & ff$HAB2 == "MES"], na.rm = T)

table(ff$SUPPL_DATE[ff$YEAR == "2016" & ff$HAB2 == "MES"], useNA = "always")


# 2016 - WET
dim(ff[ff$YEAR == "2016" & ff$HAB2 == "WET",])
mean(ff$INITIATION[ff$YEAR == "2016" & ff$HAB2 == "WET"], na.rm = T)
sd(ff$INITIATION[ff$YEAR == "2016" & ff$HAB2 == "WET"], na.rm = T)

mean(ff$CLUTCH[ff$YEAR == "2016" & ff$HAB2 == "WET"], na.rm = T)
sd(ff$CLUTCH[ff$YEAR == "2016" & ff$HAB2 == "WET"], na.rm = T)

table(ff$SUPPL_DATE[ff$YEAR == "2016" & ff$HAB2 == "WET"], useNA = "always")

# 2017 - MES
dim(ff[ff$YEAR == "2017" & ff$HAB2 == "MES",])
mean(ff$INITIATION[ff$YEAR == "2017" & ff$HAB2 == "MES"], na.rm = T)
sd(ff$INITIATION[ff$YEAR == "2017" & ff$HAB2 == "MES"], na.rm = T)

mean(ff$CLUTCH[ff$YEAR == "2017" & ff$HAB2 == "MES"], na.rm = T)
sd(ff$CLUTCH[ff$YEAR == "2017" & ff$HAB2 == "MES"], na.rm = T)

table(ff$SUPPL_DATE[ff$YEAR == "2017" & ff$HAB2 == "MES"], useNA = "always")

# 2017 - WET
dim(ff[ff$YEAR == "2017" & ff$HAB2 == "WET",])
mean(ff$INITIATION[ff$YEAR == "2017" & ff$HAB2 == "WET"], na.rm = T)
sd(ff$INITIATION[ff$YEAR == "2017" & ff$HAB2 == "WET"], na.rm = T)

mean(ff$CLUTCH[ff$YEAR == "2017" & ff$HAB2 == "WET"], na.rm = T)
sd(ff$CLUTCH[ff$YEAR == "2017" & ff$HAB2 == "WET"], na.rm = T)

table(ff$SUPPL_DATE[ff$YEAR == "2017" & ff$HAB2 == "WET"], useNA = "always")



#### WATER NESTS ####
# Information extraction for food nest
w <- split(water, paste(water$YEAR, water$ID))
ww <- lapply(w, function(x) {
  x <- x[1,]
  x
})

ww <- do.call("rbind", ww)

# 2015 - MES
dim(ww[ww$YEAR == "2015" & ww$HAB2 == "MES",])
mean(ww$INITIATION[ww$YEAR == "2015" & ww$HAB2 == "MES"], na.rm = T)
sd(ww$INITIATION[ww$YEAR == "2015" & ww$HAB2 == "MES"], na.rm = T)

mean(ww$CLUTCH[ww$YEAR == "2015" & ww$HAB2 == "MES"], na.rm = T)
sd(ww$CLUTCH[ww$YEAR == "2015" & ww$HAB2 == "MES"], na.rm = T)

table(ww$SUPPL_DATE[ww$YEAR == "2015" & ww$HAB2 == "MES"], useNA = "always")

# 2015 - WET
dim(ww[ww$YEAR == "2015" & ww$HAB2 == "WET",])
mean(ww$INITIATION[ww$YEAR == "2015" & ww$HAB2 == "WET"], na.rm = T)
sd(ww$INITIATION[ww$YEAR == "2015" & ww$HAB2 == "WET"], na.rm = T)

mean(ww$CLUTCH[ww$YEAR == "2015" & ww$HAB2 == "WET"], na.rm = T)
sd(ww$CLUTCH[ww$YEAR == "2015" & ww$HAB2 == "WET"], na.rm = T)

table(ww$SUPPL_DATE[ww$YEAR == "2015" & ww$HAB2 == "WET"], useNA = "always")

# 2016 - MES
dim(ww[ww$YEAR == "2016" & ww$HAB2 == "MES",])
mean(ww$INITIATION[ww$YEAR == "2016" & ww$HAB2 == "MES"], na.rm = T)
sd(ww$INITIATION[ww$YEAR == "2016" & ww$HAB2 == "MES"], na.rm = T)

mean(ww$CLUTCH[ww$YEAR == "2016" & ww$HAB2 == "MES"], na.rm = T)
sd(ww$CLUTCH[ww$YEAR == "2016" & ww$HAB2 == "MES"], na.rm = T)

table(ww$SUPPL_DATE[ww$YEAR == "2016" & ww$HAB2 == "MES"], useNA = "always")

# 2016 - WET
dim(ww[ww$YEAR == "2016" & ww$HAB2 == "WET",])
mean(ww$INITIATION[ww$YEAR == "2016" & ww$HAB2 == "WET"], na.rm = T)
sd(ww$INITIATION[ww$YEAR == "2016" & ww$HAB2 == "WET"], na.rm = T)

mean(ww$CLUTCH[ww$YEAR == "2016" & ww$HAB2 == "WET"], na.rm = T)
sd(ww$CLUTCH[ww$YEAR == "2016" & ww$HAB2 == "WET"], na.rm = T)

table(ww$SUPPL_DATE[ww$YEAR == "2016" & ww$HAB2 == "WET"], useNA = "always")

# 2017 - MES
dim(ww[ww$YEAR == "2017" & ww$HAB2 == "MES",])
mean(ww$INITIATION[ww$YEAR == "2017" & ww$HAB2 == "MES"], na.rm = T)
sd(ww$INITIATION[ww$YEAR == "2017" & ww$HAB2 == "MES"], na.rm = T)

mean(ww$CLUTCH[ww$YEAR == "2017" & ww$HAB2 == "MES"], na.rm = T)
sd(ww$CLUTCH[ww$YEAR == "2017" & ww$HAB2 == "MES"], na.rm = T)

table(ww$SUPPL_DATE[ww$YEAR == "2017" & ww$HAB2 == "MES"], useNA = "always")

# 2017 - WET
dim(ww[ww$YEAR == "2017" & ww$HAB2 == "WET",])
mean(ww$INITIATION[ww$YEAR == "2017" & ww$HAB2 == "WET"], na.rm = T)
sd(ww$INITIATION[ww$YEAR == "2017" & ww$HAB2 == "WET"], na.rm = T)

mean(ww$CLUTCH[ww$YEAR == "2017" & ww$HAB2 == "WET"], na.rm = T)
sd(ww$CLUTCH[ww$YEAR == "2017" & ww$HAB2 == "WET"], na.rm = T)

table(ww$SUPPL_DATE[ww$YEAR == "2017" & ww$HAB2 == "WET"], useNA = "always")




#### CONTROL NESTS ####
# Information extraction for food nest
t <- split(temoin, paste(temoin$YEAR, temoin$ID))
tt <- lapply(t, function(x) {
  x <- x[1,]
  x
})


# 2015 - MES
dim(tt[tt$YEAR == "2015" & tt$HAB2 == "MES",])
mean(tt$INITIATION[tt$YEAR == "2015" & tt$HAB2 == "MES"], na.rm = T)
sd(tt$INITIATION[tt$YEAR == "2015" & tt$HAB2 == "MES"], na.rm = T)

mean(tt$CLUTCH[tt$YEAR == "2015" & tt$HAB2 == "MES"], na.rm = T)
sd(tt$CLUTCH[tt$YEAR == "2015" & tt$HAB2 == "MES"], na.rm = T)

# 2015 - WET
dim(tt[tt$YEAR == "2015" & tt$HAB2 == "WET",])
mean(tt$INITIATION[tt$YEAR == "2015" & tt$HAB2 == "WET"], na.rm = T)
sd(tt$INITIATION[tt$YEAR == "2015" & tt$HAB2 == "WET"], na.rm = T)

mean(tt$CLUTCH[tt$YEAR == "2015" & tt$HAB2 == "WET"], na.rm = T)
sd(tt$CLUTCH[tt$YEAR == "2015" & tt$HAB2 == "WET"], na.rm = T)

# 2016 - MES
dim(tt[tt$YEAR == "2016" & tt$HAB2 == "MES",])
mean(tt$INITIATION[tt$YEAR == "2016" & tt$HAB2 == "MES"], na.rm = T)
sd(tt$INITIATION[tt$YEAR == "2016" & tt$HAB2 == "MES"], na.rm = T)

mean(tt$CLUTCH[tt$YEAR == "2016" & tt$HAB2 == "MES"], na.rm = T)
sd(tt$CLUTCH[tt$YEAR == "2016" & tt$HAB2 == "MES"], na.rm = T)

# 2016 - WET
dim(tt[tt$YEAR == "2016" & tt$HAB2 == "WET",])
mean(tt$INITIATION[tt$YEAR == "2016" & tt$HAB2 == "WET"], na.rm = T)
sd(tt$INITIATION[tt$YEAR == "2016" & tt$HAB2 == "WET"], na.rm = T)

mean(tt$CLUTCH[tt$YEAR == "2016" & tt$HAB2 == "WET"], na.rm = T)
sd(tt$CLUTCH[tt$YEAR == "2016" & tt$HAB2 == "WET"], na.rm = T)

# 2017 - MES
dim(tt[tt$YEAR == "2017" & tt$HAB2 == "MES",])
mean(tt$INITIATION[tt$YEAR == "2017" & tt$HAB2 == "MES"], na.rm = T)
sd(tt$INITIATION[tt$YEAR == "2017" & tt$HAB2 == "MES"], na.rm = T)

mean(tt$CLUTCH[tt$YEAR == "2017" & tt$HAB2 == "MES"], na.rm = T)
sd(tt$CLUTCH[tt$YEAR == "2017" & tt$HAB2 == "MES"], na.rm = T)

# 2017 - WET
dim(tt[tt$YEAR == "2017" & tt$HAB2 == "WET",])
mean(tt$INITIATION[tt$YEAR == "2017" & tt$HAB2 == "WET"], na.rm = T)
sd(tt$INITIATION[tt$YEAR == "2017" & tt$HAB2 == "WET"], na.rm = T)

mean(tt$CLUTCH[tt$YEAR == "2017" & tt$HAB2 == "WET"], na.rm = T)
sd(tt$CLUTCH[tt$YEAR == "2017" & tt$HAB2 == "WET"], na.rm = T)