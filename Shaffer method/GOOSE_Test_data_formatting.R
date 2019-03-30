####### Comparison between both data formatting for Shaffer method ################
rm(list = ls())
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()

goo <- read.table("GOOSE-Test format data-Shaffer.txt", h = TRUE, sep = "\t")
head(goo)
summary(goo)

# Status code
goo$Status <- as.character(goo$Status)
goo$Status[goo$Status == "Yes"] <- "1"
goo$Status[goo$Status == "No"] <- "0"
goo$Status <- as.factor(goo$Status)

j <- unique(goo$No_ter)

longD <- data.frame()
shortD <- data.frame()

for(i in j){
  data.1 <- goo[goo$No_ter == i,][-1,]
  data.1$EXPO <- diff(goo[goo$No_ter == i,]$Date)
  data.1$AGE <- (goo$Date[goo$No_ter == i][1] - unique(data.1$Init)) + cumsum(data.1$EXPO)
  
  longD <- rbind(longD, data.1)
  
  if(any(as.character(goo$Status[goo$No_ter == i]) == "0")){
    expo.1 <- sum(data.1$EXPO[-length(data.1$EXPO)])
    expo.2 <- data.1$EXPO[length(data.1$EXPO)]
    
    line.1 <- cbind(goo[goo$No_ter == i,][1,], EXPO = expo.1, AGE = unique(data.1$AGE))
    line.2 <- cbind(goo[goo$No_ter == i,][dim(goo[goo$No_ter == i,])[1],], EXPO = expo.2, AGE = unique(data.1$AGE))
    
    shortD <- rbind(shortD, line.1, line.2)
  }else{
    expo.3 <- sum(data.1$EXPO)
    
    line.3 <- cbind(goo[goo$No_ter == i,][1,], EXPO = expo.3, AGE = unique(data.1$AGE))
    
    shortD <- rbind(shortD, line.3)
  }
}

#### Add temperature values in long dataframe ####
tp <- read.csv("TEMP_PondInlet_1995-2017.csv", sep = ";", h = T, dec = ".")
tp <- tp[tp$Year == 2017,]
head(tp)
summary(tp)

tp$date_jj <- strptime(tp$Date, format = "%Y-%m-%d")
tp$date_jj <- tp$date_jj$yday +1

for(i in 1:length(longD$No)){
  j.1 <- longD$Date[i] - longD$EXPO[i]
  j.2 <- longD$Date[i]
  
  
  longD$temp[i] <- mean(tp$Mean_Temp[tp$date_jj <= j.2 & tp$date_jj >= j.1]) # mean temperature for each interval
}

for(i in unique(longD$No_ter)){
  j.3 <- unique(longD$Init[longD$No_ter == i])
  j.4 <- longD$Date[longD$No_ter == i][length(longD$Date[longD$No_ter == i])]
    
    
    longD$temp_nid[longD$No_ter == i] <- mean(tp$Mean_Temp[tp$date_jj >= j.3 & tp$date_jj <= j.4]) # Here is mean temperature between the initiation date and last date of visit
}
#### *** WARNING ! Here is coarse computation of mean temp *** ####

#### Viewing data ####
utils::View(longD)
utils::View(shortD)

# Delete double check in the same of nests (i.e., EXPO = 0)
longD <- longD[!longD$EXPO == 0,]

# Function link
logexp <- function(exposure = 1) {
  linkfun <- function(mu) qlogis(mu^(1/exposure))
  ## FIXME: is there some trick we can play here to allow
  ## evaluation in the context of the 'data' argument?
  linkinv <- function(eta) plogis(eta)^exposure
  logit_mu_eta <- function(eta) {
    ifelse(abs(eta)>30,.Machine$double.eps,
           exp(eta)/(1+exp(eta))^2)
    ## OR .Call(stats:::C_logit_mu_eta, eta, PACKAGE = "stats")
  }
  mu.eta <- function(eta) {
    exposure * plogis(eta)^(exposure-1) *
      logit_mu_eta(eta)
  }
  valideta <- function(eta) TRUE
  link <- paste("logexp(", deparse(substitute(exposure)), ")",
                sep="")
  structure(list(linkfun = linkfun, linkinv = linkinv,
                 mu.eta = mu.eta, valideta = valideta,
                 name = link),
            class = "link-glm")
}

#### Models comparison ####
data <- list(shortD = shortD, longD = longD)

for(i in c(1, 2)){
  mod <- glm(Status ~ 1,
             family=binomial(link=logexp(data[[i]]$EXPO)),
             data = data[[i]])
  print(names(data)[i])
  print(summary(mod))
}

#--------------------------#
mod <- glm(Status ~ AGE + temp,
             family=binomial(link=logexp(longD$EXPO)),
             data = longD)

summary(mod)
#--------------------------#
mod.1 <- glm(Status ~ AGE + temp_nid,
           family=binomial(link=logexp(longD$EXPO)),
           data = longD)

summary(mod.1)
# ----------------------#
require(lme4)
mod.w <- glmer(Status ~ AGE + scale(temp_nid) + (1|No_ter),
           family=binomial(link=logexp(longD$EXPO)),
           data = longD)
summary(mod.w)

# ----------------------#
# require(lme4)
# mod.11 <- glmer(Status ~ AGE + temp_nid + (1|No_ter),
#              family=binomial(link=logexp(longD$EXPO)),
#              data = longD)
# summary(mod.11)

#### Confirmation that I have to use the long version of the dataset, with one row per visit, irrespective of the status of the nest. The number of exposition days and the age of the nest have to change for each visit.
### Better to keep one value of temperature and precipitation per the lifespan of each nest rather than one value per intervals cause I think this setting answers to a different question (short term impact - few day - od the local climate on the survival of the nest)
