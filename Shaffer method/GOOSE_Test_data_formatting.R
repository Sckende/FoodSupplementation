####### Comparison between both data formatting for Shaffer method ################
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

long.data <- data.frame()
short.data <- data.frame()

for(i in j){
  data.1 <- goo[goo$No_ter == i,][-1,]
  data.1$EXPO <- diff(goo[goo$No_ter == i,]$Date)
  data.1$AGE <- goo$Date[goo$No_ter == i][1] - unique(data.1$Init)
  
  long.data <- rbind(long.data, data.1)
  
  if(any(as.character(goo$Status[goo$No_ter == i]) == "0")){
    expo.1 <- sum(data.1$EXPO[-length(data.1$EXPO)])
    expo.2 <- data.1$EXPO[length(data.1$EXPO)]
    
    line.1 <- cbind(goo[goo$No_ter == i,][1,], EXPO = expo.1, AGE = unique(data.1$AGE))
    line.2 <- cbind(goo[goo$No_ter == i,][dim(goo[goo$No_ter == i,])[1],], EXPO = expo.2, AGE = unique(data.1$AGE))
    
    short.data <- rbind(short.data, line.1, line.2)
  }else{
    expo.3 <- sum(data.1$EXPO)
    
    line.3 <- cbind(goo[goo$No_ter == i,][1,], EXPO = expo.3, AGE = unique(data.1$AGE))
    
    short.data <- rbind(short.data, line.3)
  }
}

#### Add temperature values in long dataframe ####
tp <- read.csv("TEMP_PondInlet_1995-2017.csv", sep = ";", h = T, dec = ".")
tp <- tp[tp$Year == 2017,]
head(tp)
summary(tp)

tp$date_jj <- strptime(tp$Date, format = "%Y-%m-%d")
tp$date_jj <- tp$date_jj$yday +1

for(i in 1:length(long.data$No)){
  j.1 <- long.data$Date[i] - long.data$EXPO[i]
  j.2 <- long.data$Date[i]
  
  
  long.data$temp[i] <- mean(tp$Mean_Temp[tp$date_jj <= j.2 & tp$date_jj >= j.1]) # mean temperature for each interval
}

for(i in unique(long.data$No_ter)){
  j.3 <- unique(long.data$Init[long.data$No_ter == i])
  j.4 <- long.data$Date[long.data$No_ter == i][length(long.data$Date[long.data$No_ter == i])]
    
    
    long.data$temp_nid[long.data$No_ter == i] <- mean(tp$Mean_Temp[tp$date_jj >= j.3 & tp$date_jj <= j.4]) # Here is mean temperature between the initiation date and last date of visit
}
#### *** WARNING ! Here is coarse computation of mean temp *** ####

#### Viewing data ####
utils::View(long.data)
utils::View(short.data)

# Delete double check in the same of nests (i.e., EXPO = 0)
long.data <- long.data[!long.data$EXPO == 0,]

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
data <- list(short.data = short.data, long.data = long.data)

for(i in c(1, 2)){
  mod <- glm(Status ~ 1,
             family=binomial(link=logexp(data[[i]]$EXPO)),
             data = data[[i]])
  print(names(data)[i])
  print(summary(mod))
}

#--------------------------#
mod <- glm(Status ~ AGE + temp,
             family=binomial(link=logexp(long.data$EXPO)),
             data = long.data)

summary(mod)
#--------------------------#
mod.1 <- glm(Status ~ AGE + temp_nid,
           family=binomial(link=logexp(long.data$EXPO)),
           data = long.data)

summary(mod.1)
# ----------------------#
require(lme4)
mod <- glmer(Status ~ AGE + temp + (1|No_ter),
           family=binomial(link=logexp(long.data$EXPO)),
           data = long.data)
summary(mod)

# ----------------------#
require(lme4)
mod.11 <- glmer(Status ~ AGE + temp_nid + (1|No_ter),
             family=binomial(link=logexp(long.data$EXPO)),
             data = long.data)
summary(mod.11)
