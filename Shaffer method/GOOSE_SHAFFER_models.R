#### Analysis ####
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm(list = ls())

tot <- read.csv("GOOSE_Data_Shaffer.csv")
summary(tot)

#tot$YEAR <- as.factor(tot$YEAR)

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

# Function for AIC comparaison between (only) glm models 
AIC.rank <- function(liste){
  if(!is.list(liste))
    stop("Argument has to be a list")
  table <- NULL
  for(i in 1:length(liste)){
    
    # if(class(liste[[i]]) %in% c("glm", "lm"))
    #   stop("At least one model is not a glm")
    
    name <- liste[[i]]$formula
    dev <- liste[[i]]$deviance
    aic <- liste[[i]]$aic
    
    table <- rbind(table, c(name, dev, aic))
    
  }
  # table <- as.data.frame(table)
  
  table <- as.data.frame(table)
  table$V2 <- as.numeric(table$V2)
  table$V3 <- as.numeric(table$V3)
  
  for(j in 1:dim(table)[1]){
    table$V4[j] <- table$V3[j] - min(table$V3)
  }
  table <- table[order(table$V4),]
  
  names(table) <- c("Model", "Deviance", "AIC", "dAIC")
  print(table)
}

######################################################################
################### WATER SUPPL MODELS #################################
##########################################################################

# Specific database building ##############################################

water <- tot[tot$YEAR == 2005 | tot$YEAR == 2015 | tot$YEAR == 2016 | tot$YEAR == 2017,]
water <- water[water$SUPPL == "W" | water$SUPPL == "TEM",]
water <- droplevels(water)
water$YEAR <- as.factor(water$YEAR)
summary(water)

# Models fitting ###########################################################
mfit0 <- glm(Fate ~ 1,
             family=binomial(link=logexp(water$EXPO)),
             data = water)
summary(mfit0)
#######################################################
mfit1 <- glm(Fate ~ HAB,
             family=binomial(link=logexp(water$EXPO)),
             data = water)
summary(mfit1)
plot(mfit1)
#######################################################
mfit2 <- glm(Fate ~ SUPPL + HAB,
             family=binomial(link=logexp(water$EXPO)),
             data = water)
summary(mfit2)
#######################################################
mfit2.1 <- glm(Fate ~ SUPPL + HAB + lmg,
             family=binomial(link=logexp(water$EXPO)),
             data = water)
summary(mfit2.1)

####################################################
mfit3 <- glm(Fate ~ SUPPL + HAB + YEAR,
             family=binomial(link=logexp(water$EXPO)),
             data = water)
summary(mfit3)
#####################################################
mfit3.1 <- glm(Fate ~ SUPPL + HAB + YEAR + lmg,
             family=binomial(link=logexp(water$EXPO)),
             data = water)
summary(mfit3.1)
#####################################################
mfit4.1 <- glm(Fate ~ SUPPL + HAB + lmg + TEMP_Y + PREC_Y,
             family = binomial(link = logexp(water$EXPO)),
             data = water)
summary(mfit4.1)
#####################################################
mfit4.2 <- glm(Fate ~ SUPPL + HAB + lmg + PREC_Y,
               family = binomial(link = logexp(water$EXPO)),
               data = water)
summary(mfit4.2)
#####################################################
mfit4.3 <- glm(Fate ~ SUPPL + HAB + lmg + TEMP_Y,
               family = binomial(link = logexp(water$EXPO)),
               data = water)
summary(mfit4.3)
#####################################################
mfit6 <- glm(Fate ~ SUPPL + HAB + TEMP_Y + PREC_Y,
               family = binomial(link = logexp(water$EXPO)),
               data = water)
summary(mfit6)
#####################################################
mfit8 <- glm(Fate ~ SUPPL + HAB + YEAR,
             family = binomial(link = logexp(water$EXPO)),
             data = water)
summary(mfit8)
#####################################################
mfit9 <- glm(Fate ~ SUPPL*TEMP_Y + SUPPL*PREC_Y + HAB + lmg,
             family = binomial(link = logexp(water$EXPO)),
             data = water)
summary(mfit9)
#####################################################
mfit9.1 <- glm(Fate ~ SUPPL*TEMP_Y + HAB + lmg,
             family = binomial(link = logexp(water$EXPO)),
             data = water)
summary(mfit9.1)
#####################################################
mfit9.2 <- glm(Fate ~ SUPPL*PREC_Y + HAB + lmg,
             family = binomial(link = logexp(water$EXPO)),
             data = water)
summary(mfit9.2)
#####################################################
mfit10 <- glm(Fate ~ HAB + lmg + TEMP_Y + PREC_Y,
               family = binomial(link = logexp(water$EXPO)),
               data = water)
summary(mfit10)
#####################################################
mfit11 <- glm(Fate ~ HAB + lmg,
              family = binomial(link = logexp(water$EXPO)),
              data = water)
summary(mfit11)
#####################################################

# mfit7 <- glmer(Fate ~ SUPPL + HAB + (1|YEAR),
#                family = binomial(link = logexp(water$EXPO)),
#                data = water,
#                nAGQ = 1)
# summary(mfit7)
#####################################################
# require(lme4)
# mfit5 <- glmer(Fate ~ SUPPL + HAB + TEMP_Y + PREC_Y + (1|YEAR),
#              family = binomial(link = logexp(water$EXPO)),
#              data = water,
#              nAGQ = 0)
# summary(mfit5)
#####################################################

##############################################################
##################### AIC models #############################
##############################################################

l <- list(mfit0, mfit1, mfit2.1, mfit3, mfit3.1, mfit4.1, mfit4.2, mfit4.3, mfit6, mfit8, mfit9, mfit10, mfit11)

AIC.rank(liste = l)

###### Models comparaison to test hypothesis ####################
l1 <- list(mfit0, mfit10, mfit11, mfit4.1)
AIC.rank(l1)

######################################################################
################### FOOD SUPPL MODELS #################################
##########################################################################
#### Analysis ####
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm(list = ls())

tot <- read.csv("GOOSE_Data_Shaffer.csv")
summary(tot)
# Specific database building ##############################################

food <- tot[tot$YEAR == 2015 | tot$YEAR == 2016 | tot$YEAR == 2017,]
food <- food[food$SUPPL == "F" | food$SUPPL == "TEM",]
food <- droplevels(food)
food$YEAR <- as.factor(food$YEAR)
food$SUPPL <- relevel(food$SUPPL, "TEM")
summary(food)


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

# Function for AIC comparaison between (only) glm models 
AIC.rank <- function(liste){
  if(!is.list(liste))
    stop("Argument has to be a list")
  table <- NULL
  for(i in 1:length(liste)){
    
    # if(class(liste[[i]]) %in% c("glm", "lm"))
    #   stop("At least one model is not a glm")
    
    name <- liste[[i]]$formula
    dev <- liste[[i]]$deviance
    aic <- liste[[i]]$aic
    
    table <- rbind(table, c(name, dev, aic))
    
  }
  # table <- as.data.frame(table)
  
  table <- as.data.frame(table)
  table$V2 <- as.numeric(table$V2)
  table$V3 <- as.numeric(table$V3)
  
  for(j in 1:dim(table)[1]){
    table$V4[j] <- table$V3[j] - min(table$V3)
  }
  table <- table[order(table$V4),]
  
  names(table) <- c("Model", "Deviance", "AIC", "dAIC")
  print(table)
}

###############################################################################
########################### Models fitting ####################################
###############################################################################
foo1 <- glm(Fate ~ 1,
            family = binomial(link = logexp(food$EXPO)),
            data = food)
summary(foo1)
###########################################################################
# foo2 <- glm(Fate ~ SUPPL + HAB + lmg + TEMP_Y + PREC_Y,
#           family = binomial(link = logexp(food$EXPO)),
#           data = food)
# summary(foo2)
# Impossible to use in the same time: lmg, TEMP_y, and PREC_y 
# Cause impossible estimate for 
# Soluce 1 : replace TEMP_Y AND PREC_Y by YEAR (factor)
# Soluce 2 : Use an individual value per nest for the climatic variables
#           Possible to use a fake duration for failed nests
###########################################################################
foo2.1 <- glm(Fate ~ HAB + SUPPL + lmg + TEMP_Y,
          family = binomial(link = logexp(food$EXPO)),
          data = food)
summary(foo2.1)
###########################################################################
foo2.2 <- glm(Fate ~ HAB + SUPPL + lmg + PREC_Y,
              family = binomial(link = logexp(food$EXPO)),
              data = food)
summary(foo2.2)
###########################################################################
foo3 <- glm(Fate ~ HAB + lmg + TEMP_Y + PREC_Y,
            family = binomial(link = logexp(food$EXPO)),
            data = food)
summary(foo3)
###########################################################################
foo4 <- glm(Fate ~ HAB + lmg + TEMP_Y,
          family = binomial(link = logexp(food$EXPO)),
          data = food)
summary(foo4)
###########################################################################
foo5 <- glm(Fate ~ HAB + lmg + PREC_Y,
            family = binomial(link = logexp(food$EXPO)),
            data = food)
summary(foo5)
###########################################################################
foo6 <- glm(Fate ~ SUPPL*TEMP_Y + SUPPL*PREC_Y + HAB + lmg,
            family = binomial(link = logexp(food$EXPO)),
            data = food)
summary(foo6)
###########################################################################
foo7 <- glm(Fate ~ SUPPL*HAB + lmg + TEMP_Y + PREC_Y,
            family = binomial(link = logexp(food$EXPO)),
            data = food)
summary(foo7)
###########################################################################
foo8 <- glm(Fate ~ SUPPL*HAB + lmg + TEMP_Y,
            family = binomial(link = logexp(food$EXPO)),
            data = food)
summary(foo8)
###########################################################################
foo9 <- glm(Fate ~ SUPPL*HAB + lmg + PREC_Y,
            family = binomial(link = logexp(food$EXPO)),
            data = food)
summary(foo9)
###########################################################################
# foo10 <- glm(Fate ~ SUPPL + HAB + lmg + YEAR,
#             family = binomial(link = logexp(food$EXPO)),
#             data = food)
# summary(foo10)
# Impossible to use in the same time: lmg and YEAR

##############################################################
##################### AIC models #############################
##############################################################

l <- list(foo1, foo2, foo2.1, foo2.2, foo3, foo4, foo5, foo6, foo7, foo8, foo9)
AIC.rank(liste = l)

###### Models comparaison to test hypothesis ####################
l1 <- list(foo1, foo6, foo3, foo2)
AIC.rank(l1)

####*** WARNINGS !!! Check models one by one !!! *** ######################

###########################################################################
#################### food - 2017 ##########################################
###########################################################################
f2017 <- glm(Fate ~ SUPPL + HAB,
             family = binomial(link = logexp(food$EXPO[food$YEAR == "2017"])),
             data = food[food$YEAR == "2017",])
summary(f2017)
###########################################################################
f2017.0 <- glm(Fate ~ 1,
             family = binomial(link = logexp(food$EXPO[food$YEAR == "2017"])),
             data = food[food$YEAR == "2017",])
summary(f2017.0)
###########################################################################
f2017.1 <- glm(Fate ~ HAB,
             family = binomial(link = logexp(food$EXPO[food$YEAR == "2017"])),
             data = food[food$YEAR == "2017",])
summary(f2017.1)
###########################################################################
f2017.2 <- glm(Fate ~ SUPPL,
               family = binomial(link = logexp(food$EXPO[food$YEAR == "2017"])),
               data = food[food$YEAR == "2017",])
summary(f2017.2)
###########################################################################
##############################################################
##################### AIC models #############################
##############################################################

l <- list(f2017,f2017.0, f2017.1, f2017.2)
AIC.rank(liste = l)

