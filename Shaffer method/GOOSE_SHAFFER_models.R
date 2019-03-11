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
######################################################################
################### WATER SUPPL MODELS #################################
##########################################################################

# Specific database building ##############################################

water <- tot[tot$YEAR == 2005 | tot$YEAR == 2015 | tot$YEAR == 2016 | tot$YEAR == 2017,]
water <- water[water$SUPPL == "W" | water$SUPPL == "TEM",]
water <- droplevels(water)
summary(water)

# Models fitting ###########################################################

mfit1 <- glm(Fate ~ HAB,
             family=binomial(link=logexp(water$EXPO)),
             data = water)
summary(mfit1)
plot(mfit1)

#######################################################
mfit2 <- glm(Fate ~ HAB + SUPPL,
             family=binomial(link=logexp(water$EXPO)),
             data = water)
summary(mfit2)
#######################################################
mfit2.1 <- glm(Fate ~ HAB + SUPPL + lmg,
             family=binomial(link=logexp(water$EXPO)),
             data = water)
summary(mfit2.1)

####################################################
water$YEAR <- as.factor(water$YEAR)
mfit3 <- glm(Fate ~ HAB + SUPPL + YEAR,
             family=binomial(link=logexp(water$EXPO)),
             data = water)
summary(mfit3)
#####################################################
mfit3.1 <- glm(Fate ~ HAB + SUPPL + YEAR + lmg,
             family=binomial(link=logexp(water$EXPO)),
             data = water)
summary(mfit3.1)
#####################################################
mfit4 <- glm(Fate ~ TEMP_Y + PREC_Y + SUPPL + HAB,
             family = binomial(link = logexp(water$EXPO)),
             data = water)
summary(mfit4)
#####################################################
mfit4.1 <- glm(Fate ~ TEMP_Y + PREC_Y + SUPPL + HAB + lmg,
             family = binomial(link = logexp(water$EXPO)),
             data = water)
summary(mfit4.1)
#####################################################
require(lme4)
mfit5 <- glmer(Fate ~ SUPPL + HAB + TEMP_Y + PREC_Y + (1|YEAR),
             family = binomial(link = logexp(water$EXPO)),
             data = water,
             nAGQ = 0)
summary(mfit5)
#####################################################
mfit6 <- glm(Fate ~ SUPPL + HAB + TEMP_Y + PREC_Y,
               family = binomial(link = logexp(water$EXPO)),
               data = water)
anova(mfit5, mfit6) # Not sure I need to integrate random effect with YEAR *****
#####################################################
mfit7 <- glmer(Fate ~ SUPPL + HAB + (1|YEAR),
               family = binomial(link = logexp(water$EXPO)),
               data = water,
               nAGQ = 1)
summary(mfit7)
#####################################################
mfit8 <- glm(Fate ~ SUPPL + HAB + TEMP_Y + PREC_Y,
             family = binomial(link = logexp(water$EXPO)),
             data = water)
#####################################################
mfit9 <- glm(Fate ~ SUPPL + HAB + YEAR,
             family = binomial(link = logexp(water$EXPO)),
             data = water)
anova(mfit9, mfit8)

######################################################################
################### FOOD SUPPL MODELS #################################
##########################################################################

# Specific database building ##############################################

food <- tot[tot$YEAR == 2015 | tot$YEAR == 2016 | tot$YEAR == 2017,]
food <- food[food$SUPPL == "F" | food$SUPPL == "TEM",]
food <- droplevels(food)
food$YEAR <- as.factor(food$YEAR)
food$SUPPL <- relevel(food$SUPPL, "TEM")
summary(food)

# Models fitting ###########################################################
f1 <- glm(food$Fate[food$YEAR == "2017"] ~ food$SUPPL[food$YEAR == "2017"],
           family = binomial(link = logexp(food$EXPO[food$YEAR == "2017"])))
summary(f1)

###########################################################################
f2 <- glm(Fate ~ HAB + SUPPL + YEAR + TEMP_Y + PREC_Y,
          family = binomial(link = logexp(food$EXPO)),
          data = food)
summary(f2)
###########################################################################
f2.1 <- glm(Fate ~ HAB + SUPPL + lmg + TEMP_Y + PREC_Y,
          family = binomial(link = logexp(food$EXPO)),
          data = food)
summary(f2.1)
###########################################################################
f3 <- glm(Fate ~ HAB + SUPPL + lmg,
          family = binomial(link = logexp(food$EXPO)),
          data = food)
summary(f3)
###########################################################################
f4 <- glm(Fate ~ HAB + SUPPL,
          family = binomial(link = logexp(food$EXPO)),
          data = food)
summary(f4)
###########################################################################
f5 <- glm(Fate ~ HAB + SUPPL + TEMP_Y * PREC_Y + lmg,
            family = binomial(link = logexp(food$EXPO)),
            data = food)
summary(f5)

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

run.food <- list(f2, f2017)
AIC.table(run.food)


# Function for AIC comparaison between (only) glm models 
AIC.table <- function(liste){
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
