######## *** Thinking point ***** ############
# For the case of the variable "NestAge" ==> There is no utility to use it in models which want to characterize an inter-annual effect. But if I want to analyse nesting success to intra-seasonal scale (or inter-indivual scale?), "NestAge" is essential (cf 2017 model !)
# See Brown et al. 2013, Shaffer 2004, and the paper ms of Royer-Boutin
# Read Grant and al 2005 --> special paper about time effect ont nesting success estimators

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
    Modnam <- paste("mod", i, sep = " ")
    
    table <- rbind(table, c(Modnam, name, dev, aic))
    
  }
  # table <- as.data.frame(table)
  
  table <- as.data.frame(table)
  table$V3 <- as.numeric(table$V3)
  table$V4 <- as.numeric(table$V4)
  
  for(j in 1:dim(table)[1]){
    table$V5[j] <- table$V4[j] - min(table$V4)
  }
  table <- table[order(table$V5),]
  
  names(table) <- c("Model", "Formula", "Deviance", "AIC", "dAIC")
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

# ####################################################
# mfit3 <- glm(Fate ~ SUPPL + HAB + YEAR,
#              family=binomial(link=logexp(water$EXPO)),
#              data = water)
# summary(mfit3)
#####################################################
# mfit3.1 <- glm(Fate ~ SUPPL + HAB + YEAR + lmg,
#              family=binomial(link=logexp(water$EXPO)),
#              data = water)
# summary(mfit3.1)
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
# mfit8 <- glm(Fate ~ SUPPL + HAB + YEAR,
#              family = binomial(link = logexp(water$EXPO)),
#              data = water)
# summary(mfit8)
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

# No models with the YEAR variable - Replaced by models with TEMP, PREC, and lmg variables ***
l <- list(mfit0, mfit1, mfit2.1, mfit4.1, mfit4.2, mfit4.3, mfit6, mfit9, mfit9.1, mfit9.2, mfit10, mfit11)

rr <- AIC.rank(liste = l)

require(AICcmodavg)
gg <- aictab(l)
gg <- as.data.frame(gg)


final.AICtable <- cbind(name = as.character(rr$Model), formula = as.character(rr$Formula), gg[, -1])
final.AICtable


###### Predictions of the best model ############################
require(visreg)
visreg(mfit4.1)

#### Plot for supplementation effect ########
summary(mfit4.1)
mfit4.1$formula
water$fitted.values <- mfit4.1$fitted.values


f1 <- as.data.frame(tapply(water$fitted.values, water$SUPPL, mean))
f2 <- as.data.frame(tapply(water$fitted.values, water$SUPPL, sd))
f <- cbind(f1, f2)
names(f) <- c("mean", "sd")
f$SUPPL <- as.factor(c("TEM", "W"))

#install.packages("gplots")
require(gplots)
barCenters <- barplot2(height = f$mean,
                       names.arg = f$SUPPL,
                       ylim = c(0, 1),
                       xlab = "Treatment",
                       border = "NA",
                       plot.grid = TRUE,
                       ylab = "Nesting success probability",
                       col = c("chartreuse4", "cyan4"))
arrows(barCenters,
       f$mean - f$sd,
       barCenters,
       f$mean + f$sd,
       angle=90,
       code=3,
       lwd = 2)
#### Plot for habitat effect ########
mfit4.1$formula
water$fitted.values <- mfit4.1$fitted.values


f1 <- as.data.frame(tapply(water$fitted.values, water$HAB, mean))
f2 <- as.data.frame(tapply(water$fitted.values, water$HAB, sd))
f <- cbind(f1, f2)
names(f) <- c("mean", "sd")
f$HAB <- as.factor(c("MES", "WET"))

#install.packages("gplots")
require(gplots)
barCenters <- barplot2(height = f$mean,
                       names.arg = f$HAB,
                       ylim = c(0, 1),
                       xlab = "Habitat",
                       border = "NA",
                       plot.grid = TRUE,
                       ylab = "Nesting success probability",
                       col = c("darkgoldenrod3", "darkgreen"))
arrows(barCenters,
       f$mean - f$sd,
       barCenters,
       f$mean + f$sd,
       angle=90,
       code=3,
       lwd = 2)
text(barCenters, 0.2, labels = c("(946)", "(139)"))
#### Plot for temperature effect ########
plot(water$TEMP_Y, water$Fate)

# min_TEMP = 0.4957
# max_TEMP = 5.944

v <- seq(0.49, 6, by = 0.01)
newdata <- data.frame(TEMP_Y = v, SUPPL = 0.90, HAB = 0.15, lmg = mean(water$lmg), PREC_Y = mean(water$PREC_Y)) # wetland = 15% of the studied area

Pwater <- predict(mfit4.1, newdata = newdata, type = "response", se.fit = TRUE)

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
    Modnam <- paste("mod", i, sep = " ")
    
    table <- rbind(table, c(Modnam, name, dev, aic))
    
  }
  # table <- as.data.frame(table)
  
  table <- as.data.frame(table)
  table$V3 <- as.numeric(table$V3)
  table$V4 <- as.numeric(table$V4)
  
  for(j in 1:dim(table)[1]){
    table$V5[j] <- table$V4[j] - min(table$V4)
  }
  table <- table[order(table$V5),]
  
  names(table) <- c("Model", "Formula", "Deviance", "AIC", "dAIC")
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
foo6.1 <- glm(Fate ~ SUPPL*TEMP_Y + SUPPL*PREC_Y + HAB,
            family = binomial(link = logexp(food$EXPO)),
            data = food)
summary(foo6.1)
###########################################################################
foo6.2 <- glm(Fate ~ SUPPL*TEMP_Y + HAB + lmg,
            family = binomial(link = logexp(food$EXPO)),
            data = food)
summary(foo6.2)
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

l <- list(foo1, foo2.1, foo2.2, foo3, foo4, foo5, foo6, foo6.1, foo6.2, foo7, foo8, foo9)

rr <- AIC.rank(liste = l)

require(AICcmodavg)
gg <- aictab(l)
gg <- as.data.frame(gg)


final.AICtable <- cbind(name = as.character(rr$Model), formula = as.character(rr$Formula), gg[, -1])
final.AICtable
####*** WARNINGS !!! Check models one by one !!! *** ######################


require(visreg)
visreg(foo6.2)
################ Best model : foo6.2 ############
#### Plot for interaction SUPPL*TEMP_Y ########
summary(food)
# min(TEMp_Y) = 4.354
# max(TEMP_Y) = 5.944

# min value of temperature
Tmin_TEM <- data.frame(SUPPL = "TEM", TEMP_Y = min(food$TEMP_Y), HAB = "WET", lmg = mean(food$lmg))
sn_Tmin_TEM <- mean(predict(foo6.2, newdata = Tmin_TEM, type = "response")); sn_Tmin_TEM
sn_Tmin_TEMsd <- sd(predict(foo6.2, newdata = Tmin_TEM, type = "response")); sn_Tmin_TEMsd

Tmin_F <- data.frame(SUPPL = "F", TEMP_Y = min(food$TEMP_Y), HAB = "WET", lmg = mean(food$lmg))
sn_Tmin_F <- mean(predict(foo6.2, newdata = Tmin_F, type = "response")); sn_Tmin_F
sn_Tmin_Fsd <- sd(predict(foo6.2, newdata = Tmin_F, type = "response")); sn_Tmin_Fsd

# max value of temperature
Tmax_TEM <- data.frame(SUPPL = "TEM", TEMP_Y = max(food$TEMP_Y), HAB = "WET", lmg = mean(food$lmg))
sn_Tmax_TEM <- mean(predict(foo6.2, newdata = Tmax_TEM, type = "response")); sn_Tmax_TEM
sn_Tmax_TEMsd <- sd(predict(foo6.2, newdata = Tmax_TEM, type = "response")); sn_Tmax_TEMsd

Tmax_F <- data.frame(SUPPL = "F", TEMP_Y = max(food$TEMP_Y), HAB = "WET", lmg = mean(food$lmg))
sn_Tmax_F <- mean(predict(foo6.2, newdata = Tmax_F, type = "response")); sn_Tmax_F
sn_Tmax_Fsd <- sd(predict(foo6.2, newdata = Tmax_F, type = "response")); sn_Tmax_Fsd

# Dataframe with prediction values
pred <- data.frame(TEMP = c(rep("LOW", 2), rep("HIGH", 2)), TREAT = rep(c("TEM", "F"), 2), mSN = c(sn_Tmin_TEM, sn_Tmin_F, sn_Tmax_TEM, sn_Tmax_F), sdSN = c(sn_Tmin_TEMsd, sn_Tmin_Fsd, sn_Tmax_TEMsd, sn_Tmax_Fsd))

# Interaction SUPPL*TEMP_Y plot #
plot(pred$mSN[pred$TEMP == "LOW"],
     ylim = c(0.38, 1),
     ylab = "Nesting success probability",
     xlab = "Treatment",
     xaxt = "n",
     type = "b",
     bty = "n",
     col = "darkblue",
     lwd = 2)
par(new = TRUE)
plot(pred$mSN[pred$TEMP == "HIGH"],
     ylim = c(0.38, 1),
     ylab = "",
     xlab = "",
     xaxt = "n",
     type = "b",
     bty = "n",
     col = "darkorange",
     lwd = 2)
predL <- pred[pred$TEMP == "LOW",]
arrows(x0 = c(1, 2),
       y0 = predL$mSN - predL$sdSN,
       x1 = c(1, 2),
       y1 = predL$mSN + predL$sdSN,
       angle=90,
       code=3,
       lwd = 2,
       col = "darkblue")
predH <- pred[pred$TEMP == "HIGH",]
arrows(x0 = c(1, 2),
       y0 = predH$mSN - predH$sdSN,
       x1 = c(1, 2),
       y1 = predH$mSN + predH$sdSN,
       angle=90,
       code=3,
       lwd = 2,
       col = "darkorange")


axis(1, at = c(1, 2), labels = c("TEM", "FOOD"))
legend(x = 1.8,
       y = 0.6,
       legend = c("low temp.", "high temp."),
       fill = c("darkblue", "darkorange"),
       border = c("darkblue", "darkorange"),
       bty = "n")



#### Plot for habitat effect ########

foo6.2$formula
food$fitted.values <- foo6.2$fitted.values


f1 <- as.data.frame(tapply(food$fitted.values, food$HAB, mean))
f2 <- as.data.frame(tapply(food$fitted.values, food$HAB, sd))
f <- cbind(f1, f2)
names(f) <- c("mean", "sd")
f$HAB <- as.factor(c("MES", "WET"))

install.packages("gplots")
require(gplots)
barCenters <- barplot2(height = f$mean,
                      names.arg = f$HAB,
                      ylim = c(0, 1),
                      xlab = "Habitat",
                      border = "NA",
                      plot.grid = TRUE,
                      ylab = "Nesting success probability",
                      col = c("darkgoldenrod3", "darkgreen"))
arrows(barCenters,
       f$mean - f$sd,
       barCenters,
       f$mean + f$sd,
       angle=90,
       code=3,
       lwd = 2)
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

