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

####################################################
water$YEAR <- as.factor(water$YEAR)
mfit3 <- glm(Fate ~ HAB + SUPPL + YEAR,
             family=binomial(link=logexp(water$EXPO)),
             data = water)
summary(mfit3)
#####################################################
mfit4 <- glm(Fate ~ TEMP_Y + PREC_Y + SUPPL + HAB,
             family = binomial(link = logexp(water$EXPO)),
             data = water)
summary(mfit4)
#####################################################

require(lme4)
mfit5 <- glmer(Fate ~ SUPPL + HAB + TEMP_Y + PREC_Y + (1|YEAR),
             family = binomial(link = logexp(water$EXPO)),
             data = water,
             nAGQ = 0)
summary(mfit5)

mfit6 <- glm(Fate ~ SUPPL + HAB + TEMP_Y + PREC_Y,
               family = binomial(link = logexp(water$EXPO)),
               data = water)
anova(mfit5, mfit6) # Not sure I need to integrate random effect with YEAR *****

mfit7 <- glmer(Fate ~ SUPPL + HAB + (1|YEAR),
               family = binomial(link = logexp(water$EXPO)),
               data = water,
               nAGQ = 1)
summary(mfit7)

mfit8 <- glm(Fate ~ SUPPL + HAB + TEMP_Y + PREC_Y,
             family = binomial(link = logexp(water$EXPO)),
             data = water)

mfit9 <- glm(Fate ~ SUPPL + HAB + YEAR,
             family = binomial(link = logexp(water$EXPO)),
             data = water)
anova(mfit9, mfit8)
