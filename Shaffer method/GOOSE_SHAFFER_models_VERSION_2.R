#### Models with logistic-exposure link ####

setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
rm(list = ls())
data <- read.table("GOOSE_SHAFFER_database_all_nests_2005_2015-2017.csv", h = T, sep = " ")
summary(data)

#### Modeles test ####
require(lme4)
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

# Models
g.0 <- glmer(NIDIF ~ 1 + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             data = data)
summary(g.0)
#----------------------------------------------------#
g.1 <- glmer(NIDIF ~ NestAge + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             data = data)
summary(g.1)

#-----------------------------------------------------#
data.small <- data[!data$YEAR == 2005,]
data.small <- droplevels(data.small)
summary(data.small)

g.2 <- glmer(NIDIF ~ YEAR + (1|ID),
             family = binomial(link = logexp(data.small$EXPO)),
             data = data.small)
summary(g.2)

#----------------------------------------------------#
g.3 <- glmer(NIDIF ~ NestAge + YEAR + (1|ID),
             family = binomial(link = logexp(data.small$EXPO)),
             data = data.small)
summary(g.3)

#----------------------------------------------------#
g.4 <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + (1|ID),
             family = binomial(link = logexp(data.small$EXPO)),
             data = data.small)
summary(g.4)

#----------------------------------------------------#
g.5 <- glmer(NIDIF ~ NestAge + SUPPL + YEAR + HAB2 + (1|ID),
             family = binomial(link = logexp(data.small$EXPO)),
             data = data.small)
summary(g.5)
library(visreg)
visreg(g.5)

library("DHARMa")
sims <- simulateResiduals(g.5)
x11()
plot(sims)

s <- simulate(g.5, 100)
?simulate.merMod

predict(g.5)

#----------------------------------------------------#
g.6 <- glmer(NIDIF ~ NestAge + SUPPL + YEAR + HAB2 + TEMP_NIDIF + (1|ID),
             family = binomial(link = logexp(data.small$EXPO)),
             data = data.small)
summary(g.6)

#----------------------------------------------------#
g.7 <- glmer(NIDIF ~ NestAge + SUPPL + YEAR + HAB2 + PREC_NIDIF + (1|ID),
             family = binomial(link = logexp(data.small$EXPO)),
             data = data.small)
summary(g.7)

#----------------------------------------------------#
g.8 <- glmer(NIDIF ~ NestAge + SUPPL + YEAR + HAB2 + PREC_NIDIF + TEMP_NIDIF + (1|ID),
             family = binomial(link = logexp(data.small$EXPO)),
             data = data.small)
summary(g.8)
#----------------------------------------------------#
g.9 <- glmer(NIDIF ~ NestAge + SUPPL + YEAR + HAB2 + PREC_EXPO + TEMP_EXPO + (1|ID),
             family = binomial(link = logexp(data.small$EXPO)),
             data = data.small)
summary(g.9)

g.10 <- glm(NIDIF ~ NestAge + SUPPL + YEAR + HAB2 + PREC_EXPO,
            family = binomial(link = logexp(data.small$EXPO)),
            data = data.small)
summary(g.10)

g.11 <- glm(NIDIF ~ NestAge + SUPPL + YEAR + HAB2 + TEMP_EXPO,
            family = binomial(link = logexp(data.small$EXPO)),
            data = data.small)
summary(g.11)

