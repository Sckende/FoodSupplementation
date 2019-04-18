#### Models with logistic-exposure link ####

setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
rm(list = ls())

#### Packages ####
library("lme4") # For generalised linear models
library("glmmTMB")
library("visreg") # Vizualisation of model effects
library("DHARMa") # For simulations
library("AICcmodavg") # For AIC comparison

#### Import data ####
data <- read.table("GOOSE_SHAFFER_database_all_nests_2005_2015-2017.csv", h = T, sep = " ")
summary(data)

data <- data[!data$YEAR == 2005,]
data$YEAR <- as.factor(data$YEAR)
data$SUPPL <- relevel(data$SUPPL, "TEM")
data$HAB2 <- relevel(data$HAB2, "MES")

#data <- data[!data$SUPPL == "W",]

#### Function link ####
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

cand.models <- list()

#### Basic models ####
    # Null
cand.models[[1]] <- glmer(NIDIF ~ 1 + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             #nAGQ = 0,
             data = data)
summary(cand.models[[1]])

    # Known effects
cand.models[[2]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             #nAGQ = 0,
             data = data)

cand.models[[2]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR,
                          family = binomial(link = logexp(data$EXPO)),
                          #nAGQ = 0,
                          data = data)

summary(cand.models[[2]])

#### Supplementation effects ####
    # Additive effects of supplementation
cand.models[[3]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             #nAGQ = 0,
             data = data)

cand.models[[3]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL,
                          family = binomial(link = logexp(data$EXPO)),
                          #nAGQ = 0,
                          data = data)

summary(cand.models[[3]])

#### *** WARNINGS - PB de convergence pour tous les modèles qui suivent *** ####
    # Interaction effects - YEAR * SUPPL
cand.models[[4]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             #control = strict_tol,
             #nAGQ = 0, #For the convergence
             data = data
             )

cand.models[[4]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL,
                          family = binomial(link = logexp(data$EXPO)),
                          data = data
)

summary(cand.models[[4]]) # Convergence problem
# 
# relgrad <- with(g.3@optinfo$derivs,solve(Hessian,gradient))
# max(abs(relgrad))

# strict_tol <- glmerControl(optCtrl = list(xtol_abs = 1e-8, ftol_abs = 1e-8))
# if (all(cand.models[[4]]@optinfo$optimizer == "nloptwrap")) {
#   cand.models.tol <- update(cand.models[[4]], control = strict_tol)
# }

    # Interaction effects - HAB2 * SUPPL
cand.models[[5]] <- glmer(NIDIF ~ NestAge + HAB2*SUPPL + YEAR + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             #nAGQ = 0,
             data = data)

cand.models[[5]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR,
                          family = binomial(link = logexp(data$EXPO)),
                          data = data)

summary(cand.models[[5]])

#### Supplementation*Local climate effects ####
    # Global temperature
cand.models[[6]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL*TEMP_NIDIF + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             data = data,
             nAGQ = 0)
summary(cand.models[[6]])

    # Global precipitation
cand.models[[7]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL*PREC_NIDIF + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             data = data,
             nAGQ = 0)
summary(cand.models[[7]])

    # Global both
cand.models[[16]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL*PREC_NIDIF + SUPPL*TEMP_NIDIF + (1|ID),
                          family = binomial(link = logexp(data$EXPO)),
                          data = data,
                          nAGQ = 0)
summary(cand.models[[16]]) # PB avec PREC_NIDIF car beaucoup de 0
    # EXPO temperature
cand.models[[8]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL*TEMP_EXPO + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             data = data,
             nAGQ = 0)
summary(cand.models[[8]])

    # EXPO precipitation
cand.models[[9]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL*PREC_EXPO + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             data = data,
             nAGQ = 0)
summary(cand.models[[9]])

    # EXPO both
cand.models[[17]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL*TEMP_EXPO + SUPPL*PREC_EXPO + (1|ID),
                          family = binomial(link = logexp(data$EXPO)),
                          data = data,
                          nAGQ = 0)
summary(cand.models[[17]])

#### Supplementation*Local climate effects + Supplementation*HAB ####
    # Global temperature
cand.models[[18]] <- glmer(NIDIF ~ NestAge + HAB2*SUPPL + YEAR + SUPPL*TEMP_NIDIF + (1|ID),
                          family = binomial(link = logexp(data$EXPO)),
                          data = data,
                          nAGQ = 0)
summary(cand.models[[18]])

# Global precipitation
cand.models[[19]] <- glmer(NIDIF ~ NestAge + HAB2*SUPPL + YEAR + SUPPL*PREC_NIDIF + (1|ID),
                           family = binomial(link = logexp(data$EXPO)),
                           data = data,
                           nAGQ = 0)
summary(cand.models[[19]])

# Global both
cand.models[[20]] <- glmer(NIDIF ~ NestAge + HAB2*SUPPL + YEAR + SUPPL*PREC_NIDIF + SUPPL*TEMP_NIDIF + (1|ID),
                           family = binomial(link = logexp(data$EXPO)),
                           data = data,
                           nAGQ = 0)
summary(cand.models[[20]])

#### Supplementation + Local climate effects ####
    # Global temperature
cand.models[[10]] <- glmer(NIDIF ~ NestAge + HAB2 + #YEAR + 
                             SUPPL  + TEMP_NIDIF + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             #nAGQ = 0,
             data = data)

cand.models[[10]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL  + TEMP_NIDIF,
                           family = binomial(link = logexp(data$EXPO)),
                           data = data)

summary(cand.models[[10]])

    # Global precipitation
cand.models[11] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + PREC_NIDIF + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             #nAGQ = 0,
             data = data)

cand.models[[11]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL  + PREC_NIDIF,
                         family = binomial(link = logexp(data$EXPO)),
                         data = data)

summary(cand.models[[11]])

    # EXPO temperature
cand.models[[12]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + TEMP_EXPO + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             data = data,
             nAGQ = 0)
summary(cand.models[[12]])

    # EXPO precipitation
cand.models[[13]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + PREC_EXPO + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             data = data,
             nAGQ = 0)
summary(cand.models[[13]])

    # EXPO global
cand.models[[14]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + PREC_EXPO + TEMP_EXPO + (1|ID),
                           family = binomial(link = logexp(data$EXPO)),
                           data = data,
                           nAGQ = 0)
summary(cand.models[[14]])

    # NIDIF global
cand.models[[15]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + PREC_NIDIF + TEMP_NIDIF + (1|ID),
                           family = binomial(link = logexp(data$EXPO)),
                           #nAGQ = 0,
                           data = data)

cand.models[[15]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + PREC_NIDIF + TEMP_NIDIF,
                           family = binomial(link = logexp(data$EXPO)),
                           data = data)

summary(cand.models[[15]])

#### AIC comparison ####
Modnames <- paste("mod", 1:length(cand.models), sep = " ")
aictab(cand.set = cand.models, modnames = Modnames, sort = TRUE)
##round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = cand.models, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)

#### Simulations with the best modele ####
sims <- simulateResiduals(cand.models[[3]])
x11()
plot(sims)

s <- simulate(cand.models[[3]], 100)
plot(s[,1])
plot(data$NIDIF, col = "red", add = TRUE)
plot(plogis(predict(cand.models[[3]])))
?simulate.merMod

plogis(predict(cand.models[[3]]))


#### Models for 2017 and supplementation food ####

data.2017 <- data[data$YEAR == 2017 & !data$SUPPL == "W", ]
data.2017 <- droplevels(data.2017)
summary(data.2017)

food.models <- list()

food.models[[1]] <- glmer(NIDIF ~ NestAge + HAB2 + SUPPL + (1|ID),
                          family = binomial(link = logexp(data.2017$EXPO)),
                          nAGQ = 0,
                          data = data.2017)
summary(food.models[[1]])
#----------------------------#
food.models[[2]] <- glmer(NIDIF ~ NestAge + HAB2 + SUPPL + TEMP_NIDIF + (1|ID),
                          family = binomial(link = logexp(data.2017$EXPO)),
                          nAGQ = 0,
                          data = data.2017)
summary(food.models[[2]])
#----------------------------#
food.models[[3]] <- glmer(NIDIF ~ NestAge + HAB2 + SUPPL + PREC_NIDIF + (1|ID),
                          family = binomial(link = logexp(data.2017$EXPO)),
                          nAGQ = 0,
                          data = data.2017)
summary(food.models[[3]])
#----------------------------#
food.models[[4]] <- glmer(NIDIF ~ NestAge + HAB2 + SUPPL + PREC_NIDIF + TEMP_NIDIF + (1|ID),
                          family = binomial(link = logexp(data.2017$EXPO)),
                          nAGQ = 0,
                          data = data.2017)
summary(food.models[[4]])
#----------------------------#
food.models[[5]] <- glmer(NIDIF ~ NestAge + HAB2*SUPPL + PREC_NIDIF + TEMP_NIDIF + (1|ID),
                          family = binomial(link = logexp(data.2017$EXPO)),
                          nAGQ = 0,
                          data = data.2017)
summary(food.models[[5]])
#----------------------------#
food.models[[6]] <- glmer(NIDIF ~ NestAge + HAB2 + SUPPL*PREC_NIDIF + (1|ID),
                          family = binomial(link = logexp(data.2017$EXPO)),
                          nAGQ = 0,
                          data = data.2017)
summary(food.models[[6]])
#----------------------------#
food.models[[7]] <- glmer(NIDIF ~ NestAge + HAB2 + SUPPL*TEMP_NIDIF + (1|ID),
                          family = binomial(link = logexp(data.2017$EXPO)),
                          nAGQ = 0,
                          data = data.2017)
summary(food.models[[7]])
#----------------------------#
food.models[[8]] <- glmer(NIDIF ~ NestAge + HAB2 + SUPPL*TEMP_NIDIF + SUPPL*PREC_NIDIF + (1|ID),
                          family = binomial(link = logexp(data.2017$EXPO)),
                          nAGQ = 0,
                          data = data.2017)
summary(food.models[[8]])

#### AIC comparison ####
Modnames <- paste("mod", 1:length(food.models), sep = " ")
aictab(cand.set = food.models, modnames = Modnames, sort = TRUE)
##round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = food.models, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)

summary(food.models[[4]])