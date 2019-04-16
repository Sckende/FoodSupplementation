#### Models with logistic-exposure link ####

setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
rm(list = ls())

#### Packages ####
library("lme4") # For generalised linear models
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
             data = data,
             nAGQ = 0)
summary(cand.models[[1]])

    # Known effects
cand.models[[2]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             data = data,
             nAGQ = 0)
summary(cand.models[[2]])

#### Supplementation effects ####
    # Additive effects of supplementation
cand.models[[3]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             data = data,
             nAGQ = 0)
summary(cand.models[[3]])

#### *** WARNINGS - PB de convergence pour tous les modèles qui suivent *** ####
    # Interaction effects - YEAR * SUPPL
cand.models[[4]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             data = data,
             #control = strict_tol
             nAGQ = 0
             ) #For the convergence
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
             data = data,
             nAGQ = 0)
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
summary(cand.models[[7]]) # PB avec PREC_NIDIF car beaucoup de 0

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

#### Supplementation + Local climate effects ####
    # Global temperature
cand.models[[10]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL  + TEMP_NIDIF + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             data = data,
             nAGQ = 0)
summary(cand.models[[10]])

    # Global precipitation
cand.models[11] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + PREC_NIDIF + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             data = data,
             nAGQ = 0)
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
                           data = data,
                           nAGQ = 0)
summary(cand.models[[15]])

#### AIC comparison ####
Modnames <- paste("mod", 1:length(cand.models), sep = " ")
aictab(cand.set = cand.models, modnames = Modnames, sort = TRUE)
##round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = cand.models, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)

#### Simulations with the best modele ####
sims <- simulateResiduals(cand.models[[16]])
x11()
plot(sims)

s <- simulate(cand.models[[15]], 100)
?simulate.merMod

plogis(predict(cand.models[[6]]))
