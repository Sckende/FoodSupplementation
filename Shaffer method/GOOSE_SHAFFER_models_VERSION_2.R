#### Models with logistic-exposure link ####

setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
rm(list = ls())

#### Packages ####
library("lme4") # For generalised linear models
library("glmmTMB")
library("optimx")
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


#### Colinearity checking ####
# Colinearity is for multiple variables which explain the same proportion of the variance
#fonction vif.mer (pour modèles mixtes):
vif.mer <- function (fit) {
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

#faire vif.mer(model) où model = modèle complet sans interactions, si une variable a une valeur >3 (Graham 2003), la supprimer et re-rouler vif.mer, jusqu'à ce que toutes les valeurs soient <3.

model <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + TEMP_NIDIF + PREC_NIDIF + (1|ID),
               family = binomial(link = logexp(data$EXPO)),
               data = data)
vif.mer(model)
# NestAge    HAB2WET   YEAR2016   YEAR2017     SUPPLF     SUPPLW TEMP_NIDIF PREC_NIDIF 
# 2.629498   1.196131   3.767380   6.116633   1.083354   1.106481   4.982207   1.909370 
#ensuite rouler le set de modèles  
model <- glmer(NIDIF ~ NestAge + HAB2 + SUPPL + TEMP_NIDIF + PREC_NIDIF + (1|ID),
               family = binomial(link = logexp(data$EXPO)),
               data = data)
vif.mer(model)

# NestAge    HAB2WET     SUPPLF     SUPPLW TEMP_NIDIF PREC_NIDIF 
# 1.456374   1.230185   1.084011   1.098437   1.453626   1.125000

model <- glmer(NIDIF ~ NestAge + HAB2 + SUPPL + YEAR + (1|ID),
               family = binomial(link = logexp(data$EXPO)),
               data = data)
vif.mer(model)

# NestAge  HAB2WET   SUPPLF   SUPPLW YEAR2016 YEAR2017 
# 1.059011 1.133831 1.100136 1.096697 1.607384 1.676832 

model <- glmer(NIDIF ~ NestAge + HAB2 + SUPPL + YEAR + TEMP_EXPO + PREC_EXPO + (1|ID),
               family = binomial(link = logexp(data$EXPO)),
               data = data)
vif.mer(model)
# NestAge   HAB2WET    SUPPLF    SUPPLW  YEAR2016  YEAR2017 TEMP_EXPO PREC_EXPO 
# 1.000329  1.000005  1.000000  1.000000  1.000001  1.000003  1.000302  1.000024
# I do really not trust this output ... 

model <- glmer(NIDIF ~ NestAge + HAB2 + SUPPL + TEMP_EXPO + PREC_EXPO + (1|ID),
               family = binomial(link = logexp(data$EXPO)),
               data = data)
vif.mer(model)
# NestAge   HAB2WET    SUPPLF    SUPPLW TEMP_EXPO PREC_EXPO 
# 2.776589  1.127051  1.108564  1.081134  2.825169  1.095394


#### I choose to keep YEAR rather than TEMP/PREC_NIDIF ####
cand.models <- list()
#### For hypothesis about supplementation and year effect ####
#### Basic models ####
    # Null
cand.models[[1]] <- glmer(NIDIF ~ 1 + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             #nAGQ = 0,
             data = data)

# cand.models[[1]] <- glm(NIDIF ~ 1,
#                           family = binomial(link = logexp(data$EXPO)),
#                           data = data)

summary(cand.models[[1]])

    # Known effects
cand.models[[2]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             #nAGQ = 0,
             data = data)

# cand.models[[2]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR,
#                           family = binomial(link = logexp(data$EXPO)),
#                           #nAGQ = 0,
#                           data = data)

summary(cand.models[[2]])

#### Supplementation effects ####
    # Additive effects of supplementation
cand.models[[3]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             #nAGQ = 0,
             data = data)

# cand.models[[3]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL,
#                           family = binomial(link = logexp(data$EXPO)),
#                           #nAGQ = 0,
#                           data = data)

summary(cand.models[[3]])

    # Interaction effects - YEAR * SUPPL
cand.models[[4]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             control = glmerControl(optimizer = "optimx",
                                    calc.derivs = FALSE,
                                    optCtrl = list(method = "nlminb",
                                                   starttests = FALSE,
                                                   kkt = FALSE)), # For the convergence
             #nAGQ = 0, 
             data = data)

# cand.models[[4]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL,
#                           family = binomial(link = logexp(data$EXPO)),
#                           data = data
# )

summary(cand.models[[4]]) # Here Variance of random effects is weird with nlminb optimizer. AND estimates of model is equivalent when I use nAGQ = 0, with a more realistic variance of random effects

    # Interaction effects - HAB2 * SUPPL
# optimx optimizer ==> "Nelder-Mead", "BFGS", "L-BFGS-B", "CG", "nlminb", "ucminf", "nlm", "uobyqa", "newuoa", "bobyqa", "Rcgmin", "Rvmmin", "spg"
  
  cand.models[[5]] <- glmer(NIDIF ~ NestAge + HAB2*SUPPL + YEAR + (1|ID),
                            family = binomial(link = logexp(data$EXPO)),
                            control = glmerControl(optimizer = "optimx",
                                                   calc.derivs = FALSE,
                                                   optCtrl = list(method = "nlminb",
                                                                  starttests = FALSE,
                                                                  kkt = FALSE)), # For the convergence
                            #nAGQ = 0,
                            data = data)
  
  # cand.models[[5]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR,
  #                           family = binomial(link = logexp(data$EXPO)),
  #                           data = data)
  
  summary(cand.models[[5]])


    # Interaction effects - HAB2 * SUPPL and YEAR * SUPPL
cand.models[[6]] <- glmer(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL + (1|ID),
                          family = binomial(link = logexp(data$EXPO)),
                          control = glmerControl(optimizer = "optimx",
                                                 calc.derivs = FALSE,
                                                 optCtrl = list(method = "nlminb",
                                                                starttests = FALSE,
                                                                kkt = FALSE)), # For the convergence
                          #nAGQ = 0,
                          data = data)

# cand.models[[6]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL,
#                         family = binomial(link = logexp(data$EXPO)),
#                         data = data)

summary(cand.models[[6]])

#### AIC comparison ####
Modnames <- paste("mod", 1:length(cand.models), sep = " ")
aictab(cand.set = cand.models, modnames = Modnames, sort = TRUE)
##round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = cand.models, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)

#### Test for the random effect ####

# créer une variable dummy (juste des 1)

data$Dummy <- factor(rep(1, each = length(data$ID))) 
# où ID est une variable (ici l'ID de ma femelle)

#comparer les structures (avec modèle le plus complexe), il faut bien garder la même structure d'effets fixes ! :
rand=list()
rand[[1]] <- glmer(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL + (1|ID),
                   family = binomial(link = logexp(data$EXPO)),
                   control = glmerControl(optimizer = "optimx",
                                          check.nlev.gtr.1="ignore",#argument qui permet de contourner le fait que ton facteur n'a qu'un niveau
                                          calc.derivs = FALSE,
                                          optCtrl = list(method = "nlminb",
                                                         starttests = FALSE,
                                                         kkt = FALSE)), # For the convergence
                   #nAGQ = 0,
                   data = data)
 
summary(rand[[1]])

rand[[2]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + (1|ID),
                   family = binomial(link = logexp(data$EXPO)),
                   #nAGQ = 0,
                   data = data)
summary(rand[[2]])

rand[[3]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + (1|ID),
                family = binomial(link = logexp(data$EXPO)),
                #nAGQ = 0,
                data = data)
summary(rand[[3]])

rand[[4]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL + (1|ID),
                   family = binomial(link = logexp(data$EXPO)),
                   control = glmerControl(optimizer = "optimx",
                                          calc.derivs = FALSE,
                                          optCtrl = list(method = "nlminb",
                                                         starttests = FALSE,
                                                         kkt = FALSE)), # For the convergence
                   #nAGQ = 0, 
                   data = data)
summary(rand[[4]])

rand[[5]] <- glmer(NIDIF ~ NestAge + HAB2*SUPPL + YEAR + (1|ID),
                          family = binomial(link = logexp(data$EXPO)),
                          control = glmerControl(optimizer = "optimx",
                                                 calc.derivs = FALSE,
                                                 optCtrl = list(method = "nlminb",
                                                                starttests = FALSE,
                                                                kkt = FALSE)), 
                          data = data)

summary(rand[[5]])


rand[[6]] <- glmer(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL + (1|ID),
                          family = binomial(link = logexp(data$EXPO)),
                          control = glmerControl(optimizer = "optimx",
                                                 calc.derivs = FALSE,
                                                 optCtrl = list(method = "nlminb",
                                                                starttests = FALSE,
                                                                kkt = FALSE)),
                          #nAGQ = 0,
                          data = data)
summary(rand[[6]])


#comparer par anova/LRT ou AIC
Modnames <- paste("mod", 1:length(rand), sep = " ")
aictab(cand.set = rand, modnames = Modnames, sort = TRUE)
##round to 4 digits after decimal point and give log-likelihood
print(aictab(cand.set = rand, modnames = Modnames, sort = TRUE),
      digits = 4, LL = TRUE)



#### For hypothesis about climate effects - When problems rise ! ####
#### *** WARNINGS - Have to change names of model *** ####
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
sims <- simulateResiduals(cand.models[[4]])
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
