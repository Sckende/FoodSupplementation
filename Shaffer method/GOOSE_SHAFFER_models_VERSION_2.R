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
#glmer.models <- list()
glm.models <- list()

#### For hypothesis about supplementation and year effect ####
#### Basic models ####
    # Null
# glmer.models[[1]] <- glmer(NIDIF ~ 1 + (1|ID),
#              family = binomial(link = logexp(data$EXPO)),
#              data = data)
# 
# gg <- glmer(NIDIF ~ 1 + (1|ID.2),
#             family = binomial(link = logexp(data$EXPO)),
#             data = data)

glm.models[[1]] <- glm(NIDIF ~ 1,
                          family = binomial(link = logexp(data$EXPO)),
                          data = data)

summary(glm.models[[1]])

    # Known effects
# glmer.models[[2]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + (1|ID),
#              family = binomial(link = logexp(data$EXPO)),
#              data = data)
# 
# gg.2 <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + (1|ID.2),
#               family = binomial(link = logexp(data$EXPO)),
#               data = data)

glm.models[[2]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR,
                          family = binomial(link = logexp(data$EXPO)),
                          data = data)

summary(glm.models[[2]])

#### Supplementation effects ####
    # Additive effects of supplementation
# glmer.models[[3]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + (1|ID),
#              family = binomial(link = logexp(data$EXPO)),
#              data = data)

glm.models[[3]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL,
                          family = binomial(link = logexp(data$EXPO)),
                          data = data)

summary(glm.models[[3]])

    # Interaction effects - YEAR * SUPPL

# glmer.models[[4]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL + (1|ID),
#              family = binomial(link = logexp(data$EXPO)),
#              control = glmerControl(optimizer = "optimx",
#                                     calc.derivs = FALSE,
#                                     optCtrl = list(method = "nlminb",
#                                                    starttests = FALSE,
#                                                    kkt = FALSE)), # For the convergence 
#              data = data)

glm.models[[4]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL,
                          family = binomial(link = logexp(data$EXPO)),
                          data = data
)

summary(glm.models[[4]]) # Here Variance of random effects is weird with nlminb optimizer. AND estimates of model is equivalent when I use nAGQ = 0, with a more realistic variance of random effects

# Interaction effects - HAB2 * SUPPL
# optimx optimizer ==> "Nelder-Mead", "BFGS", "L-BFGS-B", "CG", "nlminb", "ucminf", "nlm", "uobyqa", "newuoa", "bobyqa", "Rcgmin", "Rvmmin", "spg"
  
# glmer.models[[5]] <- glmer(NIDIF ~ NestAge + HAB2*SUPPL + YEAR + (1|ID),
#                             family = binomial(link = logexp(data$EXPO)),
#                             #nAGQ = 0,
#                             control = glmerControl(optimizer = "optimx",
#                                                    calc.derivs = FALSE,
#                                                    optCtrl = list(method = "nlminb",
#                                                                   starttests = FALSE,
#                                                                   kkt = FALSE)), # For the convergence
#                             data = data)
  
glm.models[[5]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR,
                            family = binomial(link = logexp(data$EXPO)),
                            data = data)

summary(glm.models[[5]])


    # Interaction effects - HAB2 * SUPPL and YEAR * SUPPL
# glmer.models[[6]] <- glmer(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL + (1|ID),
#                           family = binomial(link = logexp(data$EXPO)),
#                           control = glmerControl(optimizer = "optimx",
#                                                  calc.derivs = FALSE,
#                                                  optCtrl = list(method = "nlminb",
#                                                                 starttests = FALSE,
#                                                                 kkt = FALSE)), # For the convergence
#                           #nAGQ = 0,
#                           data = data)

glm.models[[6]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL,
                        family = binomial(link = logexp(data$EXPO)),
                        data = data)

summary(glm.models[[6]])

#### AIC comparison ####
# Modnames <- paste("mod", 1:length(glmer.models), sep = " ")
# aictab(cand.set = glmer.models, modnames = Modnames, sort = TRUE)

Modnames <- paste("mod", 1:length(glm.models), sep = " ")
aictab(cand.set = glm.models, modnames = Modnames, sort = TRUE)

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


#### For hypothesis about climate effects - When problems arise ! ####
#### *** WARNINGS - Have to change names of model *** ####
#### Supplementation*Local climate effects ####
clim.mod <- list()

clim.mod[[1]] <- glmer(NIDIF ~ NestAge + HAB2 + scale(TEMP_NIDIF) + (1|ID),
                       family = binomial(link = logexp(data$EXPO)),
                       data = data)
summary(clim.mod[[1]])

#-------------------#

clim.mod[[2]] <- glmer(NIDIF ~ NestAge + HAB2 + poly(scale(TEMP_NIDIF), 2) + (1|ID),
                       family = binomial(link = logexp(data$EXPO)),
                       control = glmerControl(optimizer = "optimx",
                                              calc.derivs = FALSE,
                                              optCtrl = list(method = "nlminb",
                                                             starttests = FALSE,
                                                             kkt = FALSE)),
                       data = data)
summary(clim.mod[[2]])

#-------------------#

clim.mod[[3]] <- glmer(NIDIF ~ NestAge + HAB2 + scale(TEMP_NIDIF) + poly(scale(TEMP_NIDIF), 2) + (1|ID),
                       family = binomial(link = logexp(data$EXPO)),
                       control = glmerControl(optimizer = "optimx",
                                              calc.derivs = FALSE,
                                              optCtrl = list(method = "nlminb",
                                                             starttests = FALSE,
                                                             kkt = FALSE)),
                       data = data)
summary(clim.mod[[3]])

    # Global temperature
clim.mod[[1]] <- glmer(NIDIF ~ NestAge + HAB2 + SUPPL + poly(TEMP_NIDIF, 2) + (1|ID),
             family = binomial(link = logexp(data$EXPO)),
             data = data)
summary(clim.models[[1]])

    # Global precipitation
cand.models[[7]] <- glmer(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + PREC_NIDIF + (1|ID),
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


#### Sample size for each group per YEAR/HAB/Treatment ####
size <- split(data, paste(data$YEAR, data$SUPPL, data$HAB2))


group.size <- lapply(size, function(x){
  sample_size <- length(unique(x$ID))
  t <- c(as.character(x$YEAR[1]), as.character(x$HAB2[1]), as.character(x$SUPPL[1]), sample_size)

})

group.size <- as.data.frame(do.call("rbind", group.size))
group.size$V4 <- as.numeric(as.character(group.size$V4))

summary(group.size)
levels(group.size$V3)

group.size$V3 <- factor(group.size$V3,levels(group.size$V3)[c(2, 1, 3)])

group.size <- group.size[with(group.size, order(V1, V3, V2)),]
# or
# group.size[order(group.size$V1, group.size$V3, group.size$V2),]

group.size$V5 <- paste("(", group.size$V4, ")", sep = "")

#### Predictions for the best glm model - Laurent style ####

summary(glm.models[[4]])

# Modification of reference level to check effect
data$YEAR <- relevel(data$YEAR, "2017")
data$YEAR <- relevel(data$YEAR, "2016")
glm.models[[4]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL,
                       family = binomial(link = logexp(data$EXPO)),
                       data = data
)

# New dataframe for predictions with Reference level of year == 2015 and habitat == MES
data$YEAR <- relevel(data$YEAR, "2015")
glm.models[[4]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL,
                       family = binomial(link = logexp(data$EXPO)),
                       data = data
)

pred <- data.frame(NestAge = mean(data$NestAge),
                   HAB2 = factor(rep(c("MES", "WET"), 9), levels = c("MES", "WET")),
                   YEAR = factor(c(rep("2015", 6), rep("2016", 6), rep("2017", 6)), levels = c("2015", "2016", "2017")),
                   SUPPL = factor(rep(c(rep("TEM", 2), rep("F", 2), rep("W", 2)), 3), levels = c("TEM", "F", "W")))

pp <- predict(glm.models[[4]], newdata = pred, se.fit = TRUE)

plogis(pp[[1]])^27
plogis(pp[[2]])^27

data.predict <- as.data.frame(pp)[,-3]
data.predict$SE.upper <- data.predict$fit + data.predict$se.fit
data.predict$SE.lower <- data.predict$fit - data.predict$se.fit

data.predict <- apply(data.predict, MARGIN = 2, plogis)

# Transformation for Nesting Success values
data.predict <- apply(data.predict, MARGIN = 2, function(x){
  x <- x^27
  x
})
data.predict <- cbind(pred, data.predict)

color <- c("chartreuse3", "darkorange2", "cyan3")[as.numeric(data.predict$SUPPL)] 
color.2 <- c("chartreuse4", "darkorange3", "cyan4")[as.numeric(data.predict$SUPPL)] 

#x11()

png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 1 - Geese nesting success & supplemented nests/PAPER_V2/Figures/GOOSE_Nesting_succ_suppl.tiff",
    res=300,
    width=30,
    height=25,
    pointsize=12,
    unit="cm",
    bg="transparent")
#par(mar = c(5.1, 4.1, 4.1, 2.1))

bplot <- barplot(data.predict$fit,
                 space = c(rep(c(0.2, 0), 3), rep(c(0.4, 0, 0.2, 0, 0.2, 0), 2)),
                 ylim = c(0, 1),
                 las = 1,
                 # angle = rep(c(45, 45, 135, 135, 11, 11), 3),
                 # density = 30, # angle & border are for the texture of bars
                 col = color,
                 border = color.2,
                 cex.axis = 1.2)

# Modification de texture 
# barplot(data.predict$fit,
#         space = c(rep(c(0.2, 0), 3), rep(c(0.4, 0, 0.2, 0, 0.2, 0), 2)),
#         col = color.2,
#         border = color.2,
#         angle = rep(c(135, 135, 45, 45, 11, 11), 3),
#         density = 30,
#         add = TRUE,
#         xaxt = "n",
#         yaxt = "n")

axis(1,
     at = bplot,
     rep(c("MES", "WET"), 9),
     lty = 0,
     cex.axis = 0.8,
     las = 1)

legend(bplot[length(bplot)-2],
       1.020,
       legend = c("Control", "Food", "Water"),
       pch = 15,
       pt.cex = 2,
       col = c("chartreuse3", "darkorange2", "cyan3"),
       bty = "n",
       cex = 1.2)

arrows(x0 = bplot,
       y0 = data.predict$fit,
       x1 = bplot,
       y1 = data.predict$SE.upper,
       angle = 90,
       length = 0,
       col = c("chartreuse4", "chartreuse4", "darkorange3", "darkorange3", "cyan4", "cyan4"),
       lwd = 2)
arrows(x0 = bplot,
       y0 = data.predict$fit,
       x1 = bplot,
       y1 = data.predict$SE.lower,
       angle = 90,
       length = 0,
       col = c("chartreuse4", "chartreuse4", "darkorange3", "darkorange3", "cyan4", "cyan4"),
       lwd = 2)

mtext(c("2015", "2016", "2017"),
      side = 1,
      line = 3.5,
      at = c(mean(c(bplot[[3]], bplot[[4]])), mean(c(bplot[[9]], bplot[[10]])), mean(c(bplot[[15]], bplot[[16]]))),
      cex = 2)
mtext("Goose nesting success",
      side = 2,
      line = -7,
      las = 1,
      at = 1.05,
      cex = 1.2)

text(x = bplot,
     y = 0.2,
     labels = paste("(", group.size$V4, ")", sep = ""),
     cex = 1.2)

dev.off()
