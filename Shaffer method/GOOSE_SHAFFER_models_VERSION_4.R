#### Models with logistic-exposure link AND ONE MODEL PER SUPPLEMENTATION TYPE ####
# -------------------------------- 24 mai 2019 --------------------------------- #

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
library("car") # For the Anova command
library("multcomp") # For the contrast analysis
library("emmeans") # For the contrast analysis
library("modEvA") # For the variance explained

#### Import data ####
data <- read.table("GOOSE_SHAFFER_database_all_nests_2005_2015-2017.csv", h = T, sep = " ")
summary(data)

data <- data[!data$YEAR == 2005,]
data$YEAR <- as.factor(data$YEAR)
data$SUPPL <- relevel(data$SUPPL, "TEM")
data$HAB2 <- relevel(data$HAB2, "MES")

d.foo <- data[!data$SUPPL == "W",]
d.wat <- data[!data$SUPPL == "F",]

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

# ------------------------------------------------------- #
#### Sample size for each group per YEAR/HAB/Treatment ####
# ------------------------------------------------------- #

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
names(group.size) <- c("year", "hab", "suppl", "n")

# --------------------------- #
#### Food supplementation ####
# ------------------------- #

foo <- list()

# Null model
foo[[1]] <- glm(NIDIF ~ 1,
                       family = binomial(link = logexp(d.foo$EXPO)),
                       data = d.foo)

summary(foo[[1]])

# Known effects
foo[[2]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR,
                       family = binomial(link = logexp(d.foo$EXPO)),
                       data = d.foo)

summary(foo[[2]])

# Additive supplementation effects
foo[[3]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL,
                       family = binomial(link = logexp(d.foo$EXPO)),
                       data = d.foo)

summary(foo[[3]])

# Supplementation effects with interactions
foo[[4]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL,
                       family = binomial(link = logexp(d.foo$EXPO)),
                       data = d.foo)

summary(foo[[4]])

foo[[5]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR,
                       family = binomial(link = logexp(d.foo$EXPO)),
                       data = d.foo)

summary(foo[[5]])


foo[[6]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL,
                       family = binomial(link = logexp(d.foo$EXPO)),
                       data = d.foo)

summary(foo[[6]])

# -------------------- #
#### AIC comparison ####
# -------------------- #
h <- lapply(foo, function(x){
  j <- print(x$formula)
  j
})
h <- as.vector(as.character(h))


Modnames <- paste(paste("mod", 1:length(foo), sep = " "), h, sep = "-")
AIC <- aictab(cand.set = foo, modnames = Modnames, sort = TRUE)
AIC

summary(foo[[4]])
summary(foo[[6]])

# ---------------------------- #
#### Water supplementation ####
# -------------------------- #

wat <- list()

# Null model
wat[[1]] <- glm(NIDIF ~ 1,
                family = binomial(link = logexp(d.wat$EXPO)),
                data = d.wat)

summary(wat[[1]])

# Known effects
wat[[2]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR,
                family = binomial(link = logexp(d.wat$EXPO)),
                data = d.wat)

summary(wat[[2]])

# Additive supplementation effects
wat[[3]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL,
                family = binomial(link = logexp(d.wat$EXPO)),
                data = d.wat)

summary(wat[[3]])

# Supplementation effects with interactions
wat[[4]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL,
                family = binomial(link = logexp(d.wat$EXPO)),
                data = d.wat)

summary(wat[[4]])

wat[[5]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR,
                family = binomial(link = logexp(d.wat$EXPO)),
                data = d.wat)

summary(wat[[5]])


wat[[6]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL,
                family = binomial(link = logexp(d.wat$EXPO)),
                data = d.wat)

summary(wat[[6]])

# -------------------- #
#### AIC comparison ####
# -------------------- #
h <- lapply(wat, function(x){
  j <- print(x$formula)
  j
})
h <- as.vector(as.character(h))


Modnames <- paste(paste("mod", 1:length(wat), sep = " "), h, sep = "-")
AIC <- aictab(cand.set = wat, modnames = Modnames, sort = TRUE)
print(AIC, digit = 2)


summary(wat[[3]])
summary(wat[[4]])
summary(wat[[5]])
