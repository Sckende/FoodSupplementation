#### Shaffer method ####

# Logit link creation
library(MASS)
logexp <- function(exposure = 1)
{
  linkfun <- function(mu) qlogis(mu^(1/exposure))
  ## FIXME: is there some trick we can play here to allow
  ##   evaluation in the context of the 'data' argument?
  linkinv <- function(eta)  plogis(eta)^exposure
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


# Model logistic exposure test

names(shaffer)

m1 <- glm(survive ~ habitat, family = binomial(link = logexp(shaffer$exposure)), data = shaffer)
# Message 
summary(m1)


#### Alternative method - cloglog link ####
# https://stats.stackexchange.com/questions/148699/modelling-a-binary-outcome-when-census-interval-varies 

m2 <- glm(survive ~ habitat + I(log(exposure)), family = binomial(link = "cloglog"), data = shaffer)
summary(m2)

install.packages('visreg')
require(visreg)
visreg(m2, scale = 'response')
