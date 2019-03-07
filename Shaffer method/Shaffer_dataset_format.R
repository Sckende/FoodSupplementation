#---------------- DAtaframe building to try log-exposure modele ---------------------------------------------------#
#------------- based on the same dataset used in GOOSE_MARK_supplemented_nests_VERSION 2 ----------------#
# Here, need to have one row for successful nest and 2 rows for failed nests
# For successful nests, variable EXPO = LastChecked - FirstFound
# For failed nests, creation of a first interval ended by a succes and EXPO = LsatPresent - FirstFound /// The second interval, EXPO = LastChecked - LastPresent and FAte = 0

# *** Warning ! Here, success == 1 and failed == 0 **** ###

setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm(list = ls())
sup <- read.table("GOOSE_geese_with_WF.txt", sep = ",", h = T)
head(sup)
sup <- sup[,-1]
summary(sup)

w05 <- read.table("GOOSE_Lecomte_supp_nests_2005.txt", h = T )
head(w05)
summary(w05)
#Computation of AgeDay1 variable = nest age when it was found
w05$AgeDay1 <- (w05$AgeFound - w05$FirstFound)
w05$TEMP_Y <- sup$TEMP_Y[match(w05$YEAR, sup$YEAR)]
w05$sdTEMP_Y <- sup$sdTEMP_Y[match(w05$YEAR, sup$YEAR)]
w05$cumTEMP_Y <- sup$cumTEMP_Y[match(w05$YEAR, sup$YEAR)]
w05$PREC_Y <- sup$PREC_Y[match(w05$YEAR, sup$YEAR)]

# Here I build a database only with water supplemented years (2005, 2015, 2016 & 2017)
# Variables related to the nests fate: ID / FirstFound / LAstPresent / LastChecked / FAte / AgeFound / AgeDay1
# Variables that I want to add for checking potential effects: YEAR / HAB / SUPPL / PREC_Y / TEMP_Y
tot <- sup[, c(1:4, 7:10, 14, 16, 21, 25:27)]
tot <- rbind(tot, w05)
summary(tot)

# Inversion of fate code
succ <- NULL
for(i in 1:dim(tot)[1]){
  if(tot$Fate[i] == 0){
    succ[i] <- 1
  }else{
    succ[i] <- 0
  }
}

tot$Fate <- succ

# Creation of 2 rows for each failed nests
bis_tab <- NULL
tot$EXPO <- NA


for(i in 1:dim(tot)[1]){
  if(tot$Fate[i] == 0){
    bis <- as.vector(tot[i,])
    bis$Fate <- 1
    bis$EXPO <- tot$LastPresent[i] - tot$FirstFound[i]
    tot$EXPO[i] <- tot$LastChecked[i] - tot$LastPresent[i]
    #bis_tab <- rbind(bis_tab, bis)
    tot <- rbind(tot, bis)
  }else{
    tot$EXPO[i] <- tot$LastChecked[i] - tot$FirstFound[i]
  }
}

#utils::View(tot)
#utils::View(bis_tab)

# EXPO = 0 is impossible, so use EXPO = 0.5
tot$EXPO[tot$EXPO == 0] <- 0.5
dim(tot[tot$EXPO == 0.5,])

summary(tot)

#### Analysis ####

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
# Now fit models... ####################################################### Fit a model of DSR as a function of tree height
mfit1 <- glm(Fate ~ HAB, family=binomial(link=logexp(tot$EXPO)), data = tot)
summary(mfit1)

####################################################### Fit a model of DSR as a quadratic function of nest age
mfit2 <- glm(Fate ~ HAB + SUPPL,
             family=binomial(link=logexp(tot$EXPO)), data = tot)
summary(mfit2)

####################################################