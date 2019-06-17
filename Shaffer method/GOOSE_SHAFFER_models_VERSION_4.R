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

# Supplementation date
d.suppl <- data[!data$SUPPL == "TEM",]; d.suppl <- droplevels(d.suppl); summary(d.suppl)

# Supplementation type
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

foo[[7]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + SUPPL_DATE_V2,
                family = binomial(link = logexp(d.foo$EXPO)),
                data = d.foo)

foo[[8]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL + YEAR*SUPPL_DATE_V2,
                family = binomial(link = logexp(d.foo$EXPO)),
                data = d.foo)

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

# ---------------------------------- #
#### ANOVA & Confident intervals ####
# -------------------------------- #

anova(foo[[4]])
Anova(foo[[4]])
confint(foo[[4]])
Dsquared(foo[[4]])
x11();plot(simulateResiduals(foo[[4]]))

# ----------------------------------------------------------------------------- #
#### Predictions for the best glm model per SUPPL and YEAR - estimates + IC ####
# --------------------------------------------------------------------------- #

pred <- data.frame(NestAge = mean(d.foo$NestAge),
                   HAB2 = factor(rep(c("MES", "WET"), 6), levels = c("MES", "WET")),
                   YEAR = factor(c(rep("2015", 4), rep("2016", 4), rep("2017", 4)), levels = c("2015", "2016", "2017")),
                   SUPPL = factor(rep(c(rep("TEM", 2), rep("W", 2)), 3), levels = c("TEM", "W")))

pp <- predict(foo[[4]], newdata = pred, se.fit = TRUE)
pp

pred <- cbind(pred, FIT = pp$fit, SE = pp$se.fit, N = group.size$n[!group.size$suppl == "F"])
pred

pred$IC_low <- pred$FIT - 1.96*(pred$SE/sqrt(pred$N))
pred$IC_high <- pred$FIT + 1.96*(pred$SE/sqrt(pred$N))



pred$FIT <- plogis(pred$FIT)
pred$IC_low <- plogis(pred$IC_low)
pred$IC_high <- plogis(pred$IC_high)

pred$NS <- pred$FIT^27
pred$IC_low_NS <- pred$IC_low^27
pred$IC_high_NS <- pred$IC_high^27

x11()

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 2 - Geese nesting success & supplemented nests/PAPER_V2/Figures/GOOSE_DSR_int_conf.tiff",
#     res=300,
#     width=30,
#     height=25,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")
color <- c("chartreuse3", "darkorange3")[as.numeric(pred$SUPPL)] 
color.2 <- c("chartreuse4", "darkorange4")[as.numeric(pred$SUPPL)]

bplot <- barplot(pred$NS,
                 space = c(rep(c(0.2, 0), 2), rep(c(0.4, 0, 0.2, 0), 2)),
                 ylim = c(0, 1),
                 las = 1,
                 col = color,
                 border = color.2,
                 cex.axis = 1.2)
axis(1,
     at = bplot,
     rep(c("MES", "WET"), 6),
     lty = 0,
     cex.axis = 0.8,
     las = 1,
     main = "Food supplementation")

legend(bplot[length(bplot)-2],
       1.020,
       legend = c("Control", "Water"),
       pch = 15,
       pt.cex = 2,
       col = c("chartreuse3", "darkorange3"),
       bty = "n",
       cex = 1.2)

arrows(x0 = bplot,
       y0 = pred$NS,
       x1 = bplot,
       y1 = pred$IC_high_NS,
       angle = 90,
       length = 0,
       col = c("chartreuse4", "chartreuse4", "darkorange4", "darkorange4"),
       lwd = 2)
arrows(x0 = bplot,
       y0 = pred$NS,
       x1 = bplot,
       y1 = pred$IC_low_NS,
       angle = 90,
       length = 0,
       col = c("chartreuse4", "chartreuse4", "darkorange4", "darkorange4"),
       lwd = 2)

mtext(c("2015", "2016", "2017"),
      side = 1,
      line = 3.5,
      at = c(mean(c(bplot[[2]], bplot[[3]])), mean(c(bplot[[6]], bplot[[7]])), mean(c(bplot[[10]], bplot[[11]]))),
      cex = 2)
mtext("Goose nesting success",
      side = 2,
      line = -7,
      las = 1,
      at = 1.05,
      cex = 1.2)

text(x = bplot,
     y = 0.2,
     labels = paste("(", pred$N, ")", sep = ""),
     cex = 1.2)
dev.off()

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

wat[[7]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + SUPPL_DATE_V2,
                family = binomial(link = logexp(d.wat$EXPO)),
                data = d.wat)
wat[[8]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + YEAR*SUPPL_DATE_V2,
                family = binomial(link = logexp(d.wat$EXPO)),
                data = d.wat)

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

# ---------------------------------- #
#### ANOVA & Confident intervals ####
# -------------------------------- #

anova(wat[[3]])
Anova(wat[[3]])
confint(wat[[3]])
Dsquared(wat[[3]])
x11(); plot(simulateResiduals(wat[[3]]))

# ----------------------------------------------------------------------------- #
#### Predictions for the best glm model per SUPPL and YEAR - estimates + IC ####
# --------------------------------------------------------------------------- #

pred <- data.frame(NestAge = mean(d.wat$NestAge),
                   HAB2 = factor(rep(c("MES", "WET"), 6), levels = c("MES", "WET")),
                   YEAR = factor(c(rep("2015", 4), rep("2016", 4), rep("2017", 4)), levels = c("2015", "2016", "2017")),
                   SUPPL = factor(rep(c(rep("TEM", 2), rep("W", 2)), 3), levels = c("TEM", "W")))

pp <- predict(wat[[3]], newdata = pred, se.fit = TRUE)
pp

pred <- cbind(pred, FIT = pp$fit, SE = pp$se.fit, N = group.size$n[!group.size$suppl == "F"])
pred

pred$IC_low <- pred$FIT - 1.96*(pred$SE/sqrt(pred$N))
pred$IC_high <- pred$FIT + 1.96*(pred$SE/sqrt(pred$N))



pred$FIT <- plogis(pred$FIT)
pred$IC_low <- plogis(pred$IC_low)
pred$IC_high <- plogis(pred$IC_high)

pred$NS <- pred$FIT^27
pred$IC_low_NS <- pred$IC_low^27
pred$IC_high_NS <- pred$IC_high^27

x11()

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 2 - Geese nesting success & supplemented nests/PAPER_V2/Figures/GOOSE_DSR_int_conf.tiff",
#     res=300,
#     width=30,
#     height=25,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")
color <- c("chartreuse3", "cyan3")[as.numeric(pred$SUPPL)] 
color.2 <- c("chartreuse4", "cyan4")[as.numeric(pred$SUPPL)]

bplot <- barplot(pred$NS,
                 space = c(rep(c(0.2, 0), 2), rep(c(0.4, 0, 0.2, 0), 2)),
                 ylim = c(0, 1),
                 las = 1,
                 col = color,
                 border = color.2,
                 cex.axis = 1.2)
axis(1,
     at = bplot,
     rep(c("MES", "WET"), 6),
     lty = 0,
     cex.axis = 0.8,
     las = 1)

legend(bplot[length(bplot)-2],
       1.020,
       legend = c("Control", "Water"),
       pch = 15,
       pt.cex = 2,
       col = c("chartreuse3", "cyan3"),
       bty = "n",
       cex = 1.2)

arrows(x0 = bplot,
       y0 = pred$NS,
       x1 = bplot,
       y1 = pred$IC_high_NS,
       angle = 90,
       length = 0,
       col = c("chartreuse4", "chartreuse4", "cyan4", "cyan4"),
       lwd = 2)
arrows(x0 = bplot,
       y0 = pred$NS,
       x1 = bplot,
       y1 = pred$IC_low_NS,
       angle = 90,
       length = 0,
       col = c("chartreuse4", "chartreuse4", "cyan4", "cyan4"),
       lwd = 2)

mtext(c("2015", "2016", "2017"),
      side = 1,
      line = 3.5,
      at = c(mean(c(bplot[[2]], bplot[[3]])), mean(c(bplot[[6]], bplot[[7]])), mean(c(bplot[[10]], bplot[[11]]))),
      cex = 2)
mtext("Goose nesting success",
      side = 2,
      line = -7,
      las = 1,
      at = 1.05,
      cex = 1.2)

text(x = bplot,
     y = 0.2,
     labels = paste("(", pred$N, ")", sep = ""),
     cex = 1.2)
dev.off()


# -------------------------- #
#### For only one figure ####
# ------------------------- #
# WATER
pred.wat <- data.frame(NestAge = mean(d.wat$NestAge),
                   HAB2 = factor(rep(c("MES", "WET"), 6), levels = c("MES", "WET")),
                   YEAR = factor(c(rep("2015", 4), rep("2016", 4), rep("2017", 4)), levels = c("2015", "2016", "2017")),
                   SUPPL = factor(rep(c(rep("TEM", 2), rep("W", 2)), 3), levels = c("TEM", "W")))

pp <- predict(wat[[3]], newdata = pred.wat, se.fit = TRUE)
pp

pred.wat <- cbind(pred.wat, FIT = pp$fit, SE = pp$se.fit, N = group.size$n[!group.size$suppl == "F"])
pred.wat

pred.wat$IC_low <- pred.wat$FIT - 1.96*(pred.wat$SE/sqrt(pred.wat$N))
pred.wat$IC_high <- pred.wat$FIT + 1.96*(pred.wat$SE/sqrt(pred.wat$N))



pred.wat$FIT <- plogis(pred.wat$FIT)
pred.wat$IC_low <- plogis(pred.wat$IC_low)
pred.wat$IC_high <- plogis(pred.wat$IC_high)

pred.wat$NS <- pred.wat$FIT^27
pred.wat$IC_low_NS <- pred.wat$IC_low^27
pred.wat$IC_high_NS <- pred.wat$IC_high^27

# FOOD
pred.foo <- data.frame(NestAge = mean(d.foo$NestAge),
                       HAB2 = factor(rep(c("MES", "WET"), 6), levels = c("MES", "WET")),
                       YEAR = factor(c(rep("2015", 4), rep("2016", 4), rep("2017", 4)), levels = c("2015", "2016", "2017")),
                       SUPPL = factor(rep(c(rep("TEM", 2), rep("F", 2)), 3), levels = c("TEM", "F")))

pp <- predict(foo[[4]], newdata = pred.foo, se.fit = TRUE)
pp

pred.foo <- cbind(pred.foo, FIT = pp$fit, SE = pp$se.fit, N = group.size$n[!group.size$suppl == "W"])
pred.foo

pred.foo$IC_low <- pred.foo$FIT - 1.96*(pred.foo$SE/sqrt(pred.foo$N))
pred.foo$IC_high <- pred.foo$FIT + 1.96*(pred.foo$SE/sqrt(pred.foo$N))



pred.foo$FIT <- plogis(pred.foo$FIT)
pred.foo$IC_low <- plogis(pred.foo$IC_low)
pred.foo$IC_high <- plogis(pred.foo$IC_high)

pred.foo$NS <- pred.foo$FIT^27
pred.foo$IC_low_NS <- pred.foo$IC_low^27
pred.foo$IC_high_NS <- pred.foo$IC_high^27

PRED <- rbind(pred.foo, pred.wat)
PRED <- split(PRED, paste(PRED$YEAR))
PRED[[1]]
PRED <- lapply(PRED, function(x){
  x <- x[-c(5,6),]
  x
})
PRED <- do.call("rbind", PRED)

x11()

 # png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 2 - Geese nesting success & supplemented nests/PAPER_V2/Figures/Figure_1_version_2.tiff",
 #     res=300,
 #     width=30,
 #     height=25,
 #     pointsize=12,
 #     unit="cm",
 #     bg="transparent")

color <- c("chartreuse3", "darkorange2", "cyan3")[as.numeric(PRED$SUPPL)] 
color.2 <- c("chartreuse4", "darkorange3", "cyan4")[as.numeric(PRED$SUPPL)] 
bplot <- barplot(PRED$NS,
                 space = c(rep(c(0.2, 0), 3), rep(c(0.4, 0, 0.2, 0, 0.2, 0), 2)),
                 ylim = c(0, 1),
                 las = 1,
                 col = color,
                 border = color.2,
                 cex.axis = 1.2)
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
       y0 = PRED$NS,
       x1 = bplot,
       y1 = PRED$IC_high_NS,
       angle = 90,
       length = 0,
       col = c("chartreuse4", "chartreuse4", "darkorange3", "darkorange3", "cyan4", "cyan4"),
       lwd = 2)
arrows(x0 = bplot,
       y0 = PRED$NS,
       x1 = bplot,
       y1 = PRED$IC_low_NS,
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
     labels = paste("(", PRED$N, ")", sep = ""),
     cex = 1.2)

dev.off()

graphics.off() 

# -------------------------------------- #
#### Effects of supplementation date ####
# ------------------------------------ #
#d.suppl$SUPPL_DATE <- as.factor(d.suppl$SUPPL_DATE)
d.suppl$NIDIF <- as.factor(d.suppl$NIDIF)
# suppl.split <- split(d.suppl, paste(d.suppl$YEAR, d.suppl$ID))
# suppl.split <- lapply(suppl.split, function(x){
#   x[nrow(x),]
# })
# head(suppl.split)
# 
# d.suppl <- as.data.frame(do.call("rbind", suppl.split))
# table(d.suppl$SUPPL_DATE, d.suppl$YEAR)
# d.suppl$NIDIF <- as.factor(d.suppl$NIDIF)
# d.suppl$SUPPL_DATE <- as.factor(d.suppl$SUPPL_DATE)

summary(d.suppl)

suppl <- list()

# Null model
suppl[[1]] <- glm(NIDIF ~ 1,
                family = binomial(link = logexp(d.suppl$EXPO)),
                data = d.suppl)

# Known effects
suppl[[2]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR,
                family = binomial(link = logexp(d.suppl$EXPO)),
                data = d.suppl)

# Additive supplementation effects
suppl[[3]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL,
                family = binomial(link = logexp(d.suppl$EXPO)),
                data = d.suppl)

summary(suppl[[3]])

# Supplementation dates effects
suppl[[4]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + SUPPL_DATE,
                family = binomial(link = logexp(d.suppl$EXPO)),
                data = d.suppl)
summary(suppl[[4]])

suppl[[5]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + SUPPL_DATE*YEAR,
                family = binomial(link = logexp(d.suppl$EXPO)),
                data = d.suppl)

summary(suppl[[5]])

suppl[[6]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + SUPPL_DATE*SUPPL,
                family = binomial(link = logexp(d.suppl$EXPO)),
                data = d.suppl)

summary(suppl[[6]])

suppl[[7]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + SUPPL_DATE*SUPPL + SUPPL_DATE*YEAR,
                family = binomial(link = logexp(d.suppl$EXPO)),
                data = d.suppl)

summary(suppl[[7]])

# -------------------- #
#### AIC comparison ####
# -------------------- #
h <- lapply(suppl, function(x){
  j <- print(x$formula)
  j
})
h <- as.vector(as.character(h))


Modnames <- paste(paste("mod", 1:length(suppl), sep = " "), h, sep = "-")
AIC <- aictab(cand.set = suppl, modnames = Modnames, sort = TRUE)
print(AIC, digit = 2)

confint(suppl[[3]])
Anova(suppl[[3]])
