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
# or
# group.size[order(group.size$V1, group.size$V3, group.size$V2),]

# ------------------------------ #
#### Colinearity checking ??? ####
# ------------------------------ #

# ---------------------------------------------------------- #
#### For hypothesis about supplementation and year effect ####
# ---------------------------------------------------------- #

glm.models <- list()

  # Null model
glm.models[[1]] <- glm(NIDIF ~ 1,
                          family = binomial(link = logexp(data$EXPO)),
                          data = data)

summary(glm.models[[1]])

    # Known effects
glm.models[[2]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR,
                          family = binomial(link = logexp(data$EXPO)),
                          data = data)

summary(glm.models[[2]])

  # Additive supplementation effects
glm.models[[3]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL,
                          family = binomial(link = logexp(data$EXPO)),
                          data = data)

summary(glm.models[[3]])

  # Supplementation effects with interactions
glm.models[[4]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL,
                          family = binomial(link = logexp(data$EXPO)),
                          data = data)

summary(glm.models[[4]])

glm.models[[5]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR,
                            family = binomial(link = logexp(data$EXPO)),
                            data = data)

summary(glm.models[[5]])


glm.models[[6]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL,
                        family = binomial(link = logexp(data$EXPO)),
                        data = data)

summary(glm.models[[6]])

# -------------------- #
#### AIC comparison ####
# -------------------- #
h <- lapply(glm.models, function(x){
  j <- print(x$formula)
  j
  })
h <- as.vector(as.character(h))


Modnames <- paste(paste("mod", 1:length(glm.models), sep = " "), h, sep = "-")
AIC <- aictab(cand.set = glm.models, modnames = Modnames, sort = TRUE)
AIC

# Contrast analysis for the best model
anova(glm.models[[4]])
Anova(glm.models[[4]])
Dsquared(glm.models[[4]])

# emmeans package

emmeans::emmip(glm.models[[4]], YEAR ~ SUPPL)
emmeans::emmip(glm.models[[4]], SUPPL ~ YEAR)

emmeans(glm.models[[4]], pairwise ~ SUPPL | YEAR)
emmeans(glm.models[[4]], pairwise ~ YEAR | SUPPL)

joint_tests(glm.models[[4]])

# multcomp package
summary(glht(glm.models[[4]], mcp(SUPPL = "Tukey")))
# Construction of a contrast matrix based on Tukey-contrasts for YEAR in a block-diagonal way, i.e., for each level of SUPPL

Tuk <- contrMat(table(data$YEAR), "Tukey")

kk1 <- cbind(Tuk, matrix(0, nrow = nrow(Tuk), ncol = ncol(Tuk)), matrix(0, nrow = nrow(Tuk), ncol = ncol(Tuk)))
rownames(kk1) <- paste(levels(data$SUPPL)[1], rownames(kk1), sep = ":")

kk2 <- cbind(matrix(0, nrow = nrow(Tuk), ncol = ncol(Tuk)), Tuk, matrix(0, nrow = nrow(Tuk), ncol = ncol(Tuk)))
rownames(kk2) <- paste(levels(data$SUPPL)[2], rownames(kk2), sep = ":")

kk3 <- cbind(matrix(0, nrow = nrow(Tuk), ncol = ncol(Tuk)), matrix(0, nrow = nrow(Tuk), ncol = ncol(Tuk)), Tuk)
rownames(kk3) <- paste(levels(data$SUPPL)[3], rownames(kk3), sep = ":")

kk <- rbind(kk1, kk2, kk3)
colnames(kk) <- c(colnames(Tuk), colnames(Tuk), colnames(Tuk))

# Perform the test
data$Y.S <- with(data, interaction(YEAR, SUPPL))
cell <- glm(NIDIF ~ Y.S - 1, data = data)
summary(glht(cell, linfct = kk)) 

# OR

tmpp <- expand.grid(YEAR = unique(data$YEAR),
                   SUPPL = unique(data$SUPPL), 
                   NestAge = mean(data$NestAge),
                   HAB2 = "MES")
x <- model.matrix(~ NestAge + HAB2 + YEAR*SUPPL,
                  family = binomial(link = logexp(data$EXPO)), data = tmpp)
?glht(glm.models[[4]], linfct = x)

# -------------------------------------------------------- #
#### Predictions for the best glm model - estimés + IC ####
# ------------------------------------------------------ #

pred <- data.frame(NestAge = mean(data$NestAge),
                   HAB2 = "MES",
                   YEAR = factor(c(rep("2015", 3), rep("2016", 3), rep("2017", 3)), levels = c("2015", "2016", "2017")),
                   SUPPL = factor(rep(c("TEM", "F", "W"), 3), levels = c("TEM", "F", "W")))

pp <- predict(glm.models[[4]], newdata = pred, se.fit = TRUE)
pp

pred <- cbind(pred, FIT = pp$fit, SE = pp$se.fit, N = as.vector(table(data$SUPPL, data$YEAR)))
pred

pred$IC_low <- pred$FIT - 1.96*(pred$SE/sqrt(pred$N))
pred$IC_high <- pred$FIT + 1.96*(pred$SE/sqrt(pred$N))



pred$FIT <- plogis(pred$FIT)
pred$IC_low <- plogis(pred$IC_low)
pred$IC_high <- plogis(pred$IC_high)

#x11()

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 1 - Geese nesting success & supplemented nests/PAPER_V2/Figures/GOOSE_DSR_int_conf.tiff",
#     res=300,
#     width=30,
#     height=25,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

plot(pred$FIT[pred$SUPPL == "TEM"], type = "l", ylim = c(min(pred$IC_low), 1), bty = "n", lwd = 2, xaxt = "n", xlab = "", ylab = "Daily survival rate", col = "chartreuse3")
lines(pred$FIT[pred$SUPPL == "W"], col = "cyan3", lwd = 2)
lines(pred$FIT[pred$SUPPL == "F"], col = "darkorange2", lwd = 2)

col <- c("chartreuse3", "cyan3", "darkorange2")
s <- c("TEM", "W", "F")
for(i in 1:3){
  arrows(x0 = c(1, 2, 3),
         y0 = pred$FIT[pred$SUPPL == s[i]],
         x1 = c(1, 2, 3),
         y1 = pred$IC_high[pred$SUPPL == s[i]],
         angle = 90,
         length = 0,
         col = col[i],
         lwd = 2)
  
  arrows(x0 = c(1, 2, 3),
         y0 = pred$FIT[pred$SUPPL == s[i]],
         x1 = c(1, 2, 3),
         y1 = pred$IC_low[pred$SUPPL == s[i]],
         angle = 90,
         length = 0,
         col = col[i],
         lwd = 2)
  
}
axis(1, at = c(1, 2, 3), labels = c("2015", "2016", "2017"))
legend("topright", bty = "n", legend = c("TEMOIN", "WATER", "FOOD"), col = col, lty = 1)

dev.off()

# -------------------------------------------------------- #
#### Predictions for the best glm model - Laurent style ####
# -------------------------------------------------------- #

summary(glm.models[[4]])
Anova(glm.models[[4]])
  # Checking what is happening when the YEAR reference level changes
data$YEAR <- relevel(data$YEAR, "2017")
data$YEAR <- relevel(data$YEAR, "2016")
summary(glm.models[[4]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL,
                       family = binomial(link = logexp(data$EXPO)),
                       data = data))

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
pp

  # Back transformation 
plogis(pp[[1]])^27

  # Creation of a dataframe with Nesting success values
data.predict <- as.data.frame(pp)[,-3]
data.predict$SE.upper <- data.predict$fit + data.predict$se.fit
data.predict$SE.lower <- data.predict$fit - data.predict$se.fit

data.predict <- apply(data.predict, MARGIN = 2, plogis)

data.predict <- apply(data.predict, MARGIN = 2, function(x){
  x <- x^27
  x
})
data.predict <- cbind(pred, data.predict)
data.predict <- cbind(data.predict, group.size)
data.predict$IC_low <- data.predict$fit - 1.96*((data.predict$SE.upper - data.predict$fit)/sqrt(data.predict$V4))
data.predict$IC_high <- data.predict$fit + 1.96*((data.predict$SE.upper - data.predict$fit)/sqrt(data.predict$V4))


  # Plot the results
color <- c("chartreuse3", "darkorange2", "cyan3")[as.numeric(data.predict$SUPPL)] 
color.2 <- c("chartreuse4", "darkorange3", "cyan4")[as.numeric(data.predict$SUPPL)] 

x11()

# png("C:/Users/HP_9470m/Dropbox/PHD. Claire/Chapitres de thèse/CHAPTER 1 - Geese nesting success & supplemented nests/PAPER_V2/Figures/GOOSE_Nesting_succ_suppl.tiff",
#     res=300,
#     width=30,
#     height=25,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

bplot <- barplot(data.predict$fit,
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
       y0 = data.predict$fit,
       x1 = bplot,
       y1 = data.predict$IC_high,
       angle = 90,
       length = 0,
       col = c("chartreuse4", "chartreuse4", "darkorange3", "darkorange3", "cyan4", "cyan4"),
       lwd = 2)
arrows(x0 = bplot,
       y0 = data.predict$fit,
       x1 = bplot,
       y1 = data.predict$IC_low,
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


# ----------------------------------------------------------- #
#### For hypothesis about climate effects AND YEAR - GLM  ####
# --------------------------------------------------------- #

# Test for the polynomial effect of the Temperature variable
# To do that, compare the most complex model with limear, then polynomial effect

poly.test.TEMP <- list()

poly.test.TEMP[[1]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL + SUPPL*PREC_NIDIF + SUPPL*TEMP_NIDIF,
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

poly.test.TEMP[[2]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL + SUPPL*PREC_NIDIF + SUPPL*poly(TEMP_NIDIF, 2),
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

poly.test.TEMP[[3]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL + SUPPL*PREC_NIDIF + SUPPL*poly(TEMP_NIDIF, 3),
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

Modnames <- paste("mod", 1:length(poly.test.TEMP), sep = " ")
aictab(cand.set = poly.test.TEMP, modnames = Modnames, sort = TRUE) # the best model displays a quadratic effect

poly.test.PREC <- list()

poly.test.PREC[[1]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL + SUPPL*scale(PREC_NIDIF) + SUPPL*scale(TEMP_NIDIF),
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

poly.test.PREC[[2]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL + SUPPL*poly(scale(PREC_NIDIF), 2) + SUPPL*scale(TEMP_NIDIF),
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

poly.test.PREC[[3]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL + SUPPL*poly(scale(PREC_NIDIF), 3) + SUPPL*scale(TEMP_NIDIF),
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

Modnames <- paste("mod", 1:length(poly.test.PREC), sep = " ")
aictab(cand.set = poly.test.PREC, modnames = Modnames, sort = TRUE) # the best model SEEMS to display a quadratic effect

# Set of models

clim.glm <- list()

clim.glm[[1]] <- glm(NIDIF ~ 1,
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

clim.glm[[2]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR ,
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

clim.glm[[3]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + PREC_NIDIF + poly(scale(TEMP_NIDIF), 2),
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

clim.glm[[4]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + PREC_NIDIF,
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

clim.glm[[5]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL + poly(TEMP_NIDIF, 2),
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

clim.glm[[6]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL*poly(TEMP_NIDIF, 2),
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

clim.glm[[7]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL*PREC_NIDIF,
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

clim.glm[[8]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR + SUPPL*PREC_NIDIF + SUPPL*poly(TEMP_NIDIF, 2),
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

clim.glm[[9]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL + SUPPL*PREC_NIDIF + SUPPL*poly(TEMP_NIDIF, 2),
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

clim.glm[[10]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL + PREC_NIDIF + poly(TEMP_NIDIF, 2),
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

clim.glm[[11]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL + PREC_NIDIF*SUPPL + poly(TEMP_NIDIF, 2),
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

clim.glm[[12]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL + PREC_NIDIF + poly(TEMP_NIDIF, 2)*SUPPL,
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

clim.glm[[13]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL + SUPPL*PREC_NIDIF + SUPPL*poly(TEMP_NIDIF, 2),
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

clim.glm[[14]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR + PREC_NIDIF + poly(TEMP_NIDIF, 2),
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

clim.glm[[15]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL + PREC_NIDIF + poly(TEMP_NIDIF, 2),
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

clim.glm[[16]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL + PREC_NIDIF*SUPPL + poly(TEMP_NIDIF, 2),
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

clim.glm[[17]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + YEAR*SUPPL + PREC_NIDIF + poly(TEMP_NIDIF, 2)*SUPPL,
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

# AIC table
Modnames <- paste("mod", 1:length(clim.glm), sep = " ")
AIC <- aictab(cand.set = clim.glm, modnames = Modnames, sort = TRUE)
AIC

# Best models (26 avril 2019)
  # With linear temperature effects, there were models 12 & 9
  # With quadratic temperature effects, there were models 9 and 12

data$YEAR <- relevel(data$YEAR, "2017")

clim.glm[[9]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL + SUPPL*PREC_NIDIF + SUPPL*poly(TEMP_NIDIF, 2),
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)
summary(clim.glm[[9]])
Anova(clim.glm[[9]])

clim.glm[[12]] <- glm(NIDIF ~ NestAge + HAB2 + YEAR*SUPPL + PREC_NIDIF + poly(TEMP_NIDIF, 2)*SUPPL,
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)
summary(clim.glm[[12]])
Anova(clim.glm[[12]])

# ---------------------- # 
#### Simulation tests ####
# ---------------------- #

sims <- simulateResiduals(clim.glm[[9]])
x11()
plot(sims)

sims.2 <- simulateResiduals(clim.glm[[12]])
x11()
plot(sims.2)

# -------------------------------- #
#### Computation of predictions ####
# -------------------------------- #


# Predictions
  # New dataframe for predictions with Reference level of year == 2015 and habitat == MES
data$YEAR <- relevel(data$YEAR, "2015")

# pred <- data.frame(NestAge = mean(data$NestAge),
#                    HAB2 = factor(rep(c("MES", "WET"), 27), levels = c("MES", "WET")),
#                    YEAR = factor(c(rep("2015", 18), rep("2016", 18), rep("2017", 18)), levels = c("2015", "2016", "2017")),
#                    SUPPL = factor(rep(c(rep("TEM", 2), rep("F", 2), rep("W", 2)), 9), levels = c("TEM", "F", "W")),
#                    TEMP_NIDIF = rep(c(rep(min(data$TEMP_NIDIF), 6), rep(mean(data$TEMP_NIDIF), 6), rep(max(data$TEMP_NIDIF), 6)), 3),
#                    #TEMP_NIDIF = mean(data$TEMP_NIDIF),
#                    PREC_NIDIF = mean(data$PREC_NIDIF))

# pred.1 <- data.frame(NestAge = mean(data$NestAge),
#                    HAB2 = "MES",
#                    YEAR = "2015",
#                    SUPPL = "F",
#                    TEMP_NIDIF = mean(data$TEMP_NIDIF),
#                    PREC_NIDIF = mean(data$PREC_NIDIF))

pred <- data.frame(NestAge = mean(data$NestAge),
                   HAB2 = factor(rep(c("MES", "WET"), 9), levels = c("MES", "WET")),
                   YEAR = factor(c(rep("2015", 6), rep("2016", 6), rep("2017", 6)), levels = c("2015", "2016", "2017")),
                   SUPPL = factor(rep(c(rep("TEM", 2), rep("F", 2), rep("W", 2)), 3), levels = c("TEM", "F", "W")),
                   PREC_NIDIF = mean(data$PREC_NIDIF),
                   TEMP_NIDIF = mean(data$TEMP_NIDIF))

# expand.grid()

pp <- predict(clim.glm[[1]], newdata = pred, se.fit = TRUE)
pp

pred <- cbind(pred, DSR = plogis(pp[[1]]), NS = plogis(pp[[1]])^27)
pred

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

x11()

bplot <- barplot(data.predict$fit,
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

# --------------------------------------------------------------- #
#### For hypothesis about climate effects WITHOUT YEAR - GLM  ####
# ------------------------------------------------------------- #
#dt <- data

#data <- dt[dt$YEAR == "2015",]
#data <- data[!(data$ID.2 == "2016.GM13F" | data$ID.2 == "2016.GM3F"),]
# Test without YEAR in models because predictions of this kind of model are not good

test.glm.add <- list()

test.glm.add[[1]] <- glm(NIDIF ~ 1,
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

test.glm.add[[2]] <- glm(NIDIF ~ NestAge + HAB2 + SUPPL ,
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

test.glm.add[[3]] <- glm(NIDIF ~ NestAge + HAB2 + SUPPL + poly(PREC_NIDIF) + poly(TEMP_NIDIF, 2),
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

test.glm.add[[4]] <- glm(NIDIF ~ NestAge + HAB2 + SUPPL + poly(PREC_NIDIF, 2),
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

test.glm.add[[5]] <- glm(NIDIF ~ NestAge + HAB2 + SUPPL + poly(TEMP_NIDIF, 2),
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

test.glm.add[[6]] <- glm(NIDIF ~ NestAge + HAB2 + poly(TEMP_NIDIF, 2) + poly(PREC_NIDIF, 2),
                         family = binomial(link = logexp(data$EXPO)),
                         data = data)
# ---------------------------------------------------------------------- #
test.glm.add[[6]] <- glm(NIDIF ~ NestAge + HAB2 + SUPPL*poly(PREC_NIDIF) + poly(TEMP_NIDIF, 2),
                         family = binomial(link = logexp(data$EXPO)),
                         data = data)

test.glm.add[[7]] <- glm(NIDIF ~ NestAge + SUPPL*HAB2 + SUPPL*poly(PREC_NIDIF) + poly(TEMP_NIDIF, 2),
                         family = binomial(link = logexp(data$EXPO)),
                         data = data)
test.glm.add[[8]] <- glm(NIDIF ~ NestAge + HAB2 + SUPPL*poly(PREC_NIDIF,2) + SUPPL*poly(TEMP_NIDIF, 2),
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

# AIC table
Modnames <- paste("mod", 1:length(test.glm.add), sep = " ")
AIC.test <- aictab(cand.set = test.glm.add, modnames = Modnames, sort = TRUE)
AIC.test

# --------------- #

# data$PREC_NIDIF <- as.vector(scale(data$PREC_NIDIF))
# data$TEMP_NIDIF <- as.vector(scale(data$TEMP_NIDIF))

test.glm <- list()

test.glm[[1]] <- glm(NIDIF ~ 1,
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

test.glm[[2]] <- glm(NIDIF ~ NestAge + HAB2 + SUPPL,
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

test.glm[[3]] <- glm(NIDIF ~ NestAge + HAB2 + SUPPL*poly(TEMP_NIDIF, 2),
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

test.glm[[4]] <- glm(NIDIF ~ NestAge + HAB2 + SUPPL*poly(PREC_NIDIF,2),
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

test.glm[[5]] <- glm(NIDIF ~ NestAge + HAB2 + SUPPL*poly(PREC_NIDIF,2) + SUPPL*poly(TEMP_NIDIF, 2),
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

test.glm[[6]] <- glm(NIDIF ~ NestAge + HAB2 + poly(PREC_NIDIF,2) + poly(TEMP_NIDIF, 2),
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

test.glm[[7]] <- glm(NIDIF ~ NestAge + HAB2 + poly(PREC_NIDIF,2)*SUPPL + poly(TEMP_NIDIF, 2),
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

test.glm[[8]] <- glm(NIDIF ~ NestAge + HAB2 + poly(PREC_NIDIF,2) + poly(TEMP_NIDIF, 2)*SUPPL,
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

test.glm[[9]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + SUPPL*poly(PREC_NIDIF) + SUPPL*poly(TEMP_NIDIF, 2),
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

test.glm[[10]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + poly(PREC_NIDIF,2) + poly(TEMP_NIDIF, 2),
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

test.glm[[11]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + poly(PREC_NIDIF,2)*SUPPL + poly(TEMP_NIDIF, 2),
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

test.glm[[12]] <- glm(NIDIF ~ NestAge + HAB2*SUPPL + poly(PREC_NIDIF,2) + poly(TEMP_NIDIF, 2)*SUPPL,
                      family = binomial(link = logexp(data$EXPO)),
                      data = data)

# ------------------------------------------------------------------------ #

test.glm[[13]] <- glm(NIDIF ~ NestAge + HAB2 + SUPPL + poly(PREC_NIDIF) + poly(TEMP_NIDIF, 2),
                         family = binomial(link = logexp(data$EXPO)),
                         data = data)

test.glm[[14]] <- glm(NIDIF ~ NestAge + HAB2 + SUPPL + poly(PREC_NIDIF, 2),
                         family = binomial(link = logexp(data$EXPO)),
                         data = data)

test.glm[[15]] <- glm(NIDIF ~ NestAge + HAB2 + SUPPL + poly(TEMP_NIDIF, 2),
                         family = binomial(link = logexp(data$EXPO)),
                         data = data)

test.glm[[16]] <- glm(NIDIF ~ NestAge + HAB2 + SUPPL*poly(PREC_NIDIF) + poly(TEMP_NIDIF, 2),
                         family = binomial(link = logexp(data$EXPO)),
                         data = data)

test.glm[[17]] <- glm(NIDIF ~ NestAge + SUPPL*HAB2 + SUPPL*poly(PREC_NIDIF) + poly(TEMP_NIDIF, 2),
                         family = binomial(link = logexp(data$EXPO)),
                         data = data)

# AIC table
Modnames <- paste("mod", 1:length(test.glm), sep = " ")
AIC.test.2 <- aictab(cand.set = test.glm, modnames = Modnames, sort = TRUE)
AIC.test.2

summary(test.glm[[5]])
Anova(test.glm[[5]])
plot(test.glm[[5]])

test.glm[[5]] <- glm(NIDIF ~ NestAge + HAB2 + SUPPL * poly(PREC_NIDIF, 
                                                             2) + SUPPL *poly(TEMP_NIDIF, 2),
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

# Predictions

pred.TEM <- data.frame(NestAge = mean(data$NestAge),
                       HAB2 = "MES",
                       SUPPL = "TEM",
                       PREC_NIDIF = mean(data$PREC_NIDIF),
                       TEMP_NIDIF = seq(1.8, 8.5, 0.01))

pp.TEM <- predict(test.glm[[5]], newdata = pred.TEM, se.fit = TRUE)

pred.W <- data.frame(NestAge = mean(data$NestAge),
                       HAB2 = "MES",
                       SUPPL = "W",
                       PREC_NIDIF = mean(data$PREC_NIDIF),
                       TEMP_NIDIF = seq(1.8, 8.5, 0.01))

pp.W <- predict(test.glm[[5]], newdata = pred.W, se.fit = TRUE)

pred.F <- data.frame(NestAge = mean(data$NestAge),
                       HAB2 = "MES",
                       SUPPL = "F",
                       PREC_NIDIF = mean(data$PREC_NIDIF),
                       TEMP_NIDIF = seq(1.8, 8.5, 0.01))

pp.F <- predict(test.glm[[5]], newdata = pred.F, se.fit = TRUE)

#par(mfrow = c(2, 2)) 
x11()
plot(pred.TEM$TEMP_NIDIF, plogis(pp.TEM[[1]]), type = "l", ylim = c(0, 1))
points(data$TEMP_NIDIF, data$NIDIF)
lines(pred.TEM$TEMP_NIDIF, plogis(pp.TEM[[1]] + 2*pp.TEM[[2]]), ylim = c(0, 1), col = "red")
lines(pred.TEM$TEMP_NIDIF, plogis(pp.TEM[[1]] - 2*pp.TEM[[2]]), ylim = c(0, 1), col = "red")

lines(pred.W$TEMP_NIDIF, plogis(pp.W[[1]]), col = "blue")
lines(pred.F$TEMP_NIDIF, plogis(pp.F[[1]]), col = "orange")


# TEST WITH LINEAR EFFECT OF TEMP AND PREC

test.glm[[5]] <- glm(NIDIF ~ NestAge + HAB2 + SUPPL*PREC_NIDIF + SUPPL*TEMP_NIDIF,
                     family = binomial(link = logexp(data$EXPO)),
                     data = data)

# Predictions

pred.TEM <- data.frame(NestAge = mean(data$NestAge),
                       HAB2 = "MES",
                       SUPPL = "TEM",
                       PREC_NIDIF = mean(data$PREC_NIDIF),
                       TEMP_NIDIF = seq(1.8, 8.5, 0.01))

pp.TEM <- predict(test.glm[[5]], newdata = pred.TEM, se.fit = TRUE)

pred.W <- data.frame(NestAge = mean(data$NestAge),
                     HAB2 = "MES",
                     SUPPL = "W",
                     PREC_NIDIF = mean(data$PREC_NIDIF),
                     TEMP_NIDIF = seq(1.8, 8.5, 0.01))

pp.W <- predict(test.glm[[5]], newdata = pred.W, se.fit = TRUE)

pred.F <- data.frame(NestAge = mean(data$NestAge),
                     HAB2 = "MES",
                     SUPPL = "F",
                     PREC_NIDIF = mean(data$PREC_NIDIF),
                     TEMP_NIDIF = seq(1.8, 8.5, 0.01))

pp.F <- predict(test.glm[[5]], newdata = pred.F, se.fit = TRUE)

#par(mfrow = c(2, 2)) 
x11()
plot(pred.TEM$TEMP_NIDIF, plogis(pp.TEM[[1]]), type = "l", ylim = c(0, 1))
#points(data$TEMP_NIDIF, data$NIDIF)
lines(pred.TEM$TEMP_NIDIF, plogis(pp.TEM[[1]] + 2*pp.TEM[[2]]), ylim = c(0, 1), col = "grey", lty = 3)
lines(pred.TEM$TEMP_NIDIF, plogis(pp.TEM[[1]] - 2*pp.TEM[[2]]), ylim = c(0, 1), col = "grey", lty = 3)

lines(pred.W$TEMP_NIDIF, plogis(pp.W[[1]]), col = "blue", lty = 1)
lines(pred.TEM$TEMP_NIDIF, plogis(pp.W[[1]] + 2*pp.W[[2]]), ylim = c(0, 1), col = "darkblue", lty = 3)
lines(pred.TEM$TEMP_NIDIF, plogis(pp.W[[1]] - 2*pp.W[[2]]), ylim = c(0, 1), col = "darkblue", lty = 3)

x11()
plot(pred.TEM$TEMP_NIDIF, plogis(pp.TEM[[1]]), type = "l", ylim = c(0, 1))
#points(data$TEMP_NIDIF, data$NIDIF)
lines(pred.TEM$TEMP_NIDIF, plogis(pp.TEM[[1]] + 2*pp.TEM[[2]]), ylim = c(0, 1), col = "grey", lty = 3)
lines(pred.TEM$TEMP_NIDIF, plogis(pp.TEM[[1]] - 2*pp.TEM[[2]]), ylim = c(0, 1), col = "grey", lty = 3)

lines(pred.F$TEMP_NIDIF, plogis(pp.F[[1]]), col = "orange", lty = 1)
lines(pred.TEM$TEMP_NIDIF, plogis(pp.F[[1]] + 2*pp.F[[2]]), ylim = c(0, 1), col = "dark red", lty = 3)
lines(pred.TEM$TEMP_NIDIF, plogis(pp.F[[1]] - 2*pp.F[[2]]), ylim = c(0, 1), col = "darkred", lty = 3)

#### Trash ####
# Exploration of temperature and precipitation variables

boxplot(data$PREC_NIDIF~data$YEAR)
boxplot(data$TEMP_NIDIF~data$YEAR)

plot(data$INITIATION, data$TEMP_NIDIF)


### Plot for model NIDIF ~ NestAge + HAB + PREC^2 + TEMP^2 

pred.TEMP <- data.frame(NestAge = mean(data$NestAge),
                       HAB2 = "MES",
                       PREC_NIDIF = mean(data$PREC_NIDIF),
                       TEMP_NIDIF = seq(0.5, 8.5, 0.01))
pp <- predict(test.glm.add[[6]], newdata = pred.TEMP, se.fit = TRUE)
plot(pred.TEMP$TEMP_NIDIF, plogis(pp[[1]]), type = "l", ylim = c(0, 1), bty = "n")
#points(data$TEMP_NIDIF, data$NIDIF)
lines(pred.TEMP$TEMP_NIDIF, plogis(pp[[1]] + 2*pp[[2]]), ylim = c(0, 1), col = "grey", lty = 3)
lines(pred.TEMP$TEMP_NIDIF, plogis(pp[[1]] - 2*pp[[2]]), ylim = c(0, 1), col = "grey", lty = 3)

pred.TEMP <- data.frame(NestAge = mean(data$NestAge),
                        HAB2 = "WET",
                        PREC_NIDIF = mean(data$PREC_NIDIF),
                        TEMP_NIDIF = seq(0.5, 8.5, 0.01))
pp <- predict(test.glm.add[[6]], newdata = pred.TEMP, se.fit = TRUE)
lines(pred.TEMP$TEMP_NIDIF, plogis(pp[[1]]), type = "l", ylim = c(0, 1), bty = "n", col = "blue")
#points(data$TEMP_NIDIF, data$NIDIF)
lines(pred.TEMP$TEMP_NIDIF, plogis(pp[[1]] + 2*pp[[2]]), ylim = c(0, 1), col = "deepskyblue", lty = 3)
lines(pred.TEMP$TEMP_NIDIF, plogis(pp[[1]] - 2*pp[[2]]), ylim = c(0, 1), col = "deepskyblue", lty = 3)


x11()
pred.TEMP <- data.frame(NestAge = mean(data$NestAge),
                        HAB2 = "MES",
                        TEMP_NIDIF = mean(data$PREC_NIDIF),
                        PREC_NIDIF = seq(0, 7.5, 0.01))
pp <- predict(test.glm.add[[6]], newdata = pred.TEMP, se.fit = TRUE)
plot(pred.TEMP$PREC_NIDIF, plogis(pp[[1]]), type = "l", ylim = c(0, 1), bty = "n")
#points(data$TEMP_NIDIF, data$NIDIF)
lines(pred.TEMP$PREC_NIDIF, plogis(pp[[1]] + 2*pp[[2]]), ylim = c(0, 1), col = "grey", lty = 3)
lines(pred.TEMP$PREC_NIDIF, plogis(pp[[1]] - 2*pp[[2]]), ylim = c(0, 1), col = "grey", lty = 3)

pred.TEMP <- data.frame(NestAge = mean(data$NestAge),
                        HAB2 = "WET",
                        TEMP_NIDIF = mean(data$PREC_NIDIF),
                        PREC_NIDIF = seq(0, 7.5, 0.01))
pp <- predict(test.glm.add[[6]], newdata = pred.TEMP, se.fit = TRUE)
lines(pred.TEMP$PREC_NIDIF, plogis(pp[[1]]), type = "l", ylim = c(0, 1), bty = "n", col = "blue")
#points(data$TEMP_NIDIF, data$NIDIF)
lines(pred.TEMP$PREC_NIDIF, plogis(pp[[1]] + 2*pp[[2]]), ylim = c(0, 1), col = "deepskyblue", lty = 3)
lines(pred.TEMP$PREC_NIDIF, plogis(pp[[1]] - 2*pp[[2]]), ylim = c(0, 1), col = "deepskyblue", lty = 3)
