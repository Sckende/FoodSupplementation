getwd()
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm(list = ls())
list.files()

h <- read.table("GOOSE-Supl_Nests_2015-2016-2017_LAST_version.txt", h = T, dec = ".", sep = "\t") # all years, with and without supplemented nests
str(h)
dim(h)
summary(h) 

#################### Data subset ####################
# Keep only true dates of initiation and hatching
hh <- subset(h, h$INI_STATUS == "TRUE")
hh <- subset(hh, hh$HATCH_STATUS == "TRUE" | hh$HATCH_STATUS == "MARIE_EST/TRUE")

# Keep only the successfull nests
hh <- subset(hh, hh$NIDIF == "S")
hh <- droplevels(hh)
summary(hh)

hh$INITIATION <- as.numeric(as.character(hh$INITIATION))
hh$HATCH <- as.numeric(as.character(hh$HATCH))


# Add rainfall year type (LOW/INTERMEDIATE/HIGH)
cum <- read.table("PREC_cum2.txt", dec = ".", h = T, sep = " ")
summary(cum)
hh$RAINFALL <- cum$RAINFALL[match(hh$YEAR, cum$YEAR)]

summary(hh)

# Add PER CAPITA the cumulative rainfall, cumulative rainfall per day and mean temperature from initiation date to hatching date 

rain <- read.table("PREC_precipitation_Bylot_1995-2017.txt", h = T, dec = ","); rain <- na.omit(rain)
temp <- read.table("TEMP_Tair moy 1989-2017 BYLCAMP.txt", h = T, dec = ",")

# Date conversion for temp data 
temp$JJ <- strptime(paste(temp$DAY, temp$MONTH, temp$YEAR, sep = "-"), format = "%d-%m-%Y")
temp$JJ <- temp$JJ$yday + 1 

# Creation of INCUB, cumPREC, PREC_rate and mean_TEMP variables
hh$X <- 1:dim(hh)[1]
TAB <- NULL
for (i in hh$X) {
  
  X <- i
  INCUB <- hh$HATCH[i] - hh$INITIATION[i]
  cumPREC <- sum(rain$RAIN[rain$YEAR == hh$YEAR[i] & rain$JJ <= hh$HATCH[i] & rain$JJ >= hh$INITIATION[i]])
  PREC_rate <- sum(rain$RAIN[rain$YEAR == hh$YEAR[i] & rain$JJ <= hh$HATCH[i] & rain$JJ >= hh$INITIATION[i]]) / INCUB
  mean_TEMP <- mean(temp$TEMP[temp$YEAR == hh$YEAR[i] & temp$JJ <= hh$HATCH[i] & temp$JJ >= hh$INITIATION[i]])
  sd_TEMP <- sd(temp$TEMP[temp$YEAR == hh$YEAR[i] & temp$JJ <= hh$HATCH[i] & temp$JJ >= hh$INITIATION[i]])
  
  M <- c(X, INCUB, cumPREC, PREC_rate, mean_TEMP, sd_TEMP)
  TAB <- as.data.frame(rbind(TAB, M))
  }

  names(TAB) <- c("X", "INCUB", "cumPREC", "PREC_rate", "mean_TEMP", "sd_TEMP")
  View(TAB)
  hh <- merge(hh, TAB, by ="X")

# Creation of fake supplementation date for TEMOIN
  hh$SUPPL_DATE <- as.numeric(as.character(hh$SUPPL_DATE))
  hh$SUPPL_DATE[hh$SUPPL == "TEM"] <- "999"
  
#### Supplementation effect on the hatching date ####
#### MODELS - STEPWISE METHOD ####
  # Distribution of the response variable
plot(hh$INCUB)
hist(hh$INCUB)

 # Models tests
require(lme4)
require(car)

hh$SUPPL <- relevel(hh$SUPPL, ref="TEM") # setting of the reference level

#### Full model ####
# Not enougth levels to consider a random effect with year (necessity of minimum 5 levels for strong results)

l.full <- lm(INCUB ~ SUPPL + HAB + cumPREC + SUPPL_DATE + CLUTCH + mean_TEMP + cumPREC*mean_TEMP, data = hh); l.full
summary(l.full)

# To check the global effect of factors
Anova(l.full, type = "III") # library car

#### Model 0 ####
# Delete interaction cause non significant
l0 <- lm(INCUB ~ SUPPL + HAB + cumPREC + SUPPL_DATE + CLUTCH + mean_TEMP, data = hh); l0
summary(l0)

# To check the global effect of factors
Anova(l0, type = "III") # library car

# Model comparison
anova(l0, l.full) #always in first the most simple model, here l0
# Non significant anova, so, models are similar and cumPREC*mean_TEMP can be deleted
# Work again as previously with the new best model - l0 - and dele SUPPL_DATE (cf l1)

#### Model 1 ####
# Delete SUPPL_DATE cause non significant
l1 <- lm(INCUB ~ SUPPL + HAB + cumPREC + CLUTCH + mean_TEMP, data = hh); l1
summary(l1)
Anova(l1, type = "III")
# Model comparison
anova(l1, l0) #always in first the most simple model, here l1
# Non significant anova, so, models are similar and SUPPL_DATE can be deleted
# Work again as previously with the new best model - l1 - and dele mean_TEMP (cf l2)

#### Model 2 ####
l2 <- lm(INCUB ~ SUPPL + HAB + cumPREC + CLUTCH, data = hh); l2
summary(l2)


Anova(l2, type = "III")

# Model comparison
anova(l2, l1)
# Non significant anova, so, models are similar and mean_TEMP can be deleted
# Work again as previously with the new best model - l1 - and dele HAB (cf l3)

#### Model 3 ####
l3 <- lm(INCUB ~ SUPPL + cumPREC + CLUTCH, data = hh); l3
summary(l3)


Anova(l3, type = "III")

# Model comparison
anova(l3, l2)
# Non significant anova, so, models are similar and HAB can be deleted
# All variables are significant. L3 is the best model

#### Assumptions tests ####

# Pauline script
require(effects)
plot(allEffects(l3)) #from library effects, gives you a (not pretty) plot of the effects of the variables in your model

# Vérification des suppositions du modèle ####
#homogénéité = graphique des valeurs prédites vs valeurs résiduelles
plot(x = fitted(l3), 
     y = resid(l3), 
     xlab = "Fitted Values",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)

#indépendance (plotter chaque variable vs les résidus)
plot(x = hh$cumPREC, 
     y = resid(l3), 
     xlab = "cumPREC",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)

plot(x = hh$CLUTCH, 
     y = resid(l3), 
     xlab = "Clutch size",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)

boxplot(resid(l3) ~ hh$SUPPL,   
        ylab = "Normalized residuals",
        data = hh, xlab = "Supplementation")
abline(h = 0, lty = 2)


#normalité
hist(resid(l3))
qqnorm(resid(l3))
qqline(resid(l3))

#leverage (check si y a des valeurs qui tirent les résidus)
lev <- hat(model.matrix(l3)) # si > 0.2, se questionner 
plot(lev)
hh[lev >0.2,]

#si valeurs qui tirent, les identifier avec :
#hh[lev >0.2,] #returns all observations (line number) with leverage values above 0.2

#### Correlation tests ####
# Function
# Début Tableau de correlation avec R2 et p-value des correlation #
panel.cor.pearson <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <-  as.numeric(cor.test(x, y,method="pearson")["estimate"])
  p <-  as.numeric(cor.test(x, y,method="pearson")["p.value"])
  txt <- format(c(r, 0.123456789), digits=2)[1]
  txt2<- format(c(p, 0.123456789), digits=2)[1]
  txt <- paste(prefix, txt, sep="")
  txt2 <- paste(prefix, "(", txt2, ")", sep="")
  cex <- 0.5
  text(0.5, 0.7, txt, cex = 2)
  if (p < 0.001) text(0.5, 0.3, "***", cex = 2, col="red")
  if (p > 0.001 & p < 0.01) text(0.5, 0.3, "**", cex = 2, col="red")
  if (p > 0.01 & p < 0.05) text(0.5, 0.3, "*", cex = 2, col="red")
  if (p > 0.05) text(0.5, 0.3, "n.s.", cex = 2)
}

panel.cor.spearman <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <-  as.numeric(cor.test(x, y,method="spearman")["estimate"])
  p <-  as.numeric(cor.test(x, y,method="spearman")["p.value"])
  txt <- format(c(r, 0.123456789), digits=2)[1]
  txt2<- format(c(p, 0.123456789), digits=2)[1]
  txt <- paste(prefix, txt, sep="")
  txt2 <- paste(prefix, "(", txt2, ")", sep="")
  cex <- 0.5
  text(0.5, 0.7, txt, cex = 1)
  if (p < 0.001) text(0.5, 0.3, "***", cex = 1, col="red")
  if (p > 0.001 & p < 0.01) text(0.5, 0.3, "**", cex = 1, col="red")
  if (p > 0.01 & p < 0.05) text(0.5, 0.3, "*", cex = 1, col="red")
  if (p > 0.05) text(0.5, 0.3, "n.s.", cex = 1)
}

panel.scatter <- function(x,y, grpfactor=NULL)
{
  pc = c(15,1, 16)
  a = min(x)
  b = max(y)
  c = min(x)
  d = max(y)
  xlim=c(a-100,b+100)
  ylim=c(c-100,d+100)
  points(x,y, cex = 1, pch= pc[unclass(grpfactor)])
  
}
# Fin Tableau de correlation avec R2 et p-value des correlation #

#la fonction pairs crée le tableau de correlation
# data = le jeu de données 
# upper.panel = au choix : 
# upper.panel = panel.cor.pearson (pour des correlations de Pearson) 
# upper.panel = panel.cor.spearman (pour des correlations de Spearman, par rang)

# Dataset with only variables in the best model
v <- hh[,c(5, 12, 14, 28)]

pairs(formula= ~ ., data=v, cex.labels = 1, gap = 0.3,
      lower.panel=panel.scatter,
      upper.panel=panel.cor.pearson,
      main="Corrélations bivariées Pearson")
