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

#### Supplementation effect on the hatching date ####

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
temp$JJ <- temp$JJ$yday + 1 #c

hh$ID <- 1:53
TAB <- NULL
for (i in hh$ID) {
  
  ID <- i
  INCUB <- hh$HATCH[i] - hh$INITIATION[i]
  cumPREC <- sum(rain$RAIN[rain$YEAR == hh$YEAR[i] & rain$JJ <= hh$HATCH[i] & rain$JJ >= hh$INITIATION[i]])
  PREC_rate <- sum(rain$RAIN[rain$YEAR == hh$YEAR[i] & rain$JJ <= hh$HATCH[i] & rain$JJ >= hh$INITIATION[i]]) / INCUB
  mean_TEMP <- mean(temp$TEMP[temp$YEAR == hh$YEAR[i] & temp$JJ <= hh$HATCH[i] & temp$JJ >= hh$INITIATION[i]])
  sd_TEMP <- sd(temp$TEMP[temp$YEAR == hh$YEAR[i] & temp$JJ <= hh$HATCH[i] & temp$JJ >= hh$INITIATION[i]])
  
  M <- c(ID, INCUB, cumPREC, PREC_rate, mean_TEMP, sd_TEMP)
  TAB <- as.data.frame(rbind(TAB, M))
  }
  View(TAB)
  names(TAB) <- c("ID", "INCUB", "cumPREC", "PREC_rate", "mean_TEMP", "sd_TEMP")
  
  hh <- merge(hh, TAB, by ="ID")

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

l0 <- lm(INCUB ~ SUPPL + HAB + cumPREC + CLUTCH + mean_TEMP, data = hh); l0
summary(l0)

# To check the global effect of factors
Anova(l0, type = "III") # library car

#### Model 1 ####
# Delete mean_TEMP cause non significant
l1 <- lm(INCUB ~ SUPPL + HAB + cumPREC + CLUTCH, data = hh); l1
summary(l1)
Anova(l1, type = "III")
# Model comparison
anova(l1, l0) #always in first the most simple model, here l1
# Non significant anova, so, models are similar and mean_TEMP can be deleted
# Work again as previously with the new best model - l1 - and dele HAB (cf l2)

#### Model 2 *** ####
l2 <- lm(INCUB ~ SUPPL + cumPREC + CLUTCH, data = hh); l2
summary(l2)


Anova(l2, type = "III")

# Model comparison
anova(l2, l1)
# Non significant ANOVA, HAB is deleted and all variables are significant. This model is the best one.


#### Assumptions tests ####

# Pauline script
require(effects)
plot(allEffects(l2)) #from library effects, gives you a (not pretty) plot of the effects of the variables in your model

# Vérification des suppositions du modèle ####
#homogénéité = graphique des valeurs prédites vs valeurs résiduelles
plot(x = fitted(l2), 
     y = resid(l2), 
     xlab = "Fitted Values",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)

#indépendance (plotter chaque variable vs les résidus)
plot(x = hh$cumPREC, 
     y = resid(l2), 
     xlab = "cumPREC",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)

plot(x = hh$CLUTCH, 
     y = resid(l2), 
     xlab = "Clutch size",
     ylab = "Normalized residuals")
abline(h = 0, lty = 2)

boxplot(resid(l2) ~ hh$SUPPL,   
        ylab = "Normalized residuals",
        data = hh, xlab = "Supplementation")
abline(h = 0, lty = 2)


#normalité
hist(resid(l2))
qqnorm(resid(l2))
qqline(resid(l2))

#leverage (check si y a des valeurs qui tirent les résidus)
lev <- hat(model.matrix(l2)) # si > 0.2, se questionner 
plot(lev)

#OU (autre méthode) :
ggplot(data.frame(lev=hatvalues(l2),pearson=residuals(l2,type="pearson")),
       aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw() #other way
hh[lev >0.2,]
#si valeurs qui tirent, les identifier avec :
#testmasschg[lev >0.2,] #returns all observations (line number) with leverage values above 0.2