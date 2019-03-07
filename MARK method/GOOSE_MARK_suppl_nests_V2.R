#---------------- GOOSE_MARK_supplemented_nests_VERSION 2 ----------------#
# SCript to analyse the DSR of supplemented nests
# 2 types of models
#     water vs. control (2005, 2015, 2016, 2017)
#     food vs. control (2015, 2016, 2017)

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

nidi <- read.table("GOOSE_breeding_informations.txt", h = T, sep = "\t", dec = ",")
head(nidi)
summary(nidi)

# Exploratoty plots
x11()
par(mfrow = c(3, 1))

plot(nidi$YEAR, nidi$NEST_SUCC, type = "h", bty = "n", yaxt = "n", xlab = "", ylab = "", lwd = 4, col = "darkorange")
axis(4)
par (new = T)
plot(sup$YEAR, sup$TEMP_Y, type = "l", ylab = "TEMP_Y", xlab = "Year", bty = "n", lwd = 4, col = "grey")


plot(nidi$YEAR, nidi$NEST_SUCC, type = "h", bty = "n", yaxt = "n", xlab = "", ylab = "", lwd = 4, col = "darkorange")
axis(4)
par (new = T)
plot(sup$YEAR, sup$cumTEMP_Y, type = "l", ylab = "cumTEMP_Y", bty = "n", lwd = 4, col = "darkgreen")

plot(nidi$YEAR, nidi$NEST_SUCC, type = "h", bty = "n", yaxt = "n", xlab = "", ylab = "", lwd = 4, col = "darkorange")
axis(4)
par (new = T)
plot(sup$YEAR, sup$PREC_Y, type = "l", ylab = "PREC_Y", bty = "n", xlab = "Year", lwd = 4, col = "darkblue")

# Here I build a database only with water supplemented years (2005, 2015, 2016 & 2017)
# Minimum ariables that I have to keep are for MARK analysis: ID / FirstFound / LAstPresent / LastChecked / FAte / AgeFound / AgeDay1
# Variables that I want to add for checking potential effects: YEAR / HAB / SUPPL / PREC_Y / TEMP_Y
sup <- sup[, c(1:4, 7:10, 14, 16, 21, 25:27)]


supW <- rbind(sup, w05)
summary(supW)

utils::View(supW)

# Water supplementation database
supW <- supW[which(supW$YEAR == 2005 |supW$YEAR == 2015 | supW$YEAR == 2016 | supW$YEAR == 2017),]
supW <- supW[which(supW$SUPPL == "W" | supW$SUPPL == "TEM"),]
supW <- droplevels(supW)


supW$YEAR <- as.factor(supW$YEAR)
supW$Fate <- as.factor(supW$Fate)

summary(supW)

# Food supplementation database
supF <- sup[which(sup$YEAR == 2015 | sup$YEAR == 2016 | sup$YEAR == 2017),]
supF <- supF[which(supF$SUPPL == "F" | supF$SUPPL == "TEM"),]
supF <- droplevels(supF)


supF$YEAR <- as.factor(supF$YEAR)
supF$Fate <- as.factor(supF$Fate)
supF$SUPPL <- relevel(supF$SUPPL, "TEM")

summary(supF)

#######################################################################################################
#### FOOD - MARK analysis ####

require(RMark)

nocc <- max(supF$LastChecked)

run.FOOD = function()
{
  # Null model
  foo0 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ 1)), delete = T)
  
  # NestAge
  foo1 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge)), delete = T)
  
  # HAB
  foo2 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB)), groups = "HAB", delete = T)
  
  # YEAR
  foo3 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR)), groups = "YEAR", delete = T)
  
  # SUPPL
  foo4 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL)), groups = "SUPPL", delete = T)

  # PREC_Y
  foo5 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ PREC_Y)), delete = T)
  
  # TEMP_Y
  foo6 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ TEMP_Y)), delete = T)
  
  # cumTEMP_Y
  foo7 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ cumTEMP_Y)), delete = T)
  
  # TEMP_Y + PREC_Y
  foo8 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ TEMP_Y + PREC_Y)), delete = T)
  
  # TEMP_Y * PREC_Y
  foo9 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ TEMP_Y * PREC_Y)), delete = T)
  #*** Estimés dégueu ****
  
  # FULL MODEL
  foo10 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + HAB + SUPPL + NestAge + TEMP_Y + PREC_Y)), groups = c("YEAR", "HAB", "SUPPL"), delete = T)
  #*** Incoherences avec les annees ET PREC_Y/TEMP_Y****
  
  # FULL MODEL - YEAR
  foo11 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + SUPPL + NestAge + TEMP_Y + PREC_Y)), groups = c( "HAB", "SUPPL"), delete = T)
  
  # FULL MODEL - YEAR - TEMP/PREC
  foo11.1 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + SUPPL + NestAge)), groups = c( "HAB", "SUPPL"), delete = T)
  
  # FULL MODEL - TEMP/PREC
  foo11.2 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + SUPPL + NestAge + YEAR)), groups = c("YEAR", "HAB", "SUPPL"), delete = T)
  
  
  return(collect.models() )
}

# run defined models
FOOD.results <- run.FOOD()
FOOD.results


#Save models list and est model
save(FOOD.results, file = "foodGEESE_V2.rda")
save(foo11, file = "foodGEESE_bestMODEL_V2.rda")

#### Run food models per year ####
#### WATER - MARK analysis ####

require(RMark)

nocc <- max(supW$LastChecked)

run.WATER = function()
{
  # Null model
  wat0 <- mark(supW, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ 1)), delete = T)
  
  # NestAge
  wat1 <- mark(supW, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge)), delete = T)
  
  # HAB
  wat2 <- mark(supW, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB)), groups = "HAB", delete = T)
  
  # YEAR
  wat3 <- mark(supW, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR)), groups = "YEAR", delete = T)
  
  # SUPPL
  wat4 <- mark(supW, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL)), groups = "SUPPL", delete = T)
  
  # PREC_Y
  wat5 <- mark(supW, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ PREC_Y)), delete = T)
  
  # TEMP_Y
  wat6 <- mark(supW, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ TEMP_Y)), delete = T)
  
  # cumTEMP_Y
  wat7 <- mark(supW, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ cumTEMP_Y)), delete = T)
  
  # TEMP_Y + PREC_Y
  wat8 <- mark(supW, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ TEMP_Y + PREC_Y)), delete = T)
  
  # TEMP_Y * PREC_Y
  wat8 <- mark(supW, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ TEMP_Y * PREC_Y)), delete = T)
  
  # FULL MODEL
  wat9 <- mark(supW, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR + HAB + SUPPL + NestAge + TEMP_Y + PREC_Y)), groups = c("YEAR", "HAB", "SUPPL"), delete = T)
  #*** Incoherences avec les annees ****
  
  # FULL MODEL - YEAR
  wat10 <- mark(supW, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + SUPPL + NestAge + TEMP_Y + PREC_Y)), groups = c( "HAB", "SUPPL"), delete = T)
  
  return(collect.models() )
}

# run defined models
WATER.results <- run.WATER()
WATER.results


#Save models list and est model
save(WATER.results, file = "waterGEESE_V2.rda")
save(wat10, file = "waterGEESE_bestMODEL_V2.rda")
