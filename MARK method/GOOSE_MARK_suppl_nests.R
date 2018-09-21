#---------------- GOOSE_MARK_supplemented_nests ----------------#
# SCript to analyse the DSR of supplemented nests
# 3 types of models
#     water vs. control (2005, 2015, 2016, 2017)
#     food vs. control (2015, 2016, 2017)
#     food + water vs. control (2015)

setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm(list = ls())
sup <- read.table("GOOSE_geese.txt", sep = ",", h = T)
head(sup)
summary(sup)

sup <- sup[,-1]

#### Water models ####
# Concerning years : 2005, 2015, 2016 & 2017

#### First WATER models - without Nicolas data (2005) ####
supW1 <- sup[which(sup$YEAR == 2015 | sup$YEAR == 2016 | sup$YEAR == 2017),]
supW1 <- supW1[which(supW1$SUPPL == "W" | supW1$SUPPL == "TEM"),]
supW1 <- droplevels(supW1)
summary(supW1)

supW1$YEAR <- as.factor(supW1$YEAR)
supW1$Fate <- as.factor(supW1$Fate)

require(RMark)

nocc <- max(supW1$LastChecked)

run.WATER.1 = function()
{
  # Null model
  wat1 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ 1)), delete = T)
  
  # YEAR
  wat2 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR)), groups = "YEAR", delete = T)
  
  #SUPPL
  wat3 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL)), groups = "SUPPL", delete = T)
  
  # HAB
  wat4 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB)), groups = "HAB", delete = T)
  
  # INITIATION
  wat5 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ INITIATION)), delete = T)
  
  # NestAge
  wat6 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge)), delete = T)
  
  # Complete model without interaction
  wat7 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + YEAR + NestAge + INITIATION)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)
  
  # **WARNING** message ! Seems to have too much factors for model wat7
  
  # SUPPL + HAB + YEAR + NestAge
  wat8 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + YEAR + NestAge)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)
  
  # SUPPL*HAB + YEAR + NestAge
  wat9 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + YEAR + NestAge)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)
  
  # SUPPL*HAB + YEAR
  wat10 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + YEAR)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)
  
  
  return(collect.models() )
}

# run defined models
WATER.1.results <- run.WATER.1()
WATER.1.results


#Save models list and est model
#save(WATER.1.results, file = "waterGEESE_V1.rda")
#save(wat8, file = "waterGEESE_V1_1.rda")
#save(wat9, file = "waterGEESE_V1_2.rda")


#### Food models ####
# Concerning years : 2015, 2016 & 2017
supF <- sup[which(sup$YEAR == 2015 | sup$YEAR == 2016 | sup$YEAR == 2017),]
supF <- supF[which(supF$SUPPL == "F" | supF$SUPPL == "TEM"),]
supF <- droplevels(supF)
summary(supF)

supF$YEAR <- as.factor(supF$YEAR)
supF$Fate <- as.factor(supF$Fate)
supF$SUPPL <- relevel(supF$SUPPL, "TEM")

require(RMark)

nocc <- max(supF$LastChecked)

run.FOOD = function()
{
  # Null model
  foo1 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ 1)), delete = T)
  
  # YEAR
  foo2 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR)), groups = "YEAR", delete = T)
  
  # SUPPL
  foo3 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL)), groups = "SUPPL", delete = T)
  
  # HAB
  foo4 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB)), groups = "HAB", delete = T)
  
  # INITIATION
  foo5 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ INITIATION)), delete = T)
  
  # NestAge
  foo6 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge)), delete = T)
  
  # SUPPL + HAB + YEAR + NestAge + INITIATION
  foo7 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + YEAR + NestAge + INITIATION)), groups = c("SUPPL", "HAB", "YEAR"), delete = T)
  # **WARNING** MESSAGE before the ouput of the model
  
  # SUPPL + HAB + YEAR + NestAge
  foo8 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + YEAR + NestAge)), groups = c("SUPPL", "HAB", "YEAR"), delete = T)
  
  # SUPPL*HAB + YEAR + NestAge
  foo9 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + YEAR + NestAge)), groups = c("SUPPL", "HAB", "YEAR"), delete = T)
  
  # SUPPL*HAB + YEAR
  foo10 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + YEAR)), groups = c("SUPPL", "HAB", "YEAR"), delete = T)
  
  # SUPPL*HAB + SUPPL*YEAR
  foo11 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + SUPPL*YEAR)), groups = c("SUPPL", "HAB", "YEAR"), delete = T)
  
  # HAB + SUPPL*YEAR
  foo12 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + SUPPL*YEAR)), groups = c("SUPPL", "HAB", "YEAR"), delete = T)
  
  # SUPPL*YEAR
  foo13 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*YEAR)), groups = c("SUPPL", "YEAR"), delete = T)
  
  
  return(collect.models() )
}

# run defined models
FOOD.results <- run.FOOD()
FOOD.results


#Save models list and est model
save(FOOD.results, file = "foodGEESE.rda")
save(foo8, file = "foodGEESE_1.rda")
save(foo7, file = "foodGEESE_2.rda")
save(foo9, file = "foodGEESE_3.rda")
save(foo12, file = "foodGEESE_4.rda")
save(foo11, file = "foodGEESE_5.rda")
