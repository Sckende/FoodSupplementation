#---------------- GOOSE_MARK_supplemented_nests ----------------#
# SCript to analyse the DSR of supplemented nests
# 3 types of models
#     water vs. control (2005, 2015, 2016, 2017)
#     food vs. control (2015, 2016, 2017)
#     food + water vs. control (2015)

setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

rm(list = ls())
sup <- read.table("GOOSE_geese_with_WF.txt", sep = ",", h = T)
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
  #wat7 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + YEAR + NestAge + INITIATION)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)
  
  # **WARNING** message ! Seems to have too much factors for model wat7
  
  # SUPPL + HAB + YEAR + NestAge
  wat8 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + YEAR + NestAge)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)
  
  # SUPPL*HAB + YEAR + NestAge
  wat9 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + YEAR + NestAge)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)
  
  # SUPPL*HAB + YEAR
  wat10 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + YEAR)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)
  
  # HAB + YEAR + NestAge
  wat10 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + YEAR + NestAge)), groups = c("HAB", "YEAR" ), delete = T)
  
  # SUPPL*HAB + YEAR
  wat11 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + YEAR)), groups = c("SUPPL", "HAB", "YEAR"), delete = T)  
  
  return(collect.models() )
}

# run defined models
WATER.1.results <- run.WATER.1()
WATER.1.results


#Save models list and est model
save(WATER.1.results, file = "waterGEESE_2015-2017.rda")
save(wat8, file = "waterGEESE_2015-2017_1.rda")
save(wat9, file = "waterGEESE_2015-2017_2.rda")

#### Second WATER models - 2005, 2015 to 2017 ####
w05 <- read.table("GOOSE_Lecomte_supp_nests_2005.txt", h = T )
#Obtention de la variable AgeDay1 = correspond à l'âge du nid lors du premier jour du suivi de nids
w05$AgeDay1 <- (w05$AgeFound - w05$FirstFound)

supW1 <- sup[which(sup$YEAR == 2005 | sup$YEAR == 2015 | sup$YEAR == 2016 | sup$YEAR == 2017),]
supW1 <- supW1[which(supW1$SUPPL == "W" | supW1$SUPPL == "TEM"),]
supW1 <- droplevels(supW1)
supW11 <- supW1[, c(1:4, 7:10, 14, 16)]
head(supW11)

supW2 <- rbind(supW11, w05)

supW2$YEAR <- as.factor(supW2$YEAR)
supW2$Fate <- as.factor(supW2$Fate)
summary(supW2)

require(RMark)

nocc <- max(supW2$LastChecked)

run.WATER.2 = function()
{
  # Null model
  wat1 <- mark(supW2, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ 1)), delete = T)
  
  # YEAR
  wat2 <- mark(supW2, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR)), groups = "YEAR", delete = T)
  
  #SUPPL
  wat3 <- mark(supW2, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL)), groups = "SUPPL", delete = T)
  
  # HAB
  wat4 <- mark(supW2, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB)), groups = "HAB", delete = T)
  
  # INITIATION
  #wat5 <- mark(supW2, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ INITIATION)), delete = T)
  
  #### ** WARNING ** --> no initiation variable for 2005 data
  
  # NestAge
  wat6 <- mark(supW2, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge)), delete = T)
  
  # Complete model without interaction
  #wat7 <- mark(supW2, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + YEAR + NestAge + INITIATION)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)
  
  #### ** WARNING ** --> no initiation variable for 2005 data
  
  # SUPPL + HAB + YEAR + NestAge
  wat8 <- mark(supW2, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + YEAR + NestAge)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)
  
  # SUPPL*HAB + YEAR + NestAge
  wat9 <- mark(supW2, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + YEAR + NestAge)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)
  
  # SUPPL*HAB + YEAR
  wat10 <- mark(supW2, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + YEAR)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)
  
  # HAB + YEAR + NestAge
  wat11 <- mark(supW2, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + YEAR + NestAge)), groups = c("HAB", "YEAR" ), delete = T)
  
  # SUPPL * YEAR + NestAge + HAB
  wat12 <- mark(supW2, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + SUPPL*YEAR + NestAge)), groups = c("HAB", "YEAR", "SUPPL" ), delete = T)
#### *** WARNINGS***
  
  # SUPPL*HAB + SUPPL*YEAR + NestAge
  wat13 <- mark(supW2, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + SUPPL*YEAR + NestAge)), groups = c("HAB", "YEAR", "SUPPL" ), delete = T)
  #### *** WARNINGS***
  
  # SUPPL*YEAR
  wat14 <- mark(supW2, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR)), groups = "YEAR", delete = T)
  #### *** WARNINGS***
  
  return(collect.models() )
}

# run defined models
WATER.2.results <- run.WATER.2()
WATER.2.results


#Save models list and est model
save(WATER.2.results, file = "waterGEESE_2005-2017.rda")
save(wat8, file = "waterGEESE_2005-2017_1.rda")
save(wat9, file = "waterGEESE_2005-2017_2.rda")

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
  
  # HAB + YEAR + NestAge
  foo14 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + YEAR + NestAge)), groups = c("HAB", "YEAR" ), delete = T)
  
  # HAB + SUPPL*YEAR + NestAge
  foo15 <- mark(supF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + SUPPL*YEAR + NestAge)), groups = c("HAB", "YEAR", "SUPPL" ), delete = T)
  
  return(collect.models() )
}

# run defined models
FOOD.results <- run.FOOD()
FOOD.results


#Save models list and est model
save(FOOD.results, file = "foodGEESE.rda")
save(foo14, file = "foodGEESE_1.rda")
save(foo8, file = "foodGEESE_2.rda")
save(foo7, file = "foodGEESE_3.rda")
save(foo9, file = "foodGEESE_4.rda")

#### Food models for 2017 ####
supF.2017 <- supF[supF$YEAR == 2017, ]
supF.2017 <- droplevels(supF.2017)
require(RMark)

nocc <- max(supF.2017$LastChecked)

run.FOOD.2017 = function()
{
  # Null model
  foo1 <- mark(supF.2017, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ 1)), delete = T)
  
  # SUPPL
  foo3 <- mark(supF.2017, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL)), groups = "SUPPL", delete = T)
  
  # HAB
  foo4 <- mark(supF.2017, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB)), groups = "HAB", delete = T)
  
  # INITIATION
  foo5 <- mark(supF.2017, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ INITIATION)), delete = T)
  
  # NestAge
  foo6 <- mark(supF.2017, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge)), delete = T)
  
  # SUPPL + HAB + NestAge + INITIATION
  foo7 <- mark(supF.2017, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + NestAge + INITIATION)), groups = c("SUPPL", "HAB"), delete = T)
  
  # SUPPL + HAB + NestAge
  foo8 <- mark(supF.2017, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + NestAge)), groups = c("SUPPL", "HAB"), delete = T)
  
  # SUPPL*HAB + NestAge
  foo9 <- mark(supF.2017, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + NestAge)), groups = c("SUPPL", "HAB"), delete = T)
  
  # SUPPL*HAB
  foo10 <- mark(supF.2017, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB)), groups = c("SUPPL", "HAB"), delete = T)
  
  # SUPPL*HAB
  foo11 <- mark(supF.2017, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB)), groups = c("SUPPL", "HAB"), delete = T)
  
  # HAB + NestAge
  foo14 <- mark(supF.2017, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + NestAge)), groups = "HAB", delete = T)
  
  return(collect.models() )
}

# run defined models
FOOD.results.2017 <- run.FOOD.2017()
FOOD.results.2017

save(FOOD.results.2017, file = "foodGEESE.2017.rda")
save(foo7, file = "foodGEESE.2017_1.rda")

#### WATER/FOOD models ####
# Only concerning 2015

supWF <- sup[which(sup$YEAR == 2015),]
supWF <- supWF[which(supWF$SUPPL == "WF" | supWF$SUPPL == "TEM"),]
supWF <- droplevels(supWF)
summary(supWF)

supWF$YEAR <- as.factor(supWF$YEAR)
supWF$Fate <- as.factor(supWF$Fate)
supWF$SUPPL <- relevel(supWF$SUPPL, "TEM")

require(RMark)

nocc <- max(supWF$LastChecked)

run.WATFOO = function()
{
  # Null model
  watfoo1 <- mark(supWF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ 1)), delete = T)
  
  # SUPPL
  watfoo2 <- mark(supWF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL)), groups = "SUPPL", delete = T)
  
  # HAB
  watfoo3 <- mark(supWF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB)), groups = "HAB", delete = T)
  
  # INITIATION
  watfoo4 <- mark(supWF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ INITIATION)), delete = T)
  
  # NestAge
  watfoo5 <- mark(supWF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge)), delete = T)
  
  # SUPPL + HAB + NestAge + INITIATION
  watfoo6 <- mark(supWF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + NestAge + INITIATION)), groups = c("SUPPL", "HAB"), delete = T)
  
  # ** WARNING message **
  
  # SUPPL + HAB + NestAge
  watfoo7 <- mark(supWF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + NestAge)), groups = c("SUPPL", "HAB"), delete = T)
  
  # SUPPL*HAB + NestAge
  watfoo8 <- mark(supWF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + NestAge)), groups = c("SUPPL", "HAB"), delete = T)
  
  # SUPPL*HAB
  watfoo9 <- mark(supWF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB)), groups = c("SUPPL", "HAB"), delete = T)
  
  # HAB + NestAge
  watfoo10 <- mark(supWF, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB + NestAge)), groups = "HAB", delete = T)
  
  return(collect.models() )
}

# run defined models
WATFOO.results <- run.WATFOO()
WATFOO.results


#Save models list and est model
save(WATFOO.results, file = "watfooGEESE.rda")
save(watfoo10, file = "watfooGEESE_1.rda")
save(watfoo7, file = "watfooGEESE_2.rda")
save(watfoo8, file = "watfooGEESE_3.rda")
