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
  wat0 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ 1)), delete = T)
  
  # YEAR
  wat1 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ YEAR)), groups = "YEAR", delete = T)
  
  #SUPL
  wat1 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL)), groups = "SUPPL", delete = T)
  
  # HAB
  wat2 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ HAB)), groups = "HAB", delete = T)
  
  # INITIATION
  wat3 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ INITIATION)), delete = T)
  
  # NestAge
  wat4 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ NestAge)), delete = T)
  
  # Complete model without interaction
  #wat5 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + YEAR + NestAge + INITIATION)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)
  
  # WARNING ! Seems to have too much factors for model wat5
  
  # SUPPL + HAB + YEAR + NestAge
  wat6 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL + HAB + YEAR + NestAge)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)
  
  # SUPPL*HAB + YEAR + NestAge
  wat7 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + YEAR + NestAge)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)
  
  # SUPPL*HAB + YEAR
  wat8 <- mark(supW1, nocc = nocc, model = "Nest", model.parameters = list(S = list(formula = ~ SUPPL*HAB + YEAR)), groups = c("SUPPL", "HAB", "YEAR" ), delete = T)
  
  
  return(collect.models() )
}

# run defined models
WATER.1.results <- run.WATER.1()
WATER.1.results


#Save models list and est model
#save(WATER.1.results, file = "waterGEESE_V1.rda")
#save(wat6, file = "waterGEESE_V1_1.rda")
#save(wat7, file = "waterGEESE_V1_2.rda")
