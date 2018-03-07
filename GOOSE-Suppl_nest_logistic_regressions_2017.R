setwd('C:/Users/God/Desktop/Claire/Chapter 1/Data')

g <- read.table('GOOSE-Suppl_BDD_2017.txt', sep = '\t', h = T)
head(g)
summary(g)

g$PRED <- as.factor(g$PRED)
g$SONDE <- as.factor(g$SONDE)
g$REAL_HATCH <- as.numeric(g$REAL_HATCH)
colnames(g[13]) <- 'PAR_PRED'

require(lme4)

#### Model with TREAT ####
#Command used to change reference level:
g$TREAT<-relevel(g$TREAT, ref="TEM")
go <- glm(ISSUE ~ TREAT, data = g,  family = 'binomial')
summary(go)


#### Model with HAB ####
#Command used to change reference level:
g$HAB<-relevel(g$HAB, ref="MES")
go1 <- glm(ISSUE ~ HAB, data = g,  family = 'binomial')
summary(go1)

#### Model with HAB + TREAT ####
#Command used to change reference level:
g$TREAT<-relevel(g$TREAT, ref="TEM")
g$HAB<-relevel(g$HAB, ref="MES")

go2 <- glm(ISSUE ~ HAB + TREAT + HAB*TREAT, data = g,  family = 'binomial')
summary(go2)
