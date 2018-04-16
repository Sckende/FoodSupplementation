####################DONNÉES 2015####################
rm(list = ls())

require(RMark)
getwd()
setwd("//home/claire/OneDriveDoc/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")

#----------------------------------#
#####Snow geese - control 2015#####
#--------------------------------#

geese<-read.table("MARK1-2015.txt", h=T, sep="\t")

summary(geese)
str(geese)
dim(geese)
#class(geese$id)

# Treat dummy variables for habitat types as factors
geese$WET <- factor(geese$WET)
geese$MES <- factor(geese$MES)

# Examine a summary of the dataset
summary(geese)

#Attention ici la valeur de "nocc" varie selon le nombre d'occasion de capture, soit du premier au dernier jour du suivi, correspond au max de "LastChecked"
nocc<-max(geese$LastChecked)

# Write a function for evaluating a set of competing models
#run.geese=function()
#{# 1. A model of constant daily survival rate (DSR)
Dot <- mark(geese, nocc=nocc, model="Nest", model.parameters = list(S = list(formula = ~1)))
# 2. DSR varies by habitat type - treats habitats as factors
# and the output provides S-hats for each habitat type
Hbt <- mark(geese, nocc=nocc, model="Nest", model.parameters = list(S = list(formula = ~WET+MES)), groups = c("WET","MES"))
# 3. DSR varies with NestAge
AgeGeese <- mark(geese, nocc=nocc, model="Nest", model.parameters = list(S = list(formula = ~NestAge)))
#4.DSR varie with NestAge and Habitat
AgeHbt <- mark(geese, nocc=nocc, model="Nest", model.parameters = list(S = list(formula = ~NestAge + WET + MES)), groups = c("WET","MES"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine table of model-selection results #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Pour faire tourner cette portion de script, voir exemple associé à RMark

#export.MARK(geese.results$Age$data,"GeeseDSR",geese.results,replace=TRUE,ind.covariates="all")
#Creates a .Rinp, .inp and optionally renamed output files that can be imported into MARK to
#create a new MARK project with all the data and output files.
geese.results # print model-selection table to screen - Bilan AICc
#exportation des r?sultats en format .txt
options(width=100) # set page width to 100 characters
sink("resultsgeese.table.txt") # capture screen output to file
print(geese.results) # send output to file
sink() # return output to screen
# remove "#" on next line to see output in notepad
#system("notepad resultsgeese.table.txt",invisible=FALSE,wait=FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for constant DSR model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Remove "#" on next line to see output
#geese.results$Dot # print MARK output to designated text editor
geese.results$Dot$results$beta # view estimated beta for model in R
geese.results$Dot$results$real # view estimated DSR estimate in R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for 'DSR by habitat' model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Remove "#" on next line to see output
# geese.results$Hab # print MARK output to designated text editor
geese.results$Hbt$design.matrix # view the design matrix that was used
geese.results$Hbt$results$beta # view estimated beta for model in R
geese.results$Hbt$results$beta.vcv # view variance-covariance matrix for beta's
geese.results$Hbt$results$real # view the estimates of Daily Survival Rate

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Model plot #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

####----------NestAge plots---------#####
# To obtain estimates of DSR for various values of 'NestAge'
# some work additional work is needed.
require(plotrix)
AgeGeese=mark(geese,nocc=nocc,model="Nest",model.parameters=list(S=list(formula=~NestAge)))
AgeGeese#the used model to plot

# Build design matrix with ages of interest
fc=find.covariates(AgeGeese,geese);fc
fc$value[1:37]=1:37 # assign 1:26 to 1st 26nest ages
design=fill.covariates(AgeGeese,fc);design # fill design matrix with values

# extract 1st 26 rows of output
Age.survival=compute.real(AgeGeese,design=design)[1:37,];Age.survival

# insert covariate columns
Age.survival=cbind(design[1:37,2],Age.survival);Age.survival
colnames(Age.survival)=c("Age","DSR","seDSR","lclDSR","uclDSR")
Age.survival # view estimates of DSR for each age
# Plot results
with(data=Age.survival,plot(Age,DSR,'l',ylim=c(0.98,1),main = "Daily survival nest with age nest - CONTROL"))
grid()
axis.break(axis=2,breakpos=0.9795,style='slash')
with(data=Age.survival,points(Age,lclDSR,'l',lty=3))
with(data=Age.survival,points(Age,uclDSR,'l',lty=3))


####----------AgeHbt plots---------#####
summary(AgeHbt)
# Build design matrix with ages of interest
fc <- find.covariates(AgeHbt, geese); fc
fc$value[1:nrow(fc)] = rep(1:(nrow(fc)/2), 2) # assign 1:xx to 1st xx ligns
design <- fill.covariates(AgeHbt, fc); design # fill design matrix with values

# extract 1st 74 rows of output
Age.survival <- compute.real(AgeHbt, design = design)[1:nrow(fc),]; Age.survival

# insert covariate columns
Age.survival <- cbind(design[1:nrow(fc), 2], Age.survival); Age.survival
HAB <- c(rep("WET", (nrow(fc)/2)), rep("MES", (nrow(fc)/2)))
Age.survival <- cbind(HAB, Age.survival)

colnames(Age.survival) <- c("HAB", "Age", "DSR", "seDSR", "lclDSR", "uclDSR")

Age.survival <- Age.survival[, -7]; Age.survival # view estimates of DSR for each age

#to calculate the NS for each habitat
#WETLAND
DSRwet <- Age.survival[Age.survival$HAB == "WET",]
NSwet <- prod(DSRwet$DSR[c(1:27)]); NSwet

#MESIC
DSRmes <- Age.survival[Age.survival$HAB == "MES",]
NSmes <- prod(DSRmes$DSR[c(1:27)]); NSmes

# Plot results
require(plotrix) # For the axis break
par(mfrow = c(2,1))
with(data = Age.survival, plot(Age[HAB == "MES"], DSR[HAB == "MES"], 'l', ylim = c(min(Age.survival$lclDSR), 1), main =paste(c("Daily survival nest with age nest & mesic habitat - CONTROL", unique(geese$AN))), xlab = "Day of exposition", ylab = paste(c("DSR", unique(geese$AN)))))
grid()
axis.break(axis = 2, breakpos = min(Age.survival$lclDSR) + 0.005, style = 'slash')
with(data = Age.survival, points(Age[HAB == "MES"], lclDSR[HAB == "MES"], 'l', lty = 3))
with(data = Age.survival, points(Age[HAB == "MES"], uclDSR[HAB == "MES"], 'l', lty = 3))

with(data = Age.survival, plot(Age[HAB == "WET"], DSR[HAB == "WET"], 'l', ylim = c(min(Age.survival$lclDSR), 1), main = paste(c("Daily survival nest with age nest & wetland habitat - CONTROL - ", unique(geese$AN))), xlab = "Day of exposition", ylab = paste(c("DSR", unique(geese$AN)))))
grid()
axis.break(axis = 2, breakpos = min(Age.survival$lclDSR) + 0.005, style='slash')
with(data = Age.survival, points(Age[HAB == "WET"], lclDSR[HAB == "WET"], 'l', lty=3))
with(data = Age.survival, points(Age[HAB == "WET"], uclDSR[HAB == "WET"], 'l', lty=3))

####~~~~~~~~NestAge (JJ 155 to JJ 166)~~~~~~~~~~~~~~~~~~~####
#to test the NestAge modele with the same spread of initiation date than the supplemented nests (155 to 166)

geese1 <- subset(geese,geese$IniDate<=166)
summary(geese1)
#DSR varies with NestAge
AgeGeese1=mark(geese1,nocc=38,model="Nest",model.parameters=list(S=list(formula=~NestAge)))

#To plot results
AgeGeese1#the used model to plot
# Build design matrix with ages of interest
fc=find.covariates(AgeGeese1,geese1);fc
fc$value[1:37]=1:37 # assign 1:26 to 1st 26nest ages
design=fill.covariates(AgeGeese1,fc);design # fill design matrix with values

# extract 1st 26 rows of output
Age.survival=compute.real(AgeGeese1,design=design)[1:37,];Age.survival

# insert covariate columns
Age.survival=cbind(design[1:37,2],Age.survival);Age.survival
colnames(Age.survival)=c("Age","DSR","seDSR","lclDSR","uclDSR")
Age.survival # view estimates of DSR for each age
# Plot results
with(data=Age.survival,plot(Age,DSR,'l',ylim=c(0.98,1),main = "Daily survival nest with age nest - CONTROL"))
grid()
require(plotrix)
axis.break(axis=2,breakpos=0.9795,style='slash')
with(data=Age.survival,points(Age,lclDSR,'l',lty=3))
with(data=Age.survival,points(Age,uclDSR,'l',lty=3))

####~~~~~~~~~~NestAge with accurated estimated dates ~~~~~~~~~~~~~~~~~~####
#to test the NestAge modele with initiation dates which were estimated
#with accurateness (observed laying or observed hatch)

###here I obtain good graphics for the nesting success by NestAge and habitat
#but the value for the computation of the nesting success with DSR is not possible for the mesic tundra

require(RMark)
getwd()
setwd("/Users/nicolas/Documents/Claire/Doc doc doc !/R analysis/Data")
geese<-read.table("MARK1-2015.txt", h=T, sep="\t")

summary(geese)
str(geese)
class(geese$id)

geese2<-subset(geese,!(geese$IniCode==2))
summary(geese2)

# Treat dummy variables for habitat types as factors
geese2$WET=factor(geese2$WET)
geese2$MES=factor(geese2$MES)

#DSR varies with NestAge
AgeGeese2=mark(geese2,nocc=38,model="Nest",model.parameters=list(S=list(formula=~NestAge)))
#DSR varies with habitat type
Hbt2=mark(geese2,nocc=38,model="Nest",model.parameters=list(S=list(formula=~WET+MES)), groups=c("WET","MES"))
#DSR varies with nest age & habitat type
AgeHab2=mark(geese2,nocc=38,model="Nest",model.parameters=list(S=list(formula=~NestAge+WET+MES)),groups=c("WET","MES"))

#Results plot
AgeHab2
# Build design matrix with ages of interest
fc=find.covariates(AgeHab2,geese2);fc
fc$value[1:74]=rep(1:37,2) # assign 1:74 to 1st 74 ligns
design=fill.covariates(AgeHab2,fc);design # fill design matrix with values

# extract 1st 74 rows of output
Age.survival=compute.real(AgeHab2,design=design)[1:74,];Age.survival

# insert covariate columns
Age.survival=cbind(design[1:74,2],Age.survival);Age.survival
HAB<-c(rep("WET",37),rep("MES",37))
Age.survival<-cbind(HAB,Age.survival)



colnames(Age.survival)=c("HAB","Age","DSR","seDSR","lclDSR","uclDSR")

Age.survival # view estimates of DSR for each age

# Plot results
require(plotrix)
par(mfrow=c(2,1))
with(data=Age.survival,plot(Age[HAB=="MES"],DSR[HAB=="MES"],'l',ylim=c(0.78,1),
                            main = "Daily survival nest with age nest & mesic habitat- CONTROL",
                            xlab = "Age of nests",ylab = "DSR"))
grid()
axis.break(axis=2,breakpos=0.785,style='slash')
with(data=Age.survival,points(Age[HAB=="MES"],lclDSR[HAB=="MES"],'l',lty=3))
with(data=Age.survival,points(Age[HAB=="MES"],uclDSR[HAB=="MES"],'l',lty=3))

with(data=Age.survival,plot(Age[HAB=="WET"],DSR[HAB=="WET"],'l',ylim=c(0.98,1),
                            main = "Daily survival nest with age nest & wetland habitat- CONTROL",
                            xlab = "Age of nests", ylab="DSR"))
grid()
axis.break(axis=2,breakpos=0.9796,style='slash')
with(data=Age.survival,points(Age[HAB=="WET"],lclDSR[HAB=="WET"],'l',lty=3))
with(data=Age.survival,points(Age[HAB=="WET"],uclDSR[HAB=="WET"],'l',lty=3))

###Other graphic
# Build design matrix with ages of interest
fc=find.covariates(AgeGeese2,geese2);fc
fc$value[1:37]=1:37 # assign 1:26 to 1st 26nest ages
design=fill.covariates(AgeGeese2,fc);design # fill design matrix with values

# extract 1st 26 rows of output
Age.survival=compute.real(AgeGeese2,design=design)[1:37,];Age.survival

# insert covariate columns
Age.survival=cbind(design[1:37,2],Age.survival);Age.survival
colnames(Age.survival)=c("Age","DSR","seDSR","lclDSR","uclDSR")
Age.survival # view estimates of DSR for each age
# Plot results
with(data=Age.survival,plot(Age,DSR,'l',ylim=c(0.98,1),main = "Daily survival nest with age nest - CONTROL"))
grid()
axis.break(axis=2,breakpos=0.9795,style='slash')
with(data=Age.survival,points(Age,lclDSR,'l',lty=3))
with(data=Age.survival,points(Age,uclDSR,'l',lty=3))
#temporal variation of the DSR is good with this subset of datas
#test of the other modeles with GEESE 2 datas#
# Treat dummy variables for habitat types as factors
geese2$WET=factor(geese2$WET)
geese2$MES=factor(geese2$MES)
#A model of constant daily survival rate (DSR)
Dot2=mark(geese2,nocc=38,model="Nest",model.parameters=list(S=list(formula=~1)))
# 2. DSR varies by habitat type - treats habitats as factors
# and the output provides S-hats for each habitat type
Hbt2=mark(geese2,nocc=38,model="Nest",model.parameters=list(S=list(formula=~WET+MES)), groups=c("WET","MES"))

Dot2$results$beta # view estimated beta for model in R
Dot2$results$real # view estimated DSR estimate in R

Hbt2$results$beta # view estimated beta for model in R
Hbt2$results$real # view estimated DSR estimate in R
#recuperation des valeurs de DSR
DSRHbt2<-compute.real(model = Hbt2, data=geese2)
DSRHbt2$NS<-DSRHbt2$estimate^27;DSRHbt2#NS for nesting success=DSR^27