#####Snow Geese - CONTROL#####

require(RMark)
getwd()
setwd("/Users/nicolas/Desktop/Doc doc doc !/R analysis/Data")
geese<-read.table("MARK1.txt", h=T, sep="\t")

summary(geese)
str(geese)

#to test the NestAge modele with initiation dates which were estimated
#with accurateness (observed laying or observed hatch)

geese2<-subset(geese,!(geese$IniCode==2))
summary(geese2)

# Treat dummy variables for habitat types as factors
geese2$WET=factor(geese2$WET)
geese2$MES=factor(geese2$MES)

#Initiation dates categories
geese2$EarlyLay<-geese2$IniDate
geese2$LastLay<-geese2$IniDate
 #?ifelse
geese2$EarlyLay<-ifelse(geese2$EarlyLay<=163,1,0)
geese2$LastLay<-ifelse(geese2$LastLay>163,1,0)
geese2$EarlyLay=factor(geese2$EarlyLay)
geese2$LastLay=factor(geese2$LastLay)

run.geese2=function ()
{
#DSR varies with NestAge
AgeGeese2=mark(geese2,nocc=38,model="Nest",model.parameters=list(S=list(formula=~NestAge)))
#DSR varies with habitat type
Hbt2=mark(geese2,nocc=38,model="Nest",model.parameters=list(S=list(formula=~WET+MES)), groups=c("WET","MES"))
#DSR varies with nest age & habitat type
AgeHab2=mark(geese2,nocc=38,model="Nest",model.parameters=list(S=list(formula=~NestAge+WET+MES)),groups=c("WET","MES"))
#test with early/lastlay as covariable
Lay2<-mark(geese2,nocc=38, model="Nest",model.parameters = list(S=list(formula=~EarlyLay+LastLay)),groups = c("EarlyLay","LastLay"))
#Model with age nest effect and lay timing
AgeLay2<-mark(geese2,nocc=38, model="Nest",model.parameters = list(S=list(formula=~NestAge+EarlyLay+LastLay)),groups = c("EarlyLay","LastLay"))
#TOTAL MODEL
total2<-mark(geese2,nocc = 38,model = "Nest",model.parameters = list(S=list(formula=~NestAge+WET+MES+EarlyLay+LastLay)),groups=c("WET","MES","EarlyLay","LastLay"))
}
# run defined models
geese2.results=run.geese2()
geese2.results
geese2.results$model.table=model.table(list(AgeGeese2,Hbt2,AgeHab2,Lay2,AgeLay2,total2))
geese2.results
total2$results$AICc

###########################Results plot####################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~AgeHab2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~AgeLay2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Build design matrix with ages of interest
fc=find.covariates(AgeLay2,geese2);fc
fc$value[1:74]=rep(1:37,2) # assign 1:74 to 1st 74 ligns
design=fill.covariates(AgeLay2,fc);design # fill design matrix with values
# extract 1st 74 rows of output
Age.survival=compute.real(AgeLay2,design=design)[1:74,];Age.survival
# insert covariate columns
Age.survival=cbind(design[1:74,2],Age.survival);Age.survival
LAY<-c(rep("EarlyLay",37),rep("LastLay",37))
Age.survival<-cbind(LAY,Age.survival)
colnames(Age.survival)=c("LAY","Age","DSR","seDSR","lclDSR","uclDSR")
Age.survival # view estimates of DSR for each age
# Plot results
require(plotrix)
par(mfrow=c(2,1))
with(data=Age.survival,plot(Age[LAY=="EarlyLay"],DSR[LAY=="EarlyLay"],'l',ylim=c(0.953,1),
                            main = "Daily survival nest with age nest & early-lay - CONTROL",
                            xlab = "Age of nests",ylab = "DSR"))
grid()
axis.break(axis=2,breakpos=0.9535,style='slash')
with(data=Age.survival,points(Age[LAY=="EarlyLay"],lclDSR[LAY=="EarlyLay"],'l',lty=3))
with(data=Age.survival,points(Age[LAY=="EarlyLay"],uclDSR[LAY=="EarlyLay"],'l',lty=3))


with(data=Age.survival,plot(Age[LAY=="LastLay"],DSR[LAY=="LastLay"],'l',ylim=c(0.965,1),
                            main = "Daily survival nest with age nest & last-lay - CONTROL",
                            xlab = "Age of nests", ylab="DSR"))
grid()
axis.break(axis=2,breakpos=0.9655,style='slash')
with(data=Age.survival,points(Age[LAY=="LastLay"],lclDSR[LAY=="LastLay"],'l',lty=3))
with(data=Age.survival,points(Age[LAY=="LastLay"],uclDSR[LAY=="LastLay"],'l',lty=3))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~AgeGeese2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Build design matrix with ages of interest
fc=find.covariates(AgeGeese2,geese2);fc
fc$value[1:37]=1:37 # assign 1:26 to 1st 26nest ages
design=fill.covariates(AgeGeese2,fc);design # fill design matrix with values
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
