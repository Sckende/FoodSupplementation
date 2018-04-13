##### Killdeer example#####
# This example is excluded from testing to reduce package check time
# EXAMPLE CODE FOR CONVERSION OF .INP TO NECESSARY DATA STRUCTURE
# read in killdeer.inp file
#killdeer=scan("killdeer.inp",what="character",sep="\n")
# strip out ; and write out all but first 2 lines which contain comments
#write(sub(";","",killdeer[3:20]),"killdeer.txt")
# read in as a dataframe and assign names
#killdeer=read.table("killdeer.txt")
#names(killdeer)=c("id","FirstFound","LastPresent","LastChecked","Fate","Freq")

#
# EXAMPLE CODE TO RUN MODELS CONTAINED IN THE MARK KILLDEER.DBF
data(killdeer)
# produce summary
summary(killdeer)
# Define function to run models that are in killdeer.dbf
# You must specify either the number of occasions (nocc) or the time.intervals
# between the occasions.
#run.killdeer=function()
#{
  Sdot=mark(killdeer,model="Nest",nocc=40, delete = T)
  STime=mark(killdeer,model="Nest",
             model.parameters=list(S=list(formula=~I(Time+1))),nocc=40,threads=2, delete = T)
  STimesq=mark(killdeer,model="Nest",
               model.parameters=list(S=list(formula=~I(Time+1)+I((Time+1)^2))),nocc=40,threads=2, delete = T)
  STime3=mark(killdeer,model="Nest",
              model.parameters=list(S=list(formula=~I(Time+1)+I((Time+1)^2)+I((Time+1)^3))),
              nocc=40,threads=2, delete = T)
#  return(collect.models())
#}
# run defined models

killdeer.results=run.killdeer()
killdeer.results$Sdot

#####Mallard example#####

# This example is excluded from testing to reduce package check time
require(RMark)
require(plotrix)
# Retrieve data
data(mallard)
mallard
# Treat dummy variables for habitat types as factors
mallard$Native=factor(mallard$Native)
mallard$Planted=factor(mallard$Planted)
mallard$Wetland=factor(mallard$Wetland)
mallard$Roadside=factor(mallard$Roadside)
# Examine a summary of the dataset
summary(mallard)
# Write a function for evaluating a set of competing models
run.mallard=function()
{
  # 1. A model of constant daily survival rate (DSR)
  Dot=mark(mallard,nocc=90,model="Nest",
           model.parameters=list(S=list(formula=~1)))
  # 2. DSR varies by habitat type - treats habitats as factors
  # and the output provides S-hats for each habitat type
  Hab=mark(mallard,nocc=90,model="Nest",
           model.parameters=list(S=list(formula=~Native+Planted+Wetland)),
           groups=c("Native","Planted","Wetland"))
  # 3. DSR varies with vegetation thickness (Robel reading)
  # Note: coefficients are estimated using the actual covariate
  # values. They are not based on standardized covariate values.
  Robel=mark(mallard,nocc=90,model="Nest",
             model.parameters=list(S=list(formula=~Robel)))
  # 4. DSR varies with the amount of native vegetation in the surrounding area
  # Note: coefficients are estimated using the actual covariate
  # values. They are not based on standardized covariate values.
  PpnGr=mark(mallard,nocc=90,model="Nest",
             model.parameters=list(S=list(formula=~PpnGrass)))
  # 5. DSR follows a trend through time
  TimeTrend=mark(mallard,nocc=90,model="Nest",
                 model.parameters=list(S=list(formula=~Time)))
  # 6. DSR varies with nest age
  Age=mark(mallard,nocc=90,model="Nest",
           model.parameters=list(S=list(formula=~NestAge)))
  # 7. DSR varies with nest age & habitat type
  AgeHab=mark(mallard,nocc=90,model="Nest",
              model.parameters=list(S=list(formula=~NestAge+Native+Planted+Wetland)),
              groups=c("Native","Planted","Wetland"))
  # 8. DSR varies with nest age & vegetation thickness
  AgeRobel=mark(mallard,nocc=90,model="Nest",
                model.parameters=list(S=list(formula=~NestAge+Robel)))
  # 9. DSR varies with nest age & amount of native vegetation in surrounding area
  AgePpnGrass=mark(mallard,nocc=90,model="Nest",
                   model.parameters=list(S=list(formula=~NestAge+PpnGrass)))
  #
  # Return model table and list of models
  #
  return(collect.models() )
}
# The next line runs the 9 models above and takes a minute or 2
mallard.results=run.mallard()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine table of model-selection results #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
export.MARK(mallard.results$Age$data,"MallDSR",mallard.results,replace=TRUE,ind.covariates="all")
mallard.results # print model-selection table to screen
options(width=100) # set page width to 100 characters
sink("results.table.txt") # capture screen output to file
print(mallard.results) # send output to file
sink() # return output to screen
# remove "#" on next line to see output in notepad
system("notepad results.table.txt",invisible=FALSE,wait=FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for constant DSR model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Remove "#" on next line to see output
# mallard.results$Dot # print MARK output to designated text editor
mallard.results$Dot$results$beta # view estimated beta for model in R
mallard.results$Dot$results$real # view estimated DSR estimate in R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for 'DSR by habitat' model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Remove "#" on next line to see output
# mallard.results$Hab # print MARK output to designated text editor
mallard.results$Hab$design.matrix # view the design matrix that was used
mallard.results$Hab$results$beta # view estimated beta for model in R
mallard.results$Hab$results$beta.vcv # view variance-covariance matrix for beta's
mallard.results$Hab$results$real # view the estimates of Daily Survival Rate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for best model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Remove "#" on next line to see output
# mallard.results$AgePpnGrass # print MARK output to designated text editor
mallard.results$AgePpnGrass$results$beta # view estimated beta's in R
mallard.results$AgePpnGrass$results$beta.vcv # view estimated var-cov matrix in R


# To obtain estimates of DSR for various values of 'NestAge' and 'PpnGrass'
# some work additional work is needed.
# Store model results in object with simpler name
AgePpnGrass=mallard.results$AgePpnGrass

# Build design matrix with ages and ppn grass values of interest
# Relevant ages are age 1 to 35 for mallards
# For ppngrass, use a value of 0.5
fc=find.covariates(AgePpnGrass,mallard)

fc$value[1:35]=1:35 # assign 1:35 to 1st 35 nest ages

fc$value[fc$var=="PpnGrass"]=0.1 # assign new value to PpnGrass
design=fill.covariates(AgePpnGrass,fc) # fill design matrix with values

# extract 1st 35 rows of output
AgePpn.survival=compute.real(AgePpnGrass,design=design)[1:35,]

# insert covariate columns
AgePpn.survival=cbind(design[1:35,c(2:3)],AgePpn.survival)
colnames(AgePpn.survival)=c("Age","PpnGrass","DSR","seDSR","lclDSR","uclDSR")
AgePpn.survival # view estimates of DSR for each age and PpnGrass combo
# Plot results
with(data=AgePpn.survival,plot(Age,DSR,'l',ylim=c(0.88,1)))
grid()
axis.break(axis=2,breakpos=0.879,style='slash')
with(data=AgePpn.survival,points(Age,lclDSR,'l',lty=3))
with(data=AgePpn.survival,points(Age,uclDSR,'l',lty=3))
# assign 17 to 1st 50 nest ages
fc$value[1:89]=17
# assign range of values to PpnGrass
fc$value[fc$var=="PpnGrass"]=seq(0.01,0.99,length=89)
design=fill.covariates(AgePpnGrass,fc) # fill design matrix with values
AgePpn.survival=compute.real(AgePpnGrass,design=design)
# insert covariate columns
AgePpn.survival=cbind(design[,c(2:3)],AgePpn.survival)
colnames(AgePpn.survival)=
  c("Age","PpnGrass","DSR","seDSR","lclDSR","uclDSR")
# view estimates of DSR for each age and PpnGrass combo
AgePpn.survival
# Plot results
# open new graphics window for new plot
dev.new()
with(data=AgePpn.survival,plot(PpnGrass,DSR,'l',ylim=c(0.88,1)))
grid()
axis.break(axis=2,breakpos=0.879,style='slash')
with(data=AgePpn.survival,points(PpnGrass,lclDSR,'l',lty=3))
with(data=AgePpn.survival,points(PpnGrass,uclDSR,'l',lty=3))
# The "rm" command can be used to remove all objects from the .Rdata file.
# Cleaning up objects as shown in this script is good programming
# practice and a good idea as long as the computations are not time consuming.
# However, if your analysis is taking several hours or days in MARK then
# clearly you'll want to hang onto the resulting objects in R and you
# won't want to use the following command. It has been commented out for
# this example; the "#" on the next line needs to be removed to do the clean up.
# rm(list=ls(all=TRUE))
# The next line deletes orphaned output files from MARK.
# ?cleanup will give a more complete description of this function.
cleanup(ask=FALSE)
