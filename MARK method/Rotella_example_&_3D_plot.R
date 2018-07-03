#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Example of use of RMark for modeling nest survival data - Mallard nests example #
# The example runs the 9 models that are used in the Nest Survival chapter #
# of the Gentle Introduction to MARK and that appear in Table 3 (page 198) of #
# Rotella, J.J., S. J. Dinsmore, T.L. Shaffer. 2004. Modeling nest-survival data: #
# a comparison of recently developed methods that can be implemented in MARK and SAS. #
# Animal Biodiversity and Conservation 27:187-204. #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
require(RMark)
data(mallard)

summary(mallard)
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
  Dot=mark(mallard,nocc=90,model="Nest",model.parameters=list(S=list(formula=~1)))
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
  PpnGr=mark(mallard,nocc=90,model="Nest",model.parameters=list(S=list(formula=~PpnGrass)))
  # 5. DSR follows a trend through time
  TimeTrend=mark(mallard,nocc=90,model="Nest",model.parameters=list(S=list(formula=~Time)))
  # 6. DSR varies with nest age
  Age=mark(mallard,nocc=90,model="Nest",model.parameters=list(S=list(formula=~NestAge)))
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

mallard.results=run.mallard() # This runs the 9 models above and takes a minute or 2

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine table of model-selection results #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
mallard.results # print model-selection table to screen
options(width=100) # set page width to 100 characters
sink("results.table.txt") # capture screen output to file
print.marklist(mallard.results) # send output to file
sink() # return output to screen
system("notepad results.table.txt",invisible=FALSE) # view results in notepad

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for constant DSR model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
mallard.results$Dot # print MARK output to designated text editor
mallard.results$Dot$results$beta # view estimated beta for model in R
mallard.results$Dot$results$real # view estimated DSR estimate in R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for ’DSR by habitat’ model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
mallard.results$Hab # print MARK output to designated text editor
mallard.results$Hab$design.matrix # view the design matrix that was used
mallard.results$Hab$results$beta # view estimated beta for model in R
mallard.results$Hab$results$beta.vcv # view variance-covariance matrix for beta’s
mallard.results$Hab$results$real # view the estimates of Daily Survival Rate

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Examine output for best model #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
mallard.results$AgePpnGrass # print MARK output to designated text editor
mallard.results$AgePpnGrass$results$beta # view estimated beta’s in R
mallard.results$AgePpnGrass$results$beta.vcv # view estimated var-cov matrix in R
# To obtain estimates of DSR for various values of ’NestAge’ and ’PpnGrass’
# some work additional work is needed.
# First, a simpler name for the object containing the preferred model results
AgePpnGrass=mallard.results$AgePpnGrass
# Build design matrix with ages and ppn grass values of interest
fc <- find.covariates(AgePpnGrass,mallard)
# iterate through sequence of ages and proportion grassland
# values to build prediction surfaces
seq.ages <- seq(2, 26, by=2)
seq.ppn <- seq(0.01,0.99,length=89)
point <- matrix(nrow=89, ncol=length(seq.ages))
lower <- matrix(nrow=89, ncol=length(seq.ages))
upper <- matrix(nrow=89, ncol=length(seq.ages))
colnum <- 0
for (iage in seq.ages) {
  fc$value[1:89]=iage # assign sequential age
  colnum <- colnum + 1
  fc$value[fc$var=="PpnGrass"]=seq.ppn # assign range of values to PpnGrass
  design=fill.covariates(AgePpnGrass,fc) # fill design matrix with values
  point[,colnum] <- compute.real(AgePpnGrass,design=design)[,"estimate"]
  lower[,colnum] <- compute.real(AgePpnGrass,design=design)[,"lcl"]
  upper[,colnum] <- compute.real(AgePpnGrass,design=design)[,"ucl"]
}
# Predicted surfaces shown in a window that can be rotated and zoomed by user
# left mouse button=rotate, right mouse=zoom
require(rgl)
open3d()
bg3d("white")
material3d(col="black")
persp3d(seq.ppn, seq.ages, point, aspect=c(1, 1, 0.5), col = "lightblue",
          xlab = "grass", ylab = "age", zlab = "DSR", zlim=range(c(upper,lower)),
          main="Daily survival rate (with CI)", sub="for model ’age and proportion grassland’")
persp3d(seq.ppn, seq.ages, upper, aspect=c(1, 1, 0.5), col = "yellow", add=TRUE)
persp3d(seq.ppn, seq.ages, lower, aspect=c(1, 1, 0.5), col = "yellow", add=TRUE)
grid3d(c("x","y+","z"))
# see rgl.snapshot(file="snapshot.png") for creating png image of generated surfaces