#### Steps for the data formatting to use the Shaffer method ####
############ COLONY DATA ########


# Load the initial data file of GSGO monitoring
setwd(dir = "/home/claire/Bureau")
list.files()
gsgo <- read.csv("NIDIFI 2017.csv")
summary(gsgo)

#### Variable treatment ####

#### ISSUE ####
# Nest fate 0:excluded, 1:Success, 2:Abandonment, 3:Destroyed, 5:Unknown
gsgo <- subset(gsgo, ISSUE != 0) # Remove excluded nests

# Categorize ISSUE in FATE (0:Success, 1:Destroyed or NA:Unknown)
for (i in 1:nrow(gsgo)) {
  if (gsgo$ISSUE[i] == 1) {gsgo$fate[i] <- 0} # Successful nests
  else {
    if (gsgo$ISSUE[i] == 3) {gsgo$fate[i] <- 1} # Destroyed nests
    else {
      if (gsgo$ISSUE[i] == 2|gsgo$ISSUE[i] == 5) {gsgo$fate[i] <- NA} # No predation for unknown fate nests or abandonned eggs
      else {gsgo$fate[i] <- NA}
    }
  }
}

#### NEST.TYPE ####
# Remove experimental nests 1995-97 (N=53)
gsgo <- subset(gsgo,NEST.TYPE != "Experimental - thermometer egg")

#### HABITAT ####
# Categorize habitat "Humide" vs "Mesique"
levels(gsgo$HABITAT)
gsgo$hab[gsgo$HABITAT=="H"|gsgo$HABITAT=="Wetland"] <- "Humide"
gsgo$hab[gsgo$HABITAT=="H-M"|gsgo$HABITAT=="HM"|gsgo$HABITAT=="M-H"|
           gsgo$HABITAT=="MH"|gsgo$HABITAT=="Wetland/mesic"|gsgo$HABITAT=="Riparian"] <- "Humide"
gsgo$hab[gsgo$HABITAT=="M"|gsgo$HABITAT=="Mesic"|gsgo$HABITAT=="M "|gsgo$HABITAT=="M-P"] <- "Mesique"
gsgo$hab[gsgo$HABITAT=="N/A"] <- NA

#### UTM.E/UTM.N ####
#Coordinates - Replace 0 by NAs
gsgo$UTM.E[gsgo$UTM.E==0] <- NA
gsgo$UTM.N[gsgo$UTM.N==0] <- NA

#### Creation of a dataframe ####
# Group necessary variables in a dataframe
gsgo_df <- data.frame(year = gsgo$AN, site = gsgo$LOC, id = gsgo$NO, FirstFound = gsgo$DATEk, LastPresent = gsgo$DATEl, LastChecked = gsgo$DATEm, fate = gsgo$fate, habitat = gsgo$hab, ISSUE = gsgo$ISSUE, UTM.E = gsgo$UTM.E, UTM.N = gsgo$UTM.N)

summary(gsgo_df)

#### DATA VALIDATION ####
# Remove nests with only one visit (FirstFound = LastChecked)
FF.equals.LC <- 0
for (i in 1:nrow(gsgo_df)){
  if (gsgo_df$FirstFound[i] == gsgo_df$LastChecked[i]) FF.equals.LC <- c(FF.equals.LC,i)
}
FF.equals.LC <- FF.equals.LC[-1]
if (length(FF.equals.LC) > 0) gsgo_df <- gsgo_df[-FF.equals.LC,]
row.names(gsgo_df) <- NULL
rm(FF.equals.LC)

# Check for nest for wich FirstFound = LastPresent when no predation (THUS ZERO EXPOSURE DAYS)
rownum <- 0
for (i in 1:nrow(gsgo_df)){
  if (is.na(gsgo_df$fate[i])==FALSE) {
    if (gsgo_df$FirstFound[i] == gsgo_df$LastPresent[i] & gsgo_df$fate[i] == 0) {
      rownum <- cbind(rownum, i)
    }
  }
}
colnames(rownum) <- NULL
rownum <- rownum[-1]
rownum
gsgo_df[rownum,]

# Check for nests for which LastPresent != LastChecked when no predation (SHOULD NOT HAPPEN)
rownum <- 0
for (i in 1:nrow(gsgo_df)){
  if (is.na(gsgo_df$fate[i])==FALSE) {
    if (gsgo_df$LastPresent[i] != gsgo_df$LastChecked[i] & gsgo_df$fate[i] == 0) {
      rownum <- cbind(rownum, i)
    }
  }
}
colnames(rownum) <- NULL
rownum <- rownum[-1]
rownum
gsgo_df[rownum,"ID"]

# Remove invalid nests 
# Check for nests for which LastPresent == LastChecked when there is predation (SHOULD NOT HAPPEN UNLESS TWO VISITS IN THE SAME DAY AND PREDATION IN THE SECOND VISIT)
rownum <- 0
for (i in 1:nrow(gsgo_df)){
  if (is.na(gsgo_df$fate[i])==FALSE) {
    if (gsgo_df$LastPresent[i] == gsgo_df$LastChecked[i] & gsgo_df$fate[i] == 1) {
      rownum <- cbind(rownum, i)
    }
  }
}
colnames(rownum) <- NULL
rownum <- rownum[-1]
rownum
if (length(rownum) > 0) gsgo_df <- data.frame(gsgo_df[-rownum,], row.names=NULL)


#### Save file ####
write.csv(gsgo_df, file = "output_gsgo.csv")

#### FORMAT DATA FOR LOG-EXPOSURE METHOD ####
# (SHAFFER 2004)
# For each nest, create one or more visit intervals depending on nest fate
# Output: a list that will then be converted in a dataframe
ID.list <- list("list",length(unique(gsgo_df$id)))
for (i in 1:length(unique(gsgo_df$id))) {
  if (gsgo_df$fate[i]==0|is.na(gsgo_df$fate[i])) nr <- 1
  if (is.na(gsgo_df$fate[i])==F) {
    if (gsgo_df$fate[i]==1) nr <- 2
  }
  ID.list[[i]] <- data.frame(id=rep(gsgo_df$id[i],nr),
                             year=rep(gsgo_df$year[i],nr),
                             site=rep(gsgo_df$site[i],nr),
                             exposure=rep(0,nr), survive=rep(0,nr),
                             UTM.E=rep(gsgo_df$UTM.E[i],nr),
                             UTM.N=rep(gsgo_df$UTM.N[i],nr),
                             habitat=rep(gsgo_df$habitat[i],nr))
  ID.list[[i]]$issue <- gsgo_df$ISSUE[i]
  # If nest success or unknown fate -> Create only one nest interval between FirstPresent and LastPresent
  if (gsgo_df$fate[i]== 0|is.na(gsgo_df$fate[i])) {
    ID.list[[i]]$survive <- 1
    ID.list[[i]]$exposure <- gsgo_df$LastPresent[i] - gsgo_df$FirstFound[i]
  }  
  # If nest failure -> Create two visit intervals:
  # First interval: between FirstFound and LastPresent
  # Second interval: between LastPresent and LastChecked
  if (is.na(gsgo_df$fate[i])==F) {
    if (gsgo_df$fate[i]== 1) {
      ID.list[[i]]$survive[1] <- 1
      ID.list[[i]]$survive[2] <- 0
      ID.list[[i]]$exposure[1] <- gsgo_df$LastPresent[i] - gsgo_df$FirstFound[i]
      ID.list[[i]]$exposure[2] <- gsgo_df$LastChecked[i] - gsgo_df$LastPresent[i]
    }
  }
}
rm(nr)

# Create empty dataframe to store data in ID.list
shaffer <- data.frame(id=as.character(),year=as.integer(),
                      site=as.character(), exposure=as.numeric(), survive=as.integer(),
                      UTM.E=as.numeric(),UTM.N=as.numeric(),habitat=as.character(),
                      issue=as.character())
# Merge together data in ID.list in one dataframe
for (i in 1:length(ID.list)) {
  if (is.null(nrow(ID.list[[i]]))==FALSE) {
    if (nrow(ID.list[[i]]) > 0) shaffer <- rbind(shaffer, ID.list[[i]])
  }
}
# In this dataframe named "shaffer", there is one or two lines per nest
# Each line for a nest represent a nest visit interval during which nest fate did not change
# For each line, the exposure time represent the number of days in the nest visit interval
# For each line, the survive variable indicates the nest fate at the end of the interval
#   survive = 1 means the nest survived at the end of the nest visit interval
#   survive = 0 means the nest did not survive at the end of the nest visit interval
# One line for successful nest or nest with unknown fate and two lines for 

# REMOVE LINES WITHOUT EXPOSURE TIME (WITHOUT MONITORING, EXPOSURE == 0 or NA)
shaffer <- shaffer[shaffer$exposure>0,]
shaffer <- shaffer[is.na(shaffer$exposure)==F,]
hist(shaffer$exposure)

#### ADD VARIABLES ####
shaffer$fyear <- as.factor(as.character(shaffer$year)) # Year as a factor

######################################## HERE I AM ###############################
# IMPORT VARIABLES
covariates <- read.delim("csv/covariates.csv",sep=";")
shaffer[names(covariates)[-1]] <- NA # creates new columns for covariates
for (i in 2:length(names(covariates))){
  for (j in 1:length(covariates[,"Year"])){
    shaffer[,names(covariates)[i]][shaffer$year == covariates[,"Year"][j]] <- covariates[,names(covariates)[i]][covariates$Year == covariates[,"Year"][j]]
  }
}
rm(ID.list,i,j)
row.names(shaffer) <- NULL

#### SAVE FILE ####
write.csv2(shaffer, file = "output/gsgo_shaffer.csv", row.names = F)