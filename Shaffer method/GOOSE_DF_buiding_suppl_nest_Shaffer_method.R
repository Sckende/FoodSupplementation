#### Database building with one row for each per nest ########
###### Supplemented nests - 2017 ###########
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()

# File with supplemented nests
g <- read.csv("GOOSE_MARK_all_SUPPL_nests_all_years.txt", sep = "\t", h = T)[,-c(3, 4, 6:8, 15:18)]
names(g)
summary(g)

# File with detailled nest monitoring in colony - 2017
t <- read.table("GOOSE_GSGO_2017.txt", h = T, sep = "\t")
head(t)

# Check if all ID are unique in the dataframe 
length(unique(g$ID)) == dim(g)[1]

# Deletion of unused row - SUPPL == WF | PRED_BEF_SUPPL
g <- droplevels(g[g$SUPPL != "WF" & g$SUPPL != "PRED_BEF_SUPPL",])


