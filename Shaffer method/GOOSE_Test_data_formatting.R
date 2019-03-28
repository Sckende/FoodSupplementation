####### Comparison between both data formatting for Shaffer method ################
setwd("C:/Users/HP_9470m/OneDrive - Université de Moncton/Doc doc doc/Ph.D. - ANALYSES/R analysis/Data")
list.files()

goo <- read.table("GOOSE-Test format data-Shaffer.txt", h = TRUE, sep = "\t")
head(goo)
summary(goo)

# Status code
goo$Status <- as.character(goo$Status)
goo$Status[goo$Status == "Yes"] <- "1"
goo$Status[goo$Status == "No"] <- "0"
goo$Status <- as.factor(goo$Status)

j <- unique(goo$No_ter)

long.data <- data.frame()
short.data <- data.frame()

for(i in j){
  data.1 <- goo[goo$No_ter == i,][-1,]
  data.1$EXPO <- diff(goo[goo$No_ter == i,]$Date)
  
  long.data <- rbind(long.data, data.1)
}

utils::View(long.data)
