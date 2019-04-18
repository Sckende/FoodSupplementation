


source("https://raw.githubusercontent.com/frousseu/UdeS/master/GStecher/newdata.R")

m<-cand.models[[4]]

nd<-newdata(model.frame(m)[,-1])


par(mfrow=c(2,2))
lapply(nd,function(i){
  plot(i[,1],plogis(predict(m,newdata=i))^27,type="l")
})





#plot(nd$NestAge$NestAge,plogis(predict(m,newdata=nd$NestAge))^27,type="l")
#plot(nd$SUPPL$SUPPL,plogis(predict(m,newdata=nd$SUPPL))^27,type="l")
