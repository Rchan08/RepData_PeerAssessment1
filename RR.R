library(data.table)
library(knitr)

act<-read.csv("activity.csv",header = TRUE,stringsAsFactors = FALSE,na.strings = "NA")
act$date<-as.Date(act$date,"%Y-%m-%d")
act<-data.table(act)

pasosdia<-act[,sum(steps),by=date]
hist(pasosdia$V1,breaks=20,col=terrain.colors(length(unique(pasosdia$date))))

act[,mean(steps,na.rm = TRUE),by=date]
act[,as.double(median(steps,na.rm = TRUE)),by=date]

pasosinterv<-act[,mean(steps,na.rm = TRUE),by=interval]
plot(pasosinterv$interval,pasosinterv$V1,type="l")
mayor<-which.max(pasosinterv$V1)
pasosinterv[mayor,interval]

sum(complete.cases(act)==FALSE)
sust<-median(act$steps,na.rm = TRUE)
y<-act

for(i in 1:length(y$steps))
if(is.na(y$steps[i])){
  y$steps[i]<-sust
}

hist(y$steps,col="blue")
y[,mean(steps),by=date]
y[,median(steps),by=date]

for(i in 1:length(y$date)){
  if(weekdays(y$date[i])%in%c("sábado","domingo")){
    y$wd[i]<-"weekend"
  }else{
    y$wd[i]<-"weekday"
  }
}

y$wd<-factor(y$wd)

par(mfrow=c(2,1),mar=c(2,2,2,1))
plot(y[wd=="weekday",mean(steps),by=interval]$V1,type="l",col="red",
     main="Weekday")
plot(y[wd=="weekend",mean(steps),by=interval]$V1,type="l",col="blue",
     main="Weekend")
dev.off()


#lines(y[wd=="weekend",mean(steps),by=interval]$V1,col="blue")
#legend("topright",legend = levels(y$wd),lwd=2,col=c("red","blue"))
