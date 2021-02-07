setwd("C:\\Users\\kabol\\Desktop\\Evolution\\Tasks\\Task_02")
Data <- read.csv("http://jonsmitchell.com/data/beren.csv", stringsAsFactors=F)
write.csv(Data, "rawdata.csv", quote=F)
length(data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Feeds <- which(Data[,9] == "bottle")
berenMilk <- Data[Feeds, ]
head(berenMilk)
Feeds <- which(Data[, "event"] == "bottle")
Feeds <- which(Data$event == "bottle")
head(Feeds)
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse= "-"))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == "birth")]
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age),]
write.csv(beren3, "beren_new.csv", quote=F, row.names=FALSE)
Question 1: The first hypothesis would indicate that the more beren drinks the more his weight woug go up, it would make more sense to say that his weight would increse with his age. The second hypotesis would also not work because there is no correlation or reason to think that the amount one drinks and the amount one sleeps has any relationship whatsoever. 
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value [Feeds])
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$age [Feeds], var)
totalFeed <-tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value [Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov (beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot(beren3$value[Feeds]~beren3$caregiver[Feeds], xlab="who gave the bottle", ylab = "amount of milk consumed (oz)")
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col="red")
pdf("r02b-totalMilkByDay.pdf", height=4, width=4)
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col="red")
dev.off()
Question 2: The graph is impossible to interpret for multiple reasons, the first being that the plotted points re so smashed and close together it makes the graph difficult to read. Also, there is not a very good way to determine if there is any real correlation between the two variables from looking at the graph. 
beren3 <-Data
beren4<- beren3[order(beren3$age),]
write.csv(beren4, "beren_new.csv", quote=F, row.names=FALSE)
Naps <- which(beren4$event=="nap")
startHour <-(beren4$start_hour)
startMin <- (beren4$start_min)
endHour<- (beren4$end_hour)
endMin <- (beren4$end_minute)
beren4$nap <- (((endHour-startHour)*60)- (endMin-startMin))
beren4
totalNaps<-tapply(beren4$nap, beren4$age, sum)
totalNaps
par(las=1,mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalNaps)),totalNaps,type="b",pch=16,xlab="age in days",ylab="time in min")
