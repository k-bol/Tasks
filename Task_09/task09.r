setwd("C:\\Users\\kabol\\Desktop\\Evolution\\Tasks\\Task_09")
library("phytools")
#Question 1-3
trees<-list()
births<- c()
Fractions<-c()
for(i in 1:100) {
  births[i] <- runif(1)
  Fractions[i] <- runif(1)
  trees[[i]] <- pbtree(b = births[i], d = (births[i] * Fractions[i]), n = 100,
                       nsim = 1)
}
trees
trees[[i]]
plot(trees[[i]])
library("geiger")
install.packages("TreeTools")
#Question 4
library("TreeTools")
Y
tips<-sapply(trees, NTip)
logtips<-log(tips)
diversification<-sapply(trees, bd.ms)
plot(diversification, logtips, xlab="net diversification", ylab="log of total number of tips")
abline(lm(diversification~logtips), col="red")
#positive correlation
cor(diversification, logtips)
#positive correlation
#Question 5
speciation<-sapply(trees, bd.km)
i<-1
numtips<-c()
avgBL<- c()
for(i in 1:length(trees)){
  # choose tree
  y<-trees[[i]]
  #find # of tips
  numtips[i]<-Ntip(y)
  #find average branch length
  avgBL[i]<-mean(y$edge.length)
}
plot(speciation, avgBL, xlab="speciation rate", ylab="average branch length")
#branch length is inversely proportional to speciation rate
#Question 6
cor(speciation, avgBL)
#correlation was -0.5
#QUestion 7
which.max(tips)
bigTree<-trees[[66]]
plot(bigTree)
rates<-c()
traits<-list()
for(i in 1:100){
  rates[i]<-runif(1)
  traits[[i]]<-fastBM(tree=bigTree, sig2=rates[i])
}
#Question 8
avgtrait<-sapply(traits, mean)
avgtrait
avgrate<-sapply(rates, mean)
avgrate
correlation<-cor(avgtrait, avgrate)
print(correlation)
plot(avgrate, avgtrait)
abline(lm(avgrate~avgtrait), col="purple")
#Positive correlation between traits and rates 
#Question 9
vartraits<-sapply(traits, var)
cor(vartraits, rates)
#Question 10
trait1<-traits[1]
trait1
trait2<-traits[2]
trait2
traitmat<-cbind(traits[[1]], traits[[2]])
traitmat
var(traitmat)
cor(traitmat[,1], traitmat[,2])
#Not significant because the correlation is almost zero. 
plot(traitmat[,1], traitmat[,2])
abline(lm(traitmat[,1]~traitmat[,2]), col="red")
