install.packages("diversitree")
library("diversitree")
transition_0to1<-0.1
transition_1to0<-0.1
speciation_0<-0.2
extinction_0<- 0.1
speciation_1<- 0.2
extinction_1<-0.1
maxN<-1e3
maxT<- 50
Pars<- c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)
simTree<- tree.bisse(Pars, max.taxa=maxN, max.t=maxT)
str(simTree)
?tree.bisse
stateTable<- table(simTree$tip.state)
stateTable / sum(stateTable)
setwd("C:\\Users\\kabol\\Desktop\\Evolution\\Tasks\\Task_10")
frequencies<- c("State 0", "State 1")
Colors<- c("blue", "yellow")
Data<- matrix(c(0.60, 0.61, 0.58, 0.43, 0.23, 0.45, 0.3, 0.4, 0.8, 0.78), nrow=2, ncol=10, byrow=TRUE)
Data
Difference<- c(0.13, 0.09, 0.04, 0.03, 0.02)
Freq1<- c(0.60, 0.61, 0.58, 0.43, 0.23)
Freq2<- c(0.45, 0.3, 0.4, 0.8, 0.78)
pdf("Question.1.pdf", height=5, width=5)
barplot(Data, main= "Changes in Frequencies of States Based on Variation of R Values", xlab= "Diverisification Diff", ylab= "Frequency", col=c("blue", "yellow"))
legend("top", frequencies, fill="blue", "yellow")
dev.off()
frequencies<- c("state 0", "state 1")
colors<- c("purple", "pink")
Data<- matrix(c(0.67, 0.9, 0.83, 0.81, 0.97, 0.99, 0.45, 0.54, 0.34, 0.85, 0.99, 0.56, 0.34, 0.77, 0.87, 0.21, 0.81, 0.54, 0.34, 0.67), nrow=2, ncol=10, byrow=TRUE)
Data
Difference<- c(0.66, 0.44, 0.22, 0.33, 0.55)
pdf("Question 2.pdf", height=9, width= 9)
barplot(Data, main= "Closeness to Zero State 1", xlab="Difference in Diverisfication Rate", ylab="Frequencies", col=c("Purple", "Pink"))
head(Data)
Freq1_Trial1<- Data[,2]
Freq1_Trial2<-Data[,5]
Freq1_Trial3<- Data[,8]
Variance1<-var(Freq1_Trial1)
Variance2<- var(Freq1_Trial2)
Variance3<-var(Freq1_Trial3)
Variance1
Variance2
Variance3
Variancematrix<- c(Variance1, Variance2, Variance3)
Variancematrix
Trial<- c(1,2,3)
Trial
pdf("Qestion 3.pdf", height=8, width=8)
barplot(Variancematrix, main="Variance of Frequency 1 per Trial", xlab= "Trial", ylab= "Variance", ylim=c(0, 0.5), col="orange")
dev.off()
head(Data)
Freq_0<- Data[,2]
Freq_0
NDR_0<- Data[,1]
pdf("MyTrend1.pdf", height=8, width=8)
plot(NDR_0, Freq_0, xlab="Net Diversification Rate", ylab="Frequency", main= "Net Diverisifications on Frequencies")
abline(lm(Freq_0~NDR_0), col="red", lty="solid")
dev.off()
Freq_1<- Data[,7]
NDR_1<-Data[,5]
pdf("MyTrend2.pdf", height=8, width=8)
plot(NDR_0, Freq_0, xlab="Net Diversification Rate", ylab="Frequency", main= "Net Diverisifications on Frequencies")
abline(lm(Freq_0~NDR_0), col="violet", lty="solid")
dev.off()
#Q1: Net diversification and frequency one showed an inverse relationship.
#Q2: State 1 never reached 0. 
#Q3: When the parameters were the same for both states the variation decreased after trial 1. 
#Q4: