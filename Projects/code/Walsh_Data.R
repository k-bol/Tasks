setwd("C:\\Users\\kabol\\Desktop\\Evolution\\Tasks\\Projects\\data")
install.packages("readxl")
library("readxl")
Walsh_Data<-read_excel("Walsh Brain Size.xlsx")
install.packages("ggpubr")
library("ggpubr")
ggscatter(Walsh_Data, x= "Population", y="In brain",
          add="reg.line", conf.int=TRUE, 
          cor.coef=TRUE, cor.method="pearson",
          xlab="Predation Level", ylab="Brain Weight")
colnames(Walsh_Data)
length(Walsh_Data)
plot(1,1, type="n", xlim=c(R, HP), ylim=c(0,2.2))
head(Walsh_Data, 172)
cor.test(Population$In_brain, Population$River)
data(Walsh_Data)
library(readxl)
Walsh_Data<-read_excel("Walsh Brain Size.xlsx")
setwd("C:\\Users\\kabol\\Desktop\\Evolution\\Tasks\\Projects\\data")
library(readxl)
Walsh_Data<-read_excel("Walsh Brain Size.xlsx")
head(Walsh_Data)
res<-cor.test(Walsh_Data$Population, Walsh_Data$`ln brain`)
Walsh_Data<- read_excel("Walsh Brain Size.xlsx")
head(Walsh_Data)
res<-cor.test(Walsh_Data$Population, Walsh_Data$`ln brain`)
res
res$estimate
y<- Walsh_Data$`ln brain`
x<-Walsh_Data$Population
plot(x,y, main="Predation vs. Brain Size",
     ylab = "Brain Size(mg)", xlab="Predation Pressure",
     pch=19, frame=FALSE)
plot(res, main="Predation vs. Brain Size", xlab="Predation", ylab="Brain Size(mg)", pch=19, frame=FALSE)
