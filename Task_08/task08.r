setwd('C:\\Users\\kabol\\Desktop\\Evolution\\Tasks\\Task_08')
library("phytools")
tree<-read.tree("https://jonsmitchell.com/data/anolis.tre")
plot(tree, type="fan")
#Question 1: 82 tips, yes branch lengths are present. 
data<-read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors = F, row.names=1)
#Question 2: "data" is species of lizards and the dimensions are 100. 
svl<-setNames(data$svl, rownames(data))
Ancestors<-fastAnc(tree, svl, vars=TRUE, CI=TRUE)
Ancestors
#Question 3: Values are stores in ace and CI95 is a confidence interval of 95% for ancestral state estimates.
#Question 4: the function reboots at internal modes and the ontrasting state is at the root. 
par(mar=c(0.1, 0.1, 0.1, 0.1))
plot(tree, type="fan", lwd=2, show.tip.label = F)
tiplabels( pch=16, cex =0.25* svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)
obj<-contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7,0.9))
fossilData<- data.frame(svl=log(c(25.4, 23.2, 17.7,19.7,24,31)), tipl=c("Anolis_aliniger","Anolis_aliniger", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
fossilData
#Question 5:
fossilNodes<-c()
nodeN<-c()
{
    for(i in 1:nrow(fossilData))
    i<-1
    if(i==1){
      print(Ancestors)
    }
  }
Node <- fastMRCA(tree, fossilData [i,"tip1"], fossilData[i,"tip2"])
Node
fossilNodes[i]<-fossilData[i, "svl"]
fossilNodes[i]
nodeN[i]<-Node
names(fossilNodes)<-nodeN
Ancestors_withFossils<-fastAnc(tree, svl, anc.states=fossilNodes, C1=TRUE, var=TRUE)
Ancestors_woFossils<-fastAnc(tree, svl, CI=TRUE, var=TRUE)
plot(Ancestors_withFossils$ace, Ancestors_woFossils$ace, xlab="fossils", ylab="no fossils")
#Question 7: Fossils increased estimated ancestral size.
#Question 8-10:
install.packages("geiger")
library("geiger")
fitContinuous(tree, svl, model="EB")
fitContinuous(tree, svl, model="OU")
fitContinuous(tree, svl, model="BM")
