install.packages("ape")
library("ape")
library("phytools")
text.string<-
  "(((((((cow, pig,), whale), (bat,(lemur,human))), (robin, iguana)), coelacanth), (gold_fish, trout)), shark);" 
vert.tree<-read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(frams="circle", bg='white', cex=1)
#1)The human is more related tot he goldfish than the shark.
vert.tree
#2)No
str(vert.tree)
tree<-read.tree(text="(((A,B), (C,D)), E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge
AnolisTree<- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", tim=c(0,50), xlim=c(0,6))
tipEdges<- which(AnolisTree$edge[,2]<=Ntip(AnolisTree))
Lengths<- AnolisTree$edge.length
names(Lengths)<- AnolisTree$tip.label
names(Lengths)[which(Lengths==min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs<-sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
?plot.phylo
#Question 3:
tree<-read.tree(text='(((A,B), (C,D)), E);')
plot.phylo(tree, type='phylogram', show.tip.label=FALSE, edge.color='blue')
#Question 4:
plot.phylo(tree, type='radial')
#Question 5:
plot(AnolisTree, cex=0.25)
#Questions 6-8
Labs<-sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
which(Lengths==min(Lengths))
AnolisTree2<-drop.tip(AnolisTree, 'Anolis_occultus')
plot(AnolisTree2, cex=0.25)
ltt(AnolisTree)
abline(0,1, lwd=2, col='red', lty=2)
#The line never goes down, it just continuously goes up and slowly gets flatter. This means that the lizzards will eventually converge. 
#Question 10:
fit.bd(AnolisTree, rho=0.2)
