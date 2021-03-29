install.packages("learnPopGen")
install.packages("phytools")
install.packages("coala")
library("coala")
library("learnPopGen")
library("phytools")
coalescent.plot()
#1)10 alleles that can be modified using n=
#2)After 10 generations
#3)2
#4)If an allele is in an individual who does not reproduce a lot then it will go to extinction.
#5)Yes
model<- coal_model(sample_size=5, loci_number=10, loci_length=500, ploidy=2)+
  feat_mutation(10)+
  feat_recombination(10)+
  sumstat_trees()+
  sumstat_nucleotide_div()
stats<-simulate(model, nsim=1)
Diversity<-stats$pi
Nloci<-length(stats$trees)
t1<-read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
#6) because there are two alleles for each individual
Agel<-max(nodeHeights(t1))
t2<-read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
#7) No
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1, t2)
t1_1<-read.tree(text=stats$trees[[1]][1])
t1_2<-read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)
for(locus in 1:Nloci) {
  ntrees<-length(stats$trees[[locus]])
  for(n in 1:ntrees)  {
    if(locus==1 && n==1)  {
      outPhy <-read.tree(text=stats$trees[[locus]][n])
    }  
    else  {
      outPhy <-ape :::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
    }
  }
}
par(mfrow=c(1,1))
densityTree(outPhy)
model3<-coal_model(10, 50)+
  feat_mutation(par_prior("theta", sample.int(100,1)))+
  sumstat_nucleotide_div()
stats<-simulate(model3, nsim=40)
mean_pi<- sapply(stats, function(x) mean(x$pi))
theta<- sapply(stats, function(x) x$pars[["theta"]])
