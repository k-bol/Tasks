setwd("C:\\Users\\kabol\\Desktop\\Evolution\\Tasks\\Task_11")
x<-rnorm(100, mean=5, sd=2)
x
y<-(x*5)+2+(rnorm(100,0:0.1))
y
plot(x, y)
abline(lm(y~x), col="violet")
coef(lm(y~x))
dev.off()
z<- c()
x<-rnorm(100, mean=5, sd=2)
for(i in 1:100){
  z[i]<-runif(1)
  y<-(x*z[i])+2+(rnorm(100, 0:0.1))
  l<-coef(lm(z[1:100]~y))
}
l
plot(z[1:100], y)
abline(lm(y~z[1:100]), col="blue")
plot(c(z, -0.029))
install.packages("meme")
library("meme")
KB<- "https://media.npr.org/assets/img/2016/03/29/ap_090911089838_sq-3271237f28995f6530d9634ff27228cae88e3440.jpg"
Katherine_meme<-meme(KB, upper="when you had code errors for 5 hours", lower= "but only needed to respell one thing", col="white", size="1.0")
Katherine_meme
