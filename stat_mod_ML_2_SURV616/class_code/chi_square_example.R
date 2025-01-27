#Drunkeness convictions in London example (HH, p 478)
library(HH)
library(epiR)
library(tidyverse)

drunk <- matrix(c(185, 207, 260, 180, 71,
         4, 13, 10, 7, 10), nrow= 2, ncol = 5, byrow = TRUE) |> 
  as.table()
names(dimnames(drunk)) <- c("Gender", "Age")
rownames(drunk) <- c("males","females") 
colnames(drunk) <- c('0-29','30-39','40-49','50-59','>=60')
prop.table(drunk)
addmargins(drunk)


chisq.test(drunk)

chisq.


bandage<-matrix(data=c(35,19,30,48),nrow=2)
bandage<-as.table(bandage)
epi.2by2(bandage)


prop.female <- drunk["female",]/apply(drunk,2,sum)
ages <- ordered(names(drunk), levels=names(drunk))

barchart(as.numeric(prop.female) ~ ages,
         horizontal=FALSE, origin=0,
         ylab="", main="proportion female",
         col=55)

drunk.er <- apply(drunk,1,sum)
drunk.ec <- apply(drunk,2,sum)
drunk.n <- sum(drunk.er)
drunk.e <- outer(drunk.er, drunk.ec) / drunk.n

chi2.ij <- (drunk -drunk.e)^2/drunk.e
chi2 <- sum(chi2.ij)

chi1.ij <- sqrt(chi2.ij) * sign(drunk -drunk.e)
chi1.ij

chi1.ij.vec <- as.vector(data.matrix(chi1.ij))

chi1 <- data.frame(chi=chi1.ij.vec,
                   sex=rep(dimnames(drunk)[[1]], 5),
                   age=rep(ages, rep(2,5)))

barchart(chi ~ age | sex, data=chi1, origin=0,
         horizontal=FALSE,
         par.strip.text=list(cex=1.2),
         scales=list(cex=1.2, alternating=F),
         layout=c(1,2),
         between=list(y=1),
         ylab=list(cex=1.2),
         xlab=list(cex=1.2),
         main=list("chi deviations for drunk.dat", cex=1.4),
)

chisq.test(data.matrix(drunk))
