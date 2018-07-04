### R code from vignette source 'qvalue.Rnw'

### http://genomine.org/papers/Storey_FDR_2011.pdf

###  Roger Day modified the demo; added a graph, and 
###  from chunk 2 onward, renamed hedenfalk to hedPvalues.

###################################################
### code chunk number 1: input
###################################################
library(qvalue)
data(hedenfalk)
length(hedenfalk)
str(hedenfalk)

hedPvalues = hedenfalk$p


###################################################
### code chunk number 2: qvalue.Rnw:83-84
###################################################
hist(hedPvalues)


###################################################
### code chunk number 3: qvalue.Rnw:92-93
###################################################
qobj <- qvalue(hedPvalues)
qobj$pi0

###################################################
###  Roger Day inserted chunk
###################################################
theDensity = density(hedPvalues, from=0, to=1, adjust = 0.2)
plot(theDensity, col="lightblue", type="h", ylim=c(0,4),
     main="", xlab="P-value")
lines(seq(0, 1, length=1000), rep(qobj$pi0, 1000), type="h", col="#00000033")
mtext("hedPvalues P-values, with rectangular null hypothesis portion", line=2)
mtext(parse(text=paste("pi[0] ==", round(qobj$pi0,3))))
text(x = 0.02, y = -0.3, cex=1.5, expression(bold(alpha)), xpd=NA, col="red")
argmin = function(v, target=0)
  sapply(target, function(target)which(abs(v-target) == min(abs(v-target))[1]))
points(x = 0.02, y=theDensity$y[ argmin(theDensity$x, target=0.02) ], col="red", type="h")


###################################################
### code chunk number 4: qvalue.Rnw:96-97
###################################################
hist(qobj$qvalues)


###################################################
### code chunk number 5: qvalue.Rnw:105-106
###################################################
summary(qobj)


###################################################
### code chunk number 6: qvalue.Rnw:119-120
###################################################
plot(qobj)


###################################################
### code chunk number 7: qvalue.Rnw:131-132
###################################################
write.qvalue(x = qobj, file = "qobj.out.txt")

