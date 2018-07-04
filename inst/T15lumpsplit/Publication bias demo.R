# From qvalue analysis of hedPvalues data

#Proportion of results significant:
mean(qobj$pvalues < 0.05)
# We use the hedPvalues pi0  for the proportion of true null hypotheses.
pi0 = qobj$pi0
# Proportion of Type I errors
alpha = 0.05
prTypeI = pi0 * alpha
# We use the Dickersin estimate to say that
#     Pr(a P greater than 0.05 is included) = 1/3.
#Dickersin, K.; Chan, S.; Chalmers, T. C. et al. (1987).
#"Publication bias and clinical trials". Controlled Clinical Trials 8 (4):
#343–353. doi:10.1016/0197-2456(87)90155-3.
# "However statistically significant
# results have been shown to be three times more likely to be published compared
# to papers with null results."

prNonsignifIncluded = 1/3

theDensity = density(hedPvalues, from=0, to=1, adjust = 0.2)


plot(
  theDensity,
  col = "lightblue",
  type = "h",
  ylim = c(0, 4),
  main = "",
  xlab = "P-value"
)
points(theDensity$x [theDensity$x < 0.05], theDensity$y [theDensity$x < 0.05],
       col='green', type='h')
points(seq(0, 0.05, length=1000), rep(pi0, 1000), type="h", col="darkgreen")
lines(seq(0.05, 1, length = 1000),
      rep(pi0, 1000),
      type = "h",
      col = "#22222233")


####

#### Reporting of P values
points(theDensity$x [theDensity$x > 0.05], 2/3 * theDensity$y [theDensity$x > 0.05],
       col=addOpacity('white'), type='h')
#lines(x=c(0.05, 1), y= 0.66 * c(1/3, 1/3))
# text(x=0.5, y=0.66*1/3 /2, "non-significant, reported", col="white", font=2)
# text(x=0.5, y=0.66 - 0.66*1/3 /2, "non-significant, NOT reported", col="white", font=2)
boxheight = 0.3
boxwidth = 0.2

tableXpos = 0.4
symbols(x = tableXpos, y=3, rectangles=matrix(c(boxwidth, boxheight), ncol=2), add = TRUE, inches=F)
symbols(x = tableXpos + boxwidth, y=3, rectangles=matrix(c(boxwidth, boxheight), ncol=2), add = TRUE, inches=F)
symbols(x = tableXpos + 2*boxwidth, y=3, rectangles=matrix(c(boxwidth, boxheight), ncol=2), add = TRUE, inches=F)
symbols(x = tableXpos, y=3 + boxheight, rectangles=matrix(c(boxwidth, boxheight), ncol=2), add = TRUE, inches=F)
symbols(x = tableXpos + boxwidth, y=3 + boxheight, rectangles=matrix(c(boxwidth, boxheight), ncol=2), add = TRUE, inches=F)
symbols(x = tableXpos + 2*boxwidth, y=3 + boxheight, rectangles=matrix(c(boxwidth, boxheight), ncol=2), add = TRUE, inches=F)
text(x = tableXpos, y = 3 + boxheight + 0.4, "signif\nP < 0.05")
# text(x = tableXpos + boxwidth, y = 3 + boxheight + 0.4, "NOT signif\nreported")
# text(x = tableXpos + 2*boxwidth, y = 3 + boxheight + 0.4, "NOT signif\nNOT reported")
text(x = tableXpos - 0.2, y = 3 + boxheight, "H0 false")
text(x = tableXpos - 0.2, y = 3 , "H0 true")

signif = hedPvalues[hedPvalues < 0.05]
nSignif = length(signif)
nonsignif = hedPvalues[hedPvalues>0.05]
nNonSignif = length(nonsignif)
nTrueH0 = pi0 * length(hedPvalues)
nTrueH0Rejected = nTypeI = alpha * nTrueH0 ## True H0 is rejected.
nTrueH0notRejected = (1-alpha) * nTrueH0 ## True H0 is NOT rejected.
nTrueH0notRejectedButReported = nTrueH0notRejected * prNonsignifIncluded ## True H0 is NOT rejected.
nTrueH0notRejectedNotReported = nTrueH0notRejected * (1-prNonsignifIncluded) ## True H0 is NOT rejected.

nFalseH0 = (1-pi0) * length(hedPvalues)
nFalseH0isRejected = nSignif - nTypeI
nFalseH0notRejected = nFalseH0 - nFalseH0isRejected
nFalseH0notRejectedButReported = nFalseH0notRejected * prNonsignifIncluded
nFalseH0notRejectedNotReported = nFalseH0notRejected * (1-prNonsignifIncluded)

H0row = c(nTypeI, nTrueH0notRejectedButReported, nTrueH0notRejectedNotReported)
HArow = c(nFalseH0isRejected, nFalseH0notRejectedButReported, nFalseH0notRejectedNotReported)
confusionCounts = rbind(H0row, HArow)
confusionMatrix = confusionCounts/length(hedPvalues)
sum(confusionMatrix)  ### OK.  3170

FalseDiscoveryRate = 1 - nFalseH0isRejected/nSignif  ### Round AFTER division!
typeIerrorAmongFiltered = nTrueH0Rejected /
  (nTrueH0Rejected + nTrueH0notRejectedButReported)
title(paste("FalseDiscoveryRate = ", round(FalseDiscoveryRate, digits = 2),
            "\nType I error = 0.05 (full reporting), ",
            round(typeIerrorAmongFiltered, digits = 2),
            " (pub bias)")
)

for(iRow in 1:2) { for(iCol in 1:3)
  text(x = tableXpos+(iCol-1)*boxwidth, y = 3+(iRow-1)*boxheight,
       round(confusionMatrix[iRow, iCol], digits = 2))
  }
lines(x=c(0.025, tableXpos), y=c(2, 3 + boxheight), lwd=2, lty=2);
lines(x=c(0.025, tableXpos), y=c(0.3, 3), lwd=2, lty=2);  #text(x=0.4, y=2.3, labels=" false discovery: Type I error=0.05", adj=0)


text(x = tableXpos + boxwidth * 3/2, y = 3 + boxheight + 0.4, "NOT signif")
text(x = tableXpos + boxwidth, y = 3 - 0.3, "reported")
lines(x=c(0.2, tableXpos + boxwidth), y=c(1, y = 3 - 0.3), lwd=2, lty=2);  #text(x=0.4, y=2.7, labels=" missed discovery: Type II error", adj=0)
text(x = tableXpos + 2*boxwidth, y = 3 - 0.3, "NOT reported", bg="lightblue") #bg doesn't work.
lines(x=c(0.2, tableXpos + 2*boxwidth), y=c(0.7, y = 3 - 0.3), lwd=2, lty=2);  #text(x=0.4, y=2.7, labels=" missed discovery: Type II error", adj=0)

#mtext("P-values when all are published", line = 2)
text(x=0.56, y=0.4, adj=c(0.5, 1), cex=1.7, col="black", font=2,
       labels = bquote(
         hat(pi) ==
         .('66% of null hypotheses are true'))) 
text(x=-0.12, y=0.66, "0.66", xpd=NA, font=2, cex=2)
lines(x=c(-0.03, 0), y=c(0.66, 0.66), lwd=3)
#text(x=0.4, y=3.8, labels=" true discovery", adj=0)

# lines(x=c(0.1, 0.4), y=c(0.3, 2));  text(x=0.4, y=2, labels=" H0 true, not rejected", adj=0)


#####

signif = hedPvalues[hedPvalues < 0.05]
nonsignif = hedPvalues[hedPvalues>0.05]
filteredNonsignif = sample(nonsignif, size = prNonsignifIncluded*length(nonsignif))
filteredSample =   c(signif, filteredNonsignif)
theDensityFiltered = density(filteredSample, from=0, to=1, adjust = 0.02)

newPi0 = length(filteredNonsignif) / length(filteredSample)

nullHypotheses = pi0 * length(hedPvalues)
nullHypothesesReportedSignificant = nullHypotheses * 0.05
nullHypothesesInFiltered = (0.05 * 1.00 + 0.95 * prNonsignifIncluded) * nullHypotheses
typeIerrorAmongFiltered =   nullHypothesesReportedSignificant/ nullHypothesesInFiltered
