plotPlightPdarkPosterior = function(
  DLdata =
    matrix(c(3,2,5,90), nrow=2,
           dimnames = list( outcome=c("R","N"), feature=c("D","L"))),
  tau=1, phi=1, mu0=0.5,
  showPrior = TRUE, showPosterior=TRUE, showLikelihood=TRUE,
  showConfIntBinormal = FALSE,
  showS = TRUE,
  showL = TRUE,
  showW = TRUE,
  fudgeFactor = 0.001,
  addFudge = TRUE,
  ColorForPrior="orange",
  ColorForPosterior="blue",
  ColorForLikelihood="black"
) {
  par(pty='s')
  plot(0:1, 0:1, xlab = "Pr(R | D)", ylab = "Pr(R | L)", pch=" ",
       cex=2)
  abline(a=0, b=1, col='grey', lty=2, lwd=2)
  addCircledLetter = function(pointLocation, bg=NA,
                              col='black', circleColor='black',
                              size=0.05, pch, cex=1){
    symbols(add = TRUE,
            x=pointLocation[1],
            y=pointLocation[2],
            circles = size, col=circleColor, inches=F, bg=bg)
    points(x=pointLocation[1],
           y=pointLocation[2],
           pch = pch,
           cex=cex, col=col)
  }
  if(showS | showL)
    title('L = Lump, S = Split', col.main='red')
  if(showS) {
      ### careful.... rows are groups, columns outcomes.
    Spoint = c(x=DLdata['R', 'D']/sum(DLdata[ , 'D']),
               y=DLdata['R', 'L']/sum(DLdata[ , 'L']) )
    confInterval_D = binom.test(x = DLdata['R', 'D'],
                                n = sum(DLdata[ , 'D'])
    )$conf.int
    #print(confInterval_D)
    lines(confInterval_D, rep(Spoint[2], 2), lwd=3)
    confInterval_L = binom.test(x = DLdata['R', 'L'],
                                n = sum(DLdata[ , 'L'])
    )$conf.int
    #print(confInterval_L)
    lines(rep(Spoint[1], 2), confInterval_L, lwd=3)
    addCircledLetter(
      pointLocation = c(Spoint[1], Spoint[2]),
      pch='S', col='red', cex=2)
  }
  if(showL) {
    Lpoint = rep(times=2,
                 sum(DLdata[ 'R', ])/sum(DLdata) )
    confInterval = binom.test(x = sum(DLdata[ 'R', ]),
               n = sum(DLdata))$conf.int
    #print(confInterval)
    lines(confInterval, confInterval, lwd=3)
    addCircledLetter(
      pointLocation = c(Lpoint[1], Lpoint[2]),
      pch='L', col = 'red', cex=2)
  }
  if(showW) {
    BayesFactor = DrWhoBayesFactor(DLdata)
    BayesProbSplit = BayesFactor/(1+BayesFactor)
    Wpoint = Spoint*BayesProbSplit + Lpoint*(1-BayesProbSplit)
    addCircledLetter(pointLocation = c(Wpoint[1], Wpoint[2]), pch='W',
                     circleColor='red', bg=NULL, col='red')
  }
  #### bivariate normal contours ####
  logit.hat = logit(apply(DLdata, 'feature', function(r)r[1]/sum(r)))
  varhat = apply(DLdata, 'feature', function(r)sum(1/r))
  if(any(DLdata==0)) fudgeFactor = max(fudgeFactor, 0.001)
    # if wanted, or if needed (if any zero cells).
  DLdataFudged = DLdata + fudgeFactor
  logit.hat.fudged = logit(apply(DLdataFudged, 'feature', function(r)r[1]/sum(r)))
  varhat.fudged = apply(DLdataFudged, 'feature', function(r)sum(1/r))
  ### deltat method with protection from zero's.
  sig11 = sig12 = sig21 = matrix(c(tau+phi,tau,tau,tau+phi),nrow=2)
  sig22 = sig11 + diag(varhat)   ## marginal variance of the data?
  logit.prior.mean = c(mu0, mu0)
  ## always use fudged here:
  postmean.logit = logit.prior.mean +
    sig12%*%solve(sig22) %*% (logit.hat.fudged-logit.prior.mean)
  postmean.p = antilogit(postmean.logit)
  postvar.logit = sig11 - sig12%*%solve(sig22)%*%sig21
  Jac = function(p) p^(-1)+(1-p)^(-1)
  postvar.p = sapply(1:2, function(d) postvar.logit[d]/Jac(postmean.p)^2)
  confints.logit = sapply(1:2,
                         function(d)postmean.logit[d]
                         +c(-1,1)*postvar.logit[d]
  )
  confints.p = antilogit(confints.logit)
  cat('confints.p\n')
  print(confints.p)

  #### Prepare to plot contours ################

  svdSig11 = svd(sig11)
  sqrtSig11 = svdSig11$v %*% diag(sqrt(svdSig11$d)) %*% svdSig11$u
  svdPostvar.logit = svd(postvar.logit)
  sqrtPostvar.logit = svdPostvar.logit$v %*% diag(sqrt(svdPostvar.logit$d)) %*% svdPostvar.logit$u

  angles = seq(0,2*pi,length=100)
  qlevel = qchisq(p=0.99, df=2)
  circlepoints = cbind(cos(angles),sin(angles))
if(showPrior) {
    for (plevel in seq(.1,.9,.1)) {
      qlevel = qchisq(p=plevel, df=2)
      contourlevel1 = antilogit(logit.prior.mean
                                + (circlepoints*qlevel) %*% sqrtSig11)
      lines(contourlevel1, col=ColorForPrior)
      if(plevel==.5){
        contourlevel1 = antilogit( logit.prior.mean
                                   + (cbind(cos(angles),sin(angles)) * qlevel ) %*% sqrtSig11)
        xorder = order(contourlevel1[ ,1])
        lines(contourlevel1[xorder,1], contourlevel1[xorder,2],col = ColorForPrior, lty=2)
      }
    }
    symbols(add = TRUE, antilogit(mu0), antilogit(mu0),
           circles = 0.05, inches=F, bg='black') #ColorForPrior)
    points(antilogit(mu0), antilogit(mu0), pch = "M",
           cex=1, col=ColorForPrior)
  }
  if(showPosterior) {
    for (plevel in seq(.1,.9,.1)) {
      qlevel = qchisq(p=plevel, df=2)
      contourlevel2 = antilogit(matrix(c(postmean.logit),nrow=100,ncol=2, byrow=T)
                                + (circlepoints * qlevel ) %*% sqrtPostvar.logit)
      lines(contourlevel2, col=ColorForPosterior, lty=1, lwd=1)
      if(plevel==.5){
        contourlevel2 = antilogit(matrix(c(postmean.logit),nrow=100, ncol=2, byrow=T)
                                  + (cbind(cos(angles),sin(angles)) * qlevel ) %*% sqrtPostvar.logit)
        xorder = order(contourlevel2[ ,1])
        lines(contourlevel2[xorder,1], contourlevel2[xorder,2], col = ColorForPosterior,lty=2)
      }
    }
    #points(Spoint[1], Spoint[2], pch='S', cex=2, col='black')
    #points(Lpoint[1], Lpoint[2], pch='L', cex=2, col='black')
#  if(showLikelihood) {
#    argmin = function(v, target=0) which(abs(v-target) == min(abs(v-target))[1])
#     probs = seq(0.01, 0.99, by = 0.01)
#     normalizedLikelihoodDark = dbinom(x = DLdata["dark", "R"], size=sum(DLdata["dark", ]),
#                                       prob=probs)
#     normalizedLikelihoodDark = normalizedLikelihoodDark/sum(normalizedLikelihoodDark)/0.01
#     cumLikelihoodDark = cumsum(normalizedLikelihoodDark)
#     normalizedLikelihoodLight = dbinom(x = DLdata["light", "R"], size=sum(DLdata["light", ]),
#                                       prob=probs)
#     normalizedLikelihoodLight = normalizedLikelihoodLight/sum(normalizedLikelihoodLight)/0.01
#     cumLikelihoodLight = cumsum(normalizedLikelihoodLight)
#     for (plevel in seq(.1,.9,.1)) {
#       D1 = cumLikelihoodDark[argmin(cumLikelihoodDark, plevel)]
#       D2 = cumLikelihoodDark[argmin(cumLikelihoodDark, 1 - plevel)]
#       L1 = cumLikelihoodLight[argmin(cumLikelihoodLight, plevel)]
#       L2 = cumLikelihoodLight[argmin(cumLikelihoodLight, 1 - plevel)]
#       cat(paste("Likelihood ", plevel, D1, D2, L1, L2, "\n"))
#       lines(c(D1, D1, D2, D2), c(L1, L2, L2, L1), col= ColorForLikelihood)
#      }
#  }
  #points(antilogit(logit.hat)[1], antilogit(logit.hat)[2],
  #       pch = "X", cex=1)
    # points(antilogit(postmean.logit[1,1]),
    #        antilogit(postmean.logit[2,1]), pch = "M",
    #        cex=2, col = ColorForPosterior)
    pointLocation = antilogit(
      c(postmean.logit[1,1], postmean.logit[2,1]))
    addCircledLetter(pointLocation,  bg='lightgrey',
                     pch = "M", cex=1, col=ColorForPosterior)
    mtext(text = paste("posterior mean for Pr(R | D) = ",
                       round(digits=2, postmean.p[1])),
          side=3, cex=1, line = 1,
          col = ColorForPosterior)
    text(x = postmean.p[1], y = 1, pos = 3,
         labels = expression("\u2193"),   ### only \u2193 works.
         # HTML('<pre class="r"><code>cat(&quot;\U2660   \U2665  \U2666  \U2663&quot;)</code></pre>
         #                        <p>♠ ♥ ♦ ♣</p>'"︎
         # DOWNWARDS BLACK ARROW  ## The fat one doesn't show up.
         #        Unicode: U+2B07 U+FE0E, UTF-8: E2 AC 87 EF B8 8E︎",
         col='blue', xpd=TRUE)
    abline(v=postmean.p[1],col=ColorForPosterior, lty=2, lwd=2)
  }
  if(showConfIntBinormal){
    lines(confints.p[ , 1], rep(postmean.p[2], 2), lwd=3)
    lines(rep(postmean.p[1], 2), confints.p[ , 2],lwd=3)

  }
  return(invisible(list(postmean.logit=postmean.logit,
                          postmean.p=postmean.p,
                          postvar.logit=postvar.logit,
                          postvar.p=postvar.p,
                          confints.p=confints.p)))
}


