## DrWhoBayesFactor.R

DirNormalizer = function(aVec=rValues$DLdata+1, log=FALSE) {
  answer = sum(sapply(aVec, lgamma)) - lgamma(sum(aVec))
  if(! log) answer = exp(answer)
  answer
}

multiChoose = function(nVec=rValues$DLdata, log=FALSE) {
  answer = lgamma(sum(nVec)+1) - sum(sapply(nVec+1, lgamma))
  #- DirNormalizer(nVec+1, log=TRUE) +
  # lgamma(sum(nVec+1)) - lgamma(sum(nVec)+length(nVec))
  if(! log) answer = exp(answer)
  return(answer)
}
BetaNorm = function(a,b)
  DirNormalizer(c(a,b))
mSplit = function(theData=rValues$DLdata)
  exp(
    multiChoose(theData, log=TRUE) +
      DirNormalizer(theData+1, log=TRUE) -
      DirNormalizer(rep(1, length(theData)), log=TRUE)
  )
mLump = function(theData=rValues$DLdata, aR=1, aN=1, aD=1, aL=1) {
  nRD=theData['R','D']
  nND=theData['N','D']
  nR=sum(theData['R', ])
  nN=sum(theData['N', ])
  nD=sum(theData[ , 'D'])
  nL=sum(theData[ , 'L'])
  #rs = rowSums(theData)[1]  # R, N
  #cs = colSums(theData)[1]  # D, L
  n = sum(theData)
  choose(n, nR) *
    choose(n, nD) *
    dhyper(nRD, nR, nN, nD) *
    BetaNorm(nR+aR, nN+aN)*BetaNorm(nD+aD, nL+aL) /
    BetaNorm(aR, aN)*BetaNorm(aD, aL)
    # BetaNorm(9,93)*BetaNorm(6,96)
}
      #/Beta(1,1)/Beta(1,1) ##prior
DrWhoBayesFactor = function(theData=rValues$DLdata) {
  BF = mSplit(theData)/mLump(theData)
  #cat("DrWhoBayesFactor: Bayes Factor is", BF, '\n')
  BF
  #6/101/102/103
}

posteriorOdds = function(priorOdds, theData)
  priorOdds * DrWhoBayesFactor(theData)

oddsToProb = function(odds) odds/(1+odds)

posteriorProb = function(priorOdds, theData)
  oddsToProb(posteriorOdds(priorOdds, theData))

observeEvent(rValues$WhoPriorOdds,  theData=rValues$WhoPriorOdds) {
  rValues$WhoPosteriorOdds = posteriorOdds(WhoPriorOdds, WhoPriorOdds)
  rValues$WhoPosteriorProb = oddsToProb(rValues$WhoPosteriorOdds)

}
