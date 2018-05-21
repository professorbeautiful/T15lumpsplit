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
Beta = function(a,b)
  DirNormalizer(c(a,b))
mSplit = function(theData=rValues$DLdata)
  exp(
    multiChoose(theData, log=TRUE) +
      DirNormalizer(theData+1, log=TRUE) -
      DirNormalizer(rep(1, length(theData)), log=TRUE)
  )
mLump = function(theData=rValues$DLdata) {
  rs = rowSums(theData)[1]
  cs = colSums(theData)[1]
  n = sum(theData)
  choose(sum(theData), rs[1])*
      choose(sum(theData), cs[1])*
      dhyper(theData[1,1],rs,n-rs,cs)*
      Beta(9,93)*Beta(6,96)
}
      #/Beta(1,1)/Beta(1,1) ##prior
DrWhoBayesFactor = function(theData=rValues$DLdata) {
  BF = mSplit(theData)/mLump(theData)
  cat("Bayes Factor", BF, '\n')
  BF
  #6/101/102/103
}
