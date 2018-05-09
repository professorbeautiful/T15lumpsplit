## DrWhoBayesFactor.R

DirNormalizer = function(aVec, log=FALSE) {
  answer = sum(sapply(aVec, lgamma)) - lgamma(sum(aVec))
  if(! log) answer = exp(answer)
  answer
}

multiChoose = function(nVec, log=FALSE) {
  answer = lgamma(sum(nVec)+1) - sum(sapply(nVec+1, lgamma))
  #- DirNormalizer(nVec+1, log=TRUE) +
  # lgamma(sum(nVec+1)) - lgamma(sum(nVec)+length(nVec))
  if(! log) answer = exp(answer)
  return(answer)
}

mSplit = function(DLdata)
  exp(
    multiChoose(DLdata, log=TRUE) +
      DirNormalizer(DLdata+1, log=TRUE) -
      DirNormalizer(rep(1, length(DLdata)), log=TRUE)
  )
mLump = function(DLdata) {
  rs = rowsum(DLdata)
  cs = colsum(DLdata)
  choose(sum(DLdata), rs[1])*
      choose(sum(DLdata), cs[1])*
      dhyper(DLdata[1,1],DLdata[1,2],DLdata[2,1],DLdata[2,2])*
      Beta(9,93)*Beta(6,96)
}
      #/Beta(1,1)/Beta(1,1) ##prior
DrWhoBayesFactor = function(DLdata) {
  mSplit(DLdata)/mLump(DLdata)
  #6/101/102/103
}
