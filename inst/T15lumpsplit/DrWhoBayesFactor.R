## DrWhoBayesFactor.R

DirNormalizer = function(aVec=DLdata+1, log=FALSE) {
  answer = sum(sapply(aVec, lgamma)) - lgamma(sum(aVec))
  if(! log) answer = exp(answer)
  answer
}

multiChoose = function(nVec=DLdata, log=FALSE) {
  answer = lgamma(sum(nVec)+1) - sum(sapply(nVec+1, lgamma))
  #- DirNormalizer(nVec+1, log=TRUE) +
  # lgamma(sum(nVec+1)) - lgamma(sum(nVec)+length(nVec))
  if(! log) answer = exp(answer)
  return(answer)
}
Beta = function(a,b)
  DirNormalizer(c(a,b))
mSplit = function(data=DLdata)
  exp(
    multiChoose(data, log=TRUE) +
      DirNormalizer(data+1, log=TRUE) -
      DirNormalizer(rep(1, length(data)), log=TRUE)
  )
mLump = function(data=DLdata) {
  rs = rowSums(data)
  cs = colSums(data)
  choose(sum(data), rs[1])*
      choose(sum(data), cs[1])*
      dhyper(data[1,1],data[1,2],data[2,1],data[2,2])*
      Beta(9,93)*Beta(6,96)
}
      #/Beta(1,1)/Beta(1,1) ##prior
DrWhoBayesFactor = function(data=DLdata) {
  mSplit(data)/mLump(data)
  #6/101/102/103
}
DrWhoBayesFactor()
