# blank is for code sections, witha run button
# QA is for a question/or/aha
# if(sequenceType=="") {
#   x <- get('x', envir=envNextNumber) + 1
#   assign('x', x, envir = envNextNumber)
# } else {
#cat("counterName = ", counterName, '\n')getCounter = function(sequenceType=""){


getCounter = function(sequenceType=""){
  counterName = paste0('counter_', sequenceType)
  if(!exists(counterName, envir = envNumberSequences) ) {
    assign(counterName, 0, envir = envNumberSequences, immediate=TRUE)
  }
  #print(paste('  object ', counterName, ' equals ',
  #            envNumberSequences[[counterName]]))
  return(envNumberSequences[[counterName]])
}

getSequenceLength = function(sequenceType=""){
  counterName = paste0('counter_', sequenceType)
  getCounter(sequenceType)
}

nextNumber = function(sequenceType=""){
  counterName = paste0('counter_', sequenceType)
  x <- getCounter(sequenceType) + 1
  assign(counterName, x, envir = envNumberSequences)
  return(x)
}
