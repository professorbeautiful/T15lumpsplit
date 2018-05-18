# blank is for code sections, witha run button
# QA is for a question/or/aha
# if(sequenceType=="") {
#   x <- get('x', envir=envNextNumber) + 1
#   assign('x', x, envir = envNextNumber)
# } else {
#cat("envirname = ", envirname, '\n')getSequenceEnvironment = function(sequenceType=""){


getSequenceEnvironment = function(sequenceType=""){
  envirname = paste0('envNextNumber', sequenceType)
  if(!exists(envirname, where = 1) ) {
    assign(envirname, new.env(), pos=1, immediate=TRUE)
    assign('x', 0, envir=get(envirname, pos=1), immediate=TRUE)
    #cat("Assigning initial counter 0 to ", envirname, '\n')
  }
  #print(paste('  object ', envirname, ' is at', find(envirname)))
  return(get(envirname, pos=1))
}

getSequenceLength = function(sequenceType=""){
    get('x', envir=getSequenceEnvironment(sequenceType))
}

nextNumber = function(sequenceType=""){
  x <- getSequenceLength(sequenceType) + 1
  assign('x', x, envir = getSequenceEnvironment(sequenceType))
  # }
  return(x)
}
