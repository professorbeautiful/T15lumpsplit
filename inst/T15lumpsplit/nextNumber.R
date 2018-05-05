nextNumber = function(sequenceType=""){
  # blank is for code sections, witha run button
  # QA is for a question/or/aha
  # if(sequenceType=="") {
  #   x <- get('x', envir=envNextNumber) + 1
  #   assign('x', x, envir = envNextNumber)
  # } else {
  envirname = paste0('envNextNumber', sequenceType)
  #cat("envirname = ", envirname, '\n')
  if(!exists(envirname, where = 1) ) {
    assign(envirname, new.env(), pos=1, immediate=TRUE)
    assign('x', 0, envir=get(envirname, pos=1), immediate=TRUE)
    #cat("Assigning initial counter 0 to ", envirname, '\n')
  }
  #print(paste('  object ', envirname, ' is at', find(envirname)))
  envirNext = get(envirname, pos=1)
  x <- get('x', envir=envirNext) + 1
  assign('x', x, envir = envirNext)
  # }
  (x)
}
