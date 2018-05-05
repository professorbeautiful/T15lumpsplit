nextNumber = function(sequenceType=""){
  # blank is for code sections, witha run button
  # QA is for a question/or/aha
  # if(sequenceType=="") {
  #   x <- get('x', envir=envNextNumber) + 1
  #   assign('x', x, envir = envNextNumber)
  # } else {
    envirname = paste0('envNextNumber', sequenceType)
    if(!exists(envirname) ) {
      assign(envirname, new.env(), pos=1, immediate=TRUE)
      assign('x', 1, envir=get(envirname, pos=1), immediate=TRUE)
    }
    #print(paste(' envirname', find(envirname)))
    envirNext = get(envirname, pos=1)
    x <- get('x', envir=envirNext) + 1
    assign('x', x, envir = envirNext)
  # }
  (x)
}

interactiveSection = function(codeToRun = "date()",
                              buttonLabel= "run code",
                              showResult = TRUE) {
  ### You need to add input$idRunCode1 to make it reactive!!
  thisSectionNumber = nextNumber()
  outputIdThisSection = paste0('RunCode', thisSectionNumber)
  buttonIdThisSection = paste0('id', outputIdThisSection)

  output[[outputIdThisSection]] = renderText({
    if(input[[buttonIdThisSection]] == 0)
      "<result will be here>"
    else
      #paste(
      capture.output(eval(parse(text=codeToRun))) #)
  })
  outputRunCode = textOutput(outputId = outputIdThisSection)
  span(paste("(", thisSectionNumber, ") "),
       # paste("thisSectionNumber", thisSectionNumber),
       # paste("outputIdThisSection", outputIdThisSection),
       # paste("buttonIdThisSection", buttonIdThisSection),
       actionButton(buttonIdThisSection, "run"),
       pre(codeToRun),  outputRunCode
  )
}
