nextNumber = function(){
  x <- get('x', envir=envNextNumber) + 1
  assign('x', x, envir = envNextNumber)
  #print(x)
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
