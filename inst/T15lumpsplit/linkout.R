#### NOTE: you will have to rebuild the package to make a local file visible ####

linkout = function(fileName, labelString) {
  # theFile = system.file(
  # package="T15lumpsplit",
  # 'T15lumpsplit/', fileName)
  cat("linkout: object-style: ", fileName, '\n')
  conditionalPanelWithCheckbox(
    labelString,
    filename,
    html=tagList(
      # https://stackoverflow.com/questions/1244788/embed-vs-object
      tags$object(
        data=theFile,
        type="application/pdf"
      )
      # tags$iframe(
      # style="height:600px; width:100%;
      #                   padding:0px;
      # margin:0px;
      # border: 0px
      # ",
      # src=theFile)
    ),
    initialValue=FALSE
  )
  #theCommand = paste0(
  #  "browseURL('", theFile, "')")
  #eval(parse(text=theCommand))
  #  "system('open ", theFile, "')")
}

linkoutLink = function(fileName, linkouttext) {
  # For opening a file in the package
  thisNumber = nextNumber(sequenceType = "linkoutLink")
  IdThisLinkout = paste0('linkoutLink', thisNumber)

  # actionLink(IdThisLinkout,
  #            label=HTML(
  #              paste0('<font color=blue>', linkouttext, '</font>'))
  # )
  # conditionalPanelWithCheckbox(
  #              #linkout(fileName)
  #                linkouttext,
  #                #filename,
  #                html=tagList(  tags$iframe(style="height:600px; width:100%",
  #                                           src=fileName)
  #                ),
  #                initialValue=FALSE
  #              )
  div(
    checkboxInput(IdThisLinkout,
                  strong(em(showhideString(linkouttext, FALSE)
                  )),
                  value=FALSE),
    conditionalPanel(condition = paste0('input.', IdThisLinkout),
                     tags$iframe(
                       style="height:600px; width:100%;
                        padding:0px;
                        margin:0px;
                        border: 0px
                       ",
                       src=fileName) )
  )
}


# NOTE: for local anchors, the name is prepended with 'section-'
# at the target, but not at the clickbait.
linkinLink = function(anchorName, linktext) {
  # For going to a local anchor
  thisNumber = nextNumber(sequenceType = "linkinLink")
  IdThisLinkin = paste0('linkinLink', thisNumber)

  onclickAction = paste0(
    "savedYposition=window.scrollY; ",
    "Shiny.onInputChange('savedYposition',
    savedYposition);",

    "currentLocationId='", IdThisLinkin, "';
    Shiny.onInputChange('currentLocationId',
    currentLocationId);",

    "Shiny.onInputChange('linktext', '",
    linktext, "');"
  )
  ## https://stackoverflow.com/questions/39273043/shiny-modules-not-working-with-renderui
  ## function(a,b,c){c=addDefaultInputOpts(c),p.setInput(a,b,c)}
  # linkinLink972
  #  Aha.  Escape is doing double-duty, with popups.

  labelString = HTML(
    paste0('<font color=blue style="text-decoration: underline">',
           linktext,
           '</font>'))
  a(id=IdThisLinkin,
    onclick=onclickAction,
    href=paste0('#section-',
                anchorName),
    labelString)
}

getContext = function(linktext) {
  whereInDTC = which(jumpList_DTC == gsub('▸ ', '', input$linktext) )
  whereInBDC = which(jumpList_BDC == gsub('▸ ', '', input$linktext) )
  if(length(whereInDTC) > 0) whichContext = 'DTC'
  if(length(whereInBDC) > 0) whichContext = 'BDC'
  if(length(whereInDTC) > 0 & length(whereInBDC) > 0)
    warn('linktextProcessor: ', 'input$linktext in both contexts? ',  input$linktext)
  return(whichContext)  ### currently, BDC overrides.
}
#debug(getContext)

linktextProcessor = function() {
  ### Copy DLdataLastUsed to theCellIds for the destination.
  ### (Or the current myChoice data instead? Harder though.)

  whichContext = getContext(input$linktext )
  if(whichContext == 'DTC') {
    thisJumpList = jumpList_DTC
    destAnalysisNumber = which(jumpList_DTC == gsub('▸ ', '', input$linktext) )
    destDTCnum = getDTCnumber(destAnalysisNumber)
    theCellIds = as.list(paste0('m', cellNames, 'idPanelDTC', destDTCnum) )
    cat('linktextProcessor: copying ', paste(DLdataLastUsed),
        ' from source=', input$currentLocationId,
        ' to dest = ', input$linktext, '\n')
    for(cellnum in 1:4)
      updateNumericInput(
        session,
        theCellIds[[cellnum]],
        value = DLdataLastUsed[cellnum])
    setDLdata(value=DLdataLastUsed, DTCnumber=destDTCnum)
    setDLdata(value=DLdataLastUsed, DTCnumber=destDTCnum, myChoice=TRUE)
  }
  if(whichContext == 'BDC') {
    destAnalysisNumber = which(jumpList_BDC == gsub('▸ ', '', input$linktext) )
    destBDCnum = getBDCnumber(destAnalysisNumber)
    cat('linktextProcessor: copying ', tauTrueLastUsed,
        ' from source=', input$currentLocationId,
        ' to dest = ', input$linktext, '\n')
    setBigData_tauTrue(value=tauTrueLastUsed, BDCnumber=destBDCnum)
    setBigData_tauTrue(value=tauTrueLastUsed, BDCnumber=destBDCnum, myChoice=TRUE)
  }

}
observeEvent(input$linktext, { linktextProcessor() } )

#From session help:
#url_protocol, url_hostname, url_port, url_pathname, url_search,
#url_hash_initial and url_hash can be used to get the components of the URL that
#was requested by the browser to load the Shiny app page. These values are from
#the browser's perspective, so neither HTTP proxies nor Shiny Server will affect
#these values. The url_search value may be used with parseQueryString to access
#query string parameters.

