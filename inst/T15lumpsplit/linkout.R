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

#From session help:
#url_protocol, url_hostname, url_port, url_pathname, url_search,
#url_hash_initial and url_hash can be used to get the components of the URL that
#was requested by the browser to load the Shiny app page. These values are from
#the browser's perspective, so neither HTTP proxies nor Shiny Server will affect
#these values. The url_search value may be used with parseQueryString to access
#query string parameters.

