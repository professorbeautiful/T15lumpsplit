### bivariateContourPlotReactive


###{r panelOfInputs}
panelOfInputs = div(
  wellPanel(style="padding: 1px;",
    #checkboxInput(inputId= 'togglePanelOfInputs', label =
    div(style='text-align:center', strong(em(
      "Whose prior to use??"))),
    #value = TRUE),
    #conditionalPanel(
    #  "input.togglePanelOfInputs",
    fluidRow(
      column(4, actionButton(style='color:red', "lumpID", label = "Lump")),
      column(4, actionButton(style='color:blue', "splitID", label = "Split")),
      column(4, actionButton(style='color:green', "whoID", label = "Who"))
    ),
  conditionalPanelWithCheckbox('parameters',
                               html=div(
                                 # fluidRow(
                                 #   column(4,
                                 numericInput("phiInput",
                                              "prior variance within group (phi)",
                                              value=0.5, min = 0.00, step=0.1),
                                 # column(4,
                                 numericInput("tauInput",
                                              "shared additional variance (tau)",
                                              value=0.5, min = 0.00, step=0.1),
                                 # column(4,
                                 numericInput("mu0Input", "shared prior mean (mu)",
                                              value=0.5, min = 0.001, step=0.1, max=0.999),
                                 # ),
                                 numericInput(inputId = 'fudgeFactor',
                                              label = 'continuity fudge factor',
                                              value=0.001)
                               )
  ))
)

###


###{r show-hide-contours}
ContoursPanelLegend = list(
  fluidRow(
    column(4, div(style='color:red',
                  "L:  Dr.Lump")),
    column(4, div(style='color:blue',
                  "S:  Dr.Split")),
    column(4, div(style='color:green',
                  "W:  Dr.Who"))
  ),
  textOutput('posteriorMeans'),
  hr(),
  div(style=paste("color:", "black"),
    "Shaded: 50% region of highest density "),
  fluidRow(
    column(6, div(#style="color:lightgreen",
                  checkboxInput("checkPrior",
                                "lighter color = prior distribution",
                                TRUE))),
    column(6, div(style="color:darkgreen",
                  checkboxInput("checkPosterior",
                                "darker color = posterior distribution",
                                TRUE))
    )
  )
  #  "X:   observed data", br(),
)
###

###{r plotPlightPdarkPosteriorReactive}
plotPlightPdarkPosteriorReactive = reactive( {
  analysisName = 'bivariateContourPlot'
  source(analysisReactiveSetup_DTC, local=TRUE)

  tau <- input$tauInput
  phi <- input$phiInput
  mu0 <- input$mu0Input
  # cat('plotPlightPdarkPosteriorReactive: ', paste(thisData), '\n')
  bivariateNormResults = rValues$bivariateNormResults <<-
    calculatePlightPdarkPosterior(DLdata=thisData,
                             tau=tau, phi=phi, mu0=mu0,
                             fudgeFactor = input$fudgeFactor
    )
  # cat('plotPlightPdarkPosteriorReactive: names of rValues$bivariateNormResults are ',
  #    paste(names(rValues$bivariateNormResults)), '\n')
  rValues$title_3 <<- paste0(
    "  tau=", input$tauInput,
    ",  phi=", input$phiInput,
    ",  mu0=", input$mu0Input
  )
  # Pass the calculations on to the plotting function.
  plotPlightPdarkPosterior(
    DLdata = thisData,
    showPrior = input$checkPrior,
    showPosterior = input$checkPosterior,
    ColorForPrior = rValues$ColorForPrior,
    ColorForPosterior = rValues$ColorForPosterior,
    showW = input$checkPosterior,
    showS =  ! input$checkPosterior,
    showL =  ! input$checkPosterior,
    bivariateNormResults = bivariateNormResults,
    showConfIntBinormal = input$checkPosterior
  )

})
###


###{r lumpReact}
lumpReact = observe({
  if(length(input$lumpID) > 0) {
    updateCheckboxInput(inputId='checkPrior',
                        label = span(style=paste("color:", lumpColor),
                        "show prior"))
    updateCheckboxInput(inputId='checkPosterior',
                        label = span(style=paste("color:", lumpColor),
                        "show posterior"))
    rValues$ColorForPrior = lumpColor
    rValues$ColorForPosterior = scales::col_darker(lumpColor, amount=10)
    rValues$tau <- 1; rValues$phi <- 0.001
    ### Lump:  no individual variation:   D is same as L.
    rValues$title_1 <- "Dr. Lump"
    rValues$title_2 <- HTML("Prior belief: <br>Pr(R|D) = Pr(R|L)")
    rValues$doctorSelected = 'lump'

  }
})
###

output$colorForDoctorSelected = renderText(
  paste0('COLOR:', colorsForDoctors[rValues$doctorSelected])
)

output$posteriorMean = renderUI(
  {
    theMean = try({
      printpaste('bivariateNormResults ',
                 rValues$bivariateNormResults)
      rValues$bivariateNormResults$postmean.p[1]})
    printpaste('theMean ', theMean)
    if(inherits(theMean, "try-error")
       | (length(theMean) != 1) ) {
      return(div())
    }
    div(style=paste0('text-align:center; color:', colorsForDoctors[rValues$doctorSelected]),
        paste("posterior mean for Pr(R | D) = ",
              signif(digits=2,  theMean))
    )
  #ColorForPosterior)
  }
)

###{r splitReact}
splitReact = observeEvent(input$splitID, {
  if(length(input$splitID) > 0) {
    cat('input$splitID)\n')
     updateCheckboxInput(inputId='checkPrior',
                         label = span(style=paste("color:", splitColor),
                         "show prior"))
    updateCheckboxInput(inputId='checkPosterior',
                         label = span(style=paste("color:", splitColor),
                         "show posterior"))

    rValues$ColorForPrior = splitColor
    rValues$ColorForPosterior = scales::col_darker(splitColor, amount=10)
    rValues$tau <<- 0; rValues$phi <<- 1
    ### Split:  D unconnected to L.
    rValues$title_1 <<- "Dr. Split"
    rValues$title_2 <<- HTML("Prior belief:<br> Pr(R|D) is unrelated to Pr(R|L).")
  }
  rValues$doctorSelected = 'split'
})
###



###{r whoReact}
whoReact = observeEvent(input$whoID, {
  if(length(input$whoID) > 0) {
    updateCheckboxInput(inputId='checkPrior',
                        label = span(style=paste("color:", whoColor),
                        "show prior"))
    updateCheckboxInput(inputId='checkPosterior',
                        label = span(style=paste("color:", whoColor),
                        "show posterior"))

    rValues$ColorForPrior = whoColor
    rValues$ColorForPosterior = scales::col_darker(whoColor, amount=10)
    rValues$tau <<- 1/2; rValues$phi <<- 1/2
    rValues$title_1 <<- HTML("Bayesian Compromise: <br>a mixture of priors for Dr.Lump and Dr. Split")
    rValues$title_2 <<- HTML("Dr.Who's prior belief: <br> Pr(R|D) is somewhat related to Pr(R|L). ")
  }
  rValues$doctorSelected = 'who'
})
###

###{r updateViews}
updateViews = observe({
  updateNumericInput(session=session, inputId="tauInput",
                     value = rValues$tau)
  updateNumericInput(session=session, inputId="phiInput",
                     value = rValues$phi)
})
###


###{r thePlot}
output$title_1_ID = renderUI({rValues$title_1})
output$title_2_ID = renderUI({rValues$title_2})
output$title_3_ID = renderUI({rValues$title_3})

thePlot = try({
  renderPlot(height=250,
                            {
                              par(mai=c(1,1,1,0.6))
                              par(mar=c(4,4,2,2) + 0.2)
                              #c(bottom, left, top, right)
                              par(pty='s')
                              plotPlightPdarkPosteriorReactive()
                            })
})
if(inherits(thePlot, "try-error")) {
  output$thePlot = NULL
} else
  output$thePlot = thePlot
###
