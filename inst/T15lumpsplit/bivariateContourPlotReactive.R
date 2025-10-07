### bivariateContourPlotReactive


###{r panelOfInputs}
panelOfInputs = div(
  wellPanel(
    #checkboxInput(inputId= 'togglePanelOfInputs', label =
    strong(em(
      "Whose prior to use?")),
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
                                              value=0, min = 0.00, step=0.1),
                                 # column(4,
                                 numericInput("tauInput",
                                              "shared additional variance (tau)",
                                              value=1, min = 0.00, step=0.1),
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
  "Shaded: 50% region of highest density ",
  fluidRow(
    column(6, div(style="color:orange",
                  checkboxInput("checkPrior",
                                "Orange = prior distribution",
                                TRUE))),
    column(6, div(style="color:blue",
                  checkboxInput("checkPosterior",
                                "Blue = posterior distribution",
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
    bivariateNormResults = bivariateNormResults,
    showConfIntBinormal = TRUE
  )

})
###


###{r lumpReact}
lumpReact = observe({
  if(length(input$lumpID) > 0) {
    #cat("lumpID\n")
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
    div(style=paste0('text-align:center; color:', colorsForDoctors[rValues$doctorSelected]),
        paste("posterior mean for Pr(R | D) = ",
              signif(digits=2,  rValues$bivariateNormResults$postmean.p[1]))
    )
  #ColorForPosterior)
  }
)

###{r splitReact}
splitReact = observe({
  if(length(input$splitID) > 0) {
    #cat("splitID\n")
    rValues$tau <<- 0; rValues$phi <<- 1
    ### Split:  D unconnected to L.
    rValues$title_1 <<- "Dr. Split"
    rValues$title_2 <<- HTML("Prior belief:<br> Pr(R|D) is unrelated to Pr(R|L).")
  }
  rValues$doctorSelected = 'split'
})
###

###{r whoReact}
# whoReact = observe({
#   if(length(input$whoID) > 0) {
#     #cat("whoID\n")
#     ### Who:  discrete mixture of Lump and Split.
#     rValues$title_1 <<- "Dr. Who: \ndiscrete mixture of Lump and Split."
#     rValues$title_2 <<- HTML(paste0("Prior belief: <BR> Pr(Split)=",
#                                     rValues$WhoPriorProb))
#     doctorSelected = 'who'
#   }
# })
###

###{r whoReact}
whoReact = observe({
  if(length(input$whoID) > 0) {
    #cat("mixedID\n")
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

output$thePlot = renderPlot(height=250,
                            {
                              par(mai=c(1,1,1,0.6))
                              par(mar=c(4,4,2,2) + 0.2)
                              #c(bottom, left, top, right)
                              par(pty='s')
                              plotPlightPdarkPosteriorReactive()
                            })
###
