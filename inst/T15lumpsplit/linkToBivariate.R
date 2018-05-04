
#```{r}
div(HTML("<a
         href='http://localhost/~Roger/lump,split-bivariate-normal-derivation.pdf' target='_blank' > Click here to view the derivation of the posterior in another window.</a>")
)
div(HTML("<a
         href='http://localhost/~Roger/lump,split-bivariate-normal-derivation.pdf' target='_blank' > Click here to view the derivation of the posterior in another window.</a>")
)
div(HTML("<a
         href='www/lump,split-bivariate-normal-derivation.pdf' target='_blank' rel='noopener'> Click here to view the derivation of the posterior in another window.</a>")
)     ####doesn't work
div(HTML(paste0(
  "<a href=",
  system.file(
    package="T15lumpsplit",
    'T15lumpsplit/lump,split-bivariate-normal-derivation.pdf'),
  "target='_blank' > Click here to view the derivation of the posterior in another window.</a>")
))   ### doesn't work
div(HTML("<a
         href='lump,split-bivariate-normal-derivation.pdf' target='_blank' rel='noopener'> Click here to view the derivation of the posterior in another window.</a>")
)     ####doesn't work
#```
