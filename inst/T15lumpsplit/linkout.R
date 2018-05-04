linkout = function(fileName) {
  theFile = system.file(
  package="T15lumpsplit",
  'T15lumpsplit/', fileName)
  interactiveSection(codeToRun = paste0(
    "system('open ", theFile, "')") )
}
