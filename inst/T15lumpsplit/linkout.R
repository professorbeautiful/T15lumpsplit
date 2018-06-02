linkout = function(fileName) {
  theFile = system.file(
  package="T15lumpsplit",
  'T15lumpsplit/', fileName)
  theCommand = paste0(
    "browseURL('", theFile, "')")
  eval(parse(text=theCommand))
#  "system('open ", theFile, "')")
}
