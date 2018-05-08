linkout = function(fileName) {
  theFile = system.file(
  package="T15lumpsplit",
  'T15lumpsplit/', fileName)
  paste0(
    "browseURL('", theFile, "')")
#  "system('open ", theFile, "')")
}
