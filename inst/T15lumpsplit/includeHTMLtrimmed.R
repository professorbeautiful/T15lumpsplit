includeHTMLtrimmed = function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- lines[4:(length(lines)) - 2]
  return(shiny::HTML(htmltools:::paste8(lines, collapse = "\r\n")))
}
