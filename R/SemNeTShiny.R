#' Start SemNeTShiny
#' @title SemNeTShiny
#' @return Nothing
#' @description An interactive Shiny application for running \code{\link[SemNeT]{SemNeT}} analysis.
#' @details This starts the Shiny application for \code{\link[SemNeT]{SemNeT}}
#' @keywords CTT
#' @examples
#' \dontrun{
#' SemNeTShiny()
#' }
#' @export

SemNeTShiny <- function() {
  
  shiny::runApp(appDir = system.file("Shiny", package="SemNeT"))
  
}