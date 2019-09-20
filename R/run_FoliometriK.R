
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Run \emph{\strong{FoliometriK}} Application
#'
#' @description This function runs the \emph{\strong{FoliometriK}} application in the user's default web browser.
#' @export
#' @import shiny
#' @importFrom  shinyalert useShinyalert shinyalert
#' @importFrom  shinyWidgets noUiSliderInput actionBttn addSpinner
#' @importFrom  shinyBS bsTooltip
#' @importFrom  EBImage display readImage as.Image
#' @importFrom  Momocs coo_plot coo_draw efourier_i
#'
#' @section Authors:
#' Victor M. Ramirez-Arrieta and Dennis Denis-Avila
#'
#' @examples
#' run_FoliometriK()
run_FoliometriK <- function() {
     appDir <- system.file("shinyapp", package = "foliometrik")
     if (appDir == "") {
          stop("Could not shinyapp directory. Try re-installing `foliometrik`.", call. = FALSE)
     }

     shiny::runApp(appDir, display.mode = "normal")
}
