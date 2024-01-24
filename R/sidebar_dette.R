

#' Title
#'
#' @return
#' @export
#'
#' @examples
sidebar_dette <- function() {
  return(sidebar(accordion(
    accordion_panel(title = "Dette",
                    pickerInput("dette_indicateur","Indicateur",choices = c(),multiple = TRUE),
                    pickerInput("dette_secteur","Secteur",choices = c(),multiple = TRUE),
                    pickerInput("dette_instruments","Instruments",choices = c(),multiple = TRUE),
                    actionButton(inputId = "dette_reset",label = "Réinitialiser"),
                    materialSwitch(inputId = "evol_dette",value = FALSE))
  )))
}
