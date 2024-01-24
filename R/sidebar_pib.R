

#' Title
#'
#' @return
#' @export
#'
#' @examples
sidebar_pib <- function() {
  return(sidebar(accordion(
    accordion_panel(title = "PIB",
                    pickerInput("pib_secteur","Secteur",choices = c(),multiple = TRUE),
                    pickerInput("pib_operation","Opération",choices = c(),multiple = TRUE),
                    pickerInput("pib_nature","Nature",choices = c(),multiple = TRUE),
                    pickerInput("pib_valorisation","Valorisation",choices = c(),multiple = TRUE),
                    pickerInput("pib_unit","Unité",choices = c(),multiple = TRUE),
                    pickerInput("pib_correction","Correction",choices = c(),multiple = TRUE),
                    actionButton(inputId = "pib_reset",label = "Réinitialiser"),
                    materialSwitch(inputId = "evol_pib",value = FALSE))
  )))
}
