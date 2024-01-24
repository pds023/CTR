

#' Title
#'
#' @return
#' @export
#'
#' @examples
sidebar_cb <- function() {
  return(sidebar(accordion(
    accordion_panel(title = "Branches",
                    pickerInput("cb_operation","Opération",choices = c(),multiple = TRUE),
                    pickerInput("cb_produit","Produit",choices = c(),multiple = TRUE),
                    pickerInput("cb_valorisation","Valorisation",choices = c(),multiple = TRUE),
                    pickerInput("cb_unité","Unité",choices = c(),multiple = TRUE),
                    pickerInput("cb_correction","Correction",choices = c(),multiple = TRUE),
                    actionButton(inputId = "cb_reset",label = "Réinitialiser"),
                    materialSwitch(inputId = "evol_cb",value = FALSE))
    )))
}
