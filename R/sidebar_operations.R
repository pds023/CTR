

#' Title
#'
#' @return
#' @export
#'
#' @examples
sidebar_operations <- function() {
  return(sidebar(accordion(
    accordion_panel(title = "Opérations",
                    pickerInput("operations_secteur","Secteur",choices = c(),multiple = TRUE),
                    pickerInput("operations_operation","Opération",choices = c(),multiple = TRUE),
                    pickerInput("operations_produit","Produit",choices = c(),multiple = TRUE),
                    pickerInput("operations_valorisation","Valorisation",choices = c(),multiple = TRUE),
                    actionButton(inputId = "operations_reset",label = "Réinitialiser"),
                    materialSwitch(inputId = "evol_operations",value = FALSE)
    ))))
}
