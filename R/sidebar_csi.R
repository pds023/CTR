

#' Title
#'
#' @return
#' @export
#'
#' @examples
sidebar_csi <- function() {
  return(sidebar(accordion(
    accordion_panel(title = "CSI",
                    pickerInput("csi_secteur","Secteur",choices = c(),multiple = TRUE),
                    pickerInput("csi_compte","Compte",choices = c(),multiple = TRUE),
                    pickerInput("csi_operation","Opération",choices = c(),multiple = TRUE),
                    pickerInput("csi_correction","Correction",choices = c(),multiple = TRUE),
                    actionButton(inputId = "csi_reset",label = "Réinitialiser"),
                    materialSwitch(inputId = "evol_csi",value = FALSE)
    )
  )))
}
