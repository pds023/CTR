

#' Title
#'
#' @return
#' @export
#'
#' @examples
sidebar_app <- function() {
  return(sidebar(title = "Filtres",
                 accordion(
                   accordion_panel(title = "Opérations",
                                   pickerInput("operations_secteur","Secteur",choices = c(),multiple = TRUE),
                                   pickerInput("operations_operation","Opération",choices = c(),multiple = TRUE),
                                   pickerInput("operations_produit","Produit",choices = c(),multiple = TRUE),
                                   pickerInput("operations_valorisation","Valorisation",choices = c(),multiple = TRUE),
                                   actionButton(inputId = "operations_reset",label = "Réinitialiser"),
                                   materialSwitch(inputId = "evol_operations",value = FALSE)
                                   ),
                   accordion_panel(title = "CSI",
                                   pickerInput("csi_secteur","Secteur",choices = c(),multiple = TRUE),
                                   pickerInput("csi_compte","Compte",choices = c(),multiple = TRUE),
                                   pickerInput("csi_operation","Opération",choices = c(),multiple = TRUE),
                                   pickerInput("csi_correction","Correction",choices = c(),multiple = TRUE),
                                   actionButton(inputId = "csi_reset",label = "Réinitialiser"),
                                   materialSwitch(inputId = "evol_csi",value = FALSE)
                                   ),
                   accordion_panel(title = "Branches",
                                   pickerInput("cb_operation","Opération",choices = c(),multiple = TRUE),
                                   pickerInput("cb_produit","Produit",choices = c(),multiple = TRUE),
                                   pickerInput("cb_valorisation","Valorisation",choices = c(),multiple = TRUE),
                                   pickerInput("cb_unité","Unité",choices = c(),multiple = TRUE),
                                   pickerInput("cb_correction","Correction",choices = c(),multiple = TRUE),
                                   actionButton(inputId = "cb_reset",label = "Réinitialiser"),
                                   materialSwitch(inputId = "evol_cb",value = FALSE)),
                   accordion_panel(title = "PIB",
                                   pickerInput("aaa28erre642","aaa",choices = c("aa"))),
                   accordion_panel(title = "Dette",
                                   pickerInput("aajkuka28642","aaa",choices = c("aa")))
                 )
  ))
}
