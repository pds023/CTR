

#' Title
#'
#' @return
#' @export
#'
#' @examples
nav_panel_operations <- function() {
  return(
      nav_panel("Opérations",icon = bs_icon("search"),
                  card(card_header("aaa"),
                       card_body(highchartOutput("operations_hc_synth"),
                                 sliderInput(inputId = "date_operation",
                                             label = "Années",
                                             min = 1949,
                                             max = 2023,
                                             value = c(2000,2023),
                                             step = 1))),
                card(card_header("Données brutes"),
                     card_body(DTOutput("donnees_operations"))))
  )
}
