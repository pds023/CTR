

#' Title
#'
#' @return
#' @export
#'
#' @examples
nav_panel_CSI <- function() {
  return(
    nav_panel("CSI",icon = bs_icon("search"),
              card(card_header("aaa"),
                   card_body(highchartOutput("csi_hc_synth"),
                             sliderInput(inputId = "date_csi",
                                         label = "Années",
                                         min = 1949,
                                         max = 2023,
                                         value = c(2000,2023),
                                         step = 1))),
              card(card_header("Données brutes"),
                   card_body(DTOutput("donnees_csi")))
    )


  )
}
