

#' Title
#'
#' @return
#' @export
#'
#' @examples
nav_panel_csi <- function() {
  return(
    nav_panel("CSI",icon = bs_icon("search"),
              layout_sidebar(
                sidebar = sidebar_csi(),
              navset_card_underline(
                nav_panel(title = "Vue d'ensemble",
                          card(card_header("Éléments clés"),
                               card_body(value_box(title = "aaa",value = 34))),
                          card(card_header("Analyse sur longue période"),
                               card_body(highchartOutput("csi_hc_synth"),
                                         sliderInput(inputId = "date_csi",
                                                     label = "Années",
                                                     min = 1949,
                                                     max = 2023,
                                                     value = c(2000,2023),
                                                     step = 1))),
                          card(card_header("Données brutes"),
                               card_body(DTOutput("donnees_csi")))
                ),
                nav_panel(title = "Analyse des révisions")
              )
              )
    )


  )
}
