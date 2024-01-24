

#' Title
#'
#' @return
#' @export
#'
#' @examples
nav_panel_pib <- function() {
  return(nav_panel("PIB",icon = bs_icon("search"),
                   navset_card_underline(
                     nav_panel(title = "Vue d'ensemble",
                               card(card_header("Éléments clés"),
                                    card_body(value_box(title = "aaa",value = 34))),
                               card(card_header("Analyse sur longue période"),
                                    card_body(highchartOutput("pib_hc_synth"))),
                               card(card_header("Données brutes"),
                                    card_body(DTOutput("donnees_pib")))
                     ),
                     nav_panel(title = "Analyse des révisions")
                   )
  )

  )
}
