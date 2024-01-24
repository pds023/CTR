#' Title
#'
#' @return
#' @export
#'
#' @examples
nav_panel_dette <- function() {
  return(nav_panel("Dette",icon = bs_icon("search"),
                                    layout_column_wrap(
                                      width = "800px", height = 1600,
                                      card(card_header("aaa"),
                                           card_body(highchartOutput("dette_hc_synth"))),
                                      card(card_header("aaa")),
                                      card(card_header("aaa")),
                                      card(card_header("aaa"))
                                    ),
                                    card(card_header("Données brutes"),
                                         card_body(DTOutput("donnees_dette"))))
  )
}
