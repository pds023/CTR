
#' Title
#'
#' @return
#' @export
#'
#' @examples
nav_menu_apropos <- function() {
  return(
    nav_menu("À propos",icon = bs_icon("info-circle-fill"),
             nav_panel("Ma démarche",icon = bs_icon("person-vcard"),
                       card(card_header("Un an, un projet"),
                            card_body(htmlOutput("apropos")))),
             nav_panel("Méthodologie",icon = bs_icon("question-octagon"),
                       card(card_header("Méthodologie"),
                            card_body(htmlOutput("ctrmeth")))),
             nav_item(
               tags$a(href="mailto:philippe.fontaine.ds@proton.me",
                      class="nav-link",
                      tags$i(class="fa fa-envelope"), " Contact")
             )
    ))
}
