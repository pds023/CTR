

#' Title
#'
#' @return
#' @export
#'
#' @examples
sidebar_pib <- function() {
  return(sidebar(accordion(
    accordion_panel(title = "PIB",
                    pickerInput("aaa28erre642","aaa",choices = c("aa")))
  )))
}
