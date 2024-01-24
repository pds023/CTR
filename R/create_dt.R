

#' Title
#'
#' @param data
#' @param length
#'
#' @return
#' @export
#'
#' @examples
create_dt <- function(data,length = 5,cols_names = NULL) {
  return(renderDT(
    data,
    filter = 'top',
    extensions = c('Responsive'),
    colnames = cols_names,
    rownames = FALSE,
    options = list(
      pageLength = length,
      language = list(
        info = 'Affichage de _START_ à _END_ résultats sur _TOTAL_',
        paginate = list(First="Première",last="Dernière",previous = 'Précédent', `next` = 'Suivant'),
        lengthMenu = "Afficher _MENU_ résultats",
        search = "Recherche",
        emptyTable= "Aucune donnée dans la table",
        infoEmpty =       "Affichage 0 à 0 de 0 entrées",
        thousands=" ",
        loadingRecords="Chargmeent...",
        zeroRecords="Aucun résultat trouvé"
      ))
  ))
}
