

#' Title
#'
#' @return
#' @export
#' @import janitor
#' @examples
preprocess_data <- function(){

  dl_data("https://api.db.nomics.world/v22/series/INSEE/CNT-2014-OPERATIONS?observations=1","operations")
  data <- as.data.table(read_parquet("raw-data/operations.parquet"))
  data <- data[,.(`SECT-INST`,
          OPERATION,
          CNA_PRODUIT,
          VALORISATION,
          value,
          period)]
  data <- clean_names(data)
  write_parquet(data,"data/operations.parquet")

  dl_data("https://api.db.nomics.world/v22/series/INSEE/CNT-2014-CSI?observations=1","csi")
  data <- as.data.table(read_parquet("raw-data/csi.parquet"))
  data <- data[UNIT_MEASURE %in% "EUROS",.(`SECT-INST`,
                                           COMPTE,
                                           OPERATION,
                                           CORRECTION,
                                           value,
                                           period)]
  data <- clean_names(data)
  write_parquet(data,"data/csi.parquet")


  dl_data("https://api.db.nomics.world/v22/series/INSEE/CNT-2014-CB?observations=1","cb")
  data <- as.data.table(read_parquet("raw-data/cb.parquet"))
  data <- data[,.(OPERATION,
                  CNA_PRODUIT,
                  VALORISATION,
                  UNIT_MEASURE,
                  CORRECTION,
                  value,
                  period)]
  data <- clean_names(data)
  write_parquet(data,"data/cb.parquet")

}
