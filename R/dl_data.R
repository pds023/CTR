
#' Title
#'
#' @return
#' @export
#' @import rdbnomics arrow data.table
#' @examples
dl_data <- function(api_request,label,dir_save = "raw-data") {
  data <- rdb(api_link = api_request)
  write_parquet(as.data.table(data),paste0(dir_save,"/",label,".parquet"))
}
