#' Title
#'
#' @param collection_table
#' @param codes
#'
#' @return
#' @export
#'
#' @examples
gether_outpatient_core <- function(collection_table, codes) {

  collection_table %<>%
    dplyr::select(source, year) %>%
    dplyr::distinct()

  tab <- get_outpatient_core(collection_table[1, 1], collection_table[1, 2], codes)

  for (i in 2:nrow(collection_table)) {

    print(i)
    tab <- dplyr::bind_rows(
      get_outpatient_core(collection_table[i, 1], collection_table[i, 2], codes),
      tab
    )

  }

  return(tab)

}
