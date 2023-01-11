#' Title
#'
#' @param collection_table
#' @param dates
#' @param range
#'
#' @return
#' @export
#'
#' @examples
gether_outpatient_core_range <- function(collection_table, dates, range) {

  collection_table %<>%
    dplyr::select(source, year) %>%
    dplyr::distinct()

  tab <- get_outpatient_core_range(collection_table[1, 1], collection_table[1, 2], dates, range)

  for (i in 2:nrow(collection_table)) {

    print(i)
    tab <- dplyr::bind_rows(
      get_outpatient_core_range(collection_table[i, 1], collection_table[i, 2], dates, range),
      tab
    )

  }

  return(tab)

}
