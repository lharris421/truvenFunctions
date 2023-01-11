#' Title
#'
#' @param collection_table
#'
#' @return
#' @export
#'
#' @examples
gether_enrollee_detail <- function(collection_table) {

  collection_table %<>%
    dplyr::select(source, year) %>%
    dplyr::distinct() %>%
    dplyr::arrange(year)

  tab <- get_enrollee_detail(collection_table[1, 1], collection_table[1, 2])

  for (i in 2:nrow(collection_table)) {

    print(i)
    tab <- dplyr::bind_rows(
      get_enrollee_detail(collection_table[i, 1], collection_table[i, 2]),
      tab
    )

  }

  return(tab)

}
