#' Title
#'
#' @param collection_table
#' @param codes
#' @param primary
#'
#' @return
#' @export
#'
#' @examples
gether_proc_facility <- function(collection_table, codes, primary) {

  collection_table %<>%
    dplyr::filter(setting == "inpatient") %>%
    dplyr::filter(year != "01")

  tab <- get_proc_facility("facility", collection_table[1, 2], collection_table[1, 3],
                           codes)

  for (i in 2:nrow(collection_table)) {

    print(i)
    tab <- dplyr::bind_rows(
      tab,
      get_proc_facility("facility", collection_table[i, 2], collection_table[i, 3], codes)
    )

  }

  return(tab)

}
