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
gether_diagnosis_inpatient <- function(collection_table, codes, primary = FALSE) {

  collection_table %<>%
    dplyr::filter(setting == "inpatient")

  tab <- get_diagnosis_inpatient(collection_table[1, 1], collection_table[1, 4], collection_table[1, 2], collection_table[1, 3],
                                 codes, primary) %>%
    dplyr::mutate(dx = as.character(dx))

  for (i in 2:nrow(collection_table)) {

    print(i)
    tab <- dplyr::bind_rows(
      tab,
      get_diagnosis_inpatient(collection_table[i, 1], collection_table[i, 4], collection_table[i, 2], collection_table[i, 3], codes, primary) %>%
        dplyr::mutate(dx = as.character(dx))
    )

  }

  return(tab)

}
