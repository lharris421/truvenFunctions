gether_diagnosis_facility <- function(collection_table, codes, primary = FALSE) {

  collection_table %<>%
    dplyr::filter(setting == "inpatient") %>%
    dplyr::filter(year != "01") %>%
    dplyr::filter(diagnosis == "dx")

  tab <- get_diagnosis_facility(collection_table[1, 4], collection_table[1, 2], collection_table[1, 3],
                                codes, primary)

  for (i in 2:nrow(collection_table)) {

    print(i)
    tab <- dplyr::bind_rows(
      tab,
      get_diagnosis_facility(collection_table[i, 4], collection_table[i, 2], collection_table[i, 3], codes, primary)
    )

  }

  return(tab)

}
