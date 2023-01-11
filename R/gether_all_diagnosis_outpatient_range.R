gether_all_diagnosis_outpatient_range <- function(collection_table, dates, range) {

  collection_table %<>%
    dplyr::filter(setting == "outpatient")

  tab <- get_all_diagnosis_outpatient_range(collection_table[1, 4], collection_table[1, 2], collection_table[1, 3], dates, range)

  for (i in 2:nrow(collection_table)) {

    print(i)
    tab <- dplyr::bind_rows(
      tab,
      get_all_diagnosis_outpatient_range(collection_table[i, 4], collection_table[i, 2], collection_table[i, 3], dates, range)
    )

  }

  return(tab)

}
