gether_associated_outpatient <- function(collection_table, codes, keep_dates) {

  collection_table %<>%
    dplyr::filter(setting == "outpatient")

  tab <- get_associated_outpatient(collection_table[1, 4], collection_table[1, 2], collection_table[1, 3], codes, keep_dates)

  for (i in 2:nrow(collection_table)) {

    print(i)
    tab <- dplyr::bind_rows(
      tab,
      get_associated_outpatient(collection_table[i, 4], collection_table[i, 2], collection_table[i, 3], codes, keep_dates)
    )

  }

  tab <- tab %>%
    dplyr::distinct()

  return(tab)

}
