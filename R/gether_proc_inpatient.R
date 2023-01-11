gether_proc_inpatient <- function(collection_table, codes, primary) {

  collection_table %<>%
    dplyr::filter(setting == "inpatient")

  tab <- get_proc_inpatient(collection_table[1, 1], collection_table[1, 2], collection_table[1, 3],
                            codes, primary = primary)

  for (i in 2:nrow(collection_table)) {

    print(i)
    tab <- dplyr::bind_rows(
      tab,
      get_proc_inpatient(collection_table[i, 1], collection_table[i, 2], collection_table[i, 3], codes, primary = primary)
    )

  }

  return(tab)

}
