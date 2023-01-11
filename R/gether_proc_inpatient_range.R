gether_proc_inpatient_range <- function(collection_table, dates, range) {

  collection_table %<>%
    dplyr::select(source, year) %>%
    dplyr::distinct()

  tab <- get_proc_inpatient_range(collection_table[1, 1], collection_table[1, 2], dates, range)

  for (i in 2:nrow(collection_table)) {

    print(i)
    tab <- dplyr::bind_rows(
      tab,
      get_proc_inpatient_range(collection_table[i, 1], collection_table[i, 2], dates, range)
    )

  }

  return(tab)

}
