gether_proc_facility_range <- function(collection_table, dates, range) {

  collection_table %<>%
    dplyr::select(source, year) %>%
    dplyr::distinct() %>%
    filter(year != "01")

  tab <- get_facility_proc_range(collection_table[1, 1], collection_table[1, 2], dates, range)

  for (i in 2:nrow(collection_table)) {

    print(i)
    tab <- dplyr::bind_rows(
      get_facility_proc_range(collection_table[i, 1], collection_table[i, 2], dates, range),
      tab
    )

  }

  return(tab)

}
