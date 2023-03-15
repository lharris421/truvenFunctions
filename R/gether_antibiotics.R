gether_antibiotics <- function(table_list, db_con, abx_list, dates = NULL, range = NULL) {

  rx_tables <- all_tables[str_detect(all_tables, "rx")]

  rx_data <- list()
  for (i in 1:length(rx_tables)) {

     tmp <- tbl(db_con, rx_tables[i]) %>%
      select(enrolid, ndcnum, date = svcdate, daysupp) %>%
      filter(ndcnum %in% abx_list) %>%
      collect(n = Inf)

    if (!is.null(dates)) {

      tmp <- tmp %>%
        dplyr::left_join(dates, by = "enrolid") %>%
        dplyr::mutate(days_within = index_date - date) %>%
        dplyr::filter(days_within >= range[1] & days_within <= range[2])

    }

    rx_data[[i]] <- tmp

    print(paste0(i, " of ", length(rx_tables)))

  }

  rx <- bind_rows(rx_data)
  return(rx)

}
