get_all_diagnosis_inpatient_range <- function(table, diagnosis, source, year, dates, range) {

  tab_name <- paste0(c(table, diagnosis, source, year), collapse = "_")
  cross <- paste0(c(table, "core", source, year), collapse = "_")
  tab <- dplyr::tbl(con, tab_name) %>%
    dplyr::select(caseid, dx) %>%
    dplyr::inner_join(dplyr::tbl(con, cross) %>% dplyr::select(caseid, admdate, enrolid), by = "caseid") %>%
    dplyr::select(-caseid) %>%
    dplyr::collect(n = Inf) %>%
    dplyr::left_join(dates, by = "enrolid") %>%
    dplyr::mutate(days_within = index_date - admdate) %>%
    dplyr::filter(days_within >= range[1] & days_within <= range[2])

  return(tab)


}
