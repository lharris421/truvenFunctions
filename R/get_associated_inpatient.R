get_associated_inpatient <- function(diagnosis, source, year, codes, keep_dates) {

  tab_name <- paste0(c("inpatient", diagnosis, source, year), collapse = "_")
  cross <- paste0(c("inpatient", "core", source, year), collapse = "_")
  tab <- dplyr::tbl(con, tab_name) %>%
    dplyr::filter(dx %in% codes) %>%
    dplyr::select(caseid) %>%
    dplyr::inner_join(dplyr::tbl(con, cross) %>% dplyr::select(caseid, admdate, enrolid), by = "caseid") %>%
    dplyr::select(-caseid) %>%
    dplyr::collect(n = Inf) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(keep_dates, by = c("admdate", "enrolid"))

  return(tab)


}
