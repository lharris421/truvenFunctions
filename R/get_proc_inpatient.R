get_proc_inpatient <- function(table, source, year, codes, primary = FALSE) {

  tab_name <- paste0(c(table, "proc", source, year), collapse = "_")
  cross <- paste0(c(table, "core", source, year), collapse = "_")
  tab <- dplyr::tbl(con, tab_name) %>%
    dplyr::filter(proc %in% codes) %>%
    dplyr::select(caseid, proc) %>%
    dplyr::inner_join(dplyr::tbl(con, cross) %>% dplyr::select(caseid, admdate, enrolid), by = "caseid") %>%
    dplyr::select(-caseid) %>%
    dplyr::collect(n = Inf)

  return(tab)


}
