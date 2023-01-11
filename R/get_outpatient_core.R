get_outpatient_core <- function(source, year, codes) {

  tab_name <- paste0(c("outpatient_core", source, year), collapse = "_")
  tab <- dplyr::tbl(con, tab_name) %>%
    dplyr::filter(proc1 %in% codes) %>%
    dplyr::select(enrolid, proc1, svcdate) %>%
    dplyr::collect(n = Inf)

}
