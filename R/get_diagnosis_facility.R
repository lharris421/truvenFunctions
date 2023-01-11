get_diagnosis_facility <- function(diagnosis, source, year, codes, primary = FALSE) {

  tab_name <- paste0(c("facility", diagnosis, source, year), collapse = "_")
  tab <- dplyr::tbl(con, tab_name) %>%
    dplyr::filter(dx %in% codes & (dx_num == 1 | primary == FALSE)) %>%
    dplyr::select(enrolid, svcdate, dx_num, dx, fachdid) %>%
    dplyr::collect(n = Inf)

  return(tab)


}
