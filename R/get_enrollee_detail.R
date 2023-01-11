get_enrollee_detail <- function(source, year) {

  tab_name <- paste0(c("enrollees", source, year), collapse = "_")
  tab <- dplyr::tbl(con, tab_name) %>%
    dplyr::select(enrolid, dobyr, sex) %>%
    dplyr::collect(n = Inf)

}
