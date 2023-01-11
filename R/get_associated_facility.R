#' Title
#'
#' @param diagnosis
#' @param source
#' @param year
#' @param codes
#' @param keep_dates
#'
#' @return
#' @export
#'
#' @examples
get_associated_facility <- function(diagnosis, source, year, codes, keep_dates) {

  tab_name <- paste0(c("facility", diagnosis, source, year), collapse = "_")
  tab <- dplyr::tbl(con, tab_name) %>%
    dplyr::filter(dx %in% codes) %>%
    dplyr::select(enrolid, svcdate) %>%
    dplyr::collect(n = Inf) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(keep_dates, by = c("svcdate" = "admdate", "enrolid" = "enrolid"))

  return(tab)


}
