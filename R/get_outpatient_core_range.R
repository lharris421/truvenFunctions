#' Title
#'
#' @param source
#' @param year
#' @param dates
#' @param range
#'
#' @return
#' @export
#'
#' @examples
get_outpatient_core_range <- function(source, year, dates, range) {

  tab_name <- paste0(c("outpatient_core", source, year), collapse = "_")
  tab <- dplyr::tbl(con, tab_name) %>%
    dplyr::select(enrolid, proc1, svcdate) %>%
    dplyr::collect(n = Inf) %>%
    dplyr::left_join(dates, by = "enrolid") %>%
    dplyr::mutate(days_within = index_date - svcdate) %>%
    dplyr::filter(days_within >= range[1] & days_within <= range[2])

}
