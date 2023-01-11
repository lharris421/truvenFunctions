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
get_proc_facility_range <- function(source, year, dates, range) {

  tab_name <- paste0(c("facility_proc", source, year), collapse = "_")
  tab <- dplyr::tbl(con, tab_name) %>%
    dplyr::select(enrolid, proc, svcdate) %>%
    dplyr::collect(n = Inf) %>%
    dplyr::left_join(dates, by = "enrolid") %>%
    dplyr::mutate(days_within = carrier_index - svcdate) %>%
    dplyr::filter(days_within >= range[1] & days_within <= range[2])

}
