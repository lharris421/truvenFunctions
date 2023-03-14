#' Title
#'
#' @param collection_table
#' @param codes
#' @param primary
#'
#' @return
#' @export
#'
#' @examples
gether_diagnosis_outpatient <- function(table_list, db_con, codes = NULL, dates = NULL, range = NULL, primary = FALSE, year_range = NULL) {

  outpatient_tables <- table_list[str_detect(table_list, "outpatient")] %>%
    str_split("_")

  outpatient_tables <- do.call(rbind, outpatient_tables) %>%
    data.frame()
  colnames(outpatient_tables) <- c("table", "diagnosis", "source", "year")

  outpatient_dx_tables <- outpatient_tables %>%
    filter(str_detect(diagnosis, "dx"))

  if (!is.null(year_range)) {
    outpatient_dx_tables %<>%
      filter(year >= year_range[1] & year <= year_range[2])
  }

  outpatient_dxs <- list()
  for (i in 1:nrow(outpatient_dx_tables)) {

    table <- outpatient_dx_tables[i,1]
    diagnosis <- outpatient_dx_tables[i,2]
    source <- outpatient_dx_tables[i,3]
    year <- outpatient_dx_tables[i,4]
    tab_name <- paste0(c(table, diagnosis, source, year), collapse = "_")

    tmp <- dplyr::tbl(db_con, tab_name) %>%
      dplyr::mutate(dx = as.character(dx)) %>%
      dplyr::filter(dx_num == 1 | primary == FALSE) %>%
      dplyr::select(enrolid, date = svcdate, dx_num, dx) %>%
      dplyr::collect(n = Inf) %>%
      dplyr::mutate(dx = as.character(dx)) %>%
      dplyr::filter(is.null(codes) | dx %in% codes)

    if (!is.null(dates)) {

      tmp <- tmp %>%
        dplyr::left_join(dates, by = "enrolid") %>%
        dplyr::mutate(days_within = index_date - date) %>%
        dplyr::filter(days_within >= range[1] & days_within <= range[2])

    }

    outpatient_dxs[[i]] <- tmp

    print(paste0(i, " of ", nrow(outpatient_dx_tables)))

  }

  outpatient_dxs <- bind_rows(outpatient_dxs)
  return(outpatient_dxs)

}
