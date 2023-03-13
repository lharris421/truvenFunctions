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
gether_diagnosis_inpatient <- function(table_list, codes = NULL, dates = NULL, range = NULL, primary = FALSE, year_range = NULL) {

  inpatient_tables <- table_list[str_detect(table_list, "inpatient")] %>%
    str_split("_")

  inpatient_tables <- do.call(rbind, inpatient_tables) %>%
    data.frame()
  colnames(inpatient_tables) <- c("table", "diagnosis", "source", "year")

  inpatient_dx_tables <- inpatient_tables %>%
    filter(str_detect(diagnosis, "dx"))

  if (!is.null(year_range)) {
    inpatient_dx_tables %<>%
      filter(year >= year_range[1] & year <= year_range[2])
  }

  inpatient_dxs <- list()
  for (i in 1:nrow(inpatient_dx_tables)) {

    table <- inpatient_dx_tables[i,1]
    diagnosis <- inpatient_dx_tables[i,2]
    source <- inpatient_dx_tables[i,3]
    year <- inpatient_dx_tables[i,4]
    tab_name <- paste0(c(table, diagnosis, source, year), collapse = "_")
    cross <- paste0(c(table, "core", source, year), collapse = "_")

    tmp <- dplyr::tbl(db_con, tab_name) %>%
      dplyr::mutate(dx = as.character(dx)) %>%
      dplyr::filter(dx_num == 1 | primary == FALSE) %>%
      dplyr::inner_join(dplyr::tbl(db_con, cross) %>% dplyr::select(caseid, date = admdate, enrolid), by = "caseid") %>%
      dplyr::select(-caseid) %>%
      dplyr::collect(n = Inf) %>%
      dplyr::mutate(dx = as.character(dx)) %>%
      dplyr::filter(is.null(codes) | dx %in% codes)

    if (!is.null(dates)) {

      tmp <- tmp %>%
        dplyr::left_join(dates, by = "enrolid") %>%
        dplyr::mutate(days_within = index_date - admdate) %>%
        dplyr::filter(days_within >= range[1] & days_within <= range[2])

    }

    print(tab_name)
    print(tmp)
    inpatient_dxs[[i]] <- tmp

    ## print(i)

  }

  inpatient_dxs <- bind_rows(inpatient_dxs)
  return(inpatient_dxs)

}
