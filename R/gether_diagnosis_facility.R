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
gether_diagnosis_facility <- function(table_list, codes = NULL, dates = NULL, range = NULL, primary = FALSE, year_range = NULL) {

  facility_tables <- table_list[str_detect(table_list, "facility")] %>%
    str_split("_")

  facility_tables <- do.call(rbind, facility_tables) %>%
    data.frame()
  colnames(facility_tables) <- c("table", "diagnosis", "source", "year")

  facility_dx_tables <- facility_tables %>%
    filter(str_detect(diagnosis, "dx"))

  if (!is.null(year_range)) {
    facility_dx_tables %<>%
      filter(year >= year_range[1] & year <= year_range[2])
  }

  facility_dxs <- list()
  for (i in 1:nrow(facility_dx_tables)) {

    table <- facility_dx_tables[i,1]
    diagnosis <- facility_dx_tables[i,2]
    source <- facility_dx_tables[i,3]
    year <- facility_dx_tables[i,4]
    tab_name <- paste0(c(table, diagnosis, source, year), collapse = "_")

    tmp <- dplyr::tbl(db_con, tab_name) %>%
      dplyr::mutate(dx = as.character(dx)) %>%
      dplyr::filter(dx_num == 1 | primary == FALSE) %>%
      dplyr::select(enrolid, date = svcdate, dx_num, dx, fachdid) %>%
      dplyr::collect(n = Inf) %>%
      dplyr::mutate(dx = as.character(dx)) %>%
      dplyr::filter(is.null(codes) | dx %in% codes)

    if (!is.null(dates)) {

      tmp <- tmp %>%
        dplyr::left_join(dates, by = "enrolid") %>%
        dplyr::mutate(days_within = index_date - date) %>%
        dplyr::filter(days_within >= range[1] & days_within <= range[2])

    }

    print(paste0(i, " of ", nrow(facility_dx_tables)))
    facility_dxs[[i]] <- tmp


  }

  facility_dxs <- bind_rows(facility_dxs)

  facility_core_tables <- all_tables[str_detect(table_list, "facility_core")]
  facility_inpatient_outpatient <- list()

  for (i in 1:length(facility_core_tables)) {

    facility_inpatient_outpatient[[i]] <- tbl(db_con, facility_core_tables[i]) %>%
      mutate(inpatient = ifelse(caseid == "", 0, 1)) %>%
      select(fachdid, inpatient) %>%
      collect(n = Inf)

  }

  facility_inpatient_outpatient <- bind_rows(facility_inpatient_outpatient)

  facility_dxs %<>%
    left_join(facility_inpatient_outpatient, by = "fachdid") %>%
    mutate(inpatient = ifelse(is.na(inpatient) | inpatient == 0, 0, 1)) %>%
    select(-fachdid)

  return(facility_dxs)

}
