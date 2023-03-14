#' Title
#'
#' @param collection_table
#' @param codes
#'
#' @return
#' @export
#'
#' @examples
gether_outpatient_core <- function(table_list, db_con, codes = NULL, dates = NULL, range = NULL, year_range = NULL) {

  outpatient_tables <- table_list[str_detect(table_list, "outpatient")] %>%
    str_split("_")

  outpatient_tables <- do.call(rbind, outpatient_tables) %>%
    data.frame()
  colnames(outpatient_tables) <- c("table", "diagnosis", "source", "year")

  outpatient_proc_tables <- outpatient_tables %>%
    filter(str_detect(diagnosis, "core"))

  if (!is.null(year_range)) {
    outpatient_proc_tables %<>%
      filter(year >= year_range[1] & year <= year_range[2])
  }

  outpatient_procs <- list()
  for (i in 1:nrow(outpatient_proc_tables)) {

    table <- outpatient_proc_tables[i,1]
    diagnosis <- outpatient_proc_tables[i,2]
    source <- outpatient_proc_tables[i,3]
    year <- outpatient_proc_tables[i,4]
    tab_name <- paste0(c(table, diagnosis, source, year), collapse = "_")

    tmp <- dplyr::tbl(db_con, tab_name) %>%
      dplyr::mutate(proc = as.character(proc1)) %>%
      dplyr::select(enrolid, date = svcdate, proc) %>%
      dplyr::collect(n = Inf) %>%
      dplyr::mutate(proc = as.character(proc)) %>%
      dplyr::filter(is.null(codes) | proc %in% codes)

    if (!is.null(dates)) {

      tmp <- tmp %>%
        dplyr::left_join(dates, by = "enrolid") %>%
        dplyr::mutate(days_within = index_date - date) %>%
        dplyr::filter(days_within >= range[1] & days_within <= range[2])

    }

    print(paste0(i, " of ", nrow(outpatient_proc_tables)))
    outpatient_procs[[i]] <- tmp


  }

  outpatient_procs <- bind_rows(outpatient_procs)
  return(outpatient_procs)

}
