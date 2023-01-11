gether_inpatient_indicator_facility <- function(all_tabs, con) {

  fac_tabs <- all_tabs[str_detect(all_tabs, "facility_core")]
  res <- list()

  for (i in 1:length(fac_tabs)) {

    res[[i]] <- tbl(con, fac_tabs[i]) %>%
      mutate(outpatient = ifelse(caseid == "", 1, 0)) %>%
      select(fachdid, outpatient) %>%
      collect(n = Inf)

  }

  bind_rows(res)

}
