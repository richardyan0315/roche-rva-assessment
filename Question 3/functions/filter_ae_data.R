filter_ae_data <- function(adae, actarm_filter, sev_levels) {
  adae %>%
    
    # Filter dataset based on selected treatment arms and remove invalid records.
    # Records with missing IDs, SOC, or severity cannot be used
    # in the subject-level summary and are excluded before aggregation
    dplyr::filter(
      ACTARM %in% actarm_filter,
      !is.na(USUBJID),    # Unique Subject Identifier
      !is.na(AESOC),      # AE System Organ Class term
      !is.na(AESEV)       # AE Severity
    ) %>%
    
    # Standardize the severity text to uppercase and remove leading/trailing spaces
    # to prevent case-sensitivity issues during grouping
    dplyr::mutate(
      AESEV = toupper(trimws(AESEV))
    ) %>%
    
    # Keep only the records that match the predefined valid severity levels
    dplyr::filter(AESEV %in% sev_levels)
}