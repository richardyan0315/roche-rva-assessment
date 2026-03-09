make_ae_plot_data <- function(df, sev_levels) {
  # Calculate the number of unique subjects for each SOC and Severity combination
  ae_plot_df <- df %>%
    # Ensure only count a subject once per SOC and Severity level
    dplyr::distinct(USUBJID, AESOC, AESEV) %>%
    # Count the occurrences, creating a 'n_subjects' column
    dplyr::count(AESOC, AESEV, name = "n_subjects") %>%
    # Pad missing severity combinations with 0 to ensure consistent chart colors/legend
    tidyr::complete(AESOC, AESEV = sev_levels, fill = list(n_subjects = 0))
  
  # Determine the overall order of System Organ Classes based on total subject counts
  soc_order_df <- ae_plot_df %>%
    dplyr::group_by(AESOC) %>%
    dplyr::summarise(total_subjects = sum(n_subjects), .groups = "drop") %>%
    # Sort SOCs in descending order of frequency
    dplyr::arrange(dplyr::desc(total_subjects))
  
  # Merge the ordering information back into the plotting dataset
  ae_plot_df %>%
    dplyr::left_join(soc_order_df, by = "AESOC") %>%
    # Convert columns to factors with specific levels to enforce plotting order in ggplot2
    dplyr::mutate(
      AESEV = factor(AESEV, levels = sev_levels),
      AESOC = factor(AESOC, levels = soc_order_df$AESOC),
      # Reverse the SOC factor levels so the highest count appears at the top of the bar chart
      AESOC = forcats::fct_rev(AESOC)
    )
}