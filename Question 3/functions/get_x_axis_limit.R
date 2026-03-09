get_x_axis_limit <- function(df_plot, min_upper = 150) {
  # Calculate the maximum number of total subjects across all SOCs
  total_max <- df_plot %>%
    dplyr::group_by(AESOC) %>%
    # Sum subjects across all severity levels for each SOC
    dplyr::summarise(total_subjects = sum(n_subjects), .groups = "drop") %>%
    # Find the highest SOC total
    dplyr::summarise(x_max = max(total_subjects), .groups = "drop") %>%
    dplyr::pull(x_max)
  
  # Return the calculated max or the defined minimum upper bound, whichever is larger.
  # This prevents the x-axis from jumping around too drastically when filtering.
  max(min_upper, total_max)
}