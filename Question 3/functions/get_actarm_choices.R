get_actarm_choices <- function(adae) {
  adae %>%
    # Remove records where Actual Treatment Arm is missing
    dplyr::filter(!is.na(ACTARM)) %>%
    # Retain only unique treatment arms
    dplyr::distinct(ACTARM) %>%
    # Sort the treatment arms alphabetically for a clean UI presentation
    dplyr::arrange(ACTARM) %>%
    # Extract the column as a vector to be used in checkboxGroupInput
    dplyr::pull(ACTARM)
}