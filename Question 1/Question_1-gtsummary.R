# Check whether required packages are installed
required_pkgs <- c("pharmaverseadam", "tidyverse", "gtsummary")
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs, dependencies = TRUE)

suppressPackageStartupMessages({
  library(pharmaverseadam)
  library(tidyverse)
  library(gtsummary)
})

# Version control
sessionInfo()

# Load raw datasets
adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

# Perform initial data exploration of adsl
table(adsl$ACTARM) # Check for invalid treatment arms (e.g., identifying "Screen Failure" subjects)
table(adsl$SAFFL)  # Review the distribution of the Safety Population flag

# Filter the denominator records for percentage calculations
adsl_saf <- adsl %>%
  filter(SAFFL == "Y", !is.na(ACTARM), ACTARM != "Screen Failure") %>%
  select(USUBJID, ACTARM) %>%
  # Relevel ACTARM factor to ensure "Placebo" appears as the first column in the final table
  mutate(ACTARM = fct_relevel(as.factor(ACTARM), "Placebo"))

# Perform initial data exploration of adae
table(adae$TRTEMFL) # Review Treatment Emergent Adverse Event (TEAE) flags

# Filter for valid TEAEs among patients with an assigned treatment arm
adae_teae <- adae %>%
  filter(TRTEMFL == "Y", !is.na(ACTARM))

# Construct Wide-Format Dichotomous Variables
# create a TRUE/FALSE flag for every level of AE

# Extract the "Overall" occurrence of any TEAE per subject
df_overall <- adae_teae %>%
  distinct(USUBJID) %>%
  mutate(term = "Treatment Emergent Adverse Events", type = "Overall", sort1 = 0, sort2 = "")

# Extract the occurrence of AEs at the System Organ Class (SOC) level per subject
df_soc <- adae_teae %>%
  distinct(USUBJID, AESOC) %>%
  mutate(term = AESOC, type = "SOC", sort1 = 1, sort2 = AESOC)

# Extract the occurrence of AEs at the Preferred Term (PT) level per subject
df_pt <- adae_teae %>%
  distinct(USUBJID, AESOC, AEDECOD) %>%
  mutate(term = AEDECOD, type = "PT", sort1 = 1, sort2 = AESOC)

# Combine all levels and create a comprehensive data dictionary
df_all_terms <- bind_rows(df_overall, df_soc, df_pt) %>% mutate(val = TRUE)

# Define the data dictionary to control precise sorting and indentation
term_dict <- df_all_terms %>%
  distinct(type, term, sort1, sort2) %>%
  # Sorting by: Overall first (0) -> Alphabetical by SOC -> SOC summary precedes PTs -> Alphabetical by PT
  arrange(sort1, sort2, type == "PT", term) %>%
  mutate(
    # Generate sequential dummy column names (v_1, v_2...) representing the exact display order
    col_name = paste0("v_", row_number()), 
    # Apply four Unicode non-breaking spaces (\U00A0) for stable HTML indentation on PT rows
    label = ifelse(type == "PT", paste0("\U00A0\U00A0\U00A0\U00A0", term), term)
  )

# Pivot to wide format and merge with the ADSL denominator
adsl_analysis <- df_all_terms %>%
  left_join(term_dict, by = c("type", "term", "sort1", "sort2")) %>%
  distinct(USUBJID, col_name, val) %>%
  # Pivot into a wide matrix where each AE term is a distinct boolean column
  pivot_wider(names_from = col_name, values_from = val, values_fill = FALSE) %>%
  # Right join with ADSL to ensure subjects with zero AEs are included in the denominator
  right_join(adsl_saf, by = "USUBJID") %>%
  # Replace NA with FALSE for subjects who did not experience specific AEs
  mutate(across(starts_with("v_"), ~replace_na(., FALSE))) %>%
  # Enforce strictly ordered columns based on the dictionary to maintain the SOC/PT hierarchy
  select(USUBJID, ACTARM, all_of(term_dict$col_name))

# Table Generation: Render using gtsummary
# Map the auto-generated column names (v_1, v_2...) back to their indented descriptive labels
var_labels <- setNames(as.list(term_dict$label), term_dict$col_name)

final_gtsummary <- adsl_analysis %>%
  select(-USUBJID) %>%
  tbl_summary(
    by      = ACTARM,
    type    = everything() ~ "dichotomous", # Treat every column as a binary Yes/No
    value   = everything() ~ TRUE,          # Only count the TRUE occurrences
    label   = var_labels,                   # Apply the mapped labels with Unicode indentation
    missing = "no"                          # Suppress default missing value rows
  ) %>%
  # Customize header formatting and add "N=xxx" to the column titles
  modify_header(
    label = "**System Organ Class / Preferred Term**",
    all_stat_cols() ~ "**{level}**<br><span style='font-weight: normal;'>N = {n}</span>"
  ) %>%
  modify_footnote(all_stat_cols() ~ "N = total number of subjects in the safety population.") %>%
  modify_column_alignment(columns = all_stat_cols(), align = "center") %>%
  as_gt() # Convert to gt object for final rendering and saving

# Save the resulting table as an HTML file
out_file <- "./Question 1/q1-teae_summary_table_gtsummary.html"
gtsave(final_gtsummary, out_file)
message("HTML table completed, saved to: ", normalizePath(out_file, mustWork = FALSE))
