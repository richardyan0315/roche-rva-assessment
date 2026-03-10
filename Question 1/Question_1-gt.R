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

# Load raw data
adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

# Do the initial checking
table(adsl$ACTARM) # checking the potential invalid ACTARM: noticed the existence of "Screen Failure"
table(adsl$SAFFL)  # safe population flag

# Calculate the denominator: the total number of subjects in the study from ADSL
denom_tbl <- adsl %>%
  filter(SAFFL == "Y", !is.na(ACTARM), ACTARM != "Screen Failure") %>%
  distinct(USUBJID, ACTARM) %>%
  count(ACTARM, name = "N") %>%
  arrange(ACTARM != "Placebo", ACTARM)

# do the initial checking
table(adae$TRTEMFL)  # Review Treatment Emergent Adverse Event (TEAE) flags

# Extract TEAE records: System Organ Class (AESOC) and Preferred Term (AEDECOD) (unique by USUBJID)
teae <- adae %>%
  filter(TRTEMFL == "Y", !is.na(ACTARM)) %>%
  distinct(USUBJID, ACTARM, AESOC, AEDECOD)

# Generate SOC summary rows (subjects with at least one TEAE in that SOC)
soc_counts <- teae %>%
  distinct(USUBJID, ACTARM, AESOC) %>%
  count(AESOC, ACTARM, name = "n") %>%
  mutate(AEDECOD = NA_character_)   # mark as SOC row 

# Generate PT detail rows (subjects per PT)
pt_counts <- teae %>%
  count(AESOC, AEDECOD, ACTARM, name = "n")

# Combine SOC and PT rows
counts_all <- bind_rows(soc_counts, pt_counts)


# Ensure every SOC/PT has at least a record for each treatment arm (fill potential zeros)
# All SOC and PT combinations (from adae), keep only SOCs with TEAE
all_soc_pt <- adae %>%
  distinct(AESOC, AEDECOD) %>%
  semi_join(distinct(teae, AESOC), by = "AESOC")

# Build complete grid: each SOC row + each PT row × all treatment arms
complete_grid <- bind_rows(
  all_soc_pt %>% distinct(AESOC) %>% mutate(AEDECOD = NA_character_),  # SOC rows
  all_soc_pt                                                           # PT rows
) %>%
  expand(nesting(AESOC, AEDECOD), ACTARM = denom_tbl$ACTARM)

# Left join actual counts, fill missing with 0
counts_complete <- complete_grid %>%
  left_join(counts_all, by = c("AESOC", "AEDECOD", "ACTARM")) %>%
  mutate(n = replace_na(n, 0))

# Add total row (subjects with at least one TEAE)
total_counts <- teae %>%
  distinct(USUBJID, ACTARM) %>%
  count(ACTARM, name = "n") %>%
  mutate(AESOC = "Treatment Emergent Adverse Events",
         AEDECOD = NA_character_)

counts_final <- bind_rows(total_counts, counts_complete)

# Join denominators and generate formatted cells

# Formatting function: keep one decimal, remove trailing zeros
fmt_n_pct <- function(n, N) {
  pct <- ifelse(N > 0, 100 * n / N, NA_real_)
  pct_str <- ifelse(
    pct >= 10 & !is.na(pct),
    as.character(round(pct)),              # ≥10% round to integer
    sprintf("%.1f", pct)                   # <10% keep one decimal (including trailing zero)
  )
  ifelse(N > 0 & !is.na(pct),
         paste0(n, " (", pct_str, "%)"),
         "0 (0%)")
}

# Generate percentage by cell/N
counts_final2 <- counts_final %>%
  left_join(denom_tbl, by = "ACTARM") %>%
  mutate(cell = fmt_n_pct(n, N))

# Convert to wide format
tab_wide <- counts_final2 %>%
  select(AESOC, AEDECOD, ACTARM, cell) %>%
  pivot_wider(names_from = ACTARM, values_from = cell) %>%
  relocate(all_of(denom_tbl$ACTARM))

# Build display table and apply desired sorting
tab_display <- tab_wide %>%
  mutate(
    # Display text: SOC rows show AESOC directly, PT rows indented with four &nbsp;
    `System Organ Class / Preferred Term` =
      ifelse(is.na(AEDECOD),
             AESOC,
             paste0("&nbsp;&nbsp;&nbsp;&nbsp;", AEDECOD)),
    # Helper sort: total row first, then SOC grouping with SOC rows before PTs
    sort_order = case_when(
      AESOC == "Treatment Emergent Adverse Events" ~ 0L,      # total row first
      TRUE ~ 1L                                               # others
    )
  ) %>%
  # Sorting logic: by sort_order (total=0), then AESOC alphabetically,
  # then SOC row first (TRUE before FALSE), then AEDECOD alphabetically (for PTs only)
  arrange(sort_order, AESOC, !is.na(AEDECOD), AEDECOD) %>%
  select(`System Organ Class / Preferred Term`, all_of(denom_tbl$ACTARM))

# Construct column labels (with N values, using HTML format)
arm_labels <- denom_tbl %>%
  mutate(label = paste0(ACTARM, "<br><span style='font-weight: normal;'>N = ", N, "</span>"))

# Create table
gt_tab <- tab_display %>%
  gt() %>%
  # Render first column as Markdown (so that &nbsp; works)
  fmt_markdown(columns = `System Organ Class / Preferred Term`) %>%
  cols_label(
    `System Organ Class / Preferred Term` = "System Organ Class / Preferred Term",
    .list = setNames(lapply(arm_labels$label, gt::html), arm_labels$ACTARM)
  ) %>%
  # Add one shared footnote to all treatment-arm column labels
  tab_footnote(
    footnote = "N = total number of subjects in the safety population.",
    locations = cells_column_labels(columns = all_of(denom_tbl$ACTARM))
  ) %>%
  # Center-align numeric columns
  cols_align(
    align = "center",
    columns = all_of(denom_tbl$ACTARM)
  ) %>%
  # Bold all column labels
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

# Save the html
out_file <- "./Question 1/q1-teae_summary_table_gt.html"
gtsave(gt_tab, out_file)
message("HTML table completed, saved to: ", normalizePath(out_file, mustWork = FALSE))

