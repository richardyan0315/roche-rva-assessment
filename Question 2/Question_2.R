# Check whether required packages are installed
required_pkgs <- c("pharmaverseadam", "tidyverse", "ggplot2")
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs, dependencies = TRUE)

suppressPackageStartupMessages({
  library(pharmaverseadam)
  library(tidyverse)
  library(ggplot2)
})

# Version control
sessionInfo()

# Load data
adae <- pharmaverseadam::adae

table(adae$AESEV)
# Define severity level
sev_levels <- c("MILD", "MODERATE", "SEVERE")

# Count unique subjects per SOC x severity
ae_plot_df <- adae %>%
  filter(
    !is.na(USUBJID),
    !is.na(AESOC),
    !is.na(AESEV)
  ) %>%
  # mutate(
  #   AESEV = toupper(trimws(AESEV)) # optional, just in case of the poor data quality
  # ) %>%
  # filter(AESEV %in% sev_levels) %>% # optional, just in case of the poor data quality
  distinct(USUBJID, AESOC, AESEV) %>%
  count(AESOC, AESEV, name = "n_subjects") %>% # ensure only count once 
  complete(AESOC, AESEV = sev_levels, fill = list(n_subjects = 0)) # ensure all level of AE exist for each SOC, even the existence of severe may not always be the case

# Order SOC by increasing total frequency
soc_order_df <- ae_plot_df %>%
  group_by(AESOC) %>%
  summarise(total_subjects = sum(n_subjects), .groups = "drop") %>%
  arrange(total_subjects)

ae_plot_df <- ae_plot_df %>%
  left_join(soc_order_df, by = "AESOC") %>%
  # sorting
  mutate(
    AESEV = factor(AESEV, levels = sev_levels),
    AESOC = factor(AESOC, levels = soc_order_df$AESOC)
  )

# Compute dynamic x-axis upper bound to match the example given
x_max <- ae_plot_df %>%
  group_by(AESOC) %>%
  summarise(total_subjects = sum(n_subjects), .groups = "drop") %>%
  summarise(x_max = max(total_subjects, 150)) %>%
  pull(x_max)

x_breaks <- seq(0, ceiling(x_max / 50) * 50, by = 50)

# ---- Plot ----
plot <- ggplot(ae_plot_df, aes(x = n_subjects, y = AESOC, fill = AESEV)) +
  geom_col(
    width = 0.78,
    position = position_stack(reverse = TRUE)
  ) +
  scale_fill_manual(
    values = c(
      "MILD"     = "#F3D9CF",
      "MODERATE" = "#F08A62",
      "SEVERE"   = "#D73027"
    ),
    breaks = sev_levels,
    drop = FALSE
  ) +
  scale_x_continuous(
    limits = c(0, x_max),
    breaks = x_breaks,
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    title = "Unique Subjects per System Organ Class and Severity Level",
    x     = "Number of Unique Subjects",
    y     = "System Organ Class",
    fill  = "Severity"
  ) +
  theme_minimal(base_family = "Arial", base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.title         = element_text(face = "bold"),
    axis.text.y        = element_text(size = 10),
    legend.title       = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "right"
  )

print(plot)

# ---- Save output ----
ggsave(
  filename = "./Question 2/q2-ae_severity_visualization.png",
  plot     = plot,
  width    = 16,
  height   = 8,
  dpi      = 300,
  bg       = "white"
)
