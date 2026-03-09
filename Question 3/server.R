# ============================================================
# server.R
#
# Purpose:
#   Provide the server logic of the dashboard
# ============================================================

shinyServer(function(input, output, session) {
  
  # Filter to get qualified data
  filtered_ae_data <- reactive({
    req(input$actarm_filter)
    
    filter_ae_data(
      adae = adae,
      actarm_filter = input$actarm_filter,
      sev_levels = sev_levels
    )
    
  })
  
  # Summaries the filtered data into plotting data
  plot_data <- reactive({
    make_ae_plot_data(
      df = filtered_ae_data(),
      sev_levels = sev_levels
    )
  })
  
  # Render the chart and update when the user changes the treatment arm selection
  output$ae_plot <- renderPlot({
    df_plot <- plot_data()
    
    validate(need(nrow(df_plot) > 0, "No vaild data available for the selected treatment arm(s)."))
    
    x_limit <- get_x_axis_limit(df_plot, min_upper = 150)
    
    ggplot(df_plot, aes(x = n_subjects, y = AESOC, fill = AESEV)) +
      geom_col(
        width    = 0.78,
        position = position_stack(reverse = TRUE)
      ) +
      scale_fill_manual(
        values = c(
          "MILD"     = "#F3D9CF",
          "MODERATE" = "#F08A62",
          "SEVERE"   = "#D73027"
        ),
        breaks = sev_levels,
        drop   = FALSE
      ) +
      scale_x_continuous(
        limits = c(0, x_limit),
        breaks = seq(0, ceiling(x_limit / 50) * 50, by = 50),
        expand = c(0, 0)
      ) +
      labs(
        title = "Unique Subjects per System Organ Class and Severity Level",
        x     = "Number of Unique Subjects",
        y     = "System Organ Class",
        fill  = "Severity"
      ) +
      theme_minimal(base_family = "Arial", base_size = 12) +
      theme(
        plot.title         = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.title         = element_text(face = "bold"),
        axis.text.y        = element_text(size = 8),
        legend.title       = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position    = "right"
      )
  }, res = 120)
})

