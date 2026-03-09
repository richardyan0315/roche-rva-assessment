# ============================================================
# ui.R
#
# Purpose:
#   Define the user interface of the dashboard
# ============================================================

shinyUI(
  fluidPage(
    
    # Title for the shiny app
    titlePanel("AE Summary Interactive Dashboard"),
    
    # Sidebar: separate user controls from visualization output
    sidebarLayout(
      sidebarPanel(
        
        # Checkboxes: to filter the chart by treatment arm (ACTARM),  allowing multi-arms
        #   All treatment arms are selected by default so that the initial
        #   view matches the full-dataset summary in Q2
        checkboxGroupInput(
          inputId = "actarm_filter",
          label = "Select Treatment Arm(s):",
          choices = actarm_choices,
          selected = actarm_choices
        ),
        width = 3
      ),
      
      # Main panel: stacked bar chart
      mainPanel(
        plotOutput("ae_plot", height = "800px"),
        width = 9
      )
      
    )
  )
)

