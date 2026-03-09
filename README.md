# roche-rva-assessment

This repository is for the Code assessment submissions of Roche RVA Specialist position.

A version of R 4.2.0 and above is recommended. The necessary R packages, along with all dependencies, can be installed from CRAN using the following command.

``` r
install.packages(c("pharmaverseadam", "tidyverse", "gtsummary", "ggplot2", "shiny"))
```

There are three folders, each folder refers to a single question in the assessment, including the R codes and corresponding outputs. Specifically,

-   **Question 1 (*TEAE Summary Table*)**:

    -   `Question_1-gt.R`: the analysis pipeline for generating a summary table of Treatment-Emergent Adverse Events, primarily based on `gt` package.
    -   `Question_1-gtsummary.R`: the analysis pipeline for generating a summary table of Treatment-Emergent Adverse Events, primarily based on `gtsummary` package.
    -   `q1-tate_summary_table_gt.html`: the output table saved in HTML format by `Question_1-gt.R`.
    -   `q1-tate_summary_table_gtsummary.html`: the output table saved in HTML format by `Question_1-gtsummary.R`.

-   **Question 2 (*AE Severity Visualization*)**:

    -   `Question_2.R`: the visualization pipeline for generating a bar chart of the distribution of adverse events.
    -   `q2-ae_severity_visualization.png`: the output bar chart saved in PNG format.

-   **Question 3 (*Interactive R Shiny Application*)**:

    -   `global.R`: prepare the shared shiny environment.

    -   `server.R`: provide the server logic of the dashboard.

    -   `ui.R`: define the user interface of the dashboard.

    -   `functions`: a folder including necessary helper functions. Each function is saved as a single R file as follows.

        -   `filter_ae_data.R`: Cleans and subsets the ADAE dataset by user-selected treatment arms, handling missing data and standardizing severity string formatting.

        -   `get_actarm_choices.R`: extracts and sorts unique, non-missing treatment arms (ACTARM) to dynamically populate UI filter selections.

        -   `get_x_axis_limit.R`: Calculates a dynamic upper limit for the chart's X-axis based on reactive data, ensuring visual stability and preventing axis scale shifting during UI interaction.

        -   `make_ae_plot_data.R`: Aggregates filtered data into unique subject counts per System Organ Class (SOC) and Severity, managing missing zero-counts and sorting SOCs by overall frequency for optimal plotting.

    -   To Launch the App:

        -   Via **RStudio**: Open any of the core files (`global.R`, `server.R`, or `ui.R`) and click **Run App**.
        -   Via **Console**: Ensure your working directory is set to the project root and run:

        ``` r
        shiny::runApp()
        ```
