library(shiny)
library(shinyjs)
library(phyloseq)
library(tidyverse)
library(vegan)
library(DT)
library(RColorBrewer)
library(cowplot)
library(openai)
library(httr)
library(jsonlite)

ui <- fluidPage(
  useShinyjs(),  # for JavaScript operations
  
  # Add custom CSS
  tags$head(
    tags$style(HTML("
      .btn-warning {
        margin-top: 15px;
        margin-bottom: 15px;
        width: 100%;
      }
      .notification {
        position: fixed;
        top: 10px;
        right: 10px;
        z-index: 9999;
      }
      .wrapped-output {
        white-space: pre-wrap;       /* preserve line breaks but wrap text */
        word-wrap: break-word;       /* break long words */
        font-family: monospace;      /* keep monospace font */
        background-color: #f8f9fa;   /* light background */
        padding: 10px;              /* add some padding */
        border-radius: 4px;         /* rounded corners */
        border: 1px solid #dee2e6;  /* subtle border */
        max-width: 100%;            /* ensure it doesn't overflow container */
        height: auto;               /* adjust height automatically */
        overflow-y: auto;           /* add scrollbar if needed */
      }
      .api-token-section {
        position: relative;
      }
      .show-hide-btn {
        position: absolute;
        right: 10px;
        top: 50%;
        transform: translateY(-50%);
        z-index: 100;
        background: none;
        border: none;
        color: #666;
        cursor: pointer;
        padding: 5px 10px;
      }
      .show-hide-btn:hover {
        color: #333;
      }
    "))
  ),
  
  titlePanel("Microbiome Diversity Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # File upload UI (always visible)
      fileInput("physeq_file", "Upload Phyloseq Object (RDS)", accept = ".rds"),
      
      # Conditional panels for plot and stats controls
      conditionalPanel(
        condition = "input.main_tabs == 'Plots'",
        # Add normalization toggle before other plot controls
        checkboxInput("normalize_alpha", "Show Normalized Values (0-1)", value = FALSE),
        
        # Plot-specific controls
        checkboxGroupInput("alpha_metrics", "Alpha Diversity Metrics",
                          choices = c("Observed", "Shannon", "Simpson"),
                          selected = c("Observed", "Shannon", "Simpson")),
        
        selectInput("color_palette", "Color Palette (for points/lines)",
                   choices = rownames(RColorBrewer::brewer.pal.info),
                   selected = "Set1"),
        
        selectInput("fill_palette", "Fill Palette (for violin plots)",
                   choices = rownames(RColorBrewer::brewer.pal.info),
                   selected = "Pastel1"),
        
        uiOutput("alpha_var_select")
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'Statistics'",
        # Statistics-specific controls
        uiOutput("stats_controls")
      ),
      
      # Dynamic reset button
      uiOutput("reset_button")
    ),
    
    mainPanel(
      tabsetPanel(id = "main_tabs",
        tabPanel("Plots",
          # Plot download panel
          div(
            actionButton("toggle_plot_download", "Show Download Options",
                        class = "collapse-button",
                        `data-target` = "#plot_download_panel"),
            
            div(id = "plot_download_panel", class = "collapse",
              wellPanel(
                fluidRow(
                  column(12,
                    h4("Download Plot"),
                    div(style = "display: inline-block; vertical-align: top; margin-right: 10px;",
                        numericInput("plot_width", "Width (inches)", value = 8, min = 1, max = 20)),
                    div(style = "display: inline-block; vertical-align: top; margin-right: 10px;",
                        numericInput("plot_height", "Height (inches)", value = 6, min = 1, max = 20)),
                    div(style = "display: inline-block; vertical-align: top; margin-right: 10px;",
                        numericInput("plot_dpi", "DPI", value = 300, min = 72, max = 600)),
                    div(style = "display: inline-block; vertical-align: top;",
                        downloadButton("download_plot_png", "Download PNG"),
                        downloadButton("download_plot_pdf", "Download PDF"))
                  )
                )
              )
            )
          ),
          
          # Plot output
          plotOutput("alpha_plot"),
          
          # Basic plot controls
          wellPanel(
            uiOutput("alpha_var_select")
          ),
          
          # Advanced plot options button
          actionButton("toggle_advanced_options", "Show Advanced Plot Options",
                      class = "collapse-button",
                      `data-target` = "#advanced_plot_panel"),
          
          # Advanced plot options panel
          div(id = "advanced_plot_panel", class = "collapse",
            wellPanel(
              h4("Advanced Plot Options"),
              
              # Plot title options
              textInput("plot_title", "Plot Title", ""),
              numericInput("title_size", "Title Font Size", value = 16, min = 8, max = 30),
              
              # Axis labels
              textInput("x_axis_label", "X-axis Label", ""),
              textInput("y_axis_label", "Y-axis Label", "Value"),
              numericInput("axis_title_size", "Axis Title Font Size", value = 14, min = 8, max = 24),
              numericInput("axis_text_size", "Axis Text Font Size", value = 12, min = 6, max = 20),
              
              # Legend customization
              textInput("legend_title", "Legend Title", ""),
              numericInput("legend_title_size", "Legend Title Font Size", value = 12, min = 8, max = 24),
              numericInput("legend_text_size", "Legend Text Font Size", value = 10, min = 6, max = 20),
              
              # Custom legend labels
              uiOutput("legend_label_inputs"),
              
              # Caption
              textInput("plot_caption", "Plot Caption", ""),
              numericInput("caption_size", "Caption Font Size", value = 10, min = 6, max = 16),
              
              # Add facet layout controls
              h4("Facet Layout"),
              numericInput("facet_nrow", "Number of Rows", 
                           value = NULL, min = 1, max = 10),
              numericInput("facet_ncol", "Number of Columns", 
                           value = NULL, min = 1, max = 10)
              
              
            )
          )
        ),
        
        tabPanel("Statistics",
          br(),
          # Add toggle button for download options
          actionButton("toggle_stats_download", "Show Download Options",
                      class = "collapse-button",
                      `data-target` = "#stats_download_panel"),
          
          # Add download panel
          div(id = "stats_download_panel", class = "collapse",
            wellPanel(
              downloadButton("download_stats_csv", "Download CSV")
            )
          ),
          
          # Table output
          DT::DTOutput("stats_table"),
          
          # Updated interpretation section
          div(
            actionButton("toggle_interpret", "Show/Hide AI Interpretation", 
                        class = "btn-info"),
            br(), br(),
            shinyjs::useShinyjs(),
            shinyjs::hidden(
              div(id = "interpretation_panel",
                wellPanel(
                  h4("AI Results Interpretation"),
                  # Add copy button
                  div(style = "position: relative;",
                    actionButton("copy_interpretation", "Copy Text", 
                                class = "btn-default",
                                style = "position: absolute; right: 0; top: -40px;"),
                    # Existing API token section...
                    div(class = "api-token-section",
                        passwordInput("hf_key", "Hugging Face API Token", 
                                    value = "", 
                                    width = "100%",
                                    placeholder = "Enter your Hugging Face API token here"),
                        actionButton("toggle_token_visibility", 
                                   label = NULL,
                                   icon = icon("eye"),
                                   class = "show-hide-btn",
                                   title = "Show/Hide API Token")
                    ),
                    div(
                      style = "margin-top: 10px; margin-bottom: 5px; font-size: 0.8em; color: #666;",
                      "Your API token is only used for this session and is not stored."
                    ),
                    # Add experimental context input
                    tags$div(
                      style = "margin-top: 15px;",
                      textAreaInput(
                        "experiment_context",
                        "Experimental Context (optional)",
                        value = "",
                        width = "100%",
                        height = "100px",
                        placeholder = "Describe your experiment here (e.g., study objectives, experimental design, important variables, expected outcomes)"
                      ),
                      div(
                        style = "margin-top: 5px; margin-bottom: 15px; font-size: 0.8em; color: #666;",
                        "Adding context will help the AI provide more relevant interpretations."
                      )
                    ),
                    actionButton("interpret_results", "Interpret Results", 
                               class = "btn-primary"),
                    br(), br(),
                    textOutput("interpretation_loading"),
                    uiOutput("interpretation_text")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

# Add custom CSS to the UI
ui <- tagList(
  tags$head(
    tags$style(HTML("
      .collapse {
        display: none;
      }
      .collapse.in {
        display: block;
      }
      .collapse-button {
        margin-bottom: 10px;
      }
      .collapsible {
        overflow: hidden;
        transition: max-height 0.3s ease-out;
      }
    "))
  ),
  ui
)

server <- function(input, output, session) {
  # Load default example dataset
  physeq_default <- reactive({
    data_path <- normalizePath(file.path(getwd(), "data", "my_physeq.rds"))
    
    readRDS(data_path) %>% 
      filter_taxa(function(x) sum(x > 0) > 0, TRUE)
  })
  
  # Reactive phyloseq object
  physeq <- reactive({
    if(!is.null(input$physeq_file)) {
      readRDS(input$physeq_file$datapath) %>% 
        filter_taxa(function(x) sum(x > 0) > 0, TRUE)
    } else {
      physeq_default()
    }
  })
  
  # Helper function to normalize values between 0 and 1
  normalize_metric <- function(x) {
    if (length(unique(x)) == 1) return(rep(1, length(x)))  # Handle constant values
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }
  
  # Modify the alpha_df reactive to include normalized values
  alpha_df <- reactive({
    req(physeq())
    
    # Get alpha diversity metrics
    alpha_div <- data.frame(
      Observed = phyloseq::estimate_richness(physeq(), measures = "Observed"),
      Shannon = phyloseq::estimate_richness(physeq(), measures = "Shannon"),
      Simpson = phyloseq::estimate_richness(physeq(), measures = "Simpson")
    )
    
    # Add normalized versions
    alpha_div$Observed_norm <- normalize_metric(alpha_div$Observed)
    alpha_div$Shannon_norm <- normalize_metric(alpha_div$Shannon)
    alpha_div$Simpson_norm <- normalize_metric(alpha_div$Simpson)
    
    # Combine with sample data
    cbind(alpha_div, data.frame(sample_data(physeq())))
  })
  
  # Alpha diversity plot controls
  output$alpha_var_select <- renderUI({
    meta_vars <- names(sample_data(physeq()))
    tagList(
      # X-axis variable
      div(style = "display: flex; align-items: flex-start; gap: 10px;",
        div(style = "flex: 2;",
          selectInput("alpha_x", "X-axis Variable", 
                     choices = meta_vars,
                     selected = "Treatment")
        ),
        div(style = "flex: 1;",
          selectInput("alpha_x_type", "Data Type",
                     choices = c("default", "factor", "numeric"),
                     selected = "default")
        )
      ),
      
      # Color variable
      div(style = "display: flex; align-items: flex-start; gap: 10px;",
        div(style = "flex: 2;",
          selectInput("alpha_color", "Color By", 
                     choices = meta_vars,
                     selected = "Treatment")
        ),
        div(style = "flex: 1;",
          selectInput("alpha_color_type", "Data Type",
                     choices = c("default", "factor", "numeric"),
                     selected = "default")
        )
      ),
      
      # Fill variable
      div(style = "display: flex; align-items: flex-start; gap: 10px;",
        div(style = "flex: 2;",
          selectInput("alpha_fill", "Fill By", 
                     choices = meta_vars,
                     selected = "Treatment")
        ),
        div(style = "flex: 1;",
          selectInput("alpha_fill_type", "Data Type",
                     choices = c("default", "factor", "numeric"),
                     selected = "default")
        )
      ),
      
      # Shape variable
      div(style = "display: flex; align-items: flex-start; gap: 10px;",
        div(style = "flex: 2;",
          selectInput("alpha_shape", "Shape By", 
                     choices = c("None", meta_vars))
        ),
        div(style = "flex: 1;",
          selectInput("alpha_shape_type", "Data Type",
                     choices = c("default", "factor", "numeric"),
                     selected = "default")
        )
      ),
      
      # Legend controls
      selectInput("alpha_legend_position", "Legend Position",
                 choices = c("none", "bottom", "left", "right", "top"),
                 selected = "none"),
      selectInput("alpha_legend_direction", "Legend Direction",
                 choices = c("vertical", "horizontal"),
                 selected = "vertical"),
      
      # Facet variables
      div(style = "display: flex; align-items: flex-start; gap: 10px;",
        div(style = "flex: 2;",
          selectizeInput("alpha_facet_x", "Facet by (Columns)", 
                        choices = c("None", meta_vars),
                        multiple = TRUE)
        ),
        div(style = "flex: 1;",
          selectInput("alpha_facet_x_type", "Data Type",
                     choices = c("default", "factor", "numeric"),
                     selected = "default")
        )
      ),
      
      div(style = "display: flex; align-items: flex-start; gap: 10px;",
        div(style = "flex: 2;",
          selectizeInput("alpha_facet_y", "Facet by (Rows)", 
                        choices = c("None", meta_vars),
                        multiple = TRUE)
        ),
        div(style = "flex: 1;",
          selectInput("alpha_facet_y_type", "Data Type",
                     choices = c("default", "factor", "numeric"),
                     selected = "default")
        )
      )
    )
  })
  
  # Helper function to convert variable type
  convert_var_type <- function(data, var_name, type) {
    if (type == "default" || is.null(var_name) || var_name == "None") {
      return(data[[var_name]])
    } else if (type == "factor") {
      return(as.factor(data[[var_name]]))
    } else if (type == "numeric") {
      return(as.numeric(as.character(data[[var_name]])))
    }
  }
  
  # Dynamic legend label inputs based on selected grouping variable
  output$legend_label_inputs <- renderUI({
    req(input$alpha_color)
    
    # Get unique values from the grouping variable
    group_data <- alpha_df()[[input$alpha_color]]
    if(!is.null(group_data)) {
      unique_groups <- sort(unique(group_data))
      
      tagList(
        h4("Custom Legend Labels"),
        lapply(seq_along(unique_groups), function(i) {
          textInput(
            inputId = paste0("legend_label_", i),
            label = paste("Label for", unique_groups[i]),
            value = as.character(unique_groups[i])
          )
        })
      )
    }
  })
  
  # Helper function to get custom labels for aesthetics
  get_custom_labels <- function(var_name, var_type) {
    if(!is.null(input[[var_name]]) && input[[var_name]] != "None") {
      group_data <- alpha_df()[[input[[var_name]]]]
      if(!is.null(group_data)) {
        unique_groups <- sort(unique(group_data))
        
        # Get custom labels if they exist, otherwise use original values
        custom_labels <- sapply(seq_along(unique_groups), function(i) {
          label_input <- input[[paste0("legend_label_", var_type, "_", i)]]
          if(!is.null(label_input) && label_input != "") {
            label_input
          } else {
            as.character(unique_groups[i])
          }
        })
        
        names(custom_labels) <- unique_groups
        return(custom_labels)
      }
    }
    return(NULL)
  }
  
  # Modify the alpha plot with corrected grouping syntax
  output$alpha_plot <- renderPlot({
    req(input$alpha_x, input$alpha_color)
    
    # Add helper function for facet dimensions
    get_facet_dim <- function(input_value) {
      if (is.null(input_value) || is.na(input_value) || input_value < 1) {
        return(NULL)
      }
      return(as.integer(input_value))
    }
    
    tryCatch({
      # Modify the data preparation to use normalized values if selected
      plot_data <- alpha_df() %>%
        tidyr::pivot_longer(
          cols = if(input$normalize_alpha) {
            paste0(input$alpha_metrics, "_norm")
          } else {
            input$alpha_metrics
          },
          names_to = "Metric",
          values_to = "Value"
        )
      
      # Clean up metric names if using normalized values
      if(input$normalize_alpha) {
        plot_data$Metric <- gsub("_norm$", "", plot_data$Metric)
      }
      
      # Helper function to create a single plot
      create_single_plot <- function(data, metric_name) {
        # Filter data for this metric
        metric_data <- data %>% dplyr::filter(Metric == metric_name)
        
        # Initialize plot with basic aesthetics
        base_aes <- list(
          x = input$alpha_x,
          y = "Value"
        )
        
        # Add conditional aesthetics
        if (!is.null(input$alpha_color)) {
          base_aes$color <- input$alpha_color
          metric_data$group <- interaction(metric_data[[input$alpha_x]], 
                                         metric_data[[input$alpha_color]])
          base_aes$group <- "group"
        }
        if (!is.null(input$alpha_fill)) {
          base_aes$fill <- input$alpha_fill
        }
        if (input$alpha_shape != "None") {
          base_aes$shape <- input$alpha_shape
        }
        
        # Create base plot
        p <- ggplot2::ggplot(metric_data, do.call(ggplot2::aes_string, base_aes))
        
        # Add violin plot
        p <- p + ggplot2::geom_violin(
          alpha = 0.3,
          trim = FALSE,
          position = ggplot2::position_dodge(width = 0.9),
          show.legend = input$alpha_legend_position != "none"
        )
        
        # Add jittered points
        p <- p + ggplot2::geom_point(
          position = ggplot2::position_jitterdodge(
            jitter.width = 0.2,
            dodge.width = 0.9
          ),
          alpha = 0.6,
          size = 2,
          show.legend = input$alpha_legend_position != "none"
        )
        
        # Determine facet formula
if (!is.null(input$alpha_facet_x) && 
    !is.null(input$alpha_facet_y) && 
    length(input$alpha_facet_x) > 0 && 
    length(input$alpha_facet_y) > 0 &&
    !all(input$alpha_facet_x == "None") &&
    !all(input$alpha_facet_y == "None")) {
  
  facet_formula <- paste(input$alpha_facet_y, "~", paste(input$alpha_facet_x, collapse = "+"))
  p <- p + ggplot2::facet_grid(as.formula(facet_formula), scales = "free")

} else if (!is.null(input$alpha_facet_x) && 
           length(input$alpha_facet_x) > 0 && 
           !all(input$alpha_facet_x == "None")) {

  facet_formula <- paste("~", paste(input$alpha_facet_x, collapse = "+"))
  p <- p + ggplot2::facet_wrap(as.formula(facet_formula), scales = "free",
                               nrow = get_facet_dim(input$facet_nrow),
                               ncol = get_facet_dim(input$facet_ncol))

} else if (!is.null(input$alpha_facet_y) && 
           length(input$alpha_facet_y) > 0 && 
           !all(input$alpha_facet_y == "None")) {

  facet_formula <- paste(input$alpha_facet_y, "~ .")
  p <- p + ggplot2::facet_grid(as.formula(facet_formula), scales = "free")
}
        
        # Apply scales
        if (!is.null(input$alpha_color)) {
          color_labels <- get_custom_labels("alpha_color", "Color")
          color_title <- input[["legend_title_Color"]] %||% input$alpha_color
          p <- p + ggplot2::scale_color_brewer(
            palette = input$color_palette,
            name = color_title,
            labels = color_labels
          )
        }
        
        if (!is.null(input$alpha_fill)) {
          fill_labels <- get_custom_labels("alpha_fill", "Fill")
          fill_title <- input[["legend_title_Fill"]] %||% input$alpha_fill
          p <- p + ggplot2::scale_fill_brewer(
            palette = input$fill_palette,
            name = fill_title,
            labels = fill_labels
          )
        }
        
        if (input$alpha_shape != "None") {
          shape_labels <- get_custom_labels("alpha_shape", "Shape")
          shape_title <- input[["legend_title_Shape"]] %||% input$alpha_shape
          p <- p + ggplot2::scale_shape_discrete(
            name = shape_title,
            labels = shape_labels
          )
        }
        
        # Add normalized y-axis settings if normalized
        if(input$normalize_alpha) {
          p <- p + 
            ggplot2::scale_y_continuous(
              limits = c(0, 1),
              breaks = seq(0, 1, 0.25),
              expand = ggplot2::expansion(mult = c(0.05, 0.05))  # Add small padding
            ) +
            ggplot2::theme(
              panel.grid.major.y = ggplot2::element_line(color = "gray80"),
              panel.grid.minor.y = ggplot2::element_blank()
            )
        }
        
        # Add labels
        plot_labs <- list()
        
        # Add metric-specific title
        plot_title <- if(input$plot_title != "") {
          paste(input$plot_title, "-", metric_name)
        } else {
          metric_name
        }
        plot_labs$title <- plot_title
        
        # Add axis labels
        if (input$x_axis_label != "") plot_labs$x <- input$x_axis_label
        
        # Add y-axis label with normalization indication if needed
        if(input$normalize_alpha) {
          plot_labs$y <- if(input$y_axis_label != "") {
            paste0(input$y_axis_label, " (Normalized 0-1)")
          } else {
            "Normalized Value (0-1)"
          }
        } else {
          if (input$y_axis_label != "") plot_labs$y <- input$y_axis_label
        }
        
        # Add caption if specified
        if (input$plot_caption != "") plot_labs$caption <- input$plot_caption
        
        # Apply labels to plot
        p <- p + do.call(ggplot2::labs, plot_labs)
        
        # Modify the theme to ensure consistent grid lines
        p <- p + ggplot2::theme_bw() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(
              size = input$title_size,
              hjust = 0.5
            ),
            axis.title = ggplot2::element_text(
              size = input$axis_title_size
            ),
            axis.text = ggplot2::element_text(
              size = input$axis_text_size
            ),
            axis.text.x = ggplot2::element_text(
              angle = 45,
              hjust = 1,
              size = input$axis_text_size
            ),
            legend.title = ggplot2::element_text(
              size = input$legend_title_size
            ),
            legend.text = ggplot2::element_text(
              size = input$legend_text_size
            ),
            legend.position = input$alpha_legend_position,
            legend.direction = input$alpha_legend_direction,
            plot.caption = ggplot2::element_text(
              size = input$caption_size
            ),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.y = if(input$normalize_alpha) {
              ggplot2::element_line(color = "gray80")
            } else {
              ggplot2::element_line()
            }
          )
        
        return(p)
      }
      
      # Create list of plots
      plots <- lapply(input$alpha_metrics, function(metric) {
        create_single_plot(plot_data, metric)
      })
      
      # Arrange plots horizontally
      gridExtra::grid.arrange(
        grobs = plots,
        nrow = 1,
        ncol = length(plots)
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error in plot creation:", conditionMessage(e),
              "\nCheck your aesthetic mappings."),
        type = "error",
        duration = 5
      )
      
      ggplot2::ggplot() + 
        ggplot2::theme_void() + 
        ggplot2::annotate("text", x = 0, y = 0, 
                         label = "Error in plot creation.\nPlease check your selections.",
                         size = 6)
    })
  })
  
  # Statistical analysis controls
  output$stats_controls <- renderUI({
    meta_vars <- names(sample_data(physeq()))
    
    tagList(
      # Add normalization toggle
      checkboxInput("normalize_stats", "Use Normalized Values (0-1)", value = FALSE),
      
      selectInput("response_var", "Response Variable",
                  choices = input$alpha_metrics),
      
      # Multiple explanatory variables selection
      selectizeInput("explanatory_vars", "Explanatory Variables",
                    choices = meta_vars,
                    multiple = TRUE),
      
      # Interaction terms selection
      conditionalPanel(
        condition = "input.explanatory_vars.length >= 2",
        checkboxGroupInput("interaction_terms", "Test interactions between:",
                         choices = NULL)
      ),
      
      actionButton("run_test", "Run Analysis")
    )
  })
  
  # Update interaction choices when explanatory variables change
  observe({
    req(input$explanatory_vars)
    if (length(input$explanatory_vars) >= 2) {
      # Create combinations of variables for interactions
      var_combinations <- utils::combn(input$explanatory_vars, 2, simplify = FALSE)
      interaction_choices <- sapply(var_combinations, function(x) {
        paste(x[1], "×", x[2])
      })
      
      updateCheckboxGroupInput(session, "interaction_terms",
                             choices = interaction_choices,
                             selected = character(0))
    }
  })
  
  # Reactive value to store statistical results
  stats_results <- reactiveVal(NULL)
  
  # Add reactive value to store model type and formula
  model_info <- reactiveVal(NULL)
  
  # Add reactive value for interpretation
  interpretation <- reactiveVal(NULL)
  
  # Add reactive value for loading state
  is_loading <- reactiveVal(FALSE)
  
  # Function to run statistical analysis
  run_statistical_analysis <- function(data, response_var, explanatory_vars, 
                                     interaction_terms = NULL, normalized = FALSE) {
    # Create base formula from explanatory variables
    formula_parts <- explanatory_vars
    
    # Add interaction terms if they exist
    if (!is.null(interaction_terms) && length(interaction_terms) > 0) {
      # Convert interaction terms from "var1 × var2" format to "var1:var2"
      interaction_parts <- lapply(interaction_terms, function(term) {
        vars <- strsplit(term, " × ")[[1]]
        paste(vars, collapse = ":")
      })
      formula_parts <- c(formula_parts, unlist(interaction_parts))
    }
    
    # Create the final formula string
    formula_str <- paste(response_var, "~", paste(formula_parts, collapse = " + "))
    
    # Fit model
    if (normalized) {
      model <- stats::glm(
        as.formula(formula_str),
        family = stats::quasibinomial(link = "logit"),
        data = data
      )
    } else {
      model <- stats::lm(
        as.formula(formula_str),
        data = data
      )
    }
    
    return(model)
  }
  
  # Update the observer to properly store model information
  observeEvent(input$run_test, {
    req(input$response_var, input$explanatory_vars)
    
    # Get data
    stats_data <- alpha_df()
    
    # Determine response variable
    response_var <- if(input$normalize_stats) {
      paste0(input$response_var, "_norm")
    } else {
      input$response_var
    }
    
    tryCatch({
      # Create formula parts
      formula_parts <- input$explanatory_vars
      
      # Add interaction terms if they exist
      if (!is.null(input$interaction_terms) && length(input$interaction_terms) > 0) {
        interaction_parts <- lapply(input$interaction_terms, function(term) {
          vars <- strsplit(term, " × ")[[1]]
          paste(vars, collapse = ":")
        })
        formula_parts <- c(formula_parts, unlist(interaction_parts))
      }
      
      # Create formula string
      formula_str <- paste(response_var, "~", paste(formula_parts, collapse = " + "))
      
      # Store model information before running the model
      model_info(list(
        type = if(input$normalize_stats) "Quasibinomial GLM" else "Linear Model",
        response = if(input$normalize_stats) {
          paste0(input$response_var, " (normalized)")
        } else {
          input$response_var
        },
        formula = formula_str
      ))
      
      # Run the model
      if(input$normalize_stats) {
        model <- stats::glm(
          as.formula(formula_str),
          family = stats::quasibinomial(link = "logit"),
          data = stats_data
        )
      } else {
        model <- stats::lm(
          as.formula(formula_str),
          data = stats_data
        )
      }
      
      # Get model summary
      model_summary <- summary(model)
      
      # Extract coefficients
      coef_table <- as.data.frame(coef(model_summary))
      
      # Create results table
      results <- data.frame(
        Term = rownames(coef_table),
        Estimate = round(coef_table[, "Estimate"], 4),
        `Std. Error` = round(coef_table[, "Std. Error"], 4),
        `Test Statistic` = round(if("t value" %in% colnames(coef_table)) {
          coef_table[, "t value"]
        } else {
          coef_table[, "z value"]
        }, 4)
      )
      
      # Add p-values with scientific notation for small values
      p_values <- if("Pr(>|t|)" %in% colnames(coef_table)) {
        coef_table[, "Pr(>|t|)"]
      } else {
        coef_table[, "Pr(>|z|)"]
      }
      
      # Format p-values using scientific notation for small values
      results$`P-value` <- sapply(p_values, function(p) {
        if (p < 0.001) {
          # Convert to scientific notation and simplify
          sci <- format(p, scientific = TRUE, digits = 3)
          # Clean up the scientific notation (e.g., "1e-04" to "1×10⁻⁴")
          sci <- gsub("e-", "×10⁻", sci)
          return(sci)
        } else {
          return(format(p, digits = 3, scientific = FALSE))
        }
      })
      
      # Add significance stars
      results$Significance <- sapply(p_values, function(p) {
        if (p < 0.001) return("***")
        if (p < 0.01) return("**")
        if (p < 0.05) return("*")
        return("")
      })
      
      # Store results
      stats_results(results)
      
      # Show success message
      showNotification(
        if(input$normalize_stats) {
          "Quasibinomial GLM analysis complete"
        } else {
          "Linear model analysis complete"
        },
        type = "message",
        duration = 5
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error in statistical analysis:", conditionMessage(e)),
        type = "error",
        duration = 5
      )
      stats_results(NULL)
    })
  })
  
  # Render the statistics table with caption
  output$stats_table <- DT::renderDT({
    req(stats_results())
    req(model_info())
    
    # Create caption text
    caption_text <- sprintf(
      "Statistical Test: %s\nResponse Variable: %s\nModel Formula: %s",
      model_info()$type,
      model_info()$response,
      model_info()$formula
    )
    
    # Create a copy of the results to work with
    results <- stats_results()
    
    DT::datatable(
      results,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(4, 'asc'))  # Sort by p-value column by default
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        'P-value',
        backgroundColor = styleInterval(
          c(0.001, 0.01, 0.05),
          c('rgba(255,180,180,1)', 
            'rgba(255,200,200,0.9)', 
            'rgba(255,220,220,0.7)', 
            'none')
        )
      ) %>%
      DT::formatStyle(
        'Significance',
        color = styleEqual(
          c('***', '**', '*', ''),
          c('#990000', '#CC0000', '#FF0000', 'black')
        ),
        fontWeight = 'bold'
      )
  })
  
  # Download handler for CSV
  output$download_stats_csv <- downloadHandler(
    filename = function() {
      paste0("statistical_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(stats_results(), file, row.names = FALSE)
    }
  )
  
  # Plot downloads
  output$download_plot_png <- downloadHandler(
    filename = function() {
      paste0("alpha_diversity_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      ggplot2::ggsave(file,
             plot = last_plot(),
             device = "png",
             width = input$plot_width,
             height = input$plot_height,
             dpi = input$plot_dpi)
    }
  )
  
  output$download_plot_pdf <- downloadHandler(
    filename = function() {
      paste0("alpha_diversity_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      ggplot2::ggsave(file,
             plot = last_plot(),
             device = "pdf",
             width = input$plot_width,
             height = input$plot_height)
    }
  )
  
  # Default values for plot settings
  plot_defaults <- list(
    alpha_x = "Treatment",
    alpha_color = "Treatment",
    alpha_fill = "Treatment",
    alpha_shape = "None",
    alpha_legend_position = "none",
    alpha_legend_direction = "vertical",
    alpha_facet_x = "None",
    alpha_facet_y = "None",
    plot_title = "",
    x_axis_label = "",
    y_axis_label = "Value",
    legend_title = "",
    plot_caption = "",
    title_size = 16,
    axis_title_size = 14,
    axis_text_size = 12,
    legend_title_size = 12,
    legend_text_size = 10,
    caption_size = 10,
    plot_width = 8,
    plot_height = 6,
    plot_dpi = 300
  )
  
  # Dynamic reset button
  output$reset_button <- renderUI({
    tagList(
      hr(),
      if (input$main_tabs == "Plots") {
        actionButton("reset_plot", "Reset Plot Settings",
                    class = "btn-warning")
      } else if (input$main_tabs == "Statistics") {
        actionButton("reset_stats", "Reset Statistical Analysis",
                    class = "btn-warning")
      }
    )
  })
  
  # Plot reset observer
  observeEvent(input$reset_plot, {
    # Reset only plot-related inputs
    for(input_name in names(plot_defaults)) {
      updateTextInput(session, input_name, value = plot_defaults[[input_name]])
      updateSelectInput(session, input_name, selected = plot_defaults[[input_name]])
      updateNumericInput(session, input_name, value = plot_defaults[[input_name]])
    }
    
    # Reset faceting
    updateSelectizeInput(session, "alpha_facet_x", selected = character(0))
    updateSelectizeInput(session, "alpha_facet_y", selected = character(0))
    
    # Collapse any expanded panels in plot tab
    shinyjs::runjs("
      $('.collapse').collapse('hide');
      $('.collapse-button').text('Show Download Options');
    ")
    
    # Show success message
    showNotification(
      "Plot settings reset to defaults",
      type = "message",
      duration = 5
    )
  })
  
  # Statistics reset observer
  observeEvent(input$reset_stats, {
    # Reset only statistics-related inputs
    updateSelectInput(session, "response_var", 
                     selected = input$alpha_metrics[1])
    updateSelectizeInput(session, "explanatory_vars", 
                        selected = character(0))
    if(!is.null(input$interaction_terms)) {
      updateCheckboxGroupInput(session, "interaction_terms",
                             selected = character(0))
    }
    
    # Clear the results table
    output$stats_table <- DT::renderDT({
      NULL
    })
    
    # Collapse any expanded panels in stats tab
    shinyjs::runjs("
      $('#stats_download_panel').collapse('hide');
      $('#stats_download_panel').prev('.collapse-button').text('Show Download Options');
    ")
    
    # Show success message
    showNotification(
      "Statistical analysis reset",
      type = "message",
      duration = 5
    )
  })
  
  # Initialize JavaScript for collapse functionality
  observe({
    shinyjs::runjs("
      $(document).on('click', '.collapse-button', function() {
        var target = $(this).data('target');
        var text = $(this).text();
        if (target === '#stats_download_panel') {
          $(this).text(text === 'Show Download Options' ? 'Hide Download Options' : 'Show Download Options');
        }
        $(target).collapse('toggle');
      });
    ")
  })
  
  # Add toggle functionality for interpretation panel
  observeEvent(input$toggle_interpret, {
    shinyjs::toggle(id = "interpretation_panel", anim = TRUE)
    
    # Update button text
    if (input$toggle_interpret %% 2 == 1) {
      updateActionButton(session, "toggle_interpret",
                        label = "Hide AI Interpretation")
    } else {
      updateActionButton(session, "toggle_interpret",
                        label = "Show AI Interpretation")
    }
  })
  
  # Update the interpretation output
  output$interpretation_text <- renderUI({
    text <- if (is.null(interpretation())) {
      "Interpretation will appear here after clicking 'Interpret Results'"
    } else {
      interpretation()
    }
    
    # Create a div with the wrapped-output class
    div(
      class = "wrapped-output",
      style = "margin-top: 10px;",
      HTML(gsub("\n", "<br/>", text))
    )
  })
  
  # Loading message
  output$interpretation_loading <- renderText({
    if (is_loading()) {
      "Getting interpretation..."
    } else {
      ""
    }
  })
  
  # Function to create prompt for the model
  create_interpretation_prompt <- function(results, model_info) {
    results_text <- paste(capture.output(print(results)), collapse = "\n")
    
    # Add context if provided
    context_section <- if (!is.null(input$experiment_context) && 
                         nchar(trimws(input$experiment_context)) > 0) {
      paste0("\nExperimental Context:\n", input$experiment_context, "\n")
    } else {
      ""
    }
    
    prompt <- paste0(
      "Please interpret these statistical results in clear, non-technical language.",
      context_section,
      "\nStatistical Analysis Details:\n",
      "Statistical Test: ", model_info$type, "\n",
      "Response Variable: ", model_info$response, "\n",
      "Model Formula: ", model_info$formula, "\n\n",
      "Results:\n", results_text, "\n\n",
      if (nchar(context_section) > 0) {
        "Please interpret these results in the context of the experimental details provided, including:\n"
      } else {
        "Please include:\n"
      },
      "1. A brief overview of what was tested\n",
      "2. The main findings and their significance\n",
      "3. What these results mean in practical terms",
      if (nchar(context_section) > 0) {
        "\n4. How these findings relate to the experimental context and objectives"
      } else {
        ""
      },
      "\nKeep the explanation concise but informative."
    )
    
    return(prompt)
  }
  
  # Update the observer for interpretation button
  observeEvent(input$interpret_results, {
    req(stats_results(), model_info(), input$hf_key)
    
    if (nchar(input$hf_key) < 1) {
      interpretation("Please enter your Hugging Face API token to get interpretation.")
      return()
    }
    
    # Set loading state
    is_loading(TRUE)
    interpretation(NULL)
    
    # Create prompt
    prompt <- create_interpretation_prompt(stats_results(), model_info())
    
    tryCatch({
      # Make API call to Hugging Face
      response <- httr::POST(
        url = "https://api-inference.huggingface.co/models/mistralai/Mixtral-8x7B-Instruct-v0.1",
        httr::add_headers(
          Authorization = paste("Bearer", input$hf_key),
          "Content-Type" = "application/json"
        ),
        body = list(
          inputs = prompt
        ),
        encode = "json"
      )
      
      # Check if request was successful
      if (httr::status_code(response) == 200) {
        # Get response content
        response_content <- httr::content(response, "text", encoding = "UTF-8")
        parsed_content <- jsonlite::fromJSON(response_content)
        
        # Extract the generated text
        if (is.character(parsed_content)) {
          interpretation(parsed_content[1])
        } else if (is.list(parsed_content)) {
          interpretation(as.character(parsed_content[[1]]))
        } else {
          interpretation("Error: Unexpected response format from API")
        }
        
      } else {
        # Handle error response
        error_content <- httr::content(response, "text", encoding = "UTF-8")
        interpretation(paste("API Error (", httr::status_code(response), "): ", 
                           error_content, sep=""))
      }
      
    }, error = function(e) {
      interpretation(paste("Error: Unable to get interpretation. ",
                         "Please check your API token and try again."))
    }, finally = {
      is_loading(FALSE)
    })
  })
  
  # Add observer for token visibility toggle
  observeEvent(input$toggle_token_visibility, {
    token_input <- paste0('$("#hf_key").attr("type", $("#hf_key").attr("type") === "password" ? "text" : "password");')
    icon_update <- paste0('$("#toggle_token_visibility i").toggleClass("fa-eye fa-eye-slash");')
    
    shinyjs::runjs(paste(token_input, icon_update))
    
    # Update button title
    if (input$toggle_token_visibility %% 2 == 1) {
      shinyjs::runjs('$("#toggle_token_visibility").attr("title", "Hide API Token");')
    } else {
      shinyjs::runjs('$("#toggle_token_visibility").attr("title", "Show API Token");')
    }
  })
  
  # Initialize password input as hidden on startup
  observe({
    shinyjs::runjs('
      $("#hf_key").attr("type", "password");
      $("#toggle_token_visibility i").addClass("fa-eye").removeClass("fa-eye-slash");
      $("#toggle_token_visibility").attr("title", "Show API Token");
    ')
  })
  
  # Update the copy button observer
  observeEvent(input$copy_interpretation, {
    # Get the interpretation text
    text <- interpretation()
    
    if (is.null(text) || nchar(trimws(text)) == 0) {
      showNotification("No text to copy. Please generate an interpretation first.", 
                      type = "warning")
      return()
    }
    
    # Use JavaScript to copy to clipboard with better error handling
    shinyjs::runjs(sprintf("
      (function() {
        const text = `%s`;
        const textarea = document.createElement('textarea');
        textarea.value = text;
        textarea.style.position = 'fixed';
        textarea.style.top = '0';
        textarea.style.left = '0';
        textarea.style.width = '2em';
        textarea.style.height = '2em';
        textarea.style.padding = '0';
        textarea.style.border = 'none';
        textarea.style.outline = 'none';
        textarea.style.boxShadow = 'none';
        textarea.style.background = 'transparent';
        
        document.body.appendChild(textarea);
        textarea.select();
        
        try {
          const successful = document.execCommand('copy');
          if (successful) {
            Shiny.setInputValue('copy_success', true, {priority: 'event'});
          } else {
            Shiny.setInputValue('copy_error', true, {priority: 'event'});
          }
        } catch (err) {
          Shiny.setInputValue('copy_error', true, {priority: 'event'});
        } finally {
          document.body.removeChild(textarea);
        }
      })();
    ", gsub("`", "\\`", text)))
  })
  
  # Update success/error notifications
  observeEvent(input$copy_success, {
    showNotification("Text successfully copied to clipboard!", 
                    type = "message", duration = 3)
  })
  
  observeEvent(input$copy_error, {
    showNotification("Copy failed. Please try again or copy manually.", 
                    type = "error", duration = 5)
  })
}

shinyApp(ui, server)