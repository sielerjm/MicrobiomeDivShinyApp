#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(phyloseq)
library(vegan)
library(DT)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Microbiome Diversity Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("physeq_file", "Upload Phyloseq Object (RDS)", accept = ".rds"),
      selectizeInput("alpha_metrics", "Alpha Diversity Metrics",
                     choices = c("Shannon", "Simpson", "Observed"),
                     multiple = TRUE),
      selectizeInput("beta_metrics", "Beta Diversity Metrics",
                     choices = c("Bray-Curtis", "Canberra"),
                     multiple = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Figures",
                 tabsetPanel(
                   id = "figures_tabs",
                   tabPanel("Alpha Diversity",
                            uiOutput("alpha_var_select"),
                            plotOutput("alpha_plot")),
                   tabPanel("Beta Diversity",
                            uiOutput("beta_var_select"),
                            plotOutput("beta_plot"))
                 )),
        tabPanel("Tables",
                 selectInput("stats_type", "Analysis Type",
                             choices = c("Alpha Diversity", "Beta Diversity")),
                 uiOutput("stats_controls"),
                 DTOutput("stats_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  physeq <- reactive({
    req(input$physeq_file)
    readRDS(input$physeq_file$datapath) %>% 
      filter_taxa(function(x) sum(x > 0) > 0, TRUE)
  })
  
  # Alpha diversity calculations
  alpha_df <- reactive({
    req(input$alpha_metrics)
    physeq() %>%
      estimate_richness(measures = input$alpha_metrics) %>%
      rownames_to_column("SampleID") %>%
      left_join(sample_data(physeq()) %>% as_tibble(), by = "SampleID")
  })
  
  # Beta diversity calculations
  beta_dist <- reactive({
    req(input$beta_metrics)
    otu <- abundances(physeq())
    dist_objects <- list()
    
    if("Bray-Curtis" %in% input$beta_metrics) {
      dist_objects$BrayCurtis <- vegdist(t(otu), method = "bray")
    }
    if("Canberra" %in% input$beta_metrics) {
      dist_objects$Canberra <- vegdist(t(otu), method = "canberra")
    }
    return(dist_objects)
  })
  
  # Plot controls
  output$alpha_var_select <- renderUI({
    meta_vars <- names(sample_data(physeq()))
    tagList(
      selectInput("alpha_color", "Color By", choices = meta_vars),
      selectInput("alpha_shape", "Shape By", choices = c("None", meta_vars))
    )
  })
  
  output$beta_var_select <- renderUI({
    meta_vars <- names(sample_data(physeq()))
    tagList(
      selectInput("beta_color", "Color By", choices = meta_vars),
      selectInput("beta_shape", "Shape By", choices = c("None", meta_vars))
    )
  })
  
  # Alpha diversity plot
  output$alpha_plot <- renderPlot({
    req(input$alpha_color)
    p <- alpha_df() %>%
      pivot_longer(cols = input$alpha_metrics,
                   names_to = "Metric",
                   values_to = "Value") %>%
      ggplot(aes_string(x = input$alpha_color, y = "Value")) +
      geom_boxplot() +
      facet_wrap(~Metric, scales = "free_y") +
      theme_bw()
    
    if(input$alpha_shape != "None") {
      p <- p + geom_point(aes_string(shape = input$alpha_shape), position = position_jitter(width = 0.2))
    }
    p
  })
  
  # Beta diversity plot
  output$beta_plot <- renderPlot({
    req(input$beta_color, beta_dist())
    meta <- sample_data(physeq()) %>% as_tibble()
    
    lapply(names(beta_dist()), function(method) {
      pcoa <- cmdscale(beta_dist()[[method]], eig = TRUE)
      df <- data.frame(
        Axis1 = pcoa$points[,1],
        Axis2 = pcoa$points[,2],
        meta
      )
      
      p <- ggplot(df, aes_string("Axis1", "Axis2", color = input$beta_color)) +
        geom_point(size = 3) +
        ggtitle(paste("PCoA -", method)) +
        theme_bw()
      
      if(input$beta_shape != "None") {
        p <- p + aes_string(shape = input$beta_shape)
      }
      p
    }) %>% 
      cowplot::plot_grid(plotlist = .)
  })
  
  # Statistical analysis
  output$stats_controls <- renderUI({
    req(input$stats_type)
    meta_vars <- names(sample_data(physeq()))
    
    tagList(
      selectInput("response_var", "Response Variable",
                  choices = if(input$stats_type == "Alpha Diversity") {
                    input$alpha_metrics
                  } else {
                    names(beta_dist())
                  }),
      selectInput("explanatory_var", "Explanatory Variable",
                  choices = meta_vars),
      actionButton("run_test", "Run Analysis")
    )
  })
  
  stats_results <- eventReactive(input$run_test, {
    req(input$response_var, input$explanatory_var)
    
    if(input$stats_type == "Alpha Diversity") {
      formula <- as.formula(paste(input$response_var, "~", input$explanatory_var))
      fit <- lm(formula, data = alpha_df())
      broom::tidy(anova(fit))
    } else {
      dist_mat <- beta_dist()[[input$response_var]]
      adonis_result <- adonis2(dist_mat ~ get(input$explanatory_var),
                               data = as.data.frame(sample_data(physeq())))
      as.data.frame(adonis_result) %>% 
        rownames_to_column("Term") %>% 
        mutate(across(where(is.numeric), ~round(., 4)))
    }
  })
  
  output$stats_table <- renderDT({
    req(stats_results())
    datatable(stats_results(), options = list(pageLength = 10))
  })
}

shinyApp(ui, server)
