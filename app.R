library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyverse)
library(magrittr)
library(lime)
library(fpc)

# Function to calculate number of rows in ggplot facets
gg_facet_nrow_ng <- function(p) {
  assertive.types::assert_is_any_of(p, 'ggplot')
  p %>%
    ggplot2::ggplot_build() %>%
    magrittr::extract2('layout') %>%
    magrittr::extract2('layout') %>%
    magrittr::extract2('ROW') %>%
    unique() %>%
    length()
}

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "Public Health Data Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("EDA", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Classification Performance", tabName = "classification_performance", icon = icon("project-diagram")),
      menuItem("Decision Tree Plot", tabName = "decision_tree_plot", icon = icon("tree")),
      menuItem("LIME Analysis", tabName = "lime_analysis", icon = icon("lightbulb")),
      menuItem("Clustering", tabName = "clustering", icon = icon("sitemap")),
      menuItem("Clustering Map", tabName = "clustering_map", icon = icon("map")),
      menuItem("Full Report", tabName = "full_report", icon = icon("file-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      # EDA Tab
      tabItem(tabName = "eda",
              fluidRow(
                box(width = 4,
                    selectInput("eda_plot", "Select Plot:", 
                                choices = c("Distribution (Total)", "Distribution (Per capita)", "Correlation Matrix", "Correlations with Target Variable"))),
                box(width = 4,
                    uiOutput("cause_selection"))
              ),
              fluidRow(
                box(width = 8, plotlyOutput("selected_plot", height = "500px"))
              )
      ),
      # Classification Performance Tab
      tabItem(tabName = "classification_performance",
              fluidRow(
                box(width = 4,
                    checkboxGroupInput("models_selected", "Select Models:", 
                                       choices = c("Dtree Model 1 (Single Variable)", "Dtree Model 2 (Selected Features)", "Bayes Model 1 (Single Variable)", "Bayes Model 2 (Selected Features)"))
                )
              ),
              fluidRow(
                box(width = 6, plotOutput("roc_plot", height = "500px")),
                box(width = 6, tableOutput("performance_table"))
              )
      ),
      # Decision Tree Plot Tab
      tabItem(tabName = "decision_tree_plot",
              fluidRow(
                box(width = 4,
                    selectInput("dtree_model_selected", "Select Decision Tree Model:", 
                                choices = c("Dtree Model 1 (Single Variable)", "Dtree Model 2 (Selected Features)"))
                )
              ),
              fluidRow(
                box(width = 8, plotOutput("decision_tree_plot_output", height = "500px"))
              )
      ),
      # LIME Analysis Tab
      tabItem(tabName = "lime_analysis",
              fluidRow(
                box(width = 4,
                    selectInput("lime_model_selected", "Select Naive Bayes Model:", 
                                choices = c("Bayes Model 1 (Single Variable)", "Bayes Model 2 (Selected Features)"))
                )
              ),
              fluidRow(
                box(width = 8, plotOutput("lime_plot_output", height = "500px"))
              )
      ),
      # Clustering Tab
      tabItem(tabName = "clustering",
              h2("Clustering section under development"),
              fluidRow(
                box(width = 4,
                    sliderInput("clustering_k", "Number of Clusters (k):", min = 2, max = 10, value = 3, step = 1, width = "100%")
                )
              ),
              fluidRow(
                box(width = 8, plotOutput("clustering_plot", height = "500px")),
                box(width = 4, tableOutput("cboot_table"))
              )
      ),
      # Clustering Map Tab
      tabItem(tabName = "clustering_map",
              h2("Clustering Map"),
              fluidRow(
                box(width = 12, plotOutput("clustering_map_plot", height = "500px"))
              )
      ),
      # Full Report Tab
      tabItem(tabName = "full_report",
              h2("Full HTML Report"),
              fluidRow(
                box(width = 12, htmlOutput("full_report_output"))
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Load datasets if available in the global environment
  
  load("app.RData") #This is the data from the last run of RMD file, automatically updated if new run happen.
  
  death_causes <- tryCatch(get("death_causes", envir = .GlobalEnv), error = function(e) NULL)
  data_capita <- tryCatch(get("data_capita", envir = .GlobalEnv), error = function(e) NULL)
  
  # Update causes selection based on available columns in the selected dataset
  output$cause_selection <- renderUI({
    if (input$eda_plot == "Distribution (Per capita)" && !is.null(data_capita)) {
      selectInput("cause", "Select Cause:", choices = names(data_capita)[!names(data_capita) %in% c("Entity", "Code", "Year")])
    } else if (input$eda_plot == "Distribution (Total)" && !is.null(death_causes)) {
      selectInput("cause", "Select Cause:", choices = names(death_causes)[!names(death_causes) %in% c("Entity", "Code", "Year")])
    }
  })
  
  # Reactive expression for selected plot
  selected_plot <- reactive({
    if (input$eda_plot == "Distribution (Total)" && !is.null(input$cause) && !is.null(death_causes)) {
      ggplot(death_causes, aes(x = .data[[input$cause]])) +
        geom_histogram(bins = 51, fill = "lightblue", color = "black") +
        geom_vline(aes(xintercept = mean(death_causes[[input$cause]], na.rm = TRUE), color = "Mean"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = median(death_causes[[input$cause]], na.rm = TRUE), color = "Median"), linetype = "dashed", size = 1) +
        scale_x_log10() +
        scale_color_manual(name = "Legend", values = c(Mean = "red", Median = "green")) +
        labs(title = paste("Log-Scaled Distribution of", input$cause, "(Total)"),
             x = paste(input$cause, "Total (Log Scale)"), 
             y = "Frequency (Log Scale)") +
        theme_minimal()
    } else if (input$eda_plot == "Distribution (Per capita)" && !is.null(input$cause) && !is.null(data_capita)) {
      ggplot(data_capita, aes(x = .data[[input$cause]])) +
        geom_histogram(bins = 51, fill = "lightblue", color = "black") +
        geom_vline(aes(xintercept = mean(data_capita[[input$cause]], na.rm = TRUE), color = "Mean"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = median(data_capita[[input$cause]], na.rm = TRUE), color = "Median"), linetype = "dashed", size = 1) +
        scale_x_log10() +
        scale_color_manual(name = "Legend", values = c(Mean = "red", Median = "green")) +
        labs(title = paste("Log-Scaled Distribution of", input$cause, "(Per Capita)"),
             x = paste(input$cause, "Per Capita (Log Scale)"), 
             y = "Frequency (Log Scale)") +
        theme_minimal()
    } else if (input$eda_plot == "Correlation Matrix" && !is.null(data_capita)) {
      library(reshape2)
      correlation_matrix_all <- cor(data_capita, use = "complete.obs")
      melted_corr_all <- melt(correlation_matrix_all)
      ggplot(data = melted_corr_all, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile(color = "black", size = 0.5) +  # Add black borders between cells
        scale_fill_gradient2(low = "blue", high = "green", mid = "white", 
                             midpoint = 0, limit = c(-1, 1), space = "Lab", 
                             name = "Correlation") +  theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1), 
              axis.text.y = element_text(size = 8),
              plot.margin = margin(10, 10, 10, 10)) +  # Adjust margin to bring vars names closer to the plot
        coord_fixed() +  ggtitle("Correlation Matrix of the Dataset")
    } else if (input$eda_plot == "Correlations with Target Variable" && !is.null(data_capita)) {
      cor_matrix2 <- cor(data_capita, use = "complete.obs")
      cor_target_var <- cor_matrix2["Low.birth.weight", ]  
      cor_df_2 <- data.frame(Variable = names(cor_target_var), Correlation = cor_target_var)
      cor_df_2 <- cor_df_2[cor_df_2$Variable != "Low.birth.weight", ]
      cor_df_2 <- cor_df_2 %>% arrange(desc(abs(Correlation)))
      
      ggplot(cor_df_2, aes(x = reorder(Variable, Correlation), y = Correlation, fill = Correlation)) +
        geom_bar(stat = "identity") +
        scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                             midpoint = 0, limit = c(-1, 1), space = "Lab", 
                             name = "Correlation") +  coord_flip() +  theme_minimal() +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold")
        ) +  ggtitle("Correlations with Target Variable (Low Birth Weight)")
    }
  })
  
  # Render selected plot
  output$selected_plot <- renderPlotly({
    ggplotly(selected_plot(), height = 500)
  })
  
  # Render ROC plot for Classification Performance tab
  output$roc_plot <- renderPlot({
    if (!is.null(input$models_selected)) {
      library(ROCit)
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curves for Selected Models")
      legend_labels <- c()
      colors <- c("blue", "green", "red", "purple")
      for (i in seq_along(input$models_selected)) {
        model_name <- input$models_selected[i]
        if (model_name == "Dtree Model 1 (Single Variable)") {
          rocit_dtree_1 <- rocit(score = prob_dtree_1, class = dTest$Low.birth.weight, negref = "Low")
          lines(rocit_dtree_1$TPR ~ rocit_dtree_1$FPR, col = colors[i], lwd = 2)
          legend_labels <- c(legend_labels, model_name)
        } else if (model_name == "Dtree Model 2 (Selected Features)") {
          rocit_dtree_2 <- rocit(score = prob_dtree_2, class = dTest$Low.birth.weight, negref = "Low")
          lines(rocit_dtree_2$TPR ~ rocit_dtree_2$FPR, col = colors[i], lwd = 2)
          legend_labels <- c(legend_labels, model_name)
        } else if (model_name == "Bayes Model 1 (Single Variable)") {
          rocit_bayes_1 <- rocit(score = prob_bayes_1, class = dTest$Low.birth.weight, negref = "Low")
          lines(rocit_bayes_1$TPR ~ rocit_bayes_1$FPR, col = colors[i], lwd = 2)
          legend_labels <- c(legend_labels, model_name)
        } else if (model_name == "Bayes Model 2 (Selected Features)") {
          rocit_bayes_2 <- rocit(score = prob_bayes_2, class = dTest$Low.birth.weight, negref = "Low")
          lines(rocit_bayes_2$TPR ~ rocit_bayes_2$FPR, col = colors[i], lwd = 2)
          legend_labels <- c(legend_labels, model_name)
        }
      }
      legend("bottomright", legend = legend_labels, col = colors[seq_along(legend_labels)], lwd = 2)
    }
  })
  
  # Render performance table for Classification Performance tab
  output$performance_table <- renderTable({
    if (!is.null(input$models_selected)) {
      comparison_df <- data.frame(
        Model = c("Dtree Model 1 (Single Variable)", 
                  "Dtree Model 2 (Selected Features)", 
                  "Bayes Model 1 (Single Variable)", 
                  "Bayes Model 2 (Selected Features)"),
        Accuracy = c(performance_dtree_1$Accuracy, 
                     performance_dtree_2$Accuracy, 
                     performance_bayes_1$Accuracy, 
                     performance_bayes_2$Accuracy),
        Precision = c(performance_dtree_1$Precision, 
                      performance_dtree_2$Precision, 
                      performance_bayes_1$Precision, 
                      performance_bayes_2$Precision),
        Recall = c(performance_dtree_1$Recall, 
                   performance_dtree_2$Recall, 
                   performance_bayes_1$Recall, 
                   performance_bayes_2$Recall),
        F1_Score = c(performance_dtree_1$F1_Score, 
                     performance_dtree_2$F1_Score, 
                     performance_bayes_1$F1_Score, 
                     performance_bayes_2$F1_Score),
        Kappa = c(performance_dtree_1$Kappa, 
                  performance_dtree_2$Kappa, 
                  performance_bayes_1$Kappa, 
                  performance_bayes_2$Kappa),
        AUC = c(performance_dtree_1$AUC, 
                performance_dtree_2$AUC, 
                performance_bayes_1$AUC, 
                performance_bayes_2$AUC),
        McNemar_PValue = formatC(c(performance_dtree_1$McNemar_PValue, 
                                   performance_dtree_2$McNemar_PValue, 
                                   performance_bayes_1$McNemar_PValue, 
                                   performance_bayes_2$McNemar_PValue), format = "e", digits = 2)
      )
      comparison_df <- comparison_df[comparison_df$Model %in% input$models_selected, ]
      comparison_df[order(-comparison_df$AUC), ]
    }
  })
  
  # Render Decision Tree Plot for Decision Tree Plot tab
  output$decision_tree_plot_output <- renderPlot({
    if (input$dtree_model_selected == "Dtree Model 1 (Single Variable)") {
      rpart.plot::rpart.plot(model_dtree_1$finalModel)
    } else if (input$dtree_model_selected == "Dtree Model 2 (Selected Features)") {
      rpart.plot::rpart.plot(model_dtree_2$finalModel)
    }
  })
  
  # Render LIME Plot for LIME Analysis tab
  output$lime_plot_output <- renderPlot({
    if (input$lime_model_selected == "Bayes Model 1 (Single Variable)") {
      apply_lime(dTrain, dTest, model_bayes_1, "Low.birth.weight")
    } else if (input$lime_model_selected == "Bayes Model 2 (Selected Features)") {
      apply_lime(dTrain, dTest, model_bayes_2, "Low.birth.weight")
    }
  })
  
  # Render Clustering Plot for Clustering tab
  output$clustering_plot <- renderPlot({
    par(cex = 0.7)
    # Plot the dendrogram
    plot(pfit, labels = names(data_clustering), main = "Cluster Dendrogram")
    rect.hclust(pfit, k = input$clustering_k, border = "blue")
  })
  
  # Render cboot.hclust stability measures for Clustering tab
  output$cboot_table <- renderTable({
    cboot <- clusterboot(data_clustering, B = 100, clustermethod = hclustCBI, method = "ward.D2", k = input$clustering_k)
    data.frame(Cluster = 1:length(cboot$bootmean), Stability = cboot$bootmean)
  })
  
  # Render Clustering Map for Clustering Map tab
  output$clustering_map_plot <- renderPlot({
    # Best k
    groups <- cutree(pfit, k = input$clustering_k)
    # Perform PCA on the scaled transposed data to calculate the principal components
    princ <- prcomp(scaled_df)  # Perform PCA on the transposed and scaled data
    nComp <- 2  # Focus on the first two principal components
    # Project the scaled data onto the first 2 principal components to form a new 2-column data frame
    project2D <- as.data.frame(predict(princ, newdata = scaled_df)[, 1:nComp])
    Variable <- row.names(scaled_df)  # Use column names as identifiers
    # Combine the projection, clusters, and variable names into one data frame
    hclust.project2D <- cbind(project2D, cluster = as.factor(groups), Variable = Variable)
    # Finding the convex hull for each cluster
    find_convex_hull <- function(proj2Ddf, groups) { do.call(rbind,
                                                             lapply(unique(groups),
                                                                    FUN = function(c) {
                                                                      f <- subset(proj2Ddf, cluster == c)  # Subset the data for each cluster
                                                                      f[chull(f$PC1, f$PC2), ] }))} # Apply convex hull on PC1 and PC2
    # Calculate the convex hull for each cluster
    hclust.hull <- find_convex_hull(hclust.project2D, groups)
    # Plotting 2D clusters map
    ggplot(hclust.project2D, aes(x = PC1, y = PC2)) +
      geom_point(aes(shape = cluster, color = cluster)) +
      geom_text(aes(label = Variable, color = cluster), hjust = 0, vjust = 1, size = 3) +
      geom_polygon(data = hclust.hull, aes(group = cluster, fill = as.factor(cluster)),
                   alpha = 0.4, linetype = 0) + 
      theme(text = element_text(size = 12))
  })
  
  # Render Full Report from knit HTML for Full Report tab
  output$full_report_output <- renderUI({
    includeHTML("project.html")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)