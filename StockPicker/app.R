#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(tidyverse)
library(ggvis)
library(cluster)
library(ggplot2)
library(rsconnect)

customer_data_raw <-
  read.csv("./Mall_Customers.csv")
customer_data <- na.omit(customer_data_raw)
names(customer_data)[4] <- "Annual_Income"
names(customer_data)[5] <- "Spending_Score"
customer_data_cluster_two <- subset(customer_data, select = -c(CustomerID, Gender, Spending_Score))
customer_data <-
  subset(customer_data, select = -c(CustomerID, Gender, Annual_Income))

customer_data_full <- na.omit(customer_data_raw)




# Define UI for application that draws a histogram
ui <- navbarPage(
  "Customer Data!",
  # Application title
  tabPanel("Summary",
           fluidRow(h1("Summary")),
           fluidRow(
             h3("What data you collected?"),
             p("
    The data set that I chose was about Mall Customers. The data given includes basic information about a group of customers like Gender, Annual Income, and Spending
    Score which is a computed value based on shopping habits of the customer. 
    "),
    h3("Why this topic is interesting or important to you?"),
    p("This topic is interesting to me because it is something that I can completely understand. Knowing everything about the data set allowed me to learn more from
      it than if I had picked something that sounded cool but maybe i didn't understand the data well enough to gain insights. "),
    h3("How did you analze the data?"),
    p("I analyzed the data using all the tools that we have learned throughout the semester, doing my best to gain inight into the data. The other tabs on this 
      website will give you some insight into how I went about analzying the data."),
    h3("What did you find in the data?"),
    p("You will find the answer to this in the other tabs of this application. "),
    h2("Clustering"),
    p("The algorithm that I decided to use for this application was clustering. The point of this algorithm is to group data points into related clusters based on the
      centroids you select. So essentially if I select five centroids the rest of the data will be placed into clusters based on how closely they are related to one of those centroids.
      This algoritm allows you to group data and then find patterns that exist in it that your business or company may find valuable. The number of centroids is generally chosen in order to keep the sum of squares
      as small as possible. 
      ")
             
             )
           ),
  tabPanel(
    "Gender Specific Data",
    fluidRow(h1("Gender Specific Data")),
    h3("Select the Gender of the Customer Data below"),
    actionGroupButtons(
      inputIds = c("Male", "Female", "Both"),
      labels = list("Male", "Female", "Both"),
      fullwidth = T
    ),
    br(),
    fluidRow(
      column(6,
             plotOutput("genderGraph"),
             plotOutput("incomeGraph"),),
      br(),
      br(),
      br(),
      column(6,
             plotOutput("spendingScoreGraph"),
             plotOutput("ageGraph"),)
    ),
    fluidRow(h3("Analysis")),
    fluidRow(p("
               Looking at this data we can see that Females spending scores are generally higher regardless of their income. We can also see that there are a lot
               more younger women than there are men. 
               ")),
  ),
  tabPanel(
    "Age Specific Data",
    fluidRow(h1("Age Specific Data")),
    radioButtons(
      inputId = "ageRadioButtons",
      label = "Select What Data to plot",
      choices = c("Income", "Spending Score", "Income and Spending Score"),
      selected = "Income",
      inline = FALSE,
      width = NULL,
      choiceNames = NULL,
      choiceValues = NULL
    ),
    fluidRow(column(6,
                    plotOutput("agePlot"),),),
    fluidRow(h3("Analysis")),
    fluidRow(p("
               Looking at this plot we can see that younger folks generally have a higher spending score even though they may have a smaller income. 
               ")),
    
  ),
  tabPanel(
    "Clustering Data",
    fluidRow(h1("Clustering Data")),
    fluidRow(h3("Age vs. Spending Score")),
    br(),
    fluidRow(column(6,
                    h4("Sum of Squares Chart"),
                    dataTableOutput("ssTable"),),
             column(6,
                    h4("Sum of Squares Plot"),
                    ggvisOutput("ssPlot"),)),
    br(),
    br(),
    br(),
    fluidRow(h3("Cluster Graph")),
    fluidRow(column(
      4,
      radioButtons(
        inputId = "clusterCount",
        label = "Select the number of clusters",
        choices = c(2,3,4,5, 6, 7, 8, 9, 10,11,12,13,14),
        selected = 8,
        inline = FALSE,
        width = NULL,
        choiceNames = NULL,
        choiceValues = NULL
      ),
    ),
    column(8,
           plotOutput("clusterGraph"),)
    ),
    br(),
    br(),
    br(),
    fluidRow(
      dataTableOutput("clusterTableSpendingScore")
    ),
    fluidRow(h3("Analysis")),
    fluidRow(p("
               If we look closely at this cluster we can learn a few things about the customers. If we look at cluster 3 we can see that they all have a high spending score
               and a relatively high income as well. This means we can target certain advertisements and things at them to get them to buy more products. Cluster 1 has a low spending score but the
               incomes are still medium to high, so we can give this to marketing and see if there is a way to get this group of people to buy more. Cluster 8 has a medium spending score
               and a medium income so again we can use this information to target specific advertisements that may get them to spend more. 
               ")),
    fluidRow(h3("Age vs. Income")),
    br(),
    fluidRow(column(6,
                    h4("Sum of Squares Chart"),
                    dataTableOutput("ssTableIncome"),),
             column(6,
                    h4("Sum of Squares Plot"),
                    ggvisOutput("ssPlotIncome"),)),
    br(),
    br(),
    br(),
    fluidRow(h3("Cluster Graph")),
    fluidRow(column(
      4,
      radioButtons(
        inputId = "clusterCountTwo",
        label = "Select the number of clusters",
        choices = c(2,3,4,5, 6, 7, 8, 9, 10,11,12,13,14),
        selected = 8,
        inline = FALSE,
        width = NULL,
        choiceNames = NULL,
        choiceValues = NULL
      ),
    ),
    column(8,
           plotOutput("clusterGraphIncome"),)
    ),
    br(),
    br(),
    br(),
    fluidRow(
      dataTableOutput("clusterTableIncome")
    ),
    fluidRow(h3("Analysis")),
    fluidRow(p("
               If we look closely at this cluster again we can learn a few things about the customers. What we can learn here is very similar to the first cluster just 
               a slightly different take. In this case cluster 1 for example has a low income but still a medium spending score so maybe targeting things like sales would entice them more
               . There are a lot more insights you could gain given marketing and advertisement teams if you help them understand the data. 
               ")),
  ),
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  gender_ractive_values <-
    reactiveValues(
      incomeGraph = ggplot(
        customer_data,
        aes(x = customer_data_full$Annual.Income..k.., fill = customer_data_full$Gender)
      ) +
        geom_histogram(bins = 10, position = "dodge") +
        xlab("Income") +
        labs(fill = "Gender"),
      spendingScoreGraph = ggplot(
        customer_data,
        aes(
          x = customer_data_full$Spending.Score..1.100.,
          fill = customer_data_full$Gender
        )
      ) +
        geom_histogram(bins = 10, position = "dodge") +
        xlab("Score") +
        labs(fill = "Gender"),
      ageGraph = ggplot(
        customer_data,
        aes(x = customer_data_full$Age, fill = customer_data_full$Gender)
      ) +
        geom_histogram(bins = 8, position = "dodge") +
        xlab("Age") +
        labs(fill = "Gender")
    )
  
  male_customer_data <- customer_data_full %>%
    filter(Gender == "Male")
  
  female_customer_data <- customer_data_full %>%
    filter(Gender == "Female")
  
  clusters = kmeans(customer_data, 8)
  
  output$genderGraph <- renderPlot({
    ggplot(customer_data_full, aes(
      x = as.factor(customer_data_raw$Gender),
      fill = as.factor(customer_data_raw$Gender)
    )) +
      geom_bar() +
      theme(legend.position = "none") +
      xlab("Gender")
  })
  
  observeEvent(input$Male, {
    gender_ractive_values$incomeGraph <-
      ggplot(male_customer_data,
             aes(x = male_customer_data$Annual.Income..k..)) +
      geom_histogram(bins = 10,
                     position = "dodge",
                     fill = "Blue") +
      xlab("Income")
    
    gender_ractive_values$spendingScoreGraph <-
      ggplot(male_customer_data,
             aes(x = male_customer_data$Spending.Score..1.100.)) +
      geom_histogram(bins = 10,
                     position = "dodge",
                     fill = "Blue") +
      xlab("Spending Score")
    
    gender_ractive_values$ageGraph <-
      ggplot(male_customer_data, aes(x = male_customer_data$Age)) +
      geom_histogram(bins = 8,
                     position = "dodge",
                     fill = "Blue") +
      xlab("Age")
  })
  
  observeEvent(input$Female, {
    gender_ractive_values$incomeGraph <-
      ggplot(female_customer_data,
             aes(x = female_customer_data$Annual.Income..k..)) +
      geom_histogram(bins = 10,
                     position = "dodge",
                     fill = "Pink") +
      xlab("Income")
    
    gender_ractive_values$spendingScoreGraph <-
      ggplot(female_customer_data,
             aes(x = female_customer_data$Spending.Score..1.100.)) +
      geom_histogram(bins = 10,
                     position = "dodge",
                     fill = "Pink") +
      xlab("Spending Score")
    
    gender_ractive_values$ageGraph <-
      ggplot(female_customer_data, aes(x = female_customer_data$Age)) +
      geom_histogram(bins = 8,
                     position = "dodge",
                     fill = "Pink") +
      xlab("Age")
  })
  
  observeEvent(input$Both, {
    gender_ractive_values$incomeGraph <-
      ggplot(
        customer_data,
        aes(x = customer_data_full$Annual.Income..k.., fill = customer_data_full$Gender)
      ) +
      geom_histogram(bins = 10, position = "dodge") +
      xlab("Income") +
      labs(fill = "Gender")
    
    gender_ractive_values$spendingScoreGraph <-
      ggplot(
        customer_data,
        aes(
          x = customer_data_full$Spending.Score..1.100.,
          fill = customer_data_full$Gender
        )
      ) +
      geom_histogram(bins = 10, position = "dodge") +
      xlab("Score") +
      labs(fill = "Gender")
    
    gender_ractive_values$ageGraph <-
      ggplot(customer_data,
             aes(x = customer_data_full$Age, fill = customer_data_full$Gender)) +
      geom_histogram(bins = 8, position = "dodge") +
      xlab("Age") +
      labs(fill = "Gender")
  })
  
  
  output$incomeGraph <- renderPlot({
    if (is.null(gender_ractive_values$incomeGraph))
      return()
    gender_ractive_values$incomeGraph
  })
  
  output$spendingScoreGraph <- renderPlot({
    if (is.null(gender_ractive_values$spendingScoreGraph))
      return()
    gender_ractive_values$spendingScoreGraph
  })
  
  output$ageGraph <- renderPlot({
    if (is.null(gender_ractive_values$ageGraph))
      return()
    gender_ractive_values$ageGraph
  })
  
  output$agePlot <- renderPlot({
    if (input$ageRadioButtons == "Income and Spending Score") {
      plot(
        customer_data_full$Age,
        customer_data_full$Spending.Score..1.100.,
        pch = 19,
        main = "Income and Spending Score by Age",
        xlab = "Age",
        ylab = "Spending Score / Income"
      )
      points(
        customer_data_full$Age,
        customer_data_full$Annual.Income..k..,
        col = "red",
        pch = 19
      )
    }
    if (input$ageRadioButtons == "Income") {
      plot(
        customer_data_full$Age,
        customer_data_full$Annual.Income..k..,
        pch = 19,
        main = "Income by Age",
        xlab = "Age",
        ylab = "Annnual Income"
      )
    }
    if (input$ageRadioButtons == "Spending Score") {
      plot(
        customer_data_full$Age,
        customer_data_full$Spending.Score..1.100.,
        pch = 19,
        main = "Spending Score by Age",
        xlab = "Age",
        ylab = "Spending Score"
      )
    }
  })
  
  output$clusterGraph <- renderPlot({
    clusters = kmeans(customer_data, input$clusterCount)
    clusplot(customer_data, clusters$cluster, color = T, shade = F, main = "K-Means Cluster Analysis")
  })
  
  output$clusterTableSpendingScore <- renderDataTable({
    clusters = kmeans(customer_data, input$clusterCount)
    customer_data$Clusters = clusters$cluster
    customer_data$Gender = customer_data_full$Gender
    customer_data$CustomeID = customer_data_full$CustomerID
    customer_data$Annual_Income = customer_data_full$Annual.Income..k..
    customer_data
    })
  
  sum_of_squares = kmeans(customer_data, centers = 1)$tot.withinss
  
  for (i in 2:15)
    sum_of_squares[i] = kmeans(customer_data, centers = i)$tot.withinss
  
  sum_of_squares_df = data.frame(c(1:15), c(sum_of_squares))
  names(sum_of_squares_df)[1] = "Clusters"
  names(sum_of_squares_df)[2] = "SS"
  
  output$ssTable <- renderDataTable(sum_of_squares_df)
  
  sum_of_squares_df %>%
    ggvis( ~ Clusters, ~ SS) %>%
    layer_points(fill := "blue") %>%
    layer_lines() %>%
    set_options(height = 300, width = 400) %>%
    bind_shiny("ssPlot", "ssPlot")
  
  #### SECOND CLUSTERING SET
  output$clusterGraphIncome <- renderPlot({
    clusters_two = kmeans(customer_data_cluster_two, input$clusterCountTwo)
    clusplot(customer_data_cluster_two, clusters_two$cluster, color = T, shade = F, main = "K-Means Cluster Analysis")
  })
  
  output$clusterTableIncome <- renderDataTable({
    clusters_two = kmeans(customer_data_cluster_two, input$clusterCountTwo)
    customer_data_cluster_two$Clusters = clusters_two$cluster
    customer_data_cluster_two$Gender = customer_data_full$Gender
    customer_data_cluster_two$CustomerID = customer_data_full$CustomerID
    customer_data_cluster_two$Spending_Score = customer_data_full$Spending.Score..1.100.
    customer_data_cluster_two
  })
  
  sum_of_squares_two = kmeans(customer_data_cluster_two, centers = 1)$tot.withinss
  
  for (i in 2:15)
    sum_of_squares_two[i] = kmeans(customer_data_cluster_two, centers = i)$tot.withinss
  
  sum_of_squares_df_two = data.frame(c(1:15), c(sum_of_squares_two))
  names(sum_of_squares_df_two)[1] = "Clusters"
  names(sum_of_squares_df_two)[2] = "SS"
  
  output$ssTableIncome <- renderDataTable(sum_of_squares_df_two)
  
  sum_of_squares_df_two %>%
    ggvis( ~ Clusters, ~ SS) %>%
    layer_points(fill := "blue") %>%
    layer_lines() %>%
    set_options(height = 300, width = 400) %>%
    bind_shiny("ssPlotIncome")
  
}

# Run the application
shinyApp(ui = ui, server = server)
