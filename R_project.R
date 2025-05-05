library(shiny)
library(shinydashboard)
library(arules)
library(dplyr)

# Define UI for the main app
ui <- dashboardPage(
  dashboardHeader(title = "Market data analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input", tabName = "input", icon = icon("dashboard")),
      menuItem("K-means Clustering Results", tabName = "kmeans_results", icon = icon("table")),
      menuItem("Apriori Rules", tabName = "apriori_rules", icon = icon("list-alt")),
      menuItem("Plots", tabName = "plots", icon = icon("chart-bar"))
    )
  ),
  dashboardBody( 
    tabItems(
      tabItem(tabName = "input",    
              fluidRow(
                column(12, textInput("file_location", "Enter CSV File Location")),
                column(12, sliderInput("support", "Apriori Support:", min = 0.001, max = 1, value = 0.01)),
                column(12, sliderInput("confidence", "Apriori Confidence:", min = 0.001, max = 1, value = 0.01)),
                column(12, sliderInput("clusters", "Number of Clusters for K-means:", min = 2, max = 4, value = 3)),
                column(12, actionButton("update_data", "Update Data"))
              )
      ),
      tabItem(tabName = "kmeans_results",
              fluidRow(
                column(12, tableOutput("cluster_table"))
              )
      ),
      tabItem(tabName = "apriori_rules",
              fluidRow(
                column(12, textOutput("num_apriori_rules")),
                column(12, tableOutput("apriori_rules_table"))
              )
      ),
      tabItem(tabName = "plots",
              fluidRow(
                column(6, plotOutput("pie_chart")),
                column(6, plotOutput("scatter_plot")),
                column(6, plotOutput("bar_plot")),
                column(6, plotOutput("hist"))
              ))
    )
  )
)


# Define server logic for the main app
server <- function(input, output ) {
  data <- reactive({
    req(input$file_location)
    grc <- read.csv(input$file_location)
     #clean duplicated rows
    grc<-distinct(grc)
    
  })
  
  observeEvent(input$update_data, {
    
    grc <- data()
    #apriori rule
    output$num_apriori_rules <- renderText({
      grc <- data()
      translist <- strsplit(grc$items, ",")
      trans <- as(translist, "transactions")
      apriori_rules <- apriori(trans, parameter = list(supp = input$support, conf = input$confidence, minlen = 2))
      num_rules<-length(apriori_rules)
      paste("number of apriori rules: ",num_rules)
    })

    output$apriori_rules_table <- renderTable({
      req(data())
      grc <- data()
      translist <- strsplit(grc$items, ",")
      trans <- as(translist, "transactions")
      apriori_rules <- apriori(trans, parameter = list(supp = input$support, conf = input$confidence, minlen = 2))
      rules <- as(apriori_rules, "data.frame")
      rules
    }, n = Inf)
   #k-mans
    output$cluster_table <- renderTable({
     
      n <- input$clusters
      totalbyage <- aggregate(total ~ age, grc, sum)
      data_matrix <- cbind(totalbyage$total, totalbyage$age)
      
      kmean_clustering <- kmeans(data_matrix, centers = 3)
      
      cluster <- data.frame(customer = grc$customer, age = grc$age, total = grc$total)
      cluster$cluster <- kmean_clustering$cluster[match(cluster$age, totalbyage$age)]
      
      cluster <- cluster %>% 
        distinct(age, .keep_all = TRUE)
      cluster
    })
    
#pie chart to compare between cash ans credit 
    output$pie_chart <- renderPlot({
      grc <- data()
      cash_credit <- aggregate(grc$total, by = list(grc$paymentType) , FUN=sum)
      colnames(cash_credit) <- c("type of payment", "total")
      x = c(cash_credit$total)
      percentage<-paste0(round(100*x/sum(x),3),"%")
      pie(x, labels = percentage, main = "Compare cash and credit total", col=c("gray","skyblue"))
      # add more information by using legend
      legend("topright", legend = c("cash", "credit"), fill = c("gray", "skyblue"))
      
    })
   # plot to Compare each age and sum of total spending

 output$scatter_plot <- renderPlot({
      grc <- data()
      spending_age<- aggregate(grc$total, by = list(grc$age), FUN= sum)
      colnames(spending_age)<-c("age","total")
      plot(spending_age$age, spending_age$total , xlab = "age" , ylab = "spending")
    
    })
      #compare between each city and its spending 
    output$bar_plot <- renderPlot({
      grc <- data()
      spending_city <- aggregate(grc$total, by = list(grc$city), FUN = sum)
      colnames(spending_city)<- c("city",("total"))
      spending_city <- arrange(spending_city,desc(total))
      barplot(spending_city$total,
              names.arg = spending_city$city,
              main = "Total Spending by City",
              col = "skyblue",
              las = 2)
    }) 
   #spending distribution 
   
    output$hist <- renderPlot({
      grc <- data()
      hist(grc$total, col="gray", main="spending" , xlab = "spending")
    })
  })
}
# Run the app
shinyApp(ui = ui, server = server)
