library("shiny")
library("ggplot2")
library("GGally")
library("plotly")

data <- read.csv("Facebook_metrics/dataset_Facebook.csv", sep = ";")

server <- function(input, output)  {
  output$plot1 <- renderPlotly({
    df <- data[c("Post.Month", "share", "Type", "Lifetime.Post.Consumptions", "Lifetime.Post.Total.Impressions")]
    df <- df[df$Post.Month == input$month, ]
    p <- ggplot(df, aes(Lifetime.Post.Total.Impressions, Lifetime.Post.Consumptions, 
                        size = share, fill = Type,
                        text = paste('</br> Type: ', Type, 
                                     '</br> Number of Likes: ', Lifetime.Post.Total.Impressions, 
                                     '</br> Lifetime Post Consumptions: ', Lifetime.Post.Consumptions, 
                                     '</br> Share: ', share)))
    p <- p + geom_point(stroke = 0.1, alpha = 0.7)
    p <- p + scale_y_continuous("Lifetime Post Consumptions \n \n", limits = c(0, 5000))
    p <- p + scale_x_continuous("\n Lifetime Post Total Impressions", limits = c(0, 1e5))
    ggplotly(p, tooltip = 'text')
  })
  
  output$plot2 <- renderPlotly({
    df <- data[c("share", "like", "comment", "Type")]
    p <- ggpairs(df, 1:4, mapping = ggplot2::aes(color = Type))
    ggplotly(p)
  })
  
  output$plot3 <- renderPlotly({
    df <- data[c("Type", "share", "like", "comment")]
    p <- ggparcoord(df, 2:4, groupColumn= 1, alphaLines = 0.3, scale =
                      "center", scaleSummary = "median")
    ggplotly(p)
  })
}


ui <- fluidPage(
  sliderInput("month", 
              "Post Month for Bubble Chart", 
              min = 1, 
              max = 12, 
              value = 1),
  
  mainPanel(
    tabsetPanel(
      tabPanel(title = "Bubble Chart", plotlyOutput("plot1", width = 800, height = 600)),
      tabPanel(title = "Scatterplot Matrix", plotlyOutput("plot2", width = 800, height = 800)),
      tabPanel(title = "Parallel Coordinates Plot", plotlyOutput("plot3", width = 800, height = 600))
    )
  )
)

shinyApp(ui = ui, server = server)