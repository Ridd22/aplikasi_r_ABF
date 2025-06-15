library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Aplikasi Distribusi Frekuensi"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Unggah File CSV:",
                accept = c("text/csv", 
                           "text/comma-separated-values,text/plain", 
                           ".csv")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Pemisah",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      numericInput("bins", "Jumlah Interval:", value = 10, min = 1),
      actionButton("calc", "Hitung Distribusi")
    ),
    
    mainPanel(
      tableOutput("freqTable"),
      plotOutput("freqPlot")
    )
  )
)

# Server
server <- function(input, output) {
  
  data <- eventReactive(input$calc, {
    req(input$file)  # Memastikan file diunggah
    df <- read.csv(input$file$datapath, header = input$header, sep = input$sep)
    return(df)
  })
  
  output$freqTable <- renderTable({
    req(data())
    # Menghitung distribusi frekuensi
    freq_data <- as.data.frame(table(cut(data()[,1], breaks = input$bins)))
    colnames(freq_data) <- c("Interval", "Frekuensi")
    return(freq_data)
  })
  
  output$freqPlot <- renderPlot({
    req(data())
    freq_data <- as.data.frame(table(cut(data()[,1], breaks = input$bins)))
    colnames(freq_data) <- c("Interval", "Frekuensi")
    
    ggplot(freq_data, aes(x = Interval, y = Frekuensi)) +
      geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
      labs(title = "Distribusi Frekuensi", x = "Interval", y = "Frekuensi") +
      theme_minimal()
  })
}

# Menjalankan aplikasi
shinyApp(ui = ui, server = server)#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
