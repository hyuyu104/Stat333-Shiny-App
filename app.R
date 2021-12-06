#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Predict Yelp Ratings"),
    
    sidebarLayout(
        sidebarPanel(
            textAreaInput("text", "Please enter the yelp comment."),
            
            actionButton("click", "Predict"),
        ),
        mainPanel(
            textOutput("react")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    df <- eventReactive(input$click, {
       input$text
    })
    output$react <- renderText({
        df()
    })

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
