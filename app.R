##Exercice : Shiny Deaths from Covid-19



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Give the page a title
    titlePanel("Death from Covid"),

    # Generate a row with a sidebar
    sidebarLayout(      
        
        # Define the sidebar with one input
        sidebarPanel(
            selectInput("countryregion", "Country/Region:", 
                        choices=SelectedCountries),
            hr(),
            helpText("Data from CSSEGIS and Data JH")
        ),
        
        # Create a spot for the barplot
        mainPanel(
            plotOutput("countryPlot")  
        )
        
    )
)


#  Define a server for the Shiny app
server <- function(input, output) {
    # Fill in the spot we created for a plot
    output$countryPlot <- renderPlot({
        # Render the plot
      
        
PlotDT[`Country/Region` %in% SelectedCountries & is.na(`Province/State`)] %>% ggplot(aes(x = Date, y = Dead)) +
            geom_line(data = PlotDT[`Country/Region` == "France" & is.na(`Province/State`)])+
            geom_line(data = PlotDT[`Country/Region` == "Germany"])+
            geom_line(data = PlotDT[`Country/Region` == "Italy"])+
            geom_line(data = PlotDT[`Country/Region` == "Spain"])+
            geom_line(data = PlotDT[`Country/Region` == input$countryregion & is.na(`Province/State`)], color = 'red')
        
        
        
        
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
