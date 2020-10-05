##Exercice : Shiny Deaths from Covid-19
#Load Library
library(shiny)
library(magrittr)
library(data.table)
library(ggplot2)
library(readr)


# import data
urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
coronaD<-urlfile%>% url %>% read_csv %>% as.data.table 
# Function
Dates<-colnames(coronaD[,-c(1:4)])
setDT(coronaD)
PlotDT = melt(coronaD, id.vars = 1:4, measure.vars = Dates, variable.name = "Date",value.name = "Dead")
PlotDT$Date = PlotDT$Date %>% as.Date(format = "%m/%d/%y")
SelectedCountries = c("France","Italy","Spain","Germany")

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
