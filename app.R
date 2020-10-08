##Exercice : Shiny Deaths from Covid-19

#résoudre le problème de selection sous variable (reactiv) : Antoine
#Rajouter Région : Antoine

#Amélioration du ggplot : Alexandra et Julieva
#-Mettre menu selection sur le haut
#-Comparaison des graphiques : rajouter plusieurs courbe en fonction de la selection utilisateur
#- changement du nom de l'axe des ordonnées en fonction de la metric'

#Rajouter deuxième onglet : Simon
#-Map
  
  
#global_scope
selected_country <- unique(PlotDT$Country_Name)
selected_topic <- unique(goalD$Topic)
selected_subtopic_1 <- list()
selected_subtopic_2 <- list()
selected_subtopic_3 <- list()



# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(
    # app title/description
    h1("MGD"),
  ),
  sidebarLayout(
    sidebarPanel(
      helpText("Here you can find some graphical information
                     about World Development Goals"),
      br(), 
      helpText("First, choose the World Development Indicators."),
      br(), 
      
      # inputs
      selectInput("topic", 
                  h2("Choose a topic", align = "center"),
                  selected_topic, 
                  "Environment"),
      
      br(), 
      
      selectInput("subtopic_1", 
                  h2("Choose a subtopic 1", align = "center"),
                  selected_subtopic_1, 
                  choices = NULL),
      
      br(), 
      
      selectInput("subtopic_2", 
                  h2("Choose a subtopic 2", align = "center"),
                  selected_subtopic_2, 
                  choices = NULL),
      
      br(), 
      
      selectInput("subtopic_3", 
                  h2("Choose a subtopic 3", align = "center"),
                  selected_subtopic_3, 
                  choices = NULL),
      
      br(), 
      
      selectInput("country", 
                  h2("Choose a country", align = "center"),
                  selected_country, 
                  "France"),
      br(),
      
      radioButtons("y_axis_choice", 
                   h2("Axis :", align = "center"), 
                   c("linear", "logarithmic")), 
      
      br(), 
      
      dateRangeInput("date_choice", 
                     h2("Choose a date range :", align="center"),
                     format = "yyyy",
                     start="1972"),
      
      br(),
      p(strong("Full data is available just below."), style="strong"),
      br(),
      a(strong("DATA AVAILABLE HERE"), href="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"),
      br(),
      img(src="https://i1.wp.com/www.un.org/sustainabledevelopment/wp-content/uploads/2015/12/english_SDG_17goals_poster_all_languages_with_UN_emblem_1.png?fit=728%2C451&ssl=1", height = 72, width = 72, style="margin-left:80px"),

      
    ),
    mainPanel(
      # outputs
      plotOutput("displot"), 
      
    )
  )
)


#  Define a server for the Shiny app
server <- function(input, output) {
  
  
  output$displot <- renderPlot({
    
    p <- switch(input$y_axis_choice,"linear" = NULL,"logarithmic"=scale_y_log10())
    
    ggplot(PlotDT[`Country_Name`==input$country &`Series_Name.x`=="Agricultural machinery, tractors"],
           aes(x= Date, y = Value)) + 
      geom_line() + scale_x_date(limits = input$date_choice) + p
    
  })
  
}
  



# Run the application 
shinyApp(ui = ui, server = server)
