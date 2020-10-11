##Exercice : Shiny Deaths from Covid-19

#resoudre le pb de selection sous variable (reactiv) : Antoine
#rajouter region : Antoine

#Amelioration du ggplot : Alexandra et Julieva
#Mettre menu selection sur le haut
#Comparaison des graphiques : rajouter 
#plusieurs courbes en fonction de la selection utilisateur
#changement du nom de laxe des ordonnees en 
#fonction de la metric 

#rajouter deuxieme onglet : simon 
#map


#library
library(shiny)
library(magrittr)
library(data.table)
library(ggplot2)
library(readr)
library(httr)
library(readxl)
library(stringr)
library(shinyWidgets)
library(plotly)

  
#global_scope
selected_country <- unique(PlotDT$Country_Name)
selected_topic <- unique(goalD$Topic)
selected_subtopic_1 <- list()
selected_subtopic_2 <- list()
selected_subtopic_3 <- list()
selected_indicator <- list()
selected_aggregation<-unique(colnames(Country_break[,2:5]))
Plot_choices<-PlotDT_Region#Initizalise

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(
    # app title/description
    h1("Millenium Development Goals"),
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
                  selected_topic),
      
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
      selectInput("indicator", 
                  h2("Choose a indicator", align = "center"),
                  selected_indicator, 
                  choices = NULL),
      
      br(), 
      
      selectInput("country", 
                  h2("Choose a country", align = "center"),
                  selected_country, 
                  "France"),
      br(), 
      selectInput("aggregation", 
                  h2("Choose an aggregation view", align = "center"),
                  choices=selected_aggregation),

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
      br(), 
  
    ),
    mainPanel(
      # outputs
      (plotlyOutput("displot")), 
      
      plotOutput("summary"),
      
    )
  )
)


#  Define a server for the Shiny app
server <- function(input, output, session) {
  
  #Store aggregation selected
  #Try whit ObservedEvent and stock change in new datatable
  
  observeEvent(input$aggregation, {
    isolate({
    if(input$aggregation =="Region") {
      Plot_choices <- PlotDT_Region
    } else if(input$aggregation =="Income_group") {
      Plot_choices <- PlotDT_Income_group
    } else {
      Plot_choices <- PlotDT_Other
    }
    })
  }
  )
  
  observeEvent(input$topic, {
    #remise a zero de l'indicator
      updateSelectInput(
        session,
        inputId = "indicator",
        choices = ''
      )
      #
    
    
    
      #look if there is something in SubTopic1
      choices <- unique(goalD[goalD$Topic == input$topic, list(SubTopic1)])
      
      #change the value of subtopic_1 in function of the value of topic  
      updateSelectInput(
      session,
      inputId = "subtopic_1",
      choices = choices
      )
  })
  
  observeEvent(input$subtopic_1, {
    #look if there is something in SubTopic2
    choices <- unique(goalD[goalD$SubTopic1 == input$subtopic_1, list(SubTopic2)])
    
    not_value_subtopic_2 = as.vector(is.na(choices[1]))
  
    #change the value of subtopic_2 in function of the value of topic
    if(not_value_subtopic_2) {
      updateSelectInput(
        session,
        inputId = "subtopic_2",
        choices = ''
      )
      
      updateSelectInput(
        session,
        inputId = "subtopic_3",
        choices = ''
      )
      
      #find indicator list
      updateSelectInput(
        session,
        inputId = "indicator",
        choices = unique(goalD[goalD$SubTopic1 == input$subtopic_1, list(Series_Name.x)])
      )
      #
      
    }
    else {
      updateSelectInput(
        session,
        inputId = "subtopic_2",
        choices = choices
      )
    }
      
  })
  
  observeEvent(input$subtopic_2, {
    if (input$subtopic_2 != '') {
    #look if there is something in SubTopic3
    choices <- unique(goalD[goalD$SubTopic2 == input$subtopic_2, list(SubTopic3)])
    
    not_value_subtopic_3 = as.vector(is.na(choices[1]))
    
    #change the value of subtopic_2 in function of the value of topic
    if(not_value_subtopic_3) {
      updateSelectInput(
        session,
        inputId = "subtopic_3",
        choices = ''
      )
      
      #find indicator list
      updateSelectInput(
        session,
        inputId = "indicator",
        choices = unique(goalD[goalD$SubTopic2 == input$subtopic_2, list(Series_Name.x)])
      )
      #
      
    }
    else {
      updateSelectInput(
        session,
        inputId = "subtopic_3",
        choices = choices
      )
    }
    }
    
  })
  
  observeEvent(input$subtopic_3, {
    if (input$subtopic_3 != '') {
      #find indicator list
      updateSelectInput(
        session,
        inputId = "indicator",
        choices = unique(goalD[goalD$SubTopic3 == input$subtopic_3, list(Series_Name.x)])
      )
      #
    }
  })
  

  
  #add condition if selected : aggregation view or single view (donc 2 groupe de plot)

  output$displot <- renderPlotly({
    
    p <- switch(input$y_axis_choice,"linear" = NULL,"logarithmic"=scale_y_log10())
  
    q<-ggplot() +geom_line(data=PlotDT[`Country_Name`==input$country &`Series_Name.x`==input$indicator], aes(x=Date, y=Value,colour="Selected Country"))+ geom_line(data=Plot_choices[`Series_Name.x`==input$indicator], aes(x= Date, y = Value, colour = `Region`)) + p
    ggplotly(q)

})
}



# Run the application 
shinyApp(ui = ui, server = server)
