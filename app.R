##Exercice : Shiny Deaths from Covid-19

#rajouter region : simon

#Mettre menu selection sur le haut : alexandra 

#histro : aggregation proporitonelle avec les indicateurs (virer pays)
#map : antoine et alexandra. Choroleptre

#(a la fin) Side panel : couper a pays pour mettre a droite. 


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
library(dplyr)
library(shinythemes)
library(shinyjs)
library(shinyBS)

  
#global_scope
#selected_country <- unique(PlotDT$Country_Name)
selected_country <- unique(PlotDT_Flags$Country_Name)
selected_topic <- unique(goalD$Topic)
selected_subtopic_1 <- list()
selected_subtopic_2 <- list()
selected_subtopic_3 <- list()
selected_indicator <- list()
#selected_aggregation<-unique(colnames(Country_break[,2:5]))
selected_aggregation<-unique(str_replace_all(colnames(Country_break[,2:5]), "_", " "))
Plot_choices<-PlotDT_Region#Initizalise
selected_year<-colnames(goalD[, c(4:39)])
#Code <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
selected_flags <- unique(PlotDT_Flags$ImageURL)
graph_value <- 0


# Define UI for application that draws a histogram
ui <- fluidPage(theme = "bootstrap.css",
                useShinyjs(),
  #CSS
  tags$head(
    tags$style(HTML("
    
      .shiny-output-error { 
        visibility: hidden; 
      }
      
      .shiny-output-error:before { 
        visibility: hidden; 
      }
      
      h1 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: green;
      }
      
      h2 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: green;
      }
      
      h3 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: black;
      }
      
      body {
        background-color: #fff;
      }
      
      .selectize-input {
        min-height: 20px;
        border: 0;
        padding: 4px;
        font-family: 'Lobster', cursive;
      }
      
    "))
  ),
  
  titlePanel(
    # app title/description
    h1("Millenium Development Goals", align="center"),
  ),
  br(),
  sidebarLayout(
    sidebarPanel(
      id="Sidebar",
      
      helpText("Here you can find some graphical information
                     about World Development Goals"),
      helpText("First, choose the World Development Indicators."), 
      br(),
      
      tabsetPanel(
        tabPanel(h2("Page 1", style="color:black"),
                 
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
                 ),
        tabPanel(h2("Page 2", style="color:black"),
                 
                 pickerInput("country",  h2("Choose a country", align = "center"), multiple = F,
                             choices = selected_country,
                             
                             choicesOpt = list(content =  
                                                 mapply(selected_country, selected_flags, FUN = function(country, flagUrl) {
                                                   HTML(paste(
                                                     tags$img(src=flagUrl, width=20, height=15),
                                                     country
                                                   ))
                                                 }
                                                 
                                                 ))),
                 
                 
                 
                 #selectInput("country", 
                 #            h2("Choose a country", align = "center"),
                 #            selected_country, 
                 #            "France"),
                 
                 br(), 
                 awesomeRadio(inputId = "aggregation", 
                              label = h2("Choose an aggregation view", align = "center"),
                              selected_aggregation, 
                              "Region",
                              status="warning"),
                 
                 br(), 
                 selectInput("year", 
                             h2("Choose a year for the map (Page 3)", align = "center"),
                             selected_year, 
                             "1972"),
                 
                 br(),
                 awesomeRadio(
                   inputId = "y_axis_choice",
                   label = h2("Axis :", align = "center"),  
                   c("linear", "logarithmic"),
                   status = "success"
                 ), 
 
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
                 )
      ),
  
    ),
    mainPanel(id ="Main",
              
      bsButton("showpanel", strong("Show/hide sidebar"), type = "toggle", value = TRUE),
      
      radioGroupButtons(
        inputId = "graph",
        label = h3("Choose a graph :"), 
        choices = c(`<i class='fa fa-line-chart'></i>` = "line", `<i class='fa fa-bar-chart'></i>` = "bar", 
                    `<i class="fas fa-globe-europe"></i>` = "globe"),
        justified = TRUE, 
      ),
      
      #tabsetPanel(type = "tabs",
      #            tabPanel(h3("GGPLOT"), (plotlyOutput("displot"))),
      #            tabPanel(h3("HISTO"), (plotlyOutput("displot2"))),
      #            tabPanel(h3("MAP"), plotlyOutput("displot3"))
      #)
      
      
      plotlyOutput("displot4"),
      
      
      htmlOutput("text")
      
      #strong("Definition : "),
      #(Long_definition),
      #p(),
      #strong("Source : "),
      #(Source),
    )
  )
)


#  Define a server for the Shiny app
server <- function(input, output, session) {
  
  output$text<-renderUI({
    
    Long_definition<-paste("Definition : ",WDI_metadata[`Indicator Name`==input$indicator][,3])
    Source<-paste("Source : ", WDI_metadata[`Indicator Name`==input$indicator][,4])
    HTML(paste(Long_definition,Source,sep='<p/>'))
  } )
  
  observeEvent(input$showpanel, {
    
    if(input$showpanel == TRUE) {
      removeCssClass("Main", "col-sm-12")
      addCssClass("Main", "col-sm-8")
      
      shinyjs::show(id = "Sidebar")
      shinyjs::enable(id = "Sidebar")
      
    }
    else {
      removeCssClass("Main", "col-sm-8")
      addCssClass("Main", "col-sm-12")
      shinyjs::hide(id = "Sidebar")
      
    }
  
  })
  
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
  
  #Store aggregation selected
  #Try whit ObservedEvent and stock change in new datatable
  
  #observeEvent(input$aggregation, {
  #  isolate({
  #  if(input$aggregation =="Region") {
  #    Plot_choices <- PlotDT_Region
  #  } else if(input$aggregation =="Income_group") {
  #    Plot_choices <- PlotDT_Income_group
  #  } else {
  #    Plot_choices <- PlotDT_Other
  #  }
  #  })
  #}
  #)
  
  #add condition if selected : aggregation view or single view (donc 2 groupe de plot)

  output$displot <- renderPlotly({
    
    p <- switch(input$y_axis_choice,"linear" = NULL,"logarithmic"=scale_y_log10())
    
    Plot_choices <- switch(input$aggregation,"Region"=PlotDT_Region,
                           "Income_group"=PlotDT_Income_group,
                           "Lending_category"=PlotDT_Lending_category, 
                           "Other"=PlotDT_Other)
    
    color <- switch(input$aggregation,"Region"=PlotDT_Region,
                    "Income_group"=PlotDT_Income_group,
                    "Lending_category"=PlotDT_Lending_category, 
                    "Other"=PlotDT_Other)
  
    q<-ggplot() +
      geom_line(data=PlotDT[`Country_Name`==input$country &`Series_Name.x`==input$indicator], 
                aes(x=Date, y=Value,colour=input$country))+ 
      geom_line(data=Plot_choices[`Series_Name.x`==input$indicator], 
                aes_string("Date", "Value", colour = input$aggregation)) + 
      xlab("Dates")+
      ylab(input$indicator)+
      p
  
    ggplotly(q)

})
  
  
  
  output$displot2 <- renderPlotly({
    p <- switch(input$y_axis_choice,"linear" = NULL,"logarithmic"=scale_y_log10())
    
    Plot_choices <- switch(input$aggregation,"Region"=PlotDT_Region,
                           "Income_group"=PlotDT_Income_group,
                           "Lending_Category"=PlotDT_Lending_category, 
                           "Other"=PlotDT_Other)
    
    q <- ggplot() + geom_bar(data=PlotDT[`Country_Name`==input$country 
                                         &`Series_Name.x`==input$indicator], 
                             aes(x=Date,y=Value, colour=input$country), stat="identity")+
      geom_bar(data=Plot_choices[`Series_Name.x`==input$indicator], 
               aes_string(x="Date",y="Value", colour=input$aggregation), stat="identity")+
      xlab("Dates")+
      ylab(input$indicator)
    q
    
    
  })
  
  #output$displot3 <- renderPlot({
  #  WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify
  #  
  #  df <- data.frame(region=input$country, 
  #                   value=11, 
  #                   stringsAsFactors=FALSE)
  #  
  #  p <- ggplot() +
  #    geom_map(data = WorldData, map = WorldData,
  #             aes(x = long, y = lat, group = group, map_id=region),
  #             fill = "white", colour = "#7f7f7f", size=0.5) + 
  #    geom_map(data = df, map=WorldData,
  #             aes(fill=value, map_id=region),
  #             colour="#7f7f7f", size=0.5) +
  #    coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  #    scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  #    scale_y_continuous(breaks=c()) +
  #    scale_x_continuous(breaks=c()) +
  #    labs(fill="legend", title="Title", x="", y="") +
  #    theme_bw() +
  #    ggtitle(paste("You choose : ", input$country))
    
  #  p 
  #})
  
  
  output$displot3 <- renderPlotly({
    agr_data <- PlotDT[year(Date) == input$year & 
                         Series_Name.x == input$indicator, 
                       list(Country_Name, Series_Name.x, Value)]
    
    agr.map <- merge(agr_data, Code_break,
                     by.x = 'Country_Name', 
                     by.y = 'Economy', all= TRUE)
    
    agr.map <- as.data.table(agr.map)
    
    idx = agr.map[, .I[which(is.na(Value))]]
    
    agr.map[idx, Value := 0]
    
    
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    fig <- plot_geo(agr.map)
    
    fig <- fig %>% add_trace(
      z = ~Value, color = ~Value, colors = 'Greens',
      text = ~Country_Name, locations = ~Code, marker = list(line = l)
    )
    
    fig <- fig %>% colorbar(title = input$indicator, tickprefix = '$')
    
    fig <- fig %>% layout(
      title = input$indicator,
      geo = g
    )
    
    fig
  })
  
  output$displot4 <- renderPlotly({
    
    
    if(input$graph == "line") {
      p <- switch(input$y_axis_choice,"linear" = NULL,"logarithmic"=scale_y_log10())
      
      
      color <- str_replace(input$aggregation, " ", "_")
      
      Plot_choices <- switch(input$aggregation,"Region"=PlotDT_Region,
                             "Income group"=PlotDT_Income_group,
                             "Lending category"=PlotDT_Lending_category, 
                             "Other"=PlotDT_Other)
      
      #color <- switch(input$aggregation,"Region"=PlotDT_Region,
      #                "Income_group"=PlotDT_Income_group,
      #                "Lending_category"=PlotDT_Lending_category, 
      #                "Other"=PlotDT_Other)
      
      q<-ggplot() +
        geom_line(data=PlotDT[`Country_Name`==input$country &`Series_Name.x`==input$indicator], 
                  aes(x=Date, y=Value,colour=input$country))+ 
        geom_line(data=Plot_choices[`Series_Name.x`==input$indicator], 
                  aes_string("Date", "Value", colour = color)) + 
        xlab("Dates")+
        ylab(input$indicator)+
        p
      
      ggplotly(q)
      
    } else if (input$graph == "bar")  {
      p <- switch(input$y_axis_choice,"linear" = NULL,"logarithmic"=scale_y_log10())
      
      color <- str_replace(input$aggregation, " ", "_")
      
      Plot_choices <- switch(input$aggregation,"Region"=PlotDT_Region,
                             "Income group"=PlotDT_Income_group,
                             "Lending category"=PlotDT_Lending_category, 
                             "Other"=PlotDT_Other)
      
      q <- ggplot() + geom_bar(data=PlotDT[`Country_Name`==input$country 
                                           &`Series_Name.x`==input$indicator], 
                               aes(x=Date,y=Value, colour=input$country), stat="identity")+
        geom_bar(data=Plot_choices[`Series_Name.x`==input$indicator], 
                 aes_string(x="Date",y="Value", colour=color), stat="identity")+
        xlab("Dates")+
        ylab(input$indicator)
      q
      
    } else if (input$graph == "globe") {
      agr_data <- PlotDT[year(Date) == input$year & 
                           Series_Name.x == input$indicator, 
                         list(Country_Name, Series_Name.x, Value)]
      
      agr.map <- merge(agr_data, Code_break,
                       by.x = 'Country_Name', 
                       by.y = 'Economy', all= TRUE)
      
      agr.map <- as.data.table(agr.map)
      
      idx = agr.map[, .I[which(is.na(Value))]]
      
      agr.map[idx, Value := 0]
      
      
      # light grey boundaries
      l <- list(color = toRGB("grey"), width = 0.5)
      
      # specify map projection/options
      g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'Mercator')
      )
      
      fig <- plot_geo(agr.map)
      
      fig <- fig %>% add_trace(
        z = ~Value, color = ~Value, colors = 'Greens',
        text = ~Country_Name, locations = ~Code, marker = list(line = l)
      )
      
      fig <- fig %>% colorbar(title = input$indicator, tickprefix = '$')
      
      fig <- fig %>% layout(
        title = input$indicator,
        geo = g
      )
      
      fig
      
    }
    
  })
   
  
  
  
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)
