# A Shiny App for the Millenium Development Goals
Julieva Cohen, Alexandra, Antoine Settelen, Simon Weiss

~One week development
---

## 1. Introduction

### 1.1 Aim of the Group Project from U3-Programming-Advanced R-Msc AI and BA - TBS
- The aim of the project is to make an interactive graphical presentation of data using Shiny App.
- The user should be able to select parameters themselves. The data must be non-trivial, requiring the use of data from several sources or from several different tables.
- Work in groups. circa Three students per group. You can self-select groups.
- There should be need for computations on the data to achive the final results.


### 1.2 Presentaiton of the project choosen and Data
The goal of our project is to build a shiny App to monitor Millennium Developement Goals Progress over time. 

In the year 2000, the member states of the United Nations agreed to a set of goals to measure the progress of global development. The aim of these goals was to increase standards of living around the world by emphasizing human capital, infrastructure, and human rights.     

The eight goals are:

    To eradicate extreme poverty and hunger
    To achieve universal primary education
    To promote gender equality and empower women
    To reduce child mortality
    To improve maternal health
    To combat HIV/AIDS, malaria, and other diseases
    To ensure environmental sustainability
    To develop a global partnership for development


Data on Millennium Development Goals (MDG) indicator trends for developing countries and for different groups of countries are curated in the World Development Indicators database. Each year the World Bank uses these data to assess the progress made towards the MDGs.    

In order to have an aggreted data, we have used a very rich database provided by **Driven Data** offering an open challenge until 2021 with data compiled from the http://www.worldbank.org/ website. The description below of the MDG below comes from their plateforme. 

Driven Data aggregated  data from 1972-2007 on over 1200 macroeconomic indicators in 214 countries around the world. A random snapshot of the data looks like the below. Each row represents a timeseries for a specific indicator and country. The row has an id, a country name, a series code, a series name, and data for the years 1972 - 2007.

![Snapshot of data]("C:/Users/swp/Documents/_Perso/Cours/M2/U3. Programing/Advanced R/0.Group Project/Shiny Project/shiny_app/img/snapshot_millenium.png")


We hope that this presentation allowed to understand our project and the real usefulness of a shiny app! 
Indeed, building interactive dashboards offers the possibility to explore the evolution trend for each country and/or region of the world, to compare them both for each macroeconomic indicator but also for each millennium goal.

### 1.3 Examples found of target data visualization for our project
http://datatopics.worldbank.org/mdgs/trends-and-projections-of-each-mdg-indicator-for-each-country.html

http://datatopics.worldbank.org/sdgs/

