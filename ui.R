#Load the library
#Map
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
library(repmis)

#Shiny
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

#Github (Update Map)
#devtools::install_github("RamiKrispin/coronavirus",force = TRUE)
#library(coronavirus)
#update_dataset() 
#data(coronavirus)
source_data("https://github.com/RamiKrispin/coronavirus/blob/master/data/coronavirus.rda?raw=true")

#Symptoms
library(markdown)
library(caret)
library(e1071)
library(sqldf)
library(DBI)
library(RSQLite)
library(stringr)

#MAP------
# import data
countries = read.csv("E:/UM/Master/Sem 1/WQD7001 PRINCIPLES OF DATA SCIENCE/Assignment/Group/Final/Map/COVID 19 Map/countries_codes_and_coordinates.csv")
worldcountry = geojson_read("E:/UM/Master/Sem 1/WQD7001 PRINCIPLES OF DATA SCIENCE/Assignment/Group/Final/Map/COVID 19 Map/50m.geojson", what = "sp")
name_chg = read.csv("E:/UM/Master/Sem 1/WQD7001 PRINCIPLES OF DATA SCIENCE/Assignment/Group/Final/Map/COVID 19 Map/countries_name_change.csv")

# process data
df_latest <- coronavirus %>%
  dplyr::group_by(country, date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  #dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(active = confirmed - death - recovered) %>%
  #dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum = cumsum(confirmed),
    death_cum = cumsum(death),
    recovered_cum = cumsum(recovered),
    active_cum = cumsum(active)
  ) %>%
  filter(date==max(date))

# Merging
df_latest = merge(df_latest, name_chg, by = "country")
colnames(df_latest)[1] = "jhi_id"
colnames(df_latest)[11] = "country"

df_merged = merge(df_latest, countries, by = "country")


# Preparing df  for leaflet(order and match with the polygon file)
df = df_merged %>% filter(alpha3 %in% worldcountry$ADM0_A3)
if (all(df$alpha3 %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
df = df[order(df$alpha3),]
worldcountry_subset = worldcountry[worldcountry$ADM0_A3 %in% df$alpha3, ]
df = df[match(worldcountry_subset$ADM0_A3, df$alpha3),]

reactive_polygons = worldcountry[worldcountry$ADM0_A3 %in% df$alpha3, ]

# create plotting parameters for map
bins = c(0,10000,100000,200000,Inf)
cv_pal <- colorBin("Oranges", domain = df$confirmed_cum, bins = bins)
#plot_map <- worldcountry[worldcountry$ADM0_A3 %in% df$alpha3, ]

# Color for each legend
cummulated_col = "#cc4c02" #orange
new_col = "#045a8d" # Blue
death_col = "black" #?
recovered_col = "#016c59" #green
active_col = "red"

#Symptoms------
#Load Training Model (NB)
load("E:/UM/Master/Sem 1/WQD7001 PRINCIPLES OF DATA SCIENCE/Assignment/Group/Final/Model/NB(wo_training).rda")

bootstrapPage(
  tags$head(includeHTML("E:/UM/Master/Sem 1/WQD7001 PRINCIPLES OF DATA SCIENCE/Assignment/Group/Final/Map/COVID 19 Map/gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "Coronavirus (COVID-19) Self Checker", id="nav",
             
             tabPanel("COVID-19 location tracker",
                      div(class="outer",
                          tags$head(includeCSS("E:/UM/Master/Sem 1/WQD7001 PRINCIPLES OF DATA SCIENCE/Assignment/Group/Final/Map/COVID 19 Map/styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                                        h3(textOutput("total_case_count"), align = "right"),
                                        h4(textOutput("total_death_count"), align = "right"),
                                        span(h4(textOutput("total_recovered_count"), align = "right"), style="color:#006d2c"),
                                        span(h4(textOutput("total_active_count"), align = "right"), style="color:#cc4c02"),
                                        h6(textOutput("latest_clean_date"), align = "right"),
                          )
                          
                      )
             ),
             tabPanel("Symptom Checker",
                      strong("Coronavirus Symptoms Results (Prediction)",style = "font-size:30px"),
                      sidebarPanel(strong("COVID-19 Symptoms Checker",style = "font-size:30px"),
                                   radioButtons("Cough",h3("Cough"),
                                                choices = list("Yes" = 2,
                                                               "No" = 1),
                                                selected = 2),
                                   radioButtons("Fever",h3("Fever"),
                                                choices = list("Yes" = 2,
                                                               "No" = 1),
                                                selected = 2),
                                   radioButtons("Sore_Throat",h3("Sore Throat"),
                                                choices = list("Yes" = 2,
                                                               "No" = 1),
                                                selected = 2),
                                   radioButtons("Shortness_of_breath",h3("Shortness of breath"),
                                                choices = list("Yes" = 2,
                                                               "No" = 1),
                                                selected = 2),
                                   radioButtons("Headache",h3("Headache"),
                                                choices = list("Yes" = 2,
                                                               "No" = 1),
                                                selected = 2),
                                   radioButtons("Age", h3("Age"),
                                                choices = list("60 and above" = 2,
                                                               "Below 60" = 1),
                                                selected = 2),
                                   radioButtons("Gender", h3("Gender"),
                                                choices = list("Male" = 2,
                                                               "Female" = 1),
                                                selected = 2),
                                   radioButtons("Abroad", h3("Travel within 14 days?"),
                                                choices = list("Yes" = 2,
                                                               "No" = 1),
                                                selected = 2),
                                   radioButtons("Contact", h3("Contact with COVID-19 Patient?"),
                                                choices = list("Yes" = 2,
                                                               "No" = 1),
                                                selected = 2),
                                   actionButton("do", "Submit")),
                      mainPanel(textOutput("corona_result"))
             ),
             navbarMenu("COVID-19 Information",
                        tabPanel("Overview",
                                 strong("Coronavirus Disease Overview",style = "font-size:30px"),
                                 p("Coronavirus disease (COVID-19) is an infectious disease caused by a newly discovered coronavirus.",style = "font-size:20px"),
                                 br(),
                                 p("Most people infected with the COVID-19 virus will experience mild to moderate respiratory illness and recover without requiring special treatment.  Older people, and those with underlying medical problems like cardiovascular disease, diabetes, chronic respiratory disease, and cancer are more likely to develop serious illness.",style = "font-size:20px", align = "justify"),
                                 br(),
                                 p("The best way to prevent and slow down transmission is be well informed about the COVID-19 virus, the disease it causes and how it spreads. Protect yourself and others from infection by washing your hands or using an alcohol based rub frequently and not touching your face.",style = "font-size:20px", align = "justify"),
                                 br(),
                                 p("The COVID-19 virus spreads primarily through droplets of saliva or discharge from the nose when an infected person coughs or sneezes, so it's important that you also practice respiratory etiquette (for example, by coughing into a flexed elbow).",style = "font-size:20px", align = "justify"),
                                 br(),
                                 p("At this time, there are no specific vaccines or treatments for COVID-19. However, there are many ongoing clinical trials evaluating potential treatments. WHO will continue to provide updated information as soon as clinical findings become available.",style = "font-size:20px", align = "justify")),
                        tabPanel("Prevention",
                                 strong("Coronavirus Disease Prevention",style = "font-size:30px"),
                                 p("To prevent infection and to slow transmission of COVID-19, do the following:",style = "font-size:20px"),
                                 p("- Wash your hands regularly with soap and water, or clean them with alcohol-based hand rub.",style = "font-size:20px"),
                                 p("- Maintain at least 1 metre distance between you and people coughing or sneezing.",style = "font-size:20px"),
                                 p("- Avoid touching your face.",style = "font-size:20px"),
                                 p("- Cover your mouth and nose when coughing or sneezing.",style = "font-size:20px"),
                                 p("- Stay home if you feel unwell.",style = "font-size:20px"),
                                 p("- Refrain from smoking and other activities that weaken the lungs.:",style = "font-size:20px"),
                                 p("- Practice physical distancing by avoiding unnecessary travel and staying away from large groups of people.",style = "font-size:20px")),
                        tabPanel("Symptoms",
                                 strong("Coronavirus Disease Symptoms",style = "font-size:30px"),
                                 p("COVID-19 affects different people in different ways. Most infected people will develop mild to moderate illness and recover without hospitalization.",style = "font-size:20px", align = "justify"),
                                 strong("Most common symptoms:",style = "font-size:20px"),
                                 p("- Fever",style = "font-size:20px"),
                                 p("- Dry cough.",style = "font-size:20px"),
                                 p("- Tiredness",style = "font-size:20px"),
                                 br(),
                                 strong("Less common symptoms:",style = "font-size:20px"),
                                 p("- Aches and pains",style = "font-size:20px"),
                                 p("- Sore throat",style = "font-size:20px"),
                                 p("- Diarrhoea",style = "font-size:20px"),
                                 p("- Conjunctivitis",style = "font-size:20px"),
                                 p("- Headache",style = "font-size:20px"),
                                 p("- Loss of taste or smell",style = "font-size:20px"),
                                 p("- A rash on skin, or discolouration of fingers or toes",style = "font-size:20px"),
                                 br(),
                                 strong("Serious symptoms:",style = "font-size:20px"),
                                 p("- Difficulty breathing or shortness of breath",style = "font-size:20px"),
                                 p("- Chest pain or pressure",style = "font-size:20px"),
                                 p("- Loss of speech or movement",style = "font-size:20px"),
                                 br(),
                                 p("Seek immediate medical attention if you have serious symptoms.  Always call before visiting your doctor or health facility.",style = "font-size:20px", align = "justify"),
                                 p("People with mild symptoms who are otherwise healthy should manage their symptoms at home.",style = "font-size:20px", align = "justify"),
                                 p("On average it takes 5-6 days from when someone is infected with the virus for symptoms to show, however it can take up to 14 days.",style = "font-size:20px", align = "justify"))
             ),
             tabPanel("Developers Details",
                      strong("Developers' details",style = "font-size:40px"),
                      p(a("Developer 1"),style = "font-size:30px"),
                      p("Name: Mun Siu Hou",style = "font-size:20px"),
                      p("Student ID: 17218313",style = "font-size:20px"),
                      p("Email: 17218313@siswa.um.edu.my",style = "font-size:20px"),
                      p(a("Developer 2"),style = "font-size:30px"),
                      p("Name: Nik Faiz Afiq Bin Nik Ab Rahman",style = "font-size:20px"),
                      p("Student ID: 17217542",style = "font-size:20px"),
                      p("Email: 17217542@siswa.um.edu.my",style = "font-size:20px"),
                      p(a("Developer 3"),style = "font-size:30px"),
                      p("Name: Arihant Jain",style = "font-size:20px"),
                      p("Student ID: 17219953",style = "font-size:20px"),
                      p("Email: 17219953@siswa.um.edu.my",style = "font-size:20px"),
                      p(a("Developer 4"),style = "font-size:30px"),
                      p("Name: Muhammad Ikhwan Nawari",style = "font-size:20px"),
                      p("Student ID: 17218915",style = "font-size:20px"),
                      p("Email: 17218915@siswa.um.edu.my",style = "font-size:20px")
             )
             
  )          
)
