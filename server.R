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


shinyServer(function(input, output, session) {
  
  output$total_case_count <- renderText({
    paste0(prettyNum(sum(df$confirmed_cum), big.mark=","), " cases")
  })
  
  output$total_death_count <- renderText({
    paste0(prettyNum(sum(df$death_cum), big.mark=","), " deaths")
  })
  
  output$total_recovered_count <- renderText({
    paste0(prettyNum(sum(df$recovered_cum), big.mark=","), " recovered")
  })
  
  output$total_active_count <- renderText({
    paste0(prettyNum(sum(df$active_cum), big.mark=","), " active cases")
  })
  
  output$latest_clean_date <- renderText({
    format(as.POSIXct(df$date[1]),"%d %B %Y")
  })
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      
      addPolygons(data = reactive_polygons, 
                  stroke = T , weight = .5, opacity = 0.5, color = "black", dashArray = "3",
                  smoothFactor = 0.1, fillOpacity = 0.2, fillColor = ~ cv_pal(df$confirmed_cum), 
                  label = sprintf("<strong>%s </strong><br/>Confirmed cases: %d<br/>Deaths: %d<br/>Recovered: %d<br/>Active Cases : %d", 
                                  df$country, df$confirmed_cum, df$death_cum, df$recovered_cum, df$active_cum) %>% 
                    lapply(htmltools::HTML)
      )%>%
      addCircleMarkers(data = df, lat = ~latitude, lng = ~longitude, radius = ~(confirmed_cum/50000),
                       stroke = T , weight = 1, opacity = 0.5,  dashArray = "1",
                       fillOpacity = 0.3, color = cummulated_col, group = "Total Cases",
                       label = sprintf("<strong>%s </strong><br/>Confirmed cases: %d", 
                                       df$country, df$confirmed_cum) %>% 
                         lapply(htmltools::HTML))%>%
      addCircleMarkers(data = df, lat = ~latitude, lng = ~longitude, radius = ~(confirmed/500),
                       stroke = T , weight = 1, opacity = 0.5,  dashArray = "1",
                       fillOpacity = 0.3, color = new_col, group = "New Cases (24h)",
                       label = sprintf("<strong>%s </strong><br/>Confirmed cases: %d", 
                                       df$country, df$confirmed) %>% 
                         lapply(htmltools::HTML))%>%
      addCircleMarkers(data = df, lat = ~latitude, lng = ~longitude, radius = ~(recovered_cum/10000),
                       stroke = T , weight = 1, opacity = 0.5,  dashArray = "1",
                       fillOpacity = 0.3, color = recovered_col, group = "Total Recovered",
                       label = sprintf("<strong>%s </strong><br/>Recovered: %d", 
                                       df$country, df$recovered_cum) %>% 
                         lapply(htmltools::HTML))%>%
      addCircleMarkers(data = df, lat = ~latitude, lng = ~longitude, radius = ~(death_cum/2500),
                       stroke = T , weight = 1, opacity = 0.5,  dashArray = "1",
                       fillOpacity = 0.3, color = death_col, group = "Total Death",
                       label = sprintf("<strong>%s </strong><br/>Deaths: %d", 
                                       df$country,  df$death_cum) %>% 
                         lapply(htmltools::HTML))%>%
      addCircleMarkers(data = df, lat = ~latitude, lng = ~longitude, radius = ~(active_cum/25000),
                       stroke = T , weight = 1, opacity = 0.5,  dashArray = "1",
                       fillOpacity = 0.3, color = active_col, group = "Total Active",
                       label = sprintf("<strong>%s </strong><br/>Active Cases : %d", 
                                       df$country,  df$active_cum) %>% 
                         lapply(htmltools::HTML))%>%
      
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Total Cases", "New Cases (24h)", "Total Recovered", "Total Death", "Total Active"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c( "New Cases (24h)", "Total Recovered", "Total Death", "Total Active"))  %>%
      fitBounds(-100,-50,80,80) %>%
      addLegend("bottomright", pal = cv_pal, values = df$confirmed_cum,
                title = "Confirmed Cases")
    
  })
  
  tryxia <- eventReactive(input$do,{
    newline <- isolate(c(input$Cough,input$Fever,input$Sore_Throat,
                         input$Shortness_of_breath,input$Headache,input$Age,input$Gender,
                         input$Abroad,input$Contact))
    if(input$Abroad==1&&input$Contact==1){
      act=2
    }else{
      act=1
    }
    newline[10] <- act
    coronanewdata <- data.frame("cough"=newline[1],"fever"=newline[2],"sore_throat"=newline[3],
                                "shortness_of_breath"=newline[4],"head_ache"=newline[5],
                                "age_60_and_above"=newline[6],"gender"=newline[7],"Abroad"=newline[8],
                                "Contact"=newline[9],"No_activite"=newline[10])
    coronanewdata$cough <- factor(coronanewdata$cough, levels = c(1,2), labels = c("0","1"))
    coronanewdata$fever <- factor(coronanewdata$fever, levels = c(1,2), labels = c("0","1"))
    coronanewdata$sore_throat <- factor(coronanewdata$sore_throat, levels = c(1,2), labels = c("0","1"))
    coronanewdata$shortness_of_breath <- factor(coronanewdata$shortness_of_breath, levels = c(1,2), labels = c("0","1"))
    coronanewdata$head_ache <- factor(coronanewdata$head_ache, levels = c(1,2), labels = c("0","1"))
    coronanewdata$age_60_and_above <- factor(coronanewdata$age_60_and_above, levels = c(1,2), labels = c("0","1"))
    coronanewdata$gender <- factor(coronanewdata$gender, levels = c(1,2), labels = c("0","1"))
    coronanewdata$Abroad <- factor(coronanewdata$Abroad, levels = c(1,2), labels = c("0","1"))
    coronanewdata$Contact <- factor(coronanewdata$Contact, levels = c(1,2), labels = c("0","1"))
    coronanewdata$No_activite <- factor(coronanewdata$No_activite, levels = c(1,2), labels = c("0","1"))
    prob <- predict(model,coronanewdata,type="raw")
    pro_T <- as.table(prob)
    percen = pro_T[1,2]*100
    if(pro_T[1,2] < 0.6){
      cat("Well, you are not suspend to get infect. Please go for check up if you are not feeling well and having similar symptoms.")
    }else{
      cat("You have",percen,"% of getting infected, please go hospital check up immediately!!")
    }
  })
  output$corona_result <- renderPrint(tryxia())
  
})
