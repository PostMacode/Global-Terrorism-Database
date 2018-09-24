#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(RJDBC)
library(rsconnect)


loadData <- function(year, at, tt, wt, success, suicide, multiple, doubtterr) {
    drv <- JDBC("com.mysql.jdbc.Driver",
                "mysql-connector-java-5.1.47-bin.jar",
                identifier.quote = "`")
    
    db <- dbConnect(drv, "jdbc:mysql://mydbinstance1.cirpeey5boxt.us-east-1.rds.amazonaws.com:3306/terrorismDB1?&autoReconnect=true&useSSL=false&maxReconnects=10", "masteruser", "masterpass")
    
    query <- sprintf("select latitude, longitude
                   from event
                   join location using (location_id)
                   join attacktype using (attacktype_id)
                   join weapontype using (weapontype_id)
                   join outcome using (event_id)
                   where 
                      event.year = %s and 
                      attacktype.attacktype_txt like %s and 
                      event.targettype_txt like %s and
                      weapontype.weapontype_txt like %s and
                      outcome.success = %s and
                      outcome.suicide = %s and
                      outcome.multiple = %s and
                      event.doubtterr = %s
                   limit 50;", year, at, tt, wt, as.integer(success), as.integer(suicide), as.integer(multiple), as.integer(doubtterr))
    data <- dbGetQuery(db, query)
    dbDisconnect(db)
    data
}

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "year", label = "Choose a year to display:", min = 1970, max = 2015, value = 1999, sep=""),
            selectInput(
                inputId = "at", label = "Choose a type of attack to display:", choices = c("'Assassination'",
                                                                                           "'Armed Assault'",
                                                                                           "'Bombing/Explosion'",
                                                                                           "'Hijacking'",
                                                                                           "'Hostage Taking (Barricade Incident)'",
                                                                                           "'Hostage Taking (Kidnapping)'",
                                                                                           "'Facility/Infrastruct Attack'",
                                                                                           "'Unarmed Assault'",
                                                                                           "'Unknown'"), selected="'Assassination'"),
            selectInput(
                inputId = "tt", label = "Choose a type of target to display:", choices = c("'Business'",
                                                                                           "'Police'",
                                                                                           "'Military'",
                                                                                           "'Abortion Related'",
                                                                                           "'Airports & Aircraft'",
                                                                                           "'Government (Diplomatic)'",
                                                                                           "'Government (General)'",
                                                                                           "'Educational Institution'",
                                                                                           "'Food or Water Supply'",
                                                                                           "'Journalists & Media'",
                                                                                           "'Maritime'",
                                                                                           "'NGO'",
                                                                                           "'Other'",
                                                                                           "'Private Citizens & Property'",
                                                                                           "'Religious Figures/Institutions'",
                                                                                           "'Telecommunication'",
                                                                                           "'Terrorists/Non-State Militia'",
                                                                                           "'Tourists'",
                                                                                           "'Transportation'",
                                                                                           "'Unknown'",
                                                                                           "'Utilities'",
                                                                                           "'Violent Political Party'"), selected="'Government (General)'"),
            selectInput(
                inputId = "wt", label = "Choose a type of weapon to display:", choices = c("'Biological'",
                                                                                           "'Chemical'",
                                                                                           "'Radiological'",
                                                                                           "'Firearms'",
                                                                                           "'Explosives/Bombs/Dynamite'",
                                                                                           "'Fake Weapons'",
                                                                                           "'Incendiary'",
                                                                                           "'Melee'",
                                                                                           "'Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)'",
                                                                                           "'Sabotage Equipment'",
                                                                                           "'Other'",
                                                                                           "'Unknown'"), selected="'Firearms'"),
            checkboxInput("success", "Successful Attack", TRUE),
            checkboxInput("suicide", "Suicide Attack", FALSE),
            checkboxInput("multiple", "Related to Other Incidents", FALSE),
            checkboxInput("doubtterr", "Terrorist Intent Confirmed", FALSE)),
        mainPanel(leafletOutput(outputId = "map"))))


server <- function(input, output, session) {
    points <- eventReactive(loadData(input$year,
                                     input$at,
                                     input$tt,
                                     input$wt,
                                     input$success,
                                     input$suicide,
                                     input$multiple,
                                     input$doubtterr), {
                                         cbind(loadData(input$year,
                                                        input$at,
                                                        input$tt,
                                                        input$wt,
                                                        input$success,
                                                        input$suicide,
                                                        input$multiple,
                                                        input$doubtterr)$longitude, loadData(input$year,
                                                                                             input$at,
                                                                                             input$tt,
                                                                                             input$wt,
                                                                                             input$success,
                                                                                             input$suicide,
                                                                                             input$multiple,
                                                                                             input$doubtterr)$latitude)
                                     }, ignoreNULL = TRUE)
    
    output$map <- renderLeaflet({
        leaflet() %>% addProviderTiles(providers$Stamen.TonerLite,
                                       options = providerTileOptions(noWrap = TRUE)) %>% addMarkers(data = points())
    })
}

shinyApp(ui, server)

