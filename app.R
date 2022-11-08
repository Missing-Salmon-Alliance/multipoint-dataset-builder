library(shiny)
library(leaflet)
library(DT)
library(sf)

ui <- fluidPage(
    
    column(
        width = 8,
        leaflet::leafletOutput('map')
    ),
    column(
        width = 4,
        textInput('name',"River Name"),
        textInput('au',"River Assessment Unit"),
        textInput('country',"River Country"),
        selectInput('status','Population Status',choices = c("Wild","Mixed","Reared")),
        shiny::actionButton('undo',"Undo last entry", class = 'btn-warning'),
        actionButton('save',"Save Entries", class = 'btn-success'),
        downloadButton('download',"Download Table", class = 'btn-primary')
    ),
    column(
        width = 8,
        DT::DTOutput('table')
    )
)


server <- function(input, output, session) {
    
    #data <- shiny::reactiveVal(data.frame(name = character(), au = character(), country = character(),status =  character(), lat = numeric(),lng = numeric(),geometry = character()))
    data <- shiny::reactiveVal(readr::read_csv('latest.csv'))
    # Geographic Detail Server
    output$map <- leaflet::renderLeaflet({
        leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 0,maxZoom = 19)) %>% # maxZoom set so that user can always see their rectangle in context of a coastline
            leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap, options = leaflet::providerTileOptions(minZoom = 0, maxZoom =10)) %>%
            leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, options = leaflet::providerTileOptions(minZoom = 11, maxZoom = 19)) %>%
            #leaflet::setView(0,55,8) %>%
            leaflet::addCircleMarkers(data = sf::st_as_sf(data(),wkt = 'geometry',crs = 4326),group = 'data')
    })
    
    output$table <- DT::renderDataTable({
        data()
    })
    
    # observe user click on map and set coordinates
    observeEvent(input$map_click,{
        
            data(
                rbind(
                    data(),
                    data.frame(
                        name = input$name,
                        au = input$au,
                        country = input$country,
                        status = input$status,
                        lat = input$map_click$lat,
                        lng = input$map_click$lng,
                        geometry = paste0("POINT(",input$map_click$lng," ",input$map_click$lat,")")
                    )
                )
            )

    })
    
    
    observeEvent(input$undo,{
        data(data()[-nrow(data()),])
    })
    
    observeEvent(input$save,{
        readr::write_csv(data(),"latest.csv")
    })
    
    output$download <- downloadHandler(
        filename = function() {
            paste('geo_table', Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
            readr::write_csv(data(),file)
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
