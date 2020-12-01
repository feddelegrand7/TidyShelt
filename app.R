library(shiny)
library(echarts4r)
library(scrollrevealR)
library(dplyr)



source("data.R")


ui <- fluidPage(theme = "styles.css",
                
                
# Let's get some fonts from Google 

tags$link(rel = "preconnect", 
          href = "https://fonts.gstatic.com"), 

tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Sansita+Swashed:wght@300&display=swap"), 




h1("TidyTuesday Week 49: Toronto Shelters", class = "main-title"),


# Let's create a class for the banner image 
# ... it should take the full width of the page

div(class = "banner-image"), 


h3("The following graph exhibits the shelters'overall occupancy evolution", 
   style = "padding-left:40px; padding-bottom:20px"), 


echarts4rOutput(outputId = "plt1"),

h3("Here a an overview of the same variable in 2019 only", 
   style = "padding-left:40px; padding-bottom:20px"), 



echarts4rOutput(outputId = "plt2"),

h3("During the same year, the shelter with the highest occupancy level was the COSTI Reception Center", 
   style = "padding-left:40px; padding-bottom:20px"), 


numericInput(inputId = "num1", 
             label = "", value = 2019, 
             min = 2017, max = 2019, step = 1),


echarts4rOutput(outputId = "plt3"),


h3("All the graphs were generated using the awesome", 
   a(href="https://echarts4r.john-coene.com/index.html", "echarts4r"), 
   style = "padding-left:40px; padding-bottom:20px"), 

div(class = "logo-echarts4r", 
    
    
    img(src = "logo.png")
    
    
    
    ), 


scroll_reveal(target = c("#plt1", "#plt2", "#plt3"), duration = 1400, distance = "50px"),

                

    
    
    
   
)

server <- function(input, output) {
    
output$plt1 <- renderEcharts4r(
    
    
    data %>% group_by(occupancy_date) %>% 
        summarise(`Total Occupancy` = sum(occupancy)) %>% 
        arrange(`occupancy_date`) %>% 
        e_charts(occupancy_date) %>% 
        e_line(`Total Occupancy`) %>% 
        e_tooltip("axis") 
    
    
    
)


output$plt2 <- renderEcharts4r(
    data %>% group_by(occupancy_date) %>% 
        summarise(`Total Occupancy` = sum(occupancy)) %>% 
        filter(occupancy_date >= "2019-01-01" & occupancy_date <= "2019-12-31") %>% 
        e_charts(occupancy_date) %>% 
        e_calendar(range = "2019") %>% 
        e_heatmap(y = `Total Occupancy`, coord_system = "calendar") %>% 
        e_visual_map(type = "continuous", max = 7114, min = 6544)
    

)

output$plt3 <- renderEcharts4r(
    
    
    if(input$num1 == "2019") {
        
        
        data %>% 
            filter(occupancy_date >= "2019-01-01" & occupancy_date <= "2019-12-31") %>% 
            group_by(shelter_name) %>% 
            summarise(`Total Occupancy in 2019` = sum(occupancy)) %>% 
            arrange(desc(`Total Occupancy in 2019`)) %>% 
            top_n(10) %>% 
            e_charts(shelter_name) %>% 
            e_bar(`Total Occupancy in 2019`) %>% 
            e_tooltip("axis")
        
        
        
        
    
    } else if (input$num1 == "2018") {
        
        data %>% 
            filter(occupancy_date >= "2018-01-01" & occupancy_date <= "2018-12-31") %>% 
            group_by(shelter_name) %>% 
            summarise(`Total Occupancy in 2018` = sum(occupancy)) %>% 
            arrange(desc(`Total Occupancy in 2018`)) %>% 
            top_n(10) %>% 
            e_charts(shelter_name) %>% 
            e_bar(`Total Occupancy in 2018`) %>% 
            e_tooltip("axis")
        
        
        
    
    } else {
        
        
        data %>% 
            filter(occupancy_date >= "2017-01-01" & occupancy_date <= "2017-12-31") %>% 
            group_by(shelter_name) %>% 
            summarise(`Total Occupancy in 2017` = sum(occupancy)) %>% 
            arrange(desc(`Total Occupancy in 2017`)) %>% 
            top_n(10) %>% 
            e_charts(shelter_name) %>% 
            e_bar(`Total Occupancy in 2017`) %>% 
            e_tooltip("axis")
        
        
        
    }
  
    
    
    
    
    
)
    
    
    
}

shinyApp(ui = ui, server = server)
