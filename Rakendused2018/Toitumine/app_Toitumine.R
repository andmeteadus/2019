# this is the app for comparing individual's food-pattern with other food patterns
# this app will predict the cluster of the inserted dietary pattern (based on 17-items FFQ)
library(shiny)
library(clue)

#load k-means object to obtain centroids
load("k8_v2")

#define colors
nice.colors <- colorRampPalette(c("greenyellow", "hotpink", "indianred1", "olivedrab1", "olivedrab4",
                                  "maroon1", "orange1", "navy", "powderblue", "salmon1", "plum1", "steelblue1"))

toiduained <- c("kohv", "tee", "leib", "sai", "kartul", "riis.mak", "puder.mysli", "piimat", "kala", "liha", "vorst", "vjuur",
              "kjuur", "vpuuv", "maiust", "karastj", "muna")

names <- c("coffee", "tea", "rye.bread", "wh.bread", "potato", "rice/pasta", "cereals", "milkprod",
           "fish", "meat", "proc.meat", "fresh.veg", "c.veg", "fruits", "sweets", "softdr.", "egg")

ui <- fluidPage(
  titlePanel("Find out which dietary-pattern cluster You belong to in our study!"),
  
  
  fluidRow(
    column(2,
           selectInput(inputId="kohv", label="How many cups of coffee do You drink on average per day?",
                       c("Not at all"=1, "1-2 cups per day"=2, "3-5 cups per day"=3, 
                         "More than 5 cups per day"=4))
    ),
    column(2, 
           selectInput(inputId="tee", label="How many cups of tea do You drink on average per day?",
                       c("Not at all"=1, "1-2 cups per day"=2, "3-5 cups per day"=3, 
                         "More than 5 cups per day"=4))
    ),
    column(2,
           selectInput(inputId="leib", label="How many loafs of rye bread do you eat on average per day?",
                       c("Not at all"=1, "1-2 loafs per day"=2, "3-5 loafs per day"=3, 
                         "More than 5 cups per day"=4))
    ),
    column(2,
           selectInput(inputId="sai", label="How many loafs of whitebread do you eat on average per day?",
                       c("Not at all"=1, "1-2 loafs per day"=2, "3-5 loafs per day"=3, 
                         "More than 5 cups per day"=4))
    ),
    
    column(2, 
           selectInput(inputId="kartul", label="On how many days per week do you consume potatos?", 
                       c("Never"=1, "1-2 days"=2, "3-5 days"=3, "6-7 days"=4))
    ),
    column(2,
           selectInput(inputId="riis.mak", label="On how many days per week do you consume rice/pasta?", 
                       c("Never"=1, "1-2 days"=2, "3-5 days"=3, "6-7 days"=4))
    )
  ),
  
  fluidRow(
    
    column(2,
           selectInput(inputId="puder.mysli", label="On how many days per week do you consume cereals?", 
                       c("Never"=1, "1-2 days"=2, "3-5 days"=3, "6-7 days"=4))
    ),
    column(2, 
           selectInput(inputId="piimat", label="On how many days per week do you consume milk-products?", 
                       c("Never"=1, "1-2 days"=2, "3-5 days"=3, "6-7 days"=4))
    ),
    
    column(2,
           selectInput(inputId="kala", label="On how many days per week do you consume fish?", 
                       c("Never"=1, "1-2 days"=2, "3-5 days"=3, "6-7 days"=4))
    ),
    column(2,
           selectInput(inputId="liha", label="On how many days per week do you consume meat?", 
                       c("Never"=1, "1-2 days"=2, "3-5 days"=3, "6-7 days"=4))
    ),
    column(2, 
           selectInput(inputId="vorst", label="On how many days per week do you consume processed meat?", 
                       c("Never"=1, "1-2 days"=2, "3-5 days"=3, "6-7 days"=4))
    ),
    column(2,
           selectInput(inputId="vjuur", label="On how many days per week do you consume fresh vegetables?", 
                       c("Never"=1, "1-2 days"=2, "3-5 days"=3, "6-7 days"=4))
    )
  ),
  
  
  fluidRow(
    column(2,
           selectInput(inputId="kjuur", label="On how many days per week do you consume cooked vegetables?", 
                       c("Never"=1, "1-2 days"=2, "3-5 days"=3, "6-7 days"=4))
    ),
    column(2, 
           selectInput(inputId="vpuuv", label="On how many days per week do you consume fruits/berries?", 
                       c("Never"=1, "1-2 days"=2, "3-5 days"=3, "6-7 days"=4))
    ),
    column(2,
           selectInput(inputId="maiust", label="On how many days per week do you consume sweets?", 
                       c("Never"=1, "1-2 days"=2, "3-5 days"=3, "6-7 days"=4))
    ),
    column(2,
           selectInput(inputId="karastj", label="On how many days per week do you consume soft-drinks?", 
                       c("Never"=1, "1-2 days"=2, "3-5 days"=3, "6-7 days"=4))
    ),
    column(2, 
           selectInput(inputId="muna", label="On how many days per week do you consume eggs?", 
                       c("Never"=1, "1-2 days"=2, "3-5 days"=3, "6-7 days"=4))
    ),
    column(2,
           actionButton("addButton", "Show my dietary pattern:"),
           h4(textOutput("tulemus"))
    )
  ),
  
  
  fluidRow(
    
    column(4, 
           plotOutput("sinutoit")
    ),
    column(8, 
           plotOutput("toiduklastrid")
    )
  )
  
)

server <- function(input, output) {
  
  output$sinutoit <- renderPlot({
    if(input$addButton > 0){
      
      inputdat <- c(input$kohv, input$tee, input$leib, input$sai, input$kartul, input$riis.mak, input$puder.mysli,
                    input$piimat, input$kala, input$liha, input$vorst, input$vjuur, input$kjuur, input$vpuuv, 
                    input$maiust, input$karastj, input$muna)
      
      input <- matrix(as.integer(inputdat), nrow=1)
      
      stars((input-1)/3, draw.segments = T,
            col.segments = nice.colors(17), main="Your dietary pattern", scale=F)
      
    }  
  }) 
  
  
  output$toiduklastrid <- renderPlot({
    if(input$addButton > 0){
      clust_means <- k8$centers
      
      stars((clust_means[, 1:17]-1)/3, draw.segments = T,  labels = case.names(clust_means[, 1:17]), key.loc=c(10,2.4), 
            col.segments = nice.colors(17), main="Dietary patterns of 49276 Estonians (unscaled)", 
            scale=F, nrow=2, ncol=5, key.labels=names)
    } 
  }) 
  
  
  
  output$tulemus <- renderText({ 
    if(input$addButton > 0){
      inputdat <- c(input$kohv, input$tee, input$leib, input$sai, input$kartul, input$riis.mak, input$puder.mysli,
                    input$piimat, input$kala, input$liha, input$vorst, input$vjuur, input$kjuur, input$vpuuv, 
                    input$maiust, input$karastj, input$muna)
      
      input <- matrix(as.integer(inputdat), nrow=1)
      colnames(input) <- toiduained
      prediction <- cl_predict(k8, newdata=input)
      
      paste("Result: based on the inserted data, You belong in our study to cluster number:", prediction)
    }
  })
  
  
  
}

shinyApp(ui=ui, server=server)
