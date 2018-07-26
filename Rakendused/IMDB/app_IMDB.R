# Autorid:
# Andreas Peeter Lätt
# Lisette Pajula

library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(shinyjs)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
load("tabel2.RData")
load("DF.RData")

lisettejoonis <- function(andmed,film,zanr= ""){
  andmed = mutate(andmed,varv="Muud filmid")
  v = andmed$averageRating[andmed$primaryTitle==film][1]
  if(v-floor(v)<=0.5){
    v_low=floor(v)
    v_up= floor(v)+0.5
  }
  else{
    v_low = floor(v)+0.5
    v_up = ceiling(v)
  }
  andmed$varv[andmed$averageRating>(v_low)& andmed$averageRating<=(v_up)] = "Valitud film" 
  andmed$varv = reorder(andmed$varv,(andmed$varv=="Muud filmid"))
  mean_rating = mean(andmed$averageRating,na.rm=TRUE)
  colors=c("darkgrey","lightgrey")
  if (zanr!="") zanr = paste("Zanr:", zanr,sep=" ")
  ggplot(andmed) + stat_bin(aes(x = averageRating,fill=varv),geom="bar",binwidth = 0.5,color="white",
                            breaks = seq(1,10,0.5)) + 
    geom_vline( aes(xintercept =mean_rating ,color="red"),size=1,linetype = "dashed") + theme_classic() + scale_fill_manual(values=colors,name=" ")+
    labs(x = "keskmine hinnang",y= "sagedus",title=paste(film, "; Hinnang:",v,"\n",zanr,sep= " "))+
    guides(color=guide_legend(title = "keskmine",label = F,title.position = "right"))+
    theme(plot.title = element_text(face="bold", size=14)) 
}

myPalette <- colorRampPalette(c("firebrick3", "gainsboro", "forestgreen"))
ratingplot = ggplot(DF, aes(x = startYear,y=averageRating )) + geom_point(aes(color=averageRating))+xlim(1915,2018)+ylim(0,10) + scale_colour_gradientn(colours = myPalette(100),name="hinnang") +coord_polar()+
  theme(axis.line=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position = "right",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

css <- "
.shiny-output-error { visibility: hidden; }
.shiny-output-error:before {
visibility: visible;
content: ''; }
}
"
# Define UI for application that draws a histogram
ui <- dashboardPage(skin="red",
                    dashboardHeader(title="IMDB app"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Filmid", tabName = "valik1",icon=icon("film")),
                        menuItem("Varia", tabName = "valik2",icon=icon("camera-retro"))
                      )),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "valik1",
                                tags$style(type="text/css", css),
                                  # Application title
                                  
                                  shinyjs::useShinyjs(), 
                                  sidebarLayout(sidebarPanel(
                                    selectInput("Film","Vali film", choices = sort(unique(DF$primaryTitle))),
                                    radioButtons("radio", label = "Mis filmidega v?rrelda soovid?",
                                                 choices = list("K?ik filmid"=1,"Sama aasta filmid" = 2, "Sama ?anri filmid" = 3), 
                                                 selected = 1),
                                    selectInput("zanrivalik","Vali ?anr", choices = "")
                                  ),
                                  mainPanel(
                                    conditionalPanel(condition="input.radio == 1", box(status="danger",plotOutput("koikfilmid_joonis"),width = "70%",solidHeader = T,title="V?rdlus k?ikide filmidega")),
                                    conditionalPanel(condition="input.radio == 2", box(status="danger",plotOutput("aastafilmid_joonis"),width = "70%",solidHeader = T,title="V?rdlus aasta l?ikes")),
                                    conditionalPanel(condition="input.radio == 3",  box(status="danger",plotOutput("zanri_joonis"),width = "70%",solidHeader = T,title="V?rdlus zanri l?ikes") )
                                    
                                  )
                                ))
                        ,
                        tabItem(tabName="valik2",
                                fluidRow((
                                  box(status="danger", plotOutput("sonapilv"),width = "6",height="500",solidHeader = T,title="Sagedaseimad s?nad pealkirjades")
                                ),
                                box(status="danger",plotOutput("rating"),br(),width="6",solidHeader = T, height="500",title="Keskmise hinnangu jaotus aastate l?ikes")
                                )
                        )
                      )
                    )
)


funkts = function(sisend = NULL){
  if(sisend == ""){
    FALSE
  }
}


server <- function(input, output, session) {
  output$koikfilmid_joonis <- renderPlot({
    lisettejoonis(DF,input$Film)
  })
  output$sonapilv <- renderPlot({
    wordcloud(words = tabel2$x, freq = tabel2$freq, min.freq = 1,
              max.words=100, random.order=FALSE, rot.per=0.10 ,
              colors=brewer.pal(8, "Dark2"))
  })
  output$rating = renderPlot({
    ratingplot
  })
  
  observe({
    output$aastafilmid_joonis <- renderPlot({
      filmiandmed = DF[DF$primaryTitle == input$Film,]
      aasta = filmiandmed$startYear[1]
      
      DF_aasta = DF[DF$startYear == aasta,]
      
      lisettejoonis(DF_aasta,input$Film)
    })
  })
  observeEvent(input$radio ,{if(input$radio != "3") shinyjs::disable("zanrivalik")
    else shinyjs::enable("zanrivalik")})
  observe({
    
    if(input$Film!="") # kui midagi on valitud...
      filmiandmed = DF[DF$primaryTitle == input$Film,]
    dt = data.table(filmiandmed)
    zanrid_filmis=character()
    for (i in colnames(dt)){
      if(dt[,get(i)]==1){
        zanrid_filmis = c(zanrid_filmis,i)
      }
    }
    shiny::validate(funkts(input$Film))
    
    updateSelectInput(session, "zanrivalik",
                      label = "Vali ?anr",
                      choices = zanrid_filmis)
  })
  ## tahame subsetti filmide algandmestikust, kus on ainult valitud zanri filmid
  observe({
    valitud = input$zanrivalik
    dt = data.table(DF)
    if(valitud!="") {DF_zanr = dt[get(valitud)==1,]
    output$zanri_joonis = renderPlot({
      lisettejoonis(DF_zanr,input$Film,zanr = valitud)})}
  })
}





# Run the application 
shinyApp(ui = ui, server = server)
