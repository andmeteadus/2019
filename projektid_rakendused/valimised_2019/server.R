library(shiny)
library(shinydashboard)
#library(dplyr)
library(tidyverse)
library(readxl)
library(envalysis)
library(reshape2)
library(ggsci)
#library(ggplot2)
library(htmltab)
#library(forcats)

options(scipen=99)
theme_set(theme_publish())

#ANDMETE SISSELUGEMINE ja ÜHENDAMINE

#reklaamikulu andmed
reklaam18 <- read.csv("reklaam18.csv", encoding="UTF-8", sep=";", row.names=1,stringsAsFactors=FALSE)[-c(4,6,10,12,14,15),]
reklaam18 %>% mutate(Erakond=row.names(reklaam18), 
                     Kvartal="2018. a IV kvartal") -> reklaam18
reklaam <- read.csv("reklaam.csv", encoding="UTF-8", row.names=1, sep=";", stringsAsFactors=FALSE)[-c(5,10,12,13),]
reklaam %>% mutate(Erakond=row.names(reklaam), 
                   Kvartal="2019. a I kvartal") -> reklaam

merge(reklaam,reklaam18,all=T) %>% arrange(Kvartal,Erakond) %>%
  mutate(Värv=rep(c("#007d5a","#8B4513","#FFDE00",
                "#800080","#489e65","#31758a", 
                "#8db751","#009FE3","#E10600"),2),
         Lühend=rep(c("KESK","EKRE","REF","Vabaerakond","Elurikkus","E200","Rohelised","Isamaa","SDE"),2),
         Kokku=gsub("\\s+","",Kokku)%>%as.numeric(),
         Telereklaam=gsub("\\s+","",Telereklaam)%>%as.numeric(),
         Raadioreklaam=gsub("\\s+","",Raadioreklaam)%>%as.numeric(),
         Internetireklaam=gsub("\\s+","",Internetireklaam)%>%as.numeric(),
         Valireklaam=gsub("\\s+","",Välireklaam)%>%as.numeric(),
         Ajakirjandusreklaam=gsub("\\s+","",Ajakirjandusreklaam)%>%as.numeric(),
         Reklaamtrykised=gsub("\\s+","",Reklaamtrükised)%>%as.numeric(),
         Erakond=ifelse(Erakond=="ISAMAA Erakond","Isamaa Erakond",Erakond)) %>% dplyr::rename(Reklaamikulud=Kokku) -> reklaamid

reklaam_koos = reklaamid %>% 
  group_by(Erakond) %>% 
  summarise(Reklaamikulud_koos = sum(Reklaamikulud))

erakonnad<-melt(data = reklaamid, Erakond.vars = "Erakond", 
                measure.vars = c("Telereklaam", "Raadioreklaam","Internetireklaam", 
                                 "Valireklaam", "Ajakirjandusreklaam","Reklaamtrykised")) %>% 
  dplyr::select("Lühend", "Kvartal", "variable", "value", "Värv", "Erakond") %>%
  dplyr::rename(Reklaamikulu=value, Reklaam=variable)

#valimiste andmed
suppressMessages(rk2019.full <- read_excel("rk2019-full.xlsx",  col_names = FALSE) %>% .[,c(1,3:4)] %>%
  dplyr::rename(Reg.nr=X__1,Kandidaat=X__3,Erakond=X__4))

rk2019.tulemused <- read.csv("rk2019-tulemused.csv",encoding="UTF-8", stringsAsFactors=FALSE)

haaled_eraldi = merge(rk2019.full,rk2019.tulemused, by=c("Reg.nr","Kandidaat")) %>% 
  filter(!Erakond%in%c("Üksikkandidaadid","Eestimaa Ühendatud Vasakpartei")) %>%
  mutate(Kandidaat=as.character(Kandidaat))

url = "https://rk2019.valimised.ee/et/election-result/election-result.html"
suppressMessages(haaled19 <- htmltab(doc = url, which=1)[-c(6,11,12),1:2])
colnames(haaled19)=c("Erakond","Hääli")
haaled19 = haaled19 %>% mutate(Aasta=2019,Hääli=as.numeric(Hääli),
                               Protsent=round(Hääli/561141,2),
                               Erakond=as.character(Erakond),
                               Värv=c("#FFDE00","#007d5a","#8B4513","#009FE3",
                                      "#E10600","#31758a","#8db751",
                                      "#489e65","#800080"),
                               Lühend=c("REF","KESK","EKRE","Isamaa","SDE","E200",
                                        "Rohelised","Elurikkus","Vabaerakond"))
haaled19_reklaam = merge(reklaam_koos,haaled19) %>% 
  mutate(Ühe_hääle_hind=round(Reklaamikulud_koos/Hääli,2))

#SERVER
server <- function(input, output) {
  output$joonis <- renderPlot({
    haaled_eraldi %>% 
      filter(Erakond==input$erakond) %>%
      top_n(input$n,Kokku) %>%
      ggplot(aes(y=Kokku,x=fct_reorder(Kandidaat,Kokku),label=Kokku)) +
      xlab("")+
      theme(axis.text.y = element_text(size=15))+
      ylab("Hääli") +
      geom_col(fill=(haaled19%>%filter(Erakond==input$erakond))$Värv) + 
      geom_label(size = 6, hjust=1.1, fill="white") +
      coord_flip()
  }, height=650)
  
  output$joonis_vahelduv <- renderPlot({
    if (input$tyyp == "yldine") {
      reklaamid %>%
        ggplot(aes(y=Reklaamikulud, x=Lühend, fill=reklaamid$Erakond)) + 
        geom_col()+
        facet_wrap(~Kvartal)+
        scale_fill_manual(values=reklaamid$Värv)+
        theme(axis.title.y = element_blank(),
              legend.position = "none", 
              legend.title = element_blank()) + coord_flip()
    }
    else {
      reklaamid %>%
        melt(measure.vars=c("Telereklaam","Raadioreklaam","Internetireklaam","Välireklaam",
                            "Ajakirjandusreklaam","Reklaamtrükised"), value.name="Reklaamikulu") %>%
        ggplot(aes(y=Reklaamikulu, x=Lühend, fill=variable)) + 
        geom_col()+
        facet_wrap(~Kvartal)+
        scale_fill_uchicago()+
        theme(axis.title.y = element_blank(),
              legend.title = element_blank(),
              legend.justification = c(1,1),
              legend.position = c(0.5,1),
              legend.text = element_text(size=15)) + 
        coord_flip()
    }
  })
  
  output$joonis3 <- renderPlot({
    data1 %>% 
      ggplot(aes(y=Reklaamikulud, x=Erakond, fill=Erakond)) + 
      geom_histogram(stat="identity")+
      scale_fill_manual(values=data1$Värv, labels=data1$Lühend) + 
      theme(axis.text.x=element_blank(), 
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "bottom", legend.title = element_blank(), legend.spacing.x = unit(0.1, 'cm')) +
      guides(fill = guide_legend(nrow = 1))
  })
  
  output$reklaam_protsent <- renderPlot({
    haaled19_reklaam %>% 
      ggplot(aes(x=Reklaamikulud_koos,y=Protsent, color=Erakond)) +
      scale_color_manual(values=haaled19_reklaam$Värv, labels=haaled19_reklaam$Lühend) +
      xlab("Kogu reklaamikulu")+
      ylab("Valimistulemus") + 
      labs(size="Ühe hääle hind") +
      geom_point(aes(size=Ühe_hääle_hind))+ scale_size(range = c(2,12))+
      scale_y_continuous(labels = scales::percent)
  })
  
  output$plot5 <- renderPlot({
    haaled19_reklaam %>% 
      ggplot(aes(x=fct_reorder(Lühend,Hääli), y=Hääli, fill=Erakond)) + 
      geom_bar(stat="identity") + 
      coord_flip() + 
      scale_fill_manual(values=haaled19_reklaam$Värv) + 
      theme(legend.position = "none", axis.title.y=element_blank()) + 
      geom_label(aes(label=Hääli), fill="white", size=6, hjust=-0.1) +
      ylim(0,200000)
  }, height=450)
  
  output$plot3 <- renderPlot({
    erakonnad %>%
      filter(Reklaam==input$kulutus, Kvartal==input$kvartal2) %>%
      ggplot(aes(x=Lühend, y=Reklaamikulu, fill=Erakond)) +
      geom_bar(stat="identity") +  
      scale_fill_manual(values=arrange(filter(erakonnad, Reklaam==input$kulutus, Kvartal==input$kvartal2), Erakond)%>% .[,"Värv"]) + 
      coord_flip() +
      theme(legend.position = "none", axis.title.y=element_blank())
  })
  
  output$plot1 <- renderPlot({
    erakonnad %>%
      filter(Erakond==input$erakond2 , Kvartal==input$kvartal) %>%
      ggplot(aes(x=Reklaam, y=Reklaamikulu, fill=Lühend)) +
      geom_bar(stat="identity") +  
      scale_fill_manual(values=filter(erakonnad, Erakond==input$erakond2, Kvartal==input$kvartal)%>% .[,"Värv"]) + 
      coord_flip() + theme(legend.position = "none", axis.title.y=element_blank())+ 
      ylim(0, 500000)
  })
  output$plot2 <- renderPlot({
    erakonnad %>%
      filter(Erakond==input$erakond2 , Kvartal==input$kvartal) %>%
      ggplot(aes(x=Reklaam, y=Reklaamikulu, fill=Lühend)) +
      geom_bar(stat="identity") +  
      scale_fill_manual(values=filter(erakonnad, Erakond==input$erakond2, Kvartal==input$kvartal)%>% .[,"Värv"]) +
      coord_flip() + theme(legend.position = "none", axis.title.y=element_blank())
  })
  
  output$plot6 <- renderPlot({
    ggplot(haaled19_reklaam, aes(x=Lühend, y=Reklaamikulud_koos, fill=Erakond)) +  
      geom_bar(stat="identity") + scale_fill_manual(values = arrange(haaled19_reklaam, Erakond)$Värv) + 
      theme(axis.text.x = element_text(angle = 40, hjust = 1)) + 
      theme(legend.position = "none", axis.title.x=element_blank())  + ylab("Reklaamikulud")
  }, height=450)
  output$plot7 <- renderPlot({
    ggplot(haaled19_reklaam, aes(x=Lühend, y=Protsent, fill=Erakond)) +
      geom_bar(stat="identity") + scale_fill_manual(values = arrange(haaled19_reklaam, Erakond)$Värv) + 
      theme(axis.text.x = element_text(angle = 40, hjust = 1)) + 
      theme(legend.position = "none", axis.title.x=element_blank()) +
      ylab("Valimistulemus")+
      scale_y_continuous(labels = scales::percent)
  }, height=450)
}

# shinyApp(ui, server)
