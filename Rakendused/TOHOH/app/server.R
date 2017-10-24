library(dplyr)
library(ggplot2)
library(wordcloud)
library(scales)
library(reshape2)
library(plotly)
library(ggfortify)

cleandata <- read.csv("meediadata_cleaned2.csv", encoding = "UTF-8")
vectors <- read.csv("vecs_final2.csv", encoding = "UTF-8")
cleandata$postag_descriptions <-factor(cleandata$postag_descriptions, levels=c("nimisõna", "tegusõna", "pärisnimi", "lausemärk", "määrsõna", "omadussõna algvõrre", "asesõna", "sidesõna", "kaassõna", "põhiarvsõna", "lühend", "omadussõna keskvõrre", "järgarvsõna", "omadussõna ülivõrre", "käändumatu omadussõna", "verbi juurde kuuluv sõna", "hüüdsõna", ""))
# eemalda tühjad postagid
cleandata <- cleandata %>% subset(postag_descriptions!="")

colors <- c("#ffc820", "#0054a6", "#cf0007", "#00adee") # delfi, err, ohtuleht, postimees
allikad <- c("delfi", "err", "ohtuleht", "postimees")
names(colors) <- allikad
ilus_allikad <- c("Delfi", "ERR", "Õhtuleht", "Postimees")


sonade_jaotus = function(cleandata, sonaliik, colors, allikad, ilus_allikad){
  if (sonaliik=="koik"){
    subdata <- cleandata %>% select(postag_descriptions, allikas) %>%
      group_by(allikas, postag_descriptions) %>%
      summarise (n = n()) %>%
      mutate(freq = n / sum(n))
    data.m <- melt(subdata, id.vars=c('postag_descriptions', 'allikas'), measure.vars='freq')
    p <- ggplot(data.m, aes(x=postag_descriptions, y=value, fill=allikas)) + geom_bar(stat="identity", position = "dodge") + scale_y_continuous(labels=percent) +
      theme_bw() + scale_fill_manual(breaks = allikad, values=colors, labels=ilus_allikad) + labs(y="Osakaal") + theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1),                                                                                                                       axis.ticks.x=element_blank(), legend.title=element_blank(), legend.position="bottom")
  }
  else{
    subdata <- cleandata %>% select(postags, allikas) %>%
      group_by(allikas, postags) %>%
      summarise (n = n()) %>%
      mutate(freq = n / sum(n)) %>% subset(postags==sonaliik) 
    p <- ggplot(subdata, aes(x=allikas, y=freq, fill=allikas)) + geom_bar(stat="identity") + scale_y_continuous(labels=percent) +
      theme_bw() + scale_fill_manual(breaks = allikad, values=colors, labels=ilus_allikad) + labs(y="Osakaal") + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
                                                                                                                       axis.ticks.x=element_blank(), legend.title=element_blank(), legend.position="bottom")
  }
  return(p)                                                                                                               
}

plot_wordcloud <- function(cleandata, sonaliik){
  if (sonaliik=="koik"){
    subdata <- cleandata %>% select(postags, allikas, lemmas_clean) %>% group_by(lemmas_clean) %>% mutate(count = n()) %>% distinct(lemmas_clean, .keep_all = TRUE)
  }
  else{
    subdata <- cleandata %>% select(postags, allikas, lemmas_clean) %>% group_by(lemmas_clean) %>% mutate(count = n()) %>% subset(postags==sonaliik) %>% distinct(lemmas_clean, .keep_all = TRUE)
  }
  wc <- wordcloud(subdata$lemmas_clean, subdata$count, max.words = 200, min.freq = 1, random.order=TRUE, colors=brewer.pal(8,"Dark2"), random.color=FALSE)
  return(wc)
}

top_sonad = function(cleandata, sonaliik, source, colors){
  if (sonaliik=="koik"){
    subdata <- cleandata %>% select(postags, allikas, lemmas_clean) %>% subset(allikas==source) %>%
      group_by(lemmas_clean) %>% mutate(count = n()) %>% filter(row_number(lemmas_clean) == 1) %>% ungroup() %>% mutate(lemmas_clean=as.character(lemmas_clean))
  }
  else{
    subdata <- cleandata %>% select(postags, allikas, lemmas_clean) %>% subset(allikas==source & postags==sonaliik) %>%
      group_by(lemmas_clean) %>% mutate(count = n()) %>% filter(row_number(lemmas_clean) == 1) %>% ungroup() %>% mutate(lemmas_clean=as.character(lemmas_clean))
  }
  top10 <- top_n(arrange(subdata, desc(count)), 10, count)
  p <- ggplot(top10, aes(x=reorder(as.factor(lemmas_clean), count), y=count)) + geom_bar(stat="identity", fill=colors[[source]]) + theme_bw() + coord_flip() + xlab("Sõna") + ylab("Sagedus")
  return(p)
}

unikaalsed_sonad = function(cleandata, sonaliik, colors, source){
  if(sonaliik=="koik"){
    subdata <- cleandata %>% select(postags, allikas, lemmas_clean, word_texts) %>% group_by(allikas, lemmas_clean) %>%
      distinct(allikas, lemmas_clean, .keep_all = TRUE) %>% group_by(lemmas_clean) %>% mutate(count = n()) %>% filter(count==1)
  }
  else{
  subdata <- cleandata %>% select(postags, allikas, lemmas_clean, word_texts) %>% subset(postags==sonaliik) %>% group_by(allikas, lemmas_clean) %>%
    distinct(allikas, lemmas_clean, .keep_all = TRUE) %>%  group_by(lemmas_clean) %>% mutate(count = n()) %>% filter(count==1) 
  }
  if (sum(subdata$allikas==source)>0){
    wc <- wordcloud(subdata[subdata$allikas==source, ]$lemmas_clean, subdata[subdata$allikas==source,]$count, scale=c(1.1,.2), random.order=TRUE, rot.per=0.1, colors=colors[[source]],random.color=FALSE, max.words=60)
  }
  else{
    wc <- c()
  }
  return(wc)
}

plot_pca = function(data, colors, allikad, ilus_allikad){
  pca = prcomp(data[,c(2:101)], center = TRUE, scale. = TRUE) 
  pca.fortify <- fortify(pca)
  pca.dat <- cbind(pca.fortify, group=data$allikas, headline=data$cleaned_pealkiri)
  
  p <- ggplot(pca.dat) +
    geom_point(aes(x=PC1, y=PC2, fill=group, col=group, text=headline), position = position_jitter(w = 0.2, h = 0.2), size=1, alpha=0.4) + theme_bw() + scale_fill_manual(values = colors, breaks=names(colors)) + 
    theme(aspect.ratio=1, legend.title=element_blank()) +
    scale_color_manual(values = colors, breaks=names(colors))
  
  pl <- ggplotly(p, tooltip = c("text")) %>% layout(legend = list(x = 0, y = 100, orientation = 'h')) 
  pl$x$data[[1]]$name <- "Delfi"
  pl$x$data[[2]]$name <- "ERR"
  pl$x$data[[3]]$name <- "Õhtuleht"
  pl$x$data[[4]]$name <- "Postimees"
  return(pl)
}


shinyServer(function(input, output, session) {
  output$jaotus <- renderPlot({
    sonade_jaotus(cleandata, sonaliik=input$sonaliik, colors, allikad, ilus_allikad)
  })
  output$sonapilv <- renderPlot({
    plot_wordcloud(cleandata, sonaliik=input$sonaliik)
  })
  output$top_delfi <- renderPlot({top_sonad(cleandata, sonaliik = input$sonaliik, source="delfi", colors)})
  output$top_err <- renderPlot({top_sonad(cleandata, sonaliik = input$sonaliik, source="err", colors)})
  output$top_ohtuleht <- renderPlot({top_sonad(cleandata, sonaliik = input$sonaliik, source="ohtuleht", colors)})
  output$top_postimees <- renderPlot({top_sonad(cleandata, sonaliik = input$sonaliik, source="postimees", colors)})
  
  output$uni_delfi <- renderPlot({unikaalsed_sonad(cleandata, sonaliik=input$sonaliik, colors, source="delfi")})
  output$uni_err <- renderPlot({unikaalsed_sonad(cleandata, sonaliik=input$sonaliik, colors, source="err")})
  output$uni_ohtuleht <- renderPlot({unikaalsed_sonad(cleandata, sonaliik=input$sonaliik, colors, source="ohtuleht")})
  output$uni_postimees <- renderPlot({unikaalsed_sonad(cleandata, sonaliik=input$sonaliik, colors, source="postimees")})
  
  output$pca <- renderPlotly({plot_pca(vectors, colors, allikad, ilus_allikad)})
  
  })
