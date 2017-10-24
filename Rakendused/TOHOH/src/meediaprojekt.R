# Pealkirjade kogumine
library(rvest)
library(readr)
library(stringr)

urls <- list(postimees="http://www.postimees.ee/", ohtuleht="http://www.ohtuleht.ee/", 
             delfi="http://www.delfi.ee/archive/viimased/?ch=all", err="http://www.err.ee/")

headlines <- list(postimees='span[itemprop="headline name"]', ohtuleht='div#main [class^="f"]', delfi='div#main h1.headline__title', err='div.header-und-lead')

load("~/Desktop/DOK/K_2017/Andmete_visualiseerimine/Projekt/meediadata.RData")

get.data <- function(urls, headlines, data){
  lehed = names(urls)
  for (leht in lehed){
    url = urls[[leht]]
    html = read_html(url, encoding="UTF-8")
    pealkirjad = html %>% html_nodes(headlines[[leht]]) %>% html_text()
    # puhasta pealkirjad
    pealkirjad = pealkirjad[pealkirjad != ""] 
    pealkirjad = unlist(lapply(pealkirjad, function(x) str_replace_all(x, "[\t\r\n»]" , "")))
    res = data.frame(pealkiri=pealkirjad, allikas=rep(leht, length(pealkirjad)), aeg=rep(Sys.time(), length(pealkirjad)))
    data[[leht]] = rbind(data[[leht]], res)
  }
  save(data, file="~/Desktop/DOK/K_2017/Andmete_visualiseerimine/Projekt/meediadata.RData")
  return(data)
}

data <- get.data(urls, headlines, data)
#save(data, file="~/Desktop/DOK/K_2017/Andmete_visualiseerimine/Projekt/meediadata.RData")
