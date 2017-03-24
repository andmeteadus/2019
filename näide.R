
#------------------------------------------------------------------------#
#                         ÜLE KAARDI LENDAMINE                           #
#------------------------------------------------------------------------#

library(geosphere)
library(leaflet)

m = leaflet()

m = addTiles(m)

gc_points = gcIntermediate(c(-128, 50), 
                           c(145, -30), 
                           n=50, addStartEnd=TRUE)
class(gc_points)
gc = data.frame(gc_points)

m = m %>%
  addPolylines(lng = gc$lon, lat = gc$lat, weight = 2, opacity = 0.6, color = 'yellow') #'#7DF9FF'

m

gc_points = gcIntermediate(c(-128, 50), 
                           c(145, -30), 
                           n=100, addStartEnd=TRUE, breakAtDateLine = TRUE)

class(gc_points)
str(gc_points)

str(gc_points[1])
str(gc_points[[1]])

gc1 = data.frame(gc_points[[1]])
gc2 = data.frame(gc_points[[2]])

m = m %>%
  addPolylines(lng = gc1$lon, lat = gc1$lat, weight = 2, opacity = 0.6, color = 'red') #'#7DF9FF'

m = m %>%
  addPolylines(lng = gc2$lon, lat = gc2$lat, weight = 2, opacity = 0.6, color = 'red') #'#7DF9FF'

m







#------------------------------------------------------------------------#
#                               HEATMAPID                                #
#------------------------------------------------------------------------#
library(dplyr)
library(reshape2)

nba <- read.table("C:\\Users\\oliver17\\Desktop\\Business\\Visualiseerimine\\4. Neljas\\nba_2013.csv", header = T,sep = ";")

heat = nba %>% 
  filter(team_name == 'Miami Heat') %>%
  mutate(x_uus = round(x,-1), y_uus = round(y,-1)) %>% 
  group_by(x_uus, y_uus) %>% 
  summarise(arv=n(), protsent=sum(shot_made_flag)/arv*100, log_arv = log(arv))
    
heat_wide1 <- dcast(heat, y_uus ~ x_uus, value.var="arv")
heat_wide2 <- dcast(heat, y_uus ~ x_uus, value.var="log_arv")

heat_wide1[is.na(heat_wide1)] <- 0
heat_wide2[is.na(heat_wide2)] <- 0


mat1 = as.matrix(heat_wide1[,2:52])
mat2 = as.matrix(heat_wide2[,2:52])

custom_rownames = as.character(heat_wide1$y_uus)
rownames(mat1) = custom_rownames
rownames(mat2) = custom_rownames


#-----------------------------------------------------#
library(ggplot2)
ggplot(heat, aes(x = x_uus, y = y_uus, fill = arv)) + geom_raster()


#-----------------------------------------------------#
library(pheatmap)

pheatmap(mat1, cluster_rows = F, cluster_cols = F)
pheatmap(mat2, cluster_rows = F, cluster_cols = F)

pheatmap(mat2, cluster_rows = F, cluster_cols = T)
pheatmap(mat2, cluster_rows = T, cluster_cols = F)
pheatmap(mat2, cluster_rows = T, cluster_cols = T)



#-----------------------------------------------------#
library(gplots)
heatmap.2(mat1, Rowv = F, Colv = F)
heatmap.2(mat2, Rowv = F, Colv = F)





#-----------------------------------------------------#
library(d3heatmap)
d3heatmap(mat1, Rowv = F, Colv = F)
d3heatmap(mat2, Rowv = F, Colv = F)





#-----------------------------------------------------#
library(superheat)
library(readr)
oppetulemused <- read.csv2("C:\\Users\\oliver17\\Desktop\\Business\\Visualiseerimine\\3. Kolmas\\UT_13_14_hinded.csv", 
                    header=T, sep = ";", quote="")

sub_hinded = oppetulemused %>%
  filter(teaduskond == 'MT') %>%
  select(aine, A, B, C, D, E, F, mi)

ained = sub_hinded[1:15, ]
hinded = ained[ ,2:8]
rownames(hinded) = ained$aine

a = matrix('', nrow = 15, ncol = 7)
a[2,2] = 'Eno'
a[5,6] = 'Okouu'


superheat(hinded)

superheat(hinded,
          scale = T,
          pretty.order.rows = T,
          pretty.order.cols = F,
          yr = hinded$A,
          X.text = a,
          yt = cor(hinded)[ ,"A"],
          yt.plot.type = 'bar',
          left.label.size = 0.7
)


