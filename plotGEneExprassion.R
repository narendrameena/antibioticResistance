#@author narumeena
#@Script ploting the matrix. 


data <- read.csv("data/medianWithAllData.csv")


rownames(data) <- data$X



data$X <- NULL

data[is.na(data)] <- 0


m = data.matrix((data))
library(grid) 
grid.raster(m) 

mydata <- data[1:400,]

#install.packages("Cairo")
library(Cairo)
Cairo(file="something.png", type="png", units="in", width=10, height=7, pointsize=12, dpi=300)



mydata <- as.matrix(data)

mydata <- mydata[1:2000,]

sa <- stack(as.data.frame(mydata))
sa$x <- rep(seq_len(nrow(mydata)), ncol(mydata))

qplot(x, values, data = sa, group = ind, colour = ind, geom = "line")

qplot(x, values, data = sa,group = ind, colour = ind)

myImagePlot(data)

my_sc_plot <- function(data){
  par(cex.lab=11.5, cex.main=10)
  myImagePlot(data)
} 

my_sc_plot(data)
image(data)

library(reshape2)
library(ggplot2)
matplot(mydata, type='l')
heatmap(data,symm=FALSE,useRaster=TRUE)
png("heatmap1.png",width=90000,height=40000)
#heatmap()
dev.off()


library(heatmap3)

#install.packages("heatmap3")
#install.packages("ggplot2")
heatmap3(data,useRaster=TRUE)
#detach("package:ggplot2", unload=TRUE)


install.packages("factoextra")
# Install cluster package
install.packages("cluster")


library("cluster")
library("factoextra")
res.dist <- get_dist(data, stand = TRUE, method = "pearson")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
