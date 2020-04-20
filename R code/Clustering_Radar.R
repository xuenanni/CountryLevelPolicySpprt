library(readr)      #required for faster file reading command
library(stringr)    #for performing operations on data type string
library(plyr)       #namespace conflicts b/w plyr and dplyr
library(dplyr)      #grouping
library(scales)
library(lubridate)  #epoch to posix_ct conversion
library(tidyr)      #data extraction and cleanup 
library(doParallel) #multicore processing for the plyr::ddply step
library(RSQLite)    #DBI framework for db access 
library(XML)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
data =read.csv('/Users/xuenanni/Desktop/thesis/mplus_results/country_comb.csv')
df = subset(data, select = c('Country',"A_est","B_est","C_est",'D_est','E_est','F_est','G_est','H_est','I_est','J_est','K_est'))                
View(df)
rownames(df) = df$Country
df = df[,-c(1)]
# Dissimilarity matrix
d <- dist(df, method = "euclidean")
# Hierarchical clustering using ward.D2
hc1 <- hclust(d, method = "ward.D2" ) 
# Plot the obtained dendrogram 
plot(hc1, cex = 0.5)
rect.hclust(hc1, k = 6, border =2:7)

#Radar Plot
# Library
library(fmsb)
data =read.csv('/Users/xuenanni/Desktop/thesis/TRB/country_comp/country_RadarPlot_input.csv')
rownames(data) = c('Cluster 1','Cluster 2','Cluster 3','Cluster 4','Cluster 5','Cluster 6')
data = data[,-c(1)]
colnames(data) = c('Roads','More Parking','More PT','BRT','Lower PT Fares','Car-light CBD','Bike Lane','Pedestrian Facilities','Car-free Ped CBD', 'Clean Cars','Clean PT')
View(data)  
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(1,11) , rep(-1,11) , data)

data = data[c(1:2,6:7),]
# Color vector
alpha = 0.1
colors_border=c( 'red','green', 'blue','cadetblue1' ,'deeppink2','gold1')
colors_in=c( alpha('red',alpha), alpha('green',alpha), alpha('blue',alpha), alpha('cadetblue1',alpha), alpha('deeppink2',alpha), alpha('gold1' ,alpha))
png("/Users/xuenanni/Desktop/thesis/TRB/country_comp/radar_45.png", width = 16, height = 10, units = 'in', res = 800)
# plot with default options:
radarchart( data  , axistype=1, pty=32,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=2, plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=c("-1", "", "", "", "1"), cglwd=0.5,
            #custom labels
            vlcex=1.2
)

# Add a legend
legend(x=1, y=1.3, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_border, text.col = "black", cex=0.8, pt.cex=2)
dev.off()