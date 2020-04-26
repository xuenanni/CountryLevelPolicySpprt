# Title: Clustering of Countries by Policy Support - Visualizations
# Authors: Xuenan Ni and Joanna Moody
# Date last modified: April 21, 2020

library(dplyr)      # grouping
library(tidyr)      # data extraction and cleanup 
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(ggplot2)
library(ggdendro)   # for dendrogram using ggplot2
library(dendextend) # for comparing two dendrograms
library(fmsb)       # for radar plot

# Read-in data and format
data = read.csv('Data/country_comb.csv')

df = subset(data, select = c('Country',"A_est","B_est","C_est",'D_est','E_est','F_est','G_est','H_est','I_est','J_est','K_est'))                
rownames(df) = df$Country
df = df[,-c(1)]

# Dissimilarity matrix
d <- dist(df, method = "euclidean")

# Hierarchical clustering using ward.D2
hc1 <- hclust(d, method = "ward.D2" ) 

### DENDROGRAM
#http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning

#Option 1
ggdendrogram(hc1)

#Option 2
plot(hc1, hang = -1, cex = 0.5, ylab = "Cultural distance based on policy support", xlab = "Country")
rect.hclust(hc1, k = 6, border = 2:7)

#Option 3
par(mar = c(8,2,1,1)) #default margins are c(5.1, 4.1, 4.1, 2.1)
png("/Users/jcmoody/Desktop/dendrogram_viridis.png", width = 8, height = 5.5, units = 'in', res = 1000)

dend <- as.dendrogram(hc1)
plot(dend, ylab="Cultural distance")
#set colors of rectangles to be 6 values from the viridis package default palette (color-blind friendly)
rect.hclust(hc1, k = 6, border = c("#440154FF", "#404788FF", "#2D708EFF", "#20A387FF", "#73D055FF", "#DCE319FF"))

#Add a legend
legend(x="topright", legend = c('Cluster 1','Cluster 2','Cluster 3','Cluster 4','Cluster 5','Cluster 6'), 
       bty = "n", pch=20 , col=c("#440154FF", "#404788FF", "#2D708EFF", "#20A387FF", "#73D055FF", "#DCE319FF"),
       text.col = "black", cex=0.8, pt.cex=2)

dev.off()

#Alternative is to set colors of branches rather than rectangles
d1 <- color_branches(dend, k = 6, col = c("#440154FF", "#404788FF", "#2D708EFF", "#20A387FF", "#73D055FF", "#DCE319FF"))
plot(d1) 



### RADAR PLOT
data = read.csv('Data/country_RadarPlot_input.csv')
rownames(data) = c('Cluster 1','Cluster 2','Cluster 3','Cluster 4','Cluster 5','Cluster 6')
data = data[,-c(1)]
colnames(data) = c('More roads','More parking','More PT','BRT',
                   'Lower PT fares','Car-light CBD','Bike lane',
                   'Pedestrian facilities','Car-free ped. CBD', 'Clean cars','Clean PT')

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot
data <- rbind(rep(1,11) , rep(-1,11) , data)

#Color vector
alpha = 0.1
colors_border=c("#440154FF", "#404788FF", "#2D708EFF", "#20A387FF", "#73D055FF", "#DCE319FF")
colors_in=c(alpha("#440154FF",alpha), alpha("#404788FF",alpha), alpha("#2D708EFF",alpha), 
            alpha("#20A387FF",alpha), alpha("#73D055FF",alpha), alpha("#DCE319FF",alpha))

# plot with default options:
par(mar = c(1,1,1,1)) #default margins are c(5.1, 4.1, 4.1, 2.1)
png("/Users/jcmoody/Desktop/radar_viridis.png", width = 8, height = 6, units = 'in', res = 1000)

radarchart( data  , axistype=1, pty=32,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=2, plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=c("-1", "", "", "", "1"), cglwd=0.5,
            #custom labels
            vlcex=1.2
)

# Add a legend
legend(x="topright", legend = rownames(data[-c(1,2),]), 
       bty = "n", pch=20 , col=colors_border, text.col = "black", cex=0.8, pt.cex=2)

dev.off()
