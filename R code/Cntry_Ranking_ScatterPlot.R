#Title: "Country Transportation Policy Support - Intercept Ranking/Visualization"
#Authors: Xuenan Ni and Joanna Moody
#Date last modified: April 21, 2020

library(ggplot2)
library(dplyr) #for filter function
library(viridis) #for color-blind friendly palette
library(maps)
library(mapproj)
library(grid)
library(gridExtra)
library(gtable)
library(reshape2)
library(ggrepel)
library(car) #for recode function

multi_ipf_compare = read.csv('Data/country_comb.csv')
sorted_multi = multi_ipf_compare[order(multi_ipf_compare$A_est, multi_ipf_compare$J_est),]

A_est = sorted_multi$A_est
B_est = sorted_multi$B_est
C_est = sorted_multi$C_est
D_est = sorted_multi$D_est
E_est = sorted_multi$E_est
F_est = sorted_multi$F_est
G_est = sorted_multi$G_est
H_est = sorted_multi$H_est
I_est = sorted_multi$I_est
J_est = sorted_multi$J_est
K_est = sorted_multi$K_est

##World Maps
#https://stackoverflow.com/questions/30706124/ploting-the-world-map-in-r
WorldData <- map_data('world')
WorldData <- WorldData %>% filter(region != "Antarctica")
WorldData <- fortify(WorldData)

sorted_multi$Country <- car::recode(sorted_multi$Country, "'Russian Federation'='Russia'; 'United States'='USA'; 'United Kingdom'='UK'")
sorted_multi$region <- sorted_multi$Country

##World Maps
m1 <- ggplot()
m1 <- m1 + geom_map(data=WorldData, map=WorldData,
                    aes(x=long, y=lat, group=group, map_id=region),
                    fill="white", colour="#7f7f7f", size=0.1)
m1 <- m1 + geom_map(data=sorted_multi, map=WorldData,
                    aes(fill=A_est, map_id=region),
                    colour="#7f7f7f", size=0.1)
m1 <- m1 + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))
m1 <- m1 + scale_fill_viridis(option="viridis", discrete=FALSE, direction=-1)
m1 <- m1 + labs(fill="", title = element_blank(), x="", y="") 
m1 <- m1 + ggtitle("(a) Build additional roads")
m1 <- m1 + theme_bw()
m1 <- m1 + theme(panel.border = element_blank(), rect = element_blank(), 
                 axis.text.x = element_blank(), axis.text.y = element_blank(),
                 axis.ticks = element_blank(), panel.grid.major = element_blank())
m1

m2 <- ggplot()
m2 <- m2 + geom_map(data=WorldData, map=WorldData,
                    aes(x=long, y=lat, group=group, map_id=region),
                    fill="white", colour="#7f7f7f", size=0.1)
m2 <- m2 + geom_map(data=sorted_multi, map=WorldData,
                    aes(fill=C_est, map_id=region),
                    colour="#7f7f7f", size=0.1)
m2 <- m2 + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))
m2 <- m2 + scale_fill_viridis(option="viridis", discrete=FALSE, direction=-1)
m2 <- m2 + labs(fill="", title = element_blank(), x="", y="") 
m2 <- m2 + ggtitle("(b) Expand bike lanes")
m2 <- m2 + theme_bw()
m2 <- m2 + theme(panel.border = element_blank(), rect = element_blank(), 
                 axis.text.x = element_blank(), axis.text.y = element_blank(),
                 axis.ticks = element_blank(), panel.grid.major = element_blank())
m2

m3 <- ggplot()
m3 <- m3 + geom_map(data=WorldData, map=WorldData,
                    aes(x=long, y=lat, group=group, map_id=region),
                    fill="white", colour="#7f7f7f", size=0.1)
m3 <- m3 + geom_map(data=sorted_multi, map=WorldData,
                    aes(fill=K_est, map_id=region),
                    colour="#7f7f7f", size=0.1)
m3 <- m3 + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))
m3 <- m3 + scale_fill_viridis(option="viridis", discrete=FALSE, direction=-1)
m3 <- m3 + labs(fill="", title = element_blank(), x="", y="") 
m3 <- m3 + ggtitle("(c) Subsidize clean energy vehicles")
m3 <- m3 + theme_bw()
m3 <- m3 + theme(panel.border = element_blank(), rect = element_blank(), 
                 axis.text.x = element_blank(), axis.text.y = element_blank(),
                 axis.ticks = element_blank(), panel.grid.major = element_blank())
m3

png("/Users/jcmoody/Desktop/worldmaps.png", res = 1000, width = 5, height = 7, units = "in")
grid.arrange(m1, m2, m3, nrow=3)
dev.off()


##Scatter plot 
ggplot(sorted_multi,aes(x = GDP_percap_PPP, y = A_est, colour=develop_status)) +
  geom_text_repel(aes(label=Country)) +
  geom_point() +
  theme_classic(base_size = 16) + 
  geom_hline(yintercept=0, linetype="dashed", color = "grey") + 
  scale_colour_manual(labels = c("Developed", "Developing", "In transition"),
                      values = c("#440154FF", "#20A387FF", "#73D055FF")) +
  labs(x = "GDP per capita adjusted by purchasing power parity",size = 10) + 
  labs(y = "Country-level public support for building additional roads",size = 10) + 
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        legend.position=c(0.9, 0.9),
        legend.title=element_blank())

ggsave("/Users/jcmoody/Desktop/road_GDP_scatter.png", dpi = 1000, width = 8, height = 5, units = "in")
 

##Correlation matrix 
ranking = read.csv('/Users/xuenanni/Desktop/Thesis/data/country_raking.csv')
View(ranking)
library(corrplot)
dev_ed = subset(ranking, develop_status == "Developed") 
dev_ing = subset(ranking, develop_status == "Developing") 
cor_matrix = dev_ing[c("A_est", 
                       "B_est",
                       "C_est",
                       "D_est",
                       "E_est",
                       "F_est",
                       "G_est",
                       "H_est",
                       "I_est",
                       "J_est",
                       "K_est",
                       "GDP_percap_PPP",
                       # "Gini_index",
                       "PassKm_perCap",
                       # "PassKm_perVeh",
                       "RegVeh_perCap",
                       "ppl_km",
                       "perc_urban"
)]
colnames(cor_matrix) = c("Roads", 
                         "Car-light CBD",
                         "Bike Lane",
                         "More PT",
                         "Pedestrian Facilities",
                         "Car-free Ped CBD",
                         "Lower PT fares",
                         "BRT",
                         "Clean PT",
                         "More Parking",
                         "Clean Cars",
                         "GDP_percap_PPP",
                         # "Gini_index",
                         "PassKm_perCap",
                         # "PassKm_perVeh",
                         "RegVeh_perCap",
                         "ppl_km",
                         "% urban_pop") 
M = cor(cor_matrix,use = "complete.obs")

png("corr_deving0412.png", width = 30, height = 30, units = 'cm', res = 1000)
corrplot(M, method = "number",type='upper')
dev.off()
