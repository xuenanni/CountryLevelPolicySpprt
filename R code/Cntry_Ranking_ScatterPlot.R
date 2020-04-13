#title: "Dalia Data Analysis - Country Ranking Regression"
#author: "Xuenan Ni"
#date: "Dec 3, 2018"
#last modified: April 12, 2020
library(ggplot2)
library(reshape2)
library(ggrepel)
multi_ipf_compare =read.csv('/Users/xuenanni/Desktop/thesis/mplus_results/country_comb.csv')
View(multi_ipf_compare)
sorted_multi = multi_ipf_compare[order(multi_ipf_compare$A_est, multi_ipf_compare$J_est),]
View(sorted_multi)
A_est =sorted_multi$A_est
B_est =sorted_multi$B_est
C_est =sorted_multi$C_est
D_est =sorted_multi$D_est
E_est =sorted_multi$E_est
F_est =sorted_multi$F_est
G_est =sorted_multi$G_est
H_est =sorted_multi$H_est
I_est =sorted_multi$I_est
J_est =sorted_multi$J_est
 
##scatter plot 
ggplot(sorted_multi,aes(x = GDP_percap_PPP, y = A_est, colour = develop_status, label = Country)) +
  geom_text_repel(aes(label=Country)) +
  geom_point() +
  theme_classic(base_size = 16) + 
  geom_hline(yintercept=0, linetype="dashed", color = "grey") + 
  labs(x = "GDP per Capita Adjusted by Purchasing Power",size = 5) + 
  labs(y = "Country Rankings of Building More Roads",size = 5) + 
  labs(title = "50 Countries' Scattor Plot of Building More Roads",size = 12)+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12))

ggsave("desktop/thesis_images/country_factor/resolution/road.png",dpi = 800, width = 30, height = 20, units = "cm")
 
##correlation matrix 
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

png("Desktop/Thesis/TRB/images/corr_deving0412.png", width = 30, height = 30, units = 'cm', res = 1000)
corrplot(M, method = "number",type='upper')
dev.off()
