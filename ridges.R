library(tidyverse)
library(ggridges)
library(ggpubr)

## read in data
core_RMDS <- read.csv('core_RMDS.csv', check.names = F) %>%
  gather() %>% #合并列，转换成长数据
  rename("Time"=key)  #修改列名

A_loop_RMDS <- read.csv('A_loop_RMDS.csv', check.names = F) %>%
  gather() %>%
  rename("Time"=key)

Distance <- read.csv('Distance.csv', check.names = F) %>%
  gather() %>% 
  rename("Time"=key)  


## set order, colour, and comparisons for significant
mylevel = c('24_ns','20_ns','16_ns','12_ns','8_ns','0_ns')

mycol1 = c("#1C273B","#2B4960","#386D75","#4B9085","#6EAD92","#91C7A3")
mycol2 = c("#29203F","#4D3565","#724C85","#96659F","#B781B3","#D39FC6")
mycol3 = c("#1F253F","#334667","#436485","#58849F","#71A4B7","#90C1C6")

my_comparisons <- list(c("0_ns","8_ns"),
                       c("8_ns","12_ns"), 
                       c("16_ns","20_ns"))



## plot
p1 <- ggplot(core_RMDS, aes(x = value, 
                            y = factor(Time, levels = mylevel),
                            fill = factor(Time, levels = mylevel),
                            color = factor(Time, levels = mylevel))) +
  geom_density_ridges()+
  geom_signif(comparisons = my_comparisons, 
              #map_signif_level = function(p) sprintf("p = %.2g", p), 
              map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05),
              step_increase = 0.02,
              y_position = c(5.5, 5.5, 5.5),
              textsize = 5, vjust = 0.5, color = "black",
              tip_length = 0) +
  scale_fill_manual(values = mycol1, guide = 'none') + 
  scale_color_manual(values = mycol1, guide = 'none') +
  scale_x_continuous(breaks = seq(2.5,5.5,0.5)) +
  theme_minimal() +
  labs(x = 'Core RMDS (A)', y = NULL) +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_text(color = mycol1, size = 14, face = 'bold',
                                   vjust = -0.3, hjust = 0,
                                   margin = margin(0,-1.8,1,1,'cm')),
        axis.text.x = element_text(vjust = 4, size = 14),
        axis.title.x = element_text(vjust = 3, size = 14))
p1

p2 <- ggplot(A_loop_RMDS, aes(x = value, 
                              y = factor(Time, levels = mylevel),
                              fill = factor(Time, levels = mylevel),
                              color = factor(Time, levels = mylevel))) +
  geom_density_ridges()+
  geom_signif(comparisons = my_comparisons, 
              #map_signif_level = function(p) sprintf("p = %.2g", p), 
              map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05),
              step_increase = 0.02,
              y_position = c(14, 14, 14),
              textsize = 5, vjust = 0.5, color = "black",
              tip_length = 0) +
  scale_fill_manual(values = mycol2, guide = 'none') + 
  scale_color_manual(values = mycol2, guide = 'none') +
  scale_x_continuous(breaks = seq(2,14,2)) +
  theme_minimal() +
  labs(x = 'A-Loop RMSD (A)', y = NULL) +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_text(color = mycol2, size = 14, face = 'bold',
                                   vjust = -0.3, hjust = 0,
                                   margin = margin(0,-1.8,1,1,'cm')),
        axis.text.x = element_text(vjust = 4, size = 14),
        axis.title.x = element_text(vjust = 3, size = 14))
p2

p3 <- ggplot(Distance, aes(x = value, 
                           y = factor(Time, levels = mylevel),
                           fill = factor(Time, levels = mylevel),
                           color = factor(Time, levels = mylevel))) +
  geom_density_ridges()+
  geom_signif(comparisons = my_comparisons, 
              #map_signif_level = function(p) sprintf("p = %.2g", p), 
              map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05),
              step_increase = 0.02,
              y_position = c(21, 21, 21),
              textsize = 5, vjust = 0.5, color = "black",
              tip_length = 0) +
  scale_fill_manual(values = mycol3, guide = 'none') + 
  scale_color_manual(values = mycol3, guide = 'none') +
  scale_x_continuous(breaks = seq(-20,15,5)) +
  theme_minimal() +
  labs(x = 'Distance (A)', y = NULL) +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_text(color = mycol3, size = 14, face = 'bold',
                                   vjust = -0.3, hjust = 0,
                                   margin = margin(1,-1.8,1,1,'cm')),
        axis.text.x = element_text(vjust = 4, size = 14),
        axis.title.x = element_text(vjust = 3, size = 14))
p3


## combine plots
ggarrange(p1, p2, p3, ncol = 1)


ggsave("ridges.png", width = 6, height = 6.5, dpi = 600)
ggsave("ridges.pdf", width = 6, height = 6.5)

