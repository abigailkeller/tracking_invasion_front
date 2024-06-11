library(tidyverse)
library(akima)

#import simulation data
joint_data <- read.csv('simulation_joint_summary.csv')
trap_data <- read.csv('simulation_trap_summary.csv')
input_data <- read.csv('simulation_scenarios.csv')

#add rank for mu and traps
input_data <- input_data %>% 
  mutate(rank_mu = rep(1:9,11),
         rank_trap = sort(rep(1:11,9)))

#find mean of each scenario
joint_summary <- joint_data %>% 
  group_by(ID) %>% 
  summarize(joint_mean_cv = mean(cv))
trap_summary <- trap_data %>% 
  group_by(ID) %>% 
  summarize(trap_mean_cv = mean(cv))

#join two datasets
all_summary <- merge(joint_summary, trap_summary, by = 'ID')

#add delta cv
all_summary <- all_summary %>% 
  mutate(deltaCV = trap_mean_cv - joint_mean_cv)

#add input data
all_summary <- merge(all_summary, input_data, by = 'ID')


#plot
heatmap <- ggplot(data = all_summary) +
  geom_tile(aes(x=rank_trap,
                y=rank_mu, 
                fill=deltaCV)) +
  scale_x_continuous(breaks = c(1,3,5,7,9,11),
                     labels = c(3,5,10,15,30,60)) +
  scale_y_continuous(breaks = c(1,3,5,7,9),
                     labels = c(0,0.05,0.15,0.5,3)) +
  labs(x='Number of Traps',
       y=expression(mu ~ '(Crabs/Trap)'),
       fill = expression(Delta~'CV')) +
  scale_fill_distiller(palette = 'RdPu') +
  theme_minimal()

ggsave(heatmap, file = 'heatmap.tiff', dpi = 700,
       compression = 'lzw')
