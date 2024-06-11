library(tidyverse)

#import data
joint_posterior <- read.csv('Joint_posterior.csv')
trap_posterior <- read.csv('Trap_only_posterior.csv')
trap_data <- read.csv("trap_data.csv")
site_data <- read.csv('site_data.csv')


#calculate trap-only and joint means and sd
joint_means <- joint_posterior %>% 
  summarize_all(mean) #joint model means
joint_sd <- joint_posterior %>% 
  summarize_all(sd) #joint model sd
trap_means <- trap_posterior %>% 
  summarize_all(mean) #trap-only means
trap_sd <- trap_posterior %>% 
  summarize_all(sd) #trap-only sd

#merge joint stats and trap stats
#joint stats - mean
joint_means <- as.data.frame(t(joint_means))
param_joint <- rownames(joint_means)
joint_means <- cbind(param_joint, joint_means)
colnames(joint_means) <- c('param', 'joint_mean')
#joint stats - sd
joint_sd <- as.data.frame(t(joint_sd))
param_joint <- rownames(joint_sd)
joint_sd <- cbind(param_joint, joint_sd)
colnames(joint_sd) <- c('param', 'joint_sd')
#trap stats - mean
trap_means <- as.data.frame(t(trap_means))
param_trap <- rownames(trap_means)
trap_means <- cbind(param_trap, trap_means)
colnames(trap_means) <- c('param', 'trap_mean')
#trap stats - sd
trap_sd <- as.data.frame(t(trap_sd))
param_trap <- rownames(trap_sd)
trap_sd <- cbind(param_trap, trap_sd)
colnames(trap_sd) <- c('param', 'trap_sd')
#merge all
joint_stats <- merge(joint_means, joint_sd, by = 'param')
trap_stats <- merge(trap_means, trap_sd, by = 'param')
all_stats <- merge(joint_stats, trap_stats, by = 'param')

#calculate number of traps per site
trap_sum <- as.data.frame(table(trap_data$Site))
colnames(trap_sum) <- c('Site', 'Count')
trap_sum <- merge(trap_sum, site_data, by = 'Site')

#add trap count to model means
all_data <- merge(all_stats, trap_sum, by = 'param')

#calculate difference in mean and coefficient of variation
all_data <- all_data %>% 
  mutate(mean_dif = joint_mean - trap_mean,
         CV_trap = trap_sd/trap_mean,
         CV_joint = joint_sd/joint_mean,
         CV_dif = CV_trap - CV_joint,
         mean_dif_abs = abs(mean_dif))

#create data for model fit line
model_fit <- as.data.frame(seq(from=3, to=420, by = 1))
colnames(model_fit) <- 'traps'
model_fit <- model_fit %>% 
  mutate(log_traps = log(traps),
         CV_pred = 54*exp(log_traps*-2.94))


#reorder region
all_data$Region <- factor(all_data$Region, levels=c('Central_Sound',
                                 'South_Sound', 'Coast', 'Whatcom'))

#plot of number of traps vs. CV dif
cv_plot <- ggplot() +
  geom_line(data = model_fit,
            aes(x = traps,
                y = CV_pred),
            color = 'gray',
            size = 1.25) +
  geom_point(data = all_data,
             aes(x = Count,
                 y = CV_dif,
                 color = as.factor(Region)),
             size = 4.5,
             alpha = 0.6) +
  scale_x_log10() +
  scale_color_manual(values = c('blue', 'orange', 
                                  'green4', 'red'),
                       labels = c('Central Sound',
                                  'South Sound', 
                                  'WA Coast', 
                                  'Whatcom')) +
  labs(x = 'Number of Traps',
       y = expression(Delta~'CV'),
       color = 'Region') +
  theme_minimal() +
  theme(legend.title = element_text(hjust = 0.5),
        text = element_text(size = 16))

ggsave(cv_plot, file = "Figure4.tiff", dpi = 700,
       compression = 'lzw')




