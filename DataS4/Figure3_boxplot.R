library(tidyverse)
library(cowplot)
library(scales)
library(patchwork)
library(bayestestR)

#import data
joint_posterior <- read.csv('Joint_posterior.csv')
trap_posterior <- read.csv('Trap_only_posterior.csv')
site_data <- read.csv('site_data.csv')

#get just mu estimates
cols_remove <- c('phi','beta','p10')
joint_mu <- joint_posterior[,!(colnames(joint_posterior) %in% cols_remove)]
trap_mu <- trap_posterior[,!(colnames(trap_posterior) %in% cols_remove)]

#calculate 75% and 89% credibility intervals
#median and credibility intervals of of joint model mu
CI_joint <- as.data.frame(matrix(NA, nrow = 20, ncol = 6))
colnames(CI_joint) <- c('param', 'median', 'low89', 'high89',
                         'low75','high75')
CI_joint$param <- colnames(joint_mu[1:20])

for(i in 1:20){
  CI_joint[i,2] <- median(joint_mu[,i])
  CI_joint[i,3] <- ci(joint_mu[,i], method = 'HDI', ci = 0.89)[2]
  CI_joint[i,4] <- ci(joint_mu[,i], method = 'HDI', ci = 0.89)[3]
  CI_joint[i,5] <- ci(joint_mu[,i], method = 'HDI', ci = 0.75)[2]
  CI_joint[i,6] <- ci(joint_mu[,i], method = 'HDI', ci = 0.75)[3]
}

CI_joint <- CI_joint %>% 
  mutate(type = 'joint')

#median and credibility intervals of of trap model mu
CI_trap <- as.data.frame(matrix(NA, nrow = 20, ncol = 6))
colnames(CI_trap) <- c('param', 'median', 'low89', 'high89',
                       'low75','high75')
CI_trap$param <- colnames(trap_mu[1:20])

for(i in 1:20){
  CI_trap[i,2] <- median(trap_mu[,i])
  CI_trap[i,3] <- ci(trap_mu[,i], method = 'HDI', ci = 0.89)[2]
  CI_trap[i,4] <- ci(trap_mu[,i], method = 'HDI', ci = 0.89)[3]
  CI_trap[i,5] <- ci(trap_mu[,i], method = 'HDI', ci = 0.75)[2]
  CI_trap[i,6] <- ci(trap_mu[,i], method = 'HDI', ci = 0.75)[3]
}

CI_trap <- CI_trap %>% 
  mutate(type = 'trap')

#combine joint and trap models
all_data <- rbind(CI_joint, CI_trap)

#add site name
all_data <- full_join(all_data,site_data, by='param')

#reorder type
all_data$type <- factor(all_data$type, 
                        levels = c('trap', 'joint'))

#big boxplot
bigboxplot <- ggplot(all_data) +
  geom_boxplot(aes(x=fct_reorder(Site,median),
                   ymin=low89, lower=low75, middle=median,
                   upper=high75, ymax=high89,
                   fill = as.factor(type)),
               stat='identity',
               width = 0.5,
               lwd = 0.75,
               fatten = 1,
               position = position_dodge(0.6)) +
  scale_fill_manual(values = c('skyblue', 'indianred1'),
                    labels = c('Trap-only Model',
                               'Joint Model')) +
  labs(x = '',
       y = expression(paste(hat(mu), ' (Green Crab Density)')),
       fill = '') +
  theme_minimal() +
  theme(legend.key.size = unit(2, 'cm'),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 30),
        legend.text = element_text(size = 30),
        plot.title = element_text(size = 30, face = 'bold')) +
  ggtitle('A')



#subset for the little boxplot
small_data <- all_data[all_data$Site=='KVI'|
                         all_data$Site=='GSC'|
                         all_data$Site=='CHU'|
                         all_data$Site=='JIM'|
                         all_data$Site=='IND'|
                         all_data$Site=='WLS'|
                         all_data$Site=='CAC'|
                         all_data$Site=='PIL'|
                         all_data$Site=='RAA'|
                         all_data$Site=='TIT',]

#small boxplot
smallboxplot <- ggplot(small_data) +
  geom_boxplot(aes(x=fct_reorder(Site,median),
                   ymin=low89, lower=low75, middle=median,
                   upper=high75, ymax=high89,
                   fill = as.factor(type)),
               stat='identity',
               width = 0.5,
               lwd = 0.75,
               fatten = 1,
               position = position_dodge(0.6)) +
  scale_fill_manual(values = c('skyblue', 'indianred1'),
                    labels = c('Trap-only Model',
                               'Joint Model')) +
  labs(x = '',
       y = expression(paste(hat(mu), ' (Green Crab Density)')),
       fill = '') +
  theme_minimal() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        plot.title = element_text(face = 'bold', size = 30),
        legend.position = "none",
        plot.background = element_rect(colour = "black", fill=NA, size=1.25)) +
  ggtitle('B')



#place zoomed in boxplot as inset within larger boxplot
plot_w_inset <- ggdraw(bigboxplot) +
  draw_plot(smallboxplot, x = 0.1, y = 0.4, 
            width = 0.45, height = 0.5)

ggsave(plot_w_inset, file = 'Figure3.tiff', dpi = 700,
       compression = 'lzw')


