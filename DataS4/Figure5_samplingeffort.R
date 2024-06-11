library(tidyverse)
library(scales)

#load csv
joint_posterior <- read.csv('Joint_posterior.csv')

#calculate trap-only and joint median
joint_median <- joint_posterior %>% 
  summarize_all(median) #joint model medians

#extract estimates
est_phi <- joint_median$phi
est_beta <- joint_median$beta
est_p10 <- joint_median$p10


#number of traps to get at least one crab
mu <- seq(from = 0.06, to = 2,
          by = 0.01)
pr <- vector(length = length(mu))
ntrap <- vector(length = length(mu))
for(i in 1:length(mu)){
  pr[i] <- 1 - pnbinom(q = 0, mu = mu[i], size = est_phi)  #prob of getting at least 1 crab in a trap
  ntrap[i] <- which(qbinom(0.05, size = 1:1000, prob = pr[i]) == 1) %>% min
}

#number of qPCR reactions to get at least one true detection (among 3 replicates)
p11 <- vector(length = length(mu))
p_3 <- vector(length = length(mu))
nqpcr_3 <- vector(length = length(mu))

for(i in 1:length(mu)){
  p11[i] <- mu[i]/(mu[i]+est_beta)
  p_3[i] <- 1 - pbinom(0, size = 3, prob = p11[i])
  nqpcr_3[i] <- which(qbinom(0.05, size = 1:1000, prob = p_3[i]) == 1) %>%  min
}

#create one dataframe
trap <- as.data.frame(cbind(mu,ntrap))
colnames(trap) <- c('mu', 'effort')
trap <- trap %>% 
  mutate(type = 'trap')
pcr3 <- as.data.frame(cbind(mu,nqpcr_3))
colnames(pcr3) <- c('mu', 'effort')
pcr3 <- pcr3 %>% 
  mutate(type = 'pcr3')
all_type <- rbind(trap, pcr3)

#plot
effort <- ggplot() +
  geom_smooth(data = all_type,
            aes(x = mu,
                y = effort, 
                color = type),
              size = 1.25,
            fill = NA) +
  labs(x = expression(mu ~ ' (Crabs/Trap)'),
       y = 'Sampling Effort',
       color = "Sampling Type")  +
  scale_color_manual(labels = c('eDNA Sampling: \n# Water Bottles \n',
                                'Traditional Sampling: \n# Traps'),
                     values = c('cornflowerblue', 
                                'coral3')) + 
  scale_x_continuous(breaks = c(0.1, 0.5, 1.0,
                                1.5, 2.0)) +
  theme_minimal() +
  theme(legend.title.align=0.5)

ggsave(effort, 'Figure5.tiff', dpi = 700,
       compression = 'lzw')


