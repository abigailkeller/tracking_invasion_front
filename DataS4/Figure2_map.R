x <- c("ggplot2", "dplyr", "basemaps", 'cowplot', 
       'ggsn', 'sf', 'RColorBrewer', 'ggnewscale')
lapply(x, library, character.only = TRUE) # load the required packages


#########
###map### 
#########

#set extent
box <- st_bbox(c(xmin = -125.278467, 
                 xmax = -121, 
                 ymax = 49.25, 
                 ymin = 46.291582), 
               crs = st_crs(4326))

#import basemap
basemap_ggplot <- basemap_ggplot(box, map_service = 'carto', 
                                 map_type = 'light_no_labels', force = TRUE)

#import csv
posterior <- read.csv('Joint_posterior.csv')
site_data <- read.csv("site_data.csv")
map_data <- read.csv('map_data.csv')

#get just mu estimates
cols_remove <- c('phi','beta','p10')
mu <- posterior[,!(colnames(posterior) %in% cols_remove)]

#calculate estimated density median
median <- mu %>% 
  summarize_all(median)
median <- as.data.frame(t(median))
param <- rownames(median)
NB_median <- cbind(param, median)

#combine medians and site data
all_data <- merge(median, site_data, by = 'param')
all_data <- merge(all_data, map_data, by = 'Site')

#take log
all_data <- all_data %>% 
  mutate(logtrans = log(V1+0.001))


#plot
final_plot <- basemap_ggplot +
  new_scale_fill() +
  new_scale_color() +
  geom_point(data=all_data, #sites
             aes(x=X,
                 y=Y,
                 fill=logtrans),
             pch=21,
             color='black',
             alpha=0.6,
             size=5.25) +
  scale_fill_gradient(low = 'white', high = 'dodgerblue4',
                      guide = guide_colorbar(frame.colour='black',
                                             frame.linewidth = 1,
                                             ticks.colour='black',
                                             ticks.linewidth = 1),
                      breaks = c(0.4061316,
                                 -1.382302,
                                 -3.473768,
                                 -5.521461),
                      labels = c(1.5,0.25,
                                 0.03,0.003)) +
  geom_text(data = all_data, #site labels
            aes(x=X_label,
                y=Y_label,
                label = Site),
            size = 3.25) +
  geom_segment(aes(x = -13550000, y = 6280000, #scale bar
                   xend = -13500000, yend = 6280000), 
               color = 'black', size = 1) +
  geom_segment(aes(x = -13550000, y = 6287500, #scale bar
                   xend = -13550000, yend = 6272500), 
               color = 'black', size = 1) +
  geom_segment(aes(x = -13500000, y = 6287500, #scale bar 
                   xend = -13500000, yend = 6272500), 
               color = 'black', size = 1) +
  annotate('text',x = -13525000, #scale bar
           y = 6300000, label = '50 km',
           size = 3.25) +
  north(location = 'topright', #north arrow
        symbol=10,
        scale = 0.13,
        x.min=-13900000,
        y.min=5900000,
        x.max=-13500000,
        y.max=6240000) +
  annotate('text',x = -13525000, #north arrow
           y = 6195000, label = 'N',
           size = 3.25) +
  geom_point(aes(x=-13620000, #Seattle
                 y=6040000),
             size = 2) +
  geom_point(aes(x=-13740000, #Victoria
                 y=6180000),
             size = 2) +
  annotate('text',x = -13595000, #Seattle
           y = 6045000, label = 'Seattle',
           size = 3) + 
  annotate('text',x = -13767000, #Victoria
           y = 6186000, label = 'Victoria',
           size = 3) +
  geom_segment(aes(x = -13880000, y = 6172000, #1999 invasion front
                   xend = -13936000, yend = 6240000), 
               color = 'indianred4', size = 1) +
  annotate('text', x = -13880000, #1999 invasion front
           y = 6154000, label = '1999', size = 3,
           color = 'indianred4') +
  geom_segment(aes(x = -13770000, y = 6132000, #2012 invasion front
                   xend = -13770000, yend = 6168000), 
               color = 'indianred4', size = 1) +
  annotate('text', x = -13780000, #2012 invasion front
           y = 6120000, label = '2012', size = 3,
           color = 'indianred4') +
  geom_segment(aes(x = -13656000, y = 6101000, #2020 invasion front
                   xend = -13647000, yend = 6113000), 
               color = 'indianred4', size = 1) +
  geom_segment(aes(x = -13647000, y = 6113000, #2020 invasion front
                   xend = -13647000, yend = 6130000), 
               color = 'indianred4', size = 1) +
  geom_segment(aes(x = -13647000, y = 6130000, #2020 invasion front
                   xend = -13655000, yend = 6134000), 
               color = 'indianred4', size = 1) +
  geom_segment(aes(x = -13655000, y = 6134000, #2020 invasion front
                   xend = -13665000, yend = 6144000), 
               color = 'indianred4', size = 1) +
  geom_segment(aes(x = -13665000, y = 6144000, #2020 invasion front
                   xend = -13655000, yend = 6175000), 
               color = 'indianred4', size = 1) +
  geom_segment(aes(x = -13655000, y = 6175000, #2020 invasion front
                   xend = -13642000, yend = 6180000), 
               color = 'indianred4', size = 1) +
  annotate('text', x = -13620000, #2020 invasion front
           y = 6180000, label = '2020', size = 3,
           color = 'indianred4') +
  geom_segment(aes(x = -13670000, y = 5880000, #invasion front legend
                   xend = -13730000, yend = 5880000), 
               color = 'indianred4', size = 2) +
  annotate('text', x = -13620000, #invasion front legend
           y = 5895000, label = 'Previously', size = 3.25) +
  annotate('text', x = -13620000, #invasion front legend
           y = 5880000, label = 'identified', size = 3.25) +
  annotate('text', x = -13620000, #invasion front legend
           y = 5865000, label = 'invasion fronts', size = 3.25) +
  scale_x_continuous(expand=c(0,0),
                     breaks = c(-13905000,
                                -13800000,
                                -13695000,
                                -13590000,
                                -13485000),
                     labels = c(-125,-124,-123,
                                -122,-121)) +
  scale_y_continuous(expand=c(0,0),
                     breaks = c(5940000,
                                6110000,
                                6280000),
                     labels = c(47,48,49)) +
  labs(x=expression('Longitude (°W)'),
       y=expression('Latitude (°N)'),
       fill = 'Crab\n Density') +
  theme(plot.margin=unit(c(0,0,0,0), 'mm'),
        legend.position = c(0.9, 0.25),
        legend.key.height = unit(0.75, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title.align = 0.5,
        legend.background = element_rect(fill='transparent'),
        panel.border = element_rect(color = "black", fill=NA, size=1.25)) 

#export
ggsave(final_plot, file = 'Figure2.tiff', compression = 'lzw',dpi = 700)
