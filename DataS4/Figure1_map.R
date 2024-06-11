x <- c("ggplot2", "dplyr", "basemaps", 'cowplot', 'ggsn', 'sf')
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

#import map data
map_data <- read.csv('map_data.csv')

#plot
plot1 <- basemap_ggplot +
  geom_point(data=map_data, #sites
             aes(x=X,
                 y=Y,
                 fill=det),
             pch=21,
             color='black',
             alpha=0.6,
             size=5.25) +
  geom_text(data = map_data, #site labels
                  aes(x=X_label,
                      y=Y_label,
                      label = Site),
                  size = 3.25) +
  annotate('text',x = -13545000, #detection legend
           y = 5865000, label = 'None',
           size = 3.25) +
  annotate('text',x = -13530000, #detection legend
           y = 5890000, label = 'eDNA only',
           size = 3.25) +
  annotate('text',x = -13520000, #detection legend
           y = 5915000, label = 'Traps & eDNA',
           size = 3.25) +
  annotate('text',x = -13530000, #detection legend
           y = 5940000, label = 'Detection',
           size = 3.25, fontface = 'bold') + 
  geom_point(aes(x=-13575000, #detection legend
                 y=5865000),
             fill = 'grey60',
             pch=21,
             color='black',
             alpha=0.6,
             size=5.25) +
  geom_point(aes(x=-13575000, #detection legend
                 y=5890000),
             fill = 'gold2',
             pch=21,
             color='black',
             alpha=0.6,
             size=5.25) +
  geom_point(aes(x=-13575000, #detection legend
                 y=5915000),
             fill = 'magenta4',
             pch=21,
             color='black',
             alpha=0.6,
             size=5.25) +
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
  geom_segment(aes(x = -13870000, y = 5855000, #WA coast polygon
                   xend = -13870000, yend = 5965000), 
                   color = 'green4', size = 1) +
  geom_segment(aes(x = -13870000, y = 5855000, #WA coast polygon 
                   xend = -13745000, yend = 5855000), 
               color = 'green4', size = 1) +
  geom_segment(aes(x = -13745000, y = 5855000, #WA coast polygon 
                   xend = -13745000, yend = 5965000), 
               color = 'green4', size = 1) +
  geom_segment(aes(x = -13870000, y = 5965000, #WA coast polygon 
                   xend = -13745000, yend = 5965000), 
               color = 'green4', size = 1) + 
  geom_segment(aes(x = -13680000, y = 5961000, #South Sound polygon 
                   xend = -13580000, yend = 5961000), 
               color = 'orange', size = 1) +
  geom_segment(aes(x = -13680000, y = 6030000, #South Sound polygon
                   xend = -13580000, yend = 6030000), 
               color = 'orange', size = 1) +
  geom_segment(aes(x = -13680000, y = 5961000, #South Sound polygon 
                   xend = -13680000, yend = 6030000), 
               color = 'orange', size = 1) +
  geom_segment(aes(x = -13580000, y = 5961000, #South Sound polygon 
                   xend = -13580000, yend = 6030000), 
               color = 'orange', size = 1) +
  geom_segment(aes(x = -13730000, y = 6306000, #Whatcom polygon 
                   xend = -13615000, yend = 6306000), 
               color = 'red', size = 1) +
  geom_segment(aes(x = -13730000, y = 6195000, #Whatcom polygon 
                   xend = -13615000, yend = 6195000), 
               color = 'red', size = 1) +
  geom_segment(aes(x = -13730000, y = 6195000, #Whatcom polygon 
                   xend = -13730000, yend = 6306000), 
               color = 'red', size = 1) +
  geom_segment(aes(x = -13615000, y = 6195000, #Whatcom polygon 
                   xend = -13615000, yend = 6306000), 
               color = 'red', size = 1) +
  geom_segment(aes(x = -13635000, y = 6075000, #Central sound polygon 
                   xend = -13635000, yend = 6168000), 
               color = 'blue', size = 1) +
  geom_segment(aes(x = -13770000, y = 6075000, #Central sound polygon 
                   xend = -13770000, yend = 6168000), 
               color = 'blue', size = 1) +
  geom_segment(aes(x = -13770000, y = 6075000, #Central sound polygon 
                   xend = -13635000, yend = 6075000), 
               color = 'blue', size = 1) +
  geom_segment(aes(x = -13770000, y = 6168000, #Central sound polygon 
                   xend = -13635000, yend = 6168000), 
               color = 'blue', size = 1) +
  geom_point(aes(x=-13700000, #Region legend
                 y=5920000),
             fill = NA,
             pch=22,
             color='red',
             size=5, stroke = 1.5) +
  geom_point(aes(x=-13700000, #Region legend
                 y=5900000),
             fill = NA,
             pch=22,
             color='blue',
             size=5, stroke = 1.5) +
  geom_point(aes(x=-13700000, #Region legend
                 y=5880000),
             fill = NA,
             pch=22,
             color='orange',
             size=5, stroke = 1.5) +
  geom_point(aes(x=-13700000, #Region legend
                 y=5860000),
             fill = NA,
             pch=22,
             color='green4',
             size=5, stroke = 1.5) +
  annotate('text',x = -13650000, #Region legend
           y = 5940000, label = 'Region',
           size = 3.25, fontface = 'bold') + 
  annotate('text',x = -13658000, #Region legend
           y = 5920000, label = 'Whatcom',
           size = 3.25) + 
  annotate('text',x = -13644000, #Region legend
           y = 5900000, label = 'Central Sound',
           size = 3.25) + 
  annotate('text',x = -13648000, #Region legend
           y = 5880000, label = 'South Sound',
           size = 3.25) + 
  annotate('text',x = -13657000, #Region legend
           y = 5860000, label = 'WA Coast',
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
       y=expression('Latitude (°N)')) +
  theme(panel.border = element_rect(color = "black", fill=NA, size=1.25),
        plot.margin=unit(c(0,0,0,0), 'mm'))

#us extent
box2 <- st_bbox(c(xmin = -129.748287, 
                 xmax = -65.412355, 
                 ymax = 50.443907, 
                 ymin = 23.302470), 
               crs = st_crs(4326))

#import basemap
basemap_us <- basemap_ggplot(box2, map_service = 'carto', 
                                 map_type = 'light_no_labels', force = TRUE)

plot2 <- basemap_us +
  scale_x_continuous(expand= c(0,0)) +
  scale_y_continuous(expand= c(0,0)) +
  geom_point(aes(x=-13600000,
                 y=,6000000),
             size = 5,
             color = 'black') +
  theme(panel.border = element_rect(color = "black", fill=NA, size=1.25),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(0,0,0,0), 'mm'),
        axis.ticks.length = unit(0,'null'))


final_plot <- ggdraw(plot1) +
  draw_plot(plot2, x = 0.1, y = 0.812, 
            width = 0.34, height = 0.19)

ggsave(final_plot, file = 'Figure1.tiff', compression = 'lzw',dpi = 700)

