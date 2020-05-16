library(gganimate)
library(tidyverse)

# read input
ligaData <- read_csv("./data/laLigaData.csv")
# set clubs as factors
ligaData$club <- as.factor(ligaData$club)
# set club's first hex code color to be bar color
col <- ligaData$color1
names(col) <- ligaData$club
# set club's second hex code color to be text color
col2 <- ligaData$color2
names(col2) <- ligaData$club

# builds the plot
p <- ggplot(ligaData, aes(rank,
                          group = club,
                          fill = club,
                          color = club)) +
  # controls bars
  geom_tile(aes(y = titles/2, # /2 keeps bar on x-axis,
                height = titles,
                width = .9),
            alpha = 1) +
  # controls club labels to the left
  geom_text(aes(y = 0,
                label = paste(club, " ")),
            family="Helvetica",
            size = 6.5,
            vjust = 0.5,
            hjust = 1) +
  # controls title numbers to the right
  geom_text(aes(y=titles,
                # convert to character to get rid of spacing problems
                label = paste(" ", as.character(titles))),
            size = 10,
            vjust = .5,
            hjust = 0) +
  # for when the axes flip
  coord_flip(clip = "off",
             expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  # makes plot blank
  theme_void() +
  scale_fill_manual(values = col) +
  scale_color_manual(values = col2) +
  guides(color = FALSE, fill = FALSE) +
  labs(title='La Liga Titles',
       # refers to year
       caption ='{closest_state}') +
  theme(plot.title = element_text(hjust = 2.65,
                                  size = 60,
                                  family="Helvetica",
                                  face = "bold",
                                  vjust = 3),
        plot.caption = element_text(size = 75,
                                    family="Helvetica",
                                    face = "italic"),
        axis.ticks.y = element_blank(),  # these relate to the axes post-flip
        axis.text.y  = element_blank(),  # these relate to the axes post-flip,
        plot.margin = margin(2,2.5,1.5,6, "cm")) +
  annotate("text",size = 3, x = 10, y = 0,
           # hack to custom place without messing up anything else
           label = " \n \n \n \n \n \n \n \n*Spanish Civil War 1937-1939 \n Source: worldfootball.net") +
  transition_states(year,
                    transition_length = 1,
                    state_length = 0) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('cubic-in-out')

# how to set the animation details
# default nframes is 100
animate(p, nframes = 300,width = 600, height = 600)
