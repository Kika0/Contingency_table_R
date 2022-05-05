library(here)
library(tidyverse)
# load data
library(tidyverse)
# Data cleaning function
library(here)
#check here() file path, which should be Contingency_table_R directory
source(here("clean_data_anon.R"))
source(here("wrangledata_functions.R"))
# load data
# df<- read_csv(here("..","data-not-synced","anon_survey1.csv")) %>% 
# new data
df<- read_csv(here("anon_survey1.csv")) %>% 
  # Clean data
  data_clean() %>% 
  # add col for numeric opinion on the ban and create ordered factors 
  #for age and number of veh owned
  ban_numeric()
#check column names
names(df)

# Plot 1: do contingency table of opinion on the ban (average)
df %>% group_by(occup, age) %>% 
  summarise(n=round(sum(opinion_ban1)/n(),2)) %>%
  ggplot(aes(x = occup, y = age, fill = n ))  +
  geom_tile(
    #aes(sum(df$opinion_ban1)) ,
    color = "white") +
  scale_fill_gradient2(high = "olivedrab", low = "tomato",
                       mid="lightyellow",midpoint = 0.5,
                       space = "Lab", name="count"
                       ,limits=c(0.25,0.75)) +
  # make sure x and y have the same scaling
  coord_fixed() +
  # add the values to the tiles, using row and name as coordinates 
  geom_text(aes(occup,age,label=n), color = "black", size = 2) +
  # specify theme elements
  theme_minimal() +
  # adjust text direction on x-axis 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1),
        text = element_text(size=15)) +
  # remove the axis labels by making them blank
  xlab("") +ylab("") + ggtitle("Views of demographics on the proposed motorbike ban") 

  theme(legend.position="none") #+
theme(
  panel.background = element_rect(fill = "transparent"), # bg of the panel
  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
  #    panel.grid.major = element_blank(), # get rid of major grid
  #     panel.grid.minor = element_blank(), # get rid of minor grid
  #   legend.background = element_rect(fill = "transparent"), # get rid of legend bg
  #   legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
)

df %>% group_by(!!sym(input$demo1), !!sym(input$demo2)) %>% 
  summarise(n=round(sum(opinion_ban1)/n(),2)) %>%
  ggplot(aes(x = !!sym(input$demo1), y = !!sym(input$demo2), fill = n ))  +
  geom_tile(
    #aes(sum(df$opinion_ban1)) ,
    color = "white") +
  scale_fill_gradient2(high = "olivedrab", low = "tomato",
                       mid="lightyellow",midpoint = 0.5,
                       space = "Lab", name="count"
                       ,limits=c(0.25,0.75)) +
  # make sure x and y have the same scaling
  coord_fixed() +
  # add the values to the tiles, using row and name as coordinates 
  geom_text(aes(!!sym(input$demo1),!!sym(input$demo2),label=n), color = "black", size = 4) +
  # specify theme elements
  theme_minimal() +
  # adjust text direction on x-axis 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,hjust = 1),
        text = element_text(size=15)) +
  # remove the axis labels by making them blank
  xlab("") +ylab("") + ggtitle("Views of demographics on the proposed motorbike ban")

# Plot 2: Occupation and type of occupation demographic group counts 

# Plot 3: Cumulative sum of values (number of motorbikes) for a given subgroup (occupation,type of occupation)