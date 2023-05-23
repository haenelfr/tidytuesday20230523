# load libraries

library(ggmap)
library(ggplot2)
library(tidyr)
library(lubridate)
library(patchwork)
library(cowplot)


# load data

squirrel_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv')

# locations
df <- squirrel_data %>% filter(!is.na(`Primary Fur Color`))
a <- ggplot(data=df,aes(x=X,y=Y)) + geom_point(alpha = 0.5,aes(color=`Primary Fur Color`)) + theme_void() + scale_color_manual(values=c("black","brown","grey")) + ggtitle("Sightings locations by fur") + theme(legend.position = "top",plot.title = element_text(size=12,face="bold"))

# donut
df <- squirrel_data %>% count(Shift) %>% mutate(x = 4)
b <- ggplot(df, aes(x = 4, y = n, fill = Shift)) +
    geom_col() +
    coord_polar(theta = "y") +
    xlim(c(0.2, 4 + 0.5)) + theme_void() + geom_text(color= "white", aes(label = round(n/sum(n)*100,0)),
    position = position_stack(vjust = 0.5)) + scale_fill_manual(values=c("#29b14b","#731963")) + ggtitle("Percentage of sightings by shift") + theme(legend.position = "top",plot.title = element_text(size=12,face="bold"))

# donut
df <- squirrel_data %>% filter(Age != "?") %>% count(Age) %>% mutate(x = 4)
c <- ggplot(df, aes(x = 4, y = n, fill = Age)) +
    geom_col() +
    coord_polar(theta = "y") +
    xlim(c(0.2, 4 + 0.5)) + theme_void() + geom_text(color= "white", aes(label = round(n/sum(n)*100,0)),
    position = position_stack(vjust = 0.5)) + scale_fill_manual(values=c("#29b14b","#731963")) + ggtitle("Percentage of sightings by age") + theme(legend.position = "top",plot.title = element_text(size=12,face="bold"))

#barplot
df <- squirrel_data %>% mutate(Month = as.factor(month(as_date(Date))))
d <- ggplot(data=df, aes(x=Month)) + geom_bar(fill = "#29b14b") + theme_void() + xlab('Month') + ylab('Sightings') + ggtitle("Sightings by month") + theme(plot.title = element_text(size=12,face="bold"),axis.text = element_text(size=8), axis.title = element_text(size=9),axis.title.y = element_text(angle = 90))

# arrange plots
bc <- plot_grid(b,c,ncol=1)
abc <- plot_grid(a,bc,rel_widths=c(2,1))
abcd <- plot_grid(abc,d,ncol=1,rel_heights=c(2,1))
abcd + plot_annotation(title = "Squirrels in Central Park: Census 2018", subtitle = "#tidytuesday 2023-05-23", caption= "Frank Hänel @web_design_fh \n Data from 2018 Central Park Squirrel Census")

