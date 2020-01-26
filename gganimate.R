
## Loading Libraries
library(ggplot2)
library(gapminder)
library(gganimate)
library(gifski)


plot1 <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, colour=continent)) +
  geom_point() +
  scale_x_log10()+ 
  transition_time(year) + 
  labs(title = "Year: {frame_time}", x=" GDP per Capita", y="Life Expectancy") + 
  facet_wrap(~continent) +
  theme_bw()

animate(plot1, nframes = 200, renderer = gifski_renderer("gganim.gif"))

