
library(tidyverse)

gapminder <-  read_csv ("Data/gapminder_data.csv")
gapminder_1977 <-  filter (gapminder, year == 1977)
a_countries <- filter(gapminder, str_starts(country, "A"))

ggplot(data= a_countries, aes(x= year, y= lifeExp, colour= continent))+
  geom_line(colour= 'blue') +
  facet_wrap (~country)

rough_plot <- ggplot(data= a_countries, aes(x= year, y= lifeExp, colour= continent))+
  geom_line() +
  facet_wrap (~country)

rough_plot+
  labs(title= "Growth in life expectancy for 'A'", #use different "" ''marks to write inside
       x='Year',
       y= 'Life Expectancy',
       colour = 'Continent',
       caption = 'Datasource = Gapminder') +
  theme_bw()+
  theme(
    panel.grid.minor= element_blank(),
    plot.title= element_text(face= "bold")
  )

life_Exp_plot <- rough_plot+
  labs(title= "Growth in life expectancy for 'A'", #use different "" ''marks to write inside
       x='Year',
       y= 'Life Expectancy',
       colour = 'Continent',
       caption = 'Datasource = Gapminder') +
  theme_bw()+
  theme(
    strip.background = element_blank(),
    panel.grid.minor= element_blank(), #element_line(size=1)
    plot.title= element_text(face= "bold"),
    axis.title = element_text(size=12,face='bold',  colour= 'purple'),
    legend.position = "bottom"
  )

life_Exp_plot

ggsave(filename= "results/lifeExp.png", plot= life_Exp_plot,
       width=20, height =14, dpi=300, units= 'cm')

install.packages("cowplot")
library(cowplot)

plot1 <- ggplot(gapminder, aes(x=gdpPercap, y=lifeExp)) + geom_point()
plot2 <- ggplot(gapminder, aes(x=continent, y=lifeExp)) + geom_boxplot()
plot3 <- ggplot(gapminder, aes(x=gdpPercap, y=pop)) + geom_point()
plot4 <- ggplot(gapminder, aes(x=lifeExp, y=pop)) + geom_point()

plot_grid(plot1, plot2, plot3,plot4, rel_heights = c(1,3), rel_widths = c(4,1))

plot_grid(plot1, plot2, plot3,plot4, labels = 'auto') 
