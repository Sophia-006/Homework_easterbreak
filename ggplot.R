library(tidyverse)

gapminder <-  read_csv ("Data/gapminder_data.csv")
gapminder_1977 <-  filter (gapminder, year == 1977)

gapminder_1977 %>% 
  ggplot(mapping = aes(x=lifeExp, y=gdpPercap, colour= continent, size=pop))+
  geom_point() +
  geom_line()+
  scale_x_log10()

gapminder_1977 %>% 
  ggplot()+
  geom_point(mapping = aes(x=lifeExp, y=gdpPercap, colour= continent, size=pop)) +
  geom_line(mapping = aes(x=lifeExp, y=gdpPercap))+
  scale_x_log10()

gapminder_1977 %>% 
  ggplot()+
  geom_point(mapping = aes(x=gdpPercap, y=lifeExp, shape= continent, size=pop)) +
  scale_x_log10()

gapminder_1977 %>% 
  ggplot(mapping = aes(x=gdpPercap, y=lifeExp, colour= continent, size=pop))+
  geom_point(colour= 'blue') +
  scale_x_log10()

gapminder_1977 %>% 
ggplot(mapping=aes(x=gdpPercap, y=lifeExp, colour=continent, size=pop))+
  geom_point(shape=21, colour="black", fill="white", size=5, stroke=5)+
  scale_x_log10()

gapminder_1977 %>% 
ggplot (mapping = aes(x = lifeExp, y = gdpPercap, colour = continent, size = pop))+ 
  geom_point(shape = 25, alpha = 0.7, fill = 'blue', stroke = 2, colour = 'orange') + 
  scale_y_log10()

gapminder_1977 %>% 
  ggplot(mapping=aes(x = lifeExp, y = gdpPercap, shape=continent, size=pop))+
  geom_point(shape=23, colour="white", fill="pink", size=5)+
  scale_x_log10()

gapminder_1977 %>% 
  ggplot(mapping=aes(x=gdpPercap, y=lifeExp, colour=continent, size=pop))+
  geom_point(alpha = .4)+
  scale_x_log10()

gapminder %>% 
  ggplot(mapping = aes(x=year, y=lifeExp, shape=continent, colour = continent))+
  geom_point(alpha= .4) +
  geom_line()+
  scale_x_log10()

gapminder %>% 
  ggplot(mapping = aes(x=year, y=lifeExp, colour = continent, group = country))+
  geom_line()+
  geom_point(colour='black', alpha= 0.3)+
  scale_x_log10()

gapminder %>% 
  ggplot (aes(x=gdpPercap, y=lifeExp, group=continent, colour= continent))+
  geom_point(shape =23, alpha = 0.5)+
  scale_x_log10()+
  geom_smooth(method = "lm", size= 2)

gapminder %>% 
  ggplot (aes(x=gdpPercap, y=lifeExp))+
  geom_point(mapping= aes(colour=continent), size=2)+
  scale_x_log10()+
  geom_smooth(mapping= aes(colour=continent), method ='lm', size= 2)
  geom_smooth(method = "lm") # no colour by continent

  #Scales
  gapminder %>% 
    ggplot (aes(x=year, y=lifeExp, colour= continent))+
    geom_point()+
    scale_colour_manual(values = c('red', 'green', 'blue', 'purple', 'black'))
    

  gapminder %>% 
    ggplot (aes(x=gdpPercap, y=lifeExp))+
    geom_point(mapping= aes(colour=continent), size=2)+
    scale_x_log10()+
    scale_colour_brewer(palette = "Dark2")
  
#separating plots
  a_countries <- filter(gapminder, str_starts(country, "A"))
  
  ggplot(a_countries, aes(x=year, y=lifeExp, colour= continent, group=country))+
    geom_line()+
    facet_wrap(~country)
  
  gapminder %>% 
    ggplot(aes(x=gdpPercap, y=lifeExp, colour= continent, size= pop))+
    geom_point()+
    scale_x_log10()+
    facet_wrap(~year)
