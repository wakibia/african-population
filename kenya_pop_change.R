## load libraries

library(tidyverse)
library(ggtext)
library(janitor)
library(ggthemes)
library(ggimage)
library(countrycode)
library(gganimate)
library(stringr)
library(lubridate)
library(htmltools)
library(extrafont)
library(showtext)

## import data
world_pop <- read_csv('countries_population.csv')


## remove the percentage formats
world_pop_cleaned <- 
  world_pop |> 
  mutate(across(contains("percent"), ~str_remove_all(., " %"))) |> 
  mutate(across(contains("urban"), ~str_remove_all(., ","))) |> 
  mutate(across(contains("percent"), ~str_squish(.))) |> 
  mutate(across(c("percent_yearly_change", "median_age", "fertility_rate"), 
                ~str_remove_all(., "(\\s+)"))) |> 
  mutate(across(c("percent_yearly_change", "median_age", "fertility_rate", "urban_population"),
                ~as.numeric(.x)))


## add continents


#world_pop_cleaned$country <- tolower(world_pop_cleaned$country)

world_pop_cleaned$continent <- countryname(sourcevar = world_pop_cleaned[["country"]],
                                        destination = "continent")


world_pop_cleaned$country_code <- countrycode(world_pop_cleaned$country, "country.name", "iso2c")


world_pop_cleaned <- 
  world_pop_cleaned |> 
  mutate(across(c("country", "continent"), ~as.factor(.)))

Africa_population <- 
  world_pop_cleaned |> 
  filter(continent == "Africa") |> 
  group_by(year) |> 
  mutate(rank_pop = rank(-population),
         Value_rel = population/population[rank_pop==1],
         Value_lbl = paste0(" ",round(population/1e3))
         ) |> 
  group_by(country) |> 
  filter(rank_pop <= 10) %>%
  ungroup() |> 
  select(country, year, population, rank_pop, Value_rel, Value_lbl, country_code)


## view(Africa_population)

## chart
staticplot = ggplot(Africa_population, aes(rank_pop, group = country,
                                       fill = country, 
                                       color = country)) +
  geom_tile(aes(y = population/2,
                height = population,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=population,label = Value_lbl, hjust=0)) +
  geom_flag(y=-10, aes(image = country_code))+
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="black", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="black"),
        plot.caption =element_text(size=12, hjust=0.5, face="italic", color="black"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))



anim = staticplot + transition_states(year, transition_length = 4, 
                                      state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'African Population per Year (1955-2020) : {closest_state}',
       subtitle  =  "Top 10 Countries",
       caption  = "Population in Thousands | Data Source: https://www.worldometers.info/world-population/")

animate(anim, 200, fps = 30,  width = 1200, height = 1000,
        duration = 10, end_pause = 90,
        renderer = gifski_renderer("pop_changes.gif"))

# 
# animate(anim, 200, fps = 30,  width = 1200, height = 1000,
#         duration = 10, end_pause = 90,
#         renderer = gifski_renderer("flagged_pop_changes.gif"))

#### kenya urban population over time
kenyan_pop <- 
  world_pop_cleaned |> 
  filter(continent == "Africa") |> 
  mutate(africa_pop = sum(population), .by = year) |> 
  mutate(africa_urban_pop = sum(urban_population), .by = year) |> 
  filter(country == "Kenya") |> 
  select(year, population, urban_population, africa_urban_pop, africa_pop) |> 
  mutate(urban_pop_percent = round(urban_population / population, 4),
         africa_urban_pop_prop = round(africa_urban_pop / africa_pop, 4)) |> 
  mutate(year = lubridate::date_decimal(year))

View(kenyan_pop)

sysfonts::font_add_google("Barlow","Barlow")
sysfonts::font_add_google("Roboto","Roboto")
#sysfonts::font_add_google(name = "Amatic SC", family = "amatic-sc")
sysfonts::font_add('fb', 'C:/Program Files/R/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('rc', 'C:/Program Files/R/fontawesome-free-6.4.0-desktop/otfs/Font Awesome 6 Brands-Regular-400.otf')
showtext::showtext_auto()
showtext::showtext_opts(dpi=300)

title=tagList(p("Kenya Urban Population: ",
                span("1955",style='color:#20A39E;'),
                " - ",
                span("2020", style="color:#20A39E;")
))

#create caption (not using htmltools - issue with Font Awesome Brands)
caption = paste0("<span style='font-family:Barlow;'>Source: Worldometer</span><br>",
                 "<span style='font-family:fb;'>&#xf099;</span>",
                 "<span style='font-family:Barlow;color:white;'>.</span>",
                 "<span style='font-family:Barlow;'>@cmwakibia</span>",
                 "<span style='font-family:Barlow;color:white;'>....</span>",
                 "<span style='font-family:fb;'>&#xf09b;</span>",
                 "<span style='font-family:Barlow;color:white;'>.</span>",
                 "<span style='font-family:Barlow;'>wakibia</span>"
)

limits = date(c("1954-01-01", "2020-01-01"))
kenyan_pop |> 
  mutate(year = ymd(year)) |> 
  ggplot(aes(year, urban_pop_percent, color = "#FFAC05"))+
  geom_line(linewidth = 2)+
  labs(title = title, x = "year", y = "Population Proportion (%)",
       caption = caption)+
  scale_x_date(date_breaks = '5 years', date_labels = '%Y', limits = limits)+
  scale_y_continuous(labels = scales::percent, limits = c(0, .30))+
  theme(
    text=element_text(family="rc"), 
    panel.background = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_textbox_simple(face="bold", size=12, margin=margin(b=5)),
    plot.caption = element_textbox_simple(color="#444444"),
    panel.grid.major.y = element_line(color="#C6C6C6", size=.4),
    axis.ticks = element_blank(),
    plot.margin  = ggplot2::margin(t=20, r=40, l=15, b=10),
    legend.position = 'none'
  )
  
ggsave("urban_population-trends.png", bg="white", units="in", width=9, height=6)



### Trend in Fertility Rate in Africa

Africa_population_rate <- 
  world_pop_cleaned |> 
  filter(continent == "Africa") |> 
  select(c(continent, country, year, fertility_rate, median_age))


## select on year 1955 and 2020

afri_pop_range <- 
  Africa_population_rate |> 
  select(-fertility_rate) |> 
  filter(year %in% range(year)) |> 
  ## transform to wide
  pivot_wider(names_from = year, values_from = median_age, names_prefix = 'year_') |> 
  ## drop NA's
  na.omit() |> 
  ## get the differennce
  mutate(age_diff = year_2020 - year_1955,
         age_diff_abs = abs(age_diff))

### top 20 countries with the highest median age diff between 1955 and 2020


top_20_countries <- 
  afri_pop_range |> 
  mutate(med_age_rank = rank(-age_diff_abs)) |> 
  filter(med_age_rank <= 20) |> 
  select(country, year_1955, year_2020, age_diff) |> 
  mutate(country = fct_reorder(country, year_2020 * if_else(age_diff < 0, -1, 1)))

showtext_auto(FALSE)


font_add_google('Merriweather', 'Merriweather')
showtext_auto()
showtext_opts(dpi = 300)


### set my chart theme
my_theme <- theme_minimal(base_size = 14, base_family = 'Merriweather') +
  theme(
    legend.position = 'none',
    plot.title.position = 'plot',
    text = element_text(color = 'grey40'),
    plot.title = element_textbox_simple(size = 14, margin = margin(b = 5, unit = 'mm'))
  )
theme_set(my_theme)

# Colors we will use later
color_palette <- thematic::okabe_ito(2)
names(color_palette) <- c(1955, 2020)

subtitle <- glue::glue(
  "Top 20 Countries With Highest Difference in Median Age Between <span style = 'color:{color_palette['1955']}'>1955</span> and <span style = 'color:{color_palette['2020']}'>2020</span>"
)

title <- glue::glue(
  "Median Age In African Countries between <span style = 'color:{color_palette['1955']}'>1955</span> and <span style = 'color:{color_palette['2020']}'>2020</span>"
)


top_countries <- unique(top_20_countries$country) |> as.character()

last_trial <- 
  Africa_population_rate |> 
  na.omit() |> 
  filter(year %in% range(year)) |> 
  mutate(year = factor(year)) |> 
  filter(country %in% top_countries)
  


top_20_countries |> 
  mutate(country = fct_reorder(country, age_diff)) |> 
  ggplot() +
    geom_segment(
      aes(y = country, yend = country, x = year_1955, xend = year_2020),
      col = 'grey60',
      size = 1.2
    ) +
    geom_point(
      data = last_trial,
      aes(x = median_age, y = country, col = year), size = 2.5
    ) +
    labs(
      x = str_to_upper('Median Age'),
      y = element_blank(),
      title = title,
      subtitle = subtitle,
      caption = 'Data: https://www.worldometers.info/world-population/population-by-country/'
    ) +
    scale_color_manual(values = color_palette) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.subtitle = element_textbox_simple(
        face ="italic", family="firasans", margin=margin(b=10)
      )
    ) +
    scale_x_continuous(expand = expansion(mult = 0.01))+
  coord_cartesian(xlim = c(15, 40), expand = TRUE)

ggsave('median_age.png', bg = "white", dpi = 300, 
       height = 7, width = 9, units = 'in')
