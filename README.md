In this repo we scrape data from [worldometer](https://www.worldometers.info/world-population/), which contains the latest world population by country upto **2020**

The spider used to extract the data run on scrapy basic spider, to run the spider run the following

`scrapy crawl worldometers_scraping_data_from_multiple_links_spider`

Our focus is the trend in changes of the African population

Extracted data was preprocessed in R and data visualization was performed using R `ggplot2`

We explored the population changes between **1955 - 2020**. We also explored the trend of the kenya urban population over the same period.


## African Population Trend between 1955 - 2020: Top 10 Countries

![top_10_countries](https://github.com/wakibia/african-population/blob/master/img/pop_changes.gif)

## Kenyan Urban Population Trend between 1955 - 2020

![kenyan_urban_population](https://github.com/wakibia/african-population/blob/master/img/urban_population-trends.png)


## African Median Age Between 1955 - 2020

![African Median Aged](https://github.com/wakibia/african-population/blob/master/img/median_age.png)

- Median age is highest in Mauritius and Reunion both of which are islands - increasing significantly from 1955 to 2020
- Median Age is Lowest in West African Countries (Chad and Male) and Central African Countries( Sao Tome, and Principles, Central African Republic, and Gabon - decreasing significantly from in 2020
- Among the countries, Chad had the youngest median age in 2020

## Fertility Rate Change in Africa Between 1955 - 2020

![Fertility Rate](https://github.com/wakibia/african-population/blob/master/img/fertility_rate.png)

- we could expect the fertility rate to decrease in the countries - The data indicate that in fact, the fertility rate has actually decreased
- However, some countries experienced a very high decrease in the fertility rate from 1955 to 2020 while others had a very low decrease
- In the fertility chart above, the top 20 countries with the lowest difference in fertility rate in 1955 and 2020
- All the countries have a fertility rate higher than 3.5 for both 1955 and 2020
- Gabon and DR Congo had the smallest change in fertility


## Urban Population Changes between 1960 and 2019
![Urban Population - Slope Graph](https://github.com/wakibia/african-population/blob/master/img/urban-pop-slope.png) 
