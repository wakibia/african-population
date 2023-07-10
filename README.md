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
