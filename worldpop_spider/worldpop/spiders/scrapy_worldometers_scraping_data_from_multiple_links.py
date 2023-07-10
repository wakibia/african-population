import scrapy


class WorldometersSpider(scrapy.Spider):
    name = 'worldometers_scraping_data_from_multiple_links_spider'
    allowed_domains = ['www.worldometers.info']
    start_urls = ['https://www.worldometers.info/world-population/population-by-country/']

    ## feed exports
    custom_settings = {
        'FEEDS': {
            'countries_population.csv': {
                'format': 'csv',
                'encoding': 'utf8',
                'overwrite': True,
            }
        }
    }


    def parse(self, response):
        # Extracting "a" elements for each country
        countries = response.xpath('//td/a')

        # Looping through the countries list
        for country in countries:
            country_name = country.xpath(".//text()").get()
            link = country.xpath(".//@href").get()

            # Absolute URL
            # absolute_url = f'https://www.worldometers.info/{link}'  # concatenating links with f-string
            # absolute_url = response.urljoin(link)  # concatenating links with urljoin
            # yield scrapy.Request(url=absolute_url)  # sending a request with the absolute url

            # Return relative URL (sending a request with the relative url)
            yield response.follow(url=link, callback=self.parse_country, meta={'country':country_name})

    # Getting data inside the "link" website
    def parse_country(self, response):
        # Getting country names and each row element inside the population table
        country = response.request.meta['country']
        rows = response.xpath("(//table[contains(@class,'table')])[1]/tbody/tr")  # You can also use the whole class value  --> response.xpath('(//table[@class="table table-striped table-bordered table-hover table-condensed table-list"])[1]/tbody/tr')
        # Looping through the rows list
        for row in rows:
            year = row.xpath(".//td[1]/text()").get()
            population = row.xpath(".//td[2]/strong/text()").get()
            yearly_change = row.xpath(".//td[3]/text()").get()
            median_age = row.xpath(".//td[6]/text()").get()
            fertility_rate = row.xpath(".//td[7]/text()").get()
            urban_population = row.xpath(".//td[10]/text()").get()


            # Return data extracted
            yield {
                'country': country,
                'year': year,
                'population': population,
                'link': response.url,
                'percent_yearly_change': yearly_change,
                'median_age': median_age,
                'fertility_rate': fertility_rate,
                'urban_population': urban_population,
            }


