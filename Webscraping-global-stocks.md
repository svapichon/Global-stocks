Webscraping global stocks
================
June 18, 2022

### List of the largest global companies by market cap:

#### <https://companiesmarketcap.com/>

Packages:

``` r
library(dplyr)
library(rvest)
library(stringr)
library(purrr)
```

For nested webpages:

``` r
get_PEratio <- function(PEratio_link){
    PEratio_page <- read_html(PEratio_link)
    number <- PEratio_page %>% 
        html_nodes(".background-ya") %>% 
        html_text()
    return(number)
}
get_category <- function(PEratio_link){
    PEratio_page <- read_html(PEratio_link)
    number <- PEratio_page %>% 
        html_nodes(".category-badge") %>% 
        html_text() %>% 
        paste(collapse = ",") %>%
        gsub("[^\x01-\x7F]", "", .) %>% # Remove non-ASCII characters
        str_replace_all("\n", "") %>% # Remove special characters
        str_trim() # Remove end spaces
    return(number)
}
get_oneDay <- function(PEratio_link){
    PEratio_page <- read_html(PEratio_link)
    number <- PEratio_page %>% 
        html_nodes("div.line1") %>% 
        html_text()
    return(number[[5]])
}
get_yoy <- function(PEratio_link){
    PEratio_page <- read_html(PEratio_link)
    number <- PEratio_page %>% 
        html_nodes("div.line1") %>% 
        html_text() 
    return(number[[6]])
}
```

Set empty data frame for iterative pages:

``` r
globalStocks <- data.frame()
```

Set the number of pages:

``` r
for(page_result in seq(from = 1, to = 2)){
    link <- paste0("https://companiesmarketcap.com/page/", 
                   page_result, "/")
    page <- read_html(link)
    
    company <- page %>% 
        html_nodes(".company-name") %>%
        html_text()
    ticker <- page %>% 
        html_nodes("div.company-code") %>% 
        html_text()
    marketCap <- page %>% 
        html_nodes(".name-td+ .td-right") %>% 
        html_text()
    price <- page %>% 
        html_nodes(".td-right+ .td-right") %>% 
        html_text()
    country <- page %>% 
        html_nodes(".responsive-hidden") %>% 
        html_text()
    country2 <- data.frame(country)
    country2 <- country2[-c(1:3), ] # Remove errors in first 3 rows
    
    PEratio_link <- page %>% 
        html_nodes("div.name-div > a") %>% 
        html_attr("href") %>% 
        paste0("https://companiesmarketcap.com", .) %>% 
        gsub("/marketcap", "/pe-ratio", .)
    
    # Use PEratio <- sapply(PEratio_link, FUN = get_PEratio, USE.NAMES = F)
    # Or use map() if Error in open.connection(x, "rb") : HTTP error 404.
    PEratio_possibly <- possibly(get_PEratio, otherwise = "This page could not be accessed.")
    PEratio <- map(PEratio_link, PEratio_possibly)
    PEratio2 <- as.numeric(PEratio) # Coerce list into numeric vector
    category_possibly <- possibly(get_category, otherwise = "This page could not be accessed.")
    category <- map(PEratio_link, category_possibly)
    category2 <- as.character(category) # Coerce list into character vector
    oneDay_possibly <- possibly(get_oneDay, otherwise = "This page could not be accessed.")
    oneDay <- map(PEratio_link, oneDay_possibly)
    oneDay2 <- as.character(oneDay) # Coerce list into character vector
    yoy_possibly <- possibly(get_yoy, otherwise = "This page could not be accessed.")
    yoy <- map(PEratio_link, yoy_possibly)
    yoy2 <- as.character(yoy) # Coerce list into character vector
    
    globalStocks <- rbind(globalStocks, data.frame(company, ticker, marketCap, oneDay2, yoy2, price,
                                                   PEratio2, country2, category2,
                                                   stringsAsFactors = F))
    
    print(paste("Page", page_result)) # For tracking pages when running
}
```

Final cleaning:

``` r
globalStocks2 <- globalStocks %>% 
    rename(Company = company, Ticker = ticker, "Market Cap" = marketCap, 
           "One-day" = oneDay2, "One-year" = yoy2, Price = price, 
           "PE ratio" = PEratio2, Country = country2, Category = category2)
```

View data:

``` r
head(globalStocks2)
```

    ##             Company  Ticker Market Cap One-day One-year   Price PE ratio
    ## 1      Saudi Aramco 2222.SR   $2.257 T   0.13%    9.03%  $10.26    20.90
    ## 2             Apple    AAPL   $2.129 T   1.15%    0.84% $131.56    20.90
    ## 3         Microsoft    MSFT   $1.852 T   1.09%   -4.54% $247.65    25.40
    ## 4 Alphabet (Google)    GOOG   $1.416 T   1.15%  -14.10%  $2,157    19.00
    ## 5            Amazon    AMZN   $1.080 T   2.47%      N/A $106.22     2.45
    ## 6             Tesla    TSLA  $673.69 B   1.72%    4.33% $650.28    77.10
    ##     Country
    ## 1 S. Arabia
    ## 2       USA
    ## 3       USA
    ## 4       USA
    ## 5       USA
    ## 6       USA
    ##                                                                                         Category
    ## 1                                                                                Energy, Oil&Gas
    ## 2                                          Tech, Software, Dow jones, Tech Hardware, Electronics
    ## 3                                                         Software, Tech, Dow jones, Video games
    ## 4                                                   Internet, Tech, Software, Autonomous driving
    ## 5                                                             E-Commerce, Internet, Tech, Retail
    ## 6 Automakers, Tech, Electric Vehicles,  Bitcoin,  Manufacturing, EV charging, Autonomous driving

Write CSV:

``` r
write.csv(globalStocks2, "Global Stocks.csv")
```
