<img src="hex-instascraper.png" align="right" width="100" height="100">

Instascraper
==============

<!-- badges: start -->
<!-- badges: end -->

Instacsraper is an R helper function package for using Instagram Scraper

Installation
--------------

You can install the released version of instascraper from [Github](https://github.com/mohrosidi/instascraper) with:

``` r
devtools::install_github("mohrosidi/instascraper")
```

``` r
# run this if instagram-scraper is not installed in your system
instascraper_install()
```

Example
---------------

This is a basic example which shows you how to solve a common problem:

``` r
library(instascraper)

## get 10 media and media metadata from Cristiano Ronaldo account 
get_media(username = "cristiano", media_type = "image", max = 10)

## get all comment data from IG post in Bandung City
get_comments(location = 100936334818444)

## get user profile information from user following data
get_profiles(following_input = TRUE, your_username = <YOUR ACCOUNT USERNAME>,
            your_password = <YOUR ACCOUNT PASSWORD>)
            
## get location id using location name
search_location("Bandung")
```

