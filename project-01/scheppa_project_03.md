---
title: "Visualizing Text and Distributions"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---

# Data Visualization Project 03


In this exercise you will explore methods to visualize text data and practice how to recreate charts that show the distributions of a continuous variable. 


## Part 1: Density Plots


```r
library(tidyverse)
library(RColorBrewer)
library(ggridges)
library(tidytext)
library(igraph)
library(ggraph)
```


Using the dataset obtained from FSU's [Florida Climate Center](https://climatecenter.fsu.edu/climate-data-access-tools/downloadable-data), for a station at Tampa International Airport (TPA) from 2016 to 2017, attempt to recreate the charts shown below


```r
library(tidyverse)
weather_tpa <- read_csv("https://github.com/reisanar/datasets/raw/master/tpa_weather_16_17.csv")
# random sample 
sample_n(weather_tpa, 4)
```

```
## # A tibble: 4 x 6
##    year month   day precipitation max_temp min_temp
##   <dbl> <dbl> <dbl>         <dbl>    <dbl>    <dbl>
## 1  2016     3    11           0         85       68
## 2  2016    10    25           0         80       64
## 3  2016     5    17           0.4       87       73
## 4  2016     4    11           0         84       66
```


```r
weather_tpa %>% 
  mutate(month = month.name[weather_tpa$month] )
```

```
## # A tibble: 367 x 6
##     year month     day precipitation max_temp min_temp
##    <dbl> <chr>   <dbl>         <dbl>    <dbl>    <dbl>
##  1  2016 January     1          0          81       70
##  2  2016 January     2          0          73       59
##  3  2016 January     3          0.18       61       50
##  4  2016 January     4          0          66       49
##  5  2016 January     5          0          68       49
##  6  2016 January     6          0          67       54
##  7  2016 January     7          0          72       56
##  8  2016 January     8          0.54       76       63
##  9  2016 January     9          0.65       78       62
## 10  2016 January    10          0          72       56
## # ... with 357 more rows
```



```r
pal = c("#440D54","#482073","#433E85","#38598C","#2D708E","#25858E","#1E9B8A","#2CB07F","#51C56A","#85D54A","#C2DF23","#FDE725")
```

See https://www.reisanar.com/slides/relationships-models#10 for a reminder on how to use this dataset with the `lubridate` package for dates and times.


(a) Recreate the plot below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_facet.png" width="80%" style="display: block; margin: auto;" />

Hint: the option `binwidth = 3` was used with the `geom_histogram()` function.

<img src="scheppa_project_03_files/figure-html/unnamed-chunk-6-1.png" width="80%" style="display: block; margin: auto;" />

(b) Recreate the plot below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_density.png" width="80%" style="display: block; margin: auto;" />

Hint: check the `kernel` parameter of the `geom_density()` function, and use `bw = 0.5`.

<img src="scheppa_project_03_files/figure-html/unnamed-chunk-8-1.png" width="80%" style="display: block; margin: auto;" />


(c) Recreate the chart below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_density_facet.png" width="80%" style="display: block; margin: auto;" />

Hint: default options for `geom_density()` were used. 

<img src="scheppa_project_03_files/figure-html/unnamed-chunk-10-1.png" width="80%" style="display: block; margin: auto;" />

(d) Recreate the chart below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_ridges.png" width="80%" style="display: block; margin: auto;" />

Hint: default options for `geom_density()` were used. 

<img src="scheppa_project_03_files/figure-html/unnamed-chunk-12-1.png" width="80%" style="display: block; margin: auto;" />

(e) Recreate the plot below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_ridges.png" width="80%" style="display: block; margin: auto;" />

Hint: use the`ggridges` package, and the `geom_density_ridges()` function paying close attention to the `quantile_lines` and `quantiles` parameters.

<img src="scheppa_project_03_files/figure-html/unnamed-chunk-14-1.png" width="80%" style="display: block; margin: auto;" />



(f) Recreate the chart below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_ridges_plasma.png" width="80%" style="display: block; margin: auto;" />

Hint: this uses the `plasma` option (color scale) for the _viridis_ palette.

<img src="scheppa_project_03_files/figure-html/unnamed-chunk-16-1.png" width="80%" style="display: block; margin: auto;" />




## Part 2: Visualizing Text Data

Review the set of slides (and additional resources linked in it) for visualizing text data: https://www.reisanar.com/slides/text-viz#1

Choose any dataset with text data, and create at least one visualization with it. For example, you can create a frequency count of most used bigrams, a sentiment analysis of the text data, a network visualization of terms commonly used together, and/or a visualization of a topic modeling approach to the problem of identifying words/documents associated to different topics in the text data you decide to use. 

Make sure to include a copy of the dataset in the `data/` folder, and reference your sources if different from the ones listed below:

- [Billboard Top 100 Lyrics](https://github.com/reisanar/datasets/blob/master/BB_top100_2015.csv)

- [RateMyProfessors comments](https://github.com/reisanar/datasets/blob/master/rmp_wit_comments.csv)

- [FL Poly News 2020](https://github.com/reisanar/datasets/blob/master/poly_news_FL20.csv)

- [FL Poly News 2019](https://github.com/reisanar/datasets/blob/master/poly_news_FL19.csv)

(to get the "raw" data from any of the links listed above, simply click on the `raw` button of the GitHub page and copy the URL to be able to read it in your computer using the `read_csv()` function)



```r
top_100 <- read.csv("https://raw.githubusercontent.com/reisanar/datasets/master/BB_top100_2015.csv")
```


```r
top_15 <- top_100 %>%
  filter(Rank %in% 1:15) %>%
  unnest_tokens(word, Lyrics, token = "words") %>%
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))

top_15 %>%
  group_by(word) %>%
  summarise(occurs = n()) %>%
  arrange(desc(occurs = n()))
```

```
## # A tibble: 757 x 2
##    word      occurs
##    <chr>      <int>
##  1 70s            1
##  2 aah            2
##  3 abel           1
##  4 absolutes      1
##  5 accent         1
##  6 accused        1
##  7 acoustic       1
##  8 affection      1
##  9 afraid         1
## 10 againaah       2
## # ... with 747 more rows
```


```r
top_15 %>%
  inner_join(get_sentiments("bing")) %>%
  count(Song, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot() + 
  geom_bar(aes(x = sentiment, 
               y = reorder(Song, sentiment)), 
           stat = "identity") + 
  labs(y = "", 
       title = "Top 15 Songs of 2015",
       subtitle = "Using Sentiment Analysis",
       caption = "Source: Billboards top 100 Lyrics") + 
  theme_minimal()
```

```
## Joining, by = "word"
```

![](scheppa_project_03_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
