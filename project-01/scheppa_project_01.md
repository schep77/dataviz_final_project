---
title: "Mini-Project 01"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.0.5
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 4.0.5
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
```

```
## v ggplot2 3.3.5     v purrr   0.3.4
## v tibble  3.1.5     v stringr 1.4.0
## v tidyr   1.1.4     v forcats 0.5.1
## v readr   2.0.2
```

```
## Warning: package 'ggplot2' was built under R version 4.0.5
```

```
## Warning: package 'tibble' was built under R version 4.0.5
```

```
## Warning: package 'tidyr' was built under R version 4.0.5
```

```
## Warning: package 'readr' was built under R version 4.0.5
```

```
## Warning: package 'forcats' was built under R version 4.0.5
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(tidyr)
library(ggplot2)
library(scales)
```

```
## 
## Attaching package: 'scales'
```

```
## The following object is masked from 'package:purrr':
## 
##     discard
```

```
## The following object is masked from 'package:readr':
## 
##     col_factor
```

```r
library(RColorBrewer)
library(plotly)
```

```
## Warning: package 'plotly' was built under R version 4.0.5
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

<br>

---

<br>


# Introduction

Aviation saftey is a topic that hits close to home, as I recently lost a co-worker in an airplane crash. For this project, my goal is to explore airplane crashes worldwide to find out how often they are occurring, what manufacturer has the most, and how many fatalities are there as a result. For my project, I am using the `Airplane Crash Data Since 1908` dataset, which is a collection of the number of crashes from 1908 till 2019.

## The Dataset


```r
#My data 
crashes <- read.csv("../data/airplanedata.csv")
head(crashes)
```

```
##         Date  Time                           Location               Operator
## 1  9/17/1908 17:18                Fort Myer, Virginia   Military - U.S. Army
## 2   9/7/1909                  Juvisy-sur-Orge, France                       
## 3  7/12/1912  6:30          Atlantic City, New Jersey   Military - U.S. Navy
## 4   8/6/1913       Victoria, British Columbia, Canada                Private
## 5   9/9/1913 18:30                 Over the North Sea Military - German Navy
## 6 10/17/1913 10:30         Near Johannisthal, Germany Military - German Navy
##   Flight..         Route                AC.Type Registration cn.ln Aboard
## 1          Demonstration       Wright Flyer III                  1      2
## 2               Air show         Wright Byplane          SC1            1
## 3            Test flight              Dirigible                         5
## 4                              Curtiss seaplane                         1
## 5                        Zeppelin L-1 (airship)                        20
## 6                        Zeppelin L-2 (airship)                        30
##   Aboard.Passangers Aboard.Crew Fatalities Fatalities.Passangers
## 1                 1           1          1                     1
## 2                 0           1          1                     0
## 3                 0           5          5                     0
## 4                 0           1          1                     0
## 5              NULL        NULL         14                  NULL
## 6              NULL        NULL         30                  NULL
##   Fatalities.Crew Ground
## 1               0      0
## 2               0      0
## 3               5      0
## 4               1      0
## 5            NULL      0
## 6            NULL      0
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Summary
## 1 During a demonstration flight, a U.S. Army flyer flown by Orville Wright nose-dived into the ground from a height of approximately 75 feet, killing Lt. Thomas E. Selfridge, 26, who was a passenger. This was the first recorded airplane fatality in history.  One of two propellers separated in flight, tearing loose the wires bracing the rudder and causing the loss of control of the aircraft.  Orville Wright suffered broken ribs, pelvis and a leg.  Selfridge suffered a crushed skull and died a short time later.
## 2                                                                                                                                                                                                                                                                                                                                                                                                 Eugene Lefebvre was the first pilot to ever be killed in an air accident, after his controls jambed while flying in an air show.
## 3                                                                                                                                                                                                                                                                                                                                                                                                                              First U.S. dirigible Akron exploded just offshore at an altitude of 1,000 ft. during a test flight.
## 4                                                                                                                                                                                                                                                                                                                                                                                                   The first fatal airplane accident in Canada occurred when American barnstormer, John M. Bryant, California aviator was killed.
## 5                                                                                                                                                                                                                                                                                                             The airship flew into a thunderstorm and encountered a severe downdraft crashing 20 miles north of Helgoland Island into the sea. The ship broke in two and the control car immediately sank drowning its occupants.
## 6                                                                                                                                                                                                                                                                                                                                                                                         Hydrogen gas which was being vented was sucked into the forward engine and ignited causing the airship to explode and burn at 3,000 ft..
```

<br>

---

<br>


# Data Wrangling and Exploration


```r
#Separating the Date column into 3 different ones to perform a better analysis
df <- separate(data = crashes,col = Date, into = c("Month", "Day", "Year"), sep = "\\/")
#Selecting the columns that I need to perform my analysis
date_df <- select(df, c(Month, Year, Fatalities, Aboard, AC.Type))
```


```r
#Changing the data type of fatalities to a numeric
date_df$Fatalities <- as.numeric(date_df$Fatalities)
```

```
## Warning: NAs introduced by coercion
```

```r
#Changing all the NAs to 0s
date_df[is.na(date_df)] <- 0
#Changing the data type of year to a numeric
date_df$Year <- as.numeric(date_df$Year)
```


```r
#Creating a new column in our dataset called decade
date_df <- date_df %>% 
  mutate(Decade = floor(Year/10) * 10)
```


```r
#Summing all the fatalities per Year
year_df <- date_df %>% 
  group_by(Year) %>% 
  summarise(Fatalities = sum(Fatalities))
```


```r
#Summing all the fatalities per Decade
decade_df <- date_df %>% 
  group_by(Decade) %>% 
  summarise(Fatalities = sum(Fatalities))
```


```r
#Creating a new dataset, that has no NAs
df_country <- crashes
df_country[is.na(df_country)] <- ""

#grouping them by Location, and tallying them up, and selecting the locations that had more than 10 crashes
df_country1 <- df_country %>% 
  group_by(Location) %>% 
  tally() %>% 
  filter(n >= 10)

#splitting the location couling into city, region, and country
df_country1$Loc <- df_country1$Location
df_country1 <- separate(data = df_country1, col = Location, into = c("City", "Region", "Country"), sep = "\\,")
df_country1[is.na(df_country1)] <- ""
```



```r
#Sub-dataset 
small_DF <- select(crashes, c(AC.Type, Aboard))
#Changing Aboard to a numeric
small_DF$Aboard <- as.numeric(small_DF$Aboard)
```

```
## Warning: NAs introduced by coercion
```

```r
small_DF <- small_DF %>%
  #Getting the first word of the manufacturer to use for later analysis
  separate(col = AC.Type, into = c("Manufacturer"), sep = "\\ ") %>% 
  #grouping by the manufacturer
  group_by(Manufacturer) %>% 
  #adding up the total aboard per manufacturer
  summarise(x = Aboard) %>% 
  arrange(desc(x)) %>% 
  #filtering out flights that had less than 250 because I felt that would skew the results
  filter(x > 250)
```

```
## Warning: Expected 1 pieces. Additional pieces discarded in 4896 rows [1, 2, 4,
## 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, ...].
```

```
## `summarise()` has grouped output by 'Manufacturer'. You can override using the `.groups` argument.
```

```r
small_DF <- small_DF %>%
  group_by(Manufacturer) %>% 
  summarise(x = sum(x)) %>% 
  arrange(desc(x)) %>% 
  filter(x > 250)

small_DF
```

```
## # A tibble: 5 x 2
##   Manufacturer     x
##   <chr>        <dbl>
## 1 Boeing        6590
## 2 McDonnell     3853
## 3 Airbus        1990
## 4 Lockheed      1214
## 5 Ilyushin       532
```


```r
#Changing the data type of Aboard to a numeriv
date_df$Aboard <- as.numeric(date_df$Aboard)
```

```
## Warning: NAs introduced by coercion
```

```r
df_fatalityRate <- date_df %>%
  separate(col = AC.Type, into = c("Manufacturer"), sep = "\\ ") %>% 
  #filtering for manufacturers that are in the dataset I created in the last chunk
  filter(Manufacturer %in% small_DF$Manufacturer) %>%
  group_by(Manufacturer, Year) %>% 
  #summing the totals across all flights per manufacturer 
  #I did it this way as I believe that this is more accurate then taking the average.
  summarise(total_Fatalities = sum(Fatalities), total_Aboard = sum(Aboard)) %>% 
  #calculating the fatality rate
  mutate(Fatality_Rate = (total_Fatalities/total_Aboard) * 100)
```

```
## Warning: Expected 1 pieces. Additional pieces discarded in 4896 rows [1, 2, 4,
## 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, ...].
```

```
## `summarise()` has grouped output by 'Manufacturer'. You can override using the `.groups` argument.
```

```r
df_fatalityRate
```

```
## # A tibble: 294 x 5
## # Groups:   Manufacturer [5]
##    Manufacturer  Year total_Fatalities total_Aboard Fatality_Rate
##    <chr>        <dbl>            <dbl>        <dbl>         <dbl>
##  1 Airbus        1976                7          258          2.71
##  2 Airbus        1987                5            5        100   
##  3 Airbus        1988              293          426         68.8 
##  4 Airbus        1990               92          146         63.0 
##  5 Airbus        1992              367          376         97.6 
##  6 Airbus        1993                2           70          2.86
##  7 Airbus        1994              349          752         46.4 
##  8 Airbus        1995               60           60        100   
##  9 Airbus        1997              234          234        100   
## 10 Airbus        1998              298          469         63.5 
## # ... with 284 more rows
```
<br>

---

<br>

# Data Analysis

## Has the number of airplane creashes increased or decreased over the years/decades?


```r
#creating the plot
ggplot() +
  
  #adding the line that represents fatalities per year
  geom_line(year_df, mapping = aes(x = Year, y = Fatalities), color = "Blue")+
  
  #adding the line that represents fatalities per decade
  geom_line(decade_df, mapping = aes(x = Decade, y = Fatalities/10), color = "Red")+
  
  #adding title, subtitle, and caption
  labs(title = "Fatalities by Year and Decade",
       subtitle = "Worldwide Airplane Fatalities from 1908 till 2019",
       caption = "Data Source: Kaggle.com") +
  geom_smooth()+
  
  #making the dual axis, and adding commas to the y-axis scales
  scale_y_continuous(
    name = "Annual Fatalities",
    labels = scales::comma,
    sec.axis = sec_axis(~.*10, name = "Decade Fatalities", labels = scales::comma))+
  
  theme_minimal() +
  
  #making the coloring of the y-axis match the color of their respective lines
  theme(axis.text.y.left = element_text(color = "blue")) +
  theme(axis.text.y.right = element_text(color = "red")) +
  
  #positioning the title left aligned, not aligned with the plot
  theme(plot.title.position = "plot")
```

![](lastname_project_01_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
#For this graph, I tried adding the colors to the title of the graph to match the lines, but i could not figure it out.
```
> In this graph, you can see that based on the years, fatalities peaked just before 1975. It then had a slight decrease. Based off of the decades, it is showing the peak fatalities around the same time, but it shows that overall the Fatalities were steadly decreasing.

<br>

---

<br>

## What Regions have the most amount of crashes?


```r
df_country1 %>% 
  ggplot(aes(x = reorder(Loc, n), y = n, fill = Region)) +
    geom_col() +
    scale_fill_brewer(palette = "Blues")+
    coord_flip() +
    labs(title = "Regions with the Most Crashes",
         y = "Number of Crashes", 
         x = "") +
    geom_text(aes(label = n), hjust = -0.5)+
  theme_minimal() +
  guides(fill = FALSE) +
  theme(plot.title.position = "plot")
```

![](lastname_project_01_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
> This graph was shows the regions with the most common plane crashes. I think it is intresting that Russia has a decent size gap over New York. I say decent because as I was looking at this graph I noticed that there was a location called "near Moscow", so adding that to the other bar of Russia, it has a total of 28 plane crashes.

<br>

---

<br>

## What is the Fatality Rate per Year by Manufacturer?



```r
#The commented out lines didn't work. I believe it has to do with me using plotly. But if I wasnt, I would have left aligned my title, and added a caption.
f <- df_fatalityRate %>% 
  ggplot(data = df_fatalityRate, mapping = aes(x = Manufacturer, y = Year, fill = Fatality_Rate))+
  labs(title = "Fatality Rate by Manufacturer") +
       #caption = "Data Source: kaggle.com") +
  geom_tile() +
  guides(fill = FALSE)
  #theme(plot.title.position = "plot")
ggplotly(f)
```

<!--html_preserve--><div id="htmlwidget-a97d56f4a375ff7d8315" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-a97d56f4a375ff7d8315">{"x":{"data":[{"x":[1,2,3,4,5],"y":[1928,1929,1930,1931,1932,1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,1949,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019],"z":[[null,0.5,null,null,null],[null,null,null,0.9,null],[null,1,null,1,null],[null,1,null,0.705882352941177,null],[null,0.777777777777778,null,null,null],[null,0.727272727272727,null,1,null],[null,1,null,0.857142857142857,null],[null,1,null,1,null],[null,1,null,1,null],[null,0.384615384615385,null,1,null],[null,null,null,0.758620689655172,null],[null,1,null,0.818181818181818,null],[null,1,null,0.872340425531915,null],[null,null,null,0.833333333333333,null],[null,1,null,1,null],[null,0.802816901408451,null,1,null],[null,null,null,0.961904761904762,null],[null,1,null,0.574468085106383,null],[null,1,null,0.75,null],[null,0,0.36,0.78,null],[null,1,1,0.636842105263158,null],[null,1,0.974358974358974,0.842857142857143,null],[null,0.777777777777778,1,1,null],[null,1,1,1,null],[null,0.803030303030303,1,0.80952380952381,null],[null,0.6875,0.859154929577465,0.574324324324324,null],[null,0.777777777777778,1,0.804651162790698,null],[null,0.424242424242424,null,0.932203389830508,null],[null,0.072463768115942,1,0.81021897810219,null],[null,1,0.955555555555556,null,null],[null,0.329545454545455,1,0.931297709923664,null],[null,1,1,0.796934865900383,null],[null,null,1,0.926356589147287,null],[null,1,0.682926829268293,0.894894894894895,0.283737024221453],[null,0.996363636363636,0.666666666666667,0.833333333333333,0.547738693467337],[null,1,0.7375,0.818181818181818,1],[null,0.821656050955414,1,0.942857142857143,1],[null,0.503865979381443,0.69047619047619,0.735537190082645,null],[null,1,1,0.757446808510638,0.911111111111111],[null,0.703389830508475,0.815384615384615,0.635294117647059,0.59375],[null,0.49469964664311,0.751445086705202,0.992481203007519,0.0592885375494071],[null,0.624074074074074,0.766423357664234,1,0.767441860465116],[null,0.367816091954023,0.82051282051282,0.951871657754011,0.625632377740304],[null,0.614886731391586,0.681818181818182,0.993055555555556,0.980392156862745],[null,0.596,1,0.596638655462185,0.941520467836257],[null,0.624605678233438,1,0.976190476190476,0.492],[null,0.586785009861933,0.923076923076923,1,0.982758620689655],[null,0.682326621923937,0.986486486486486,0.521621621621622,0.625],[0.0271317829457364,0.777542372881356,0.969849246231156,1,1],[null,0.89989350372737,0.888888888888889,1,0.583333333333333],[null,0.570200573065903,0.882352941176471,null,0.343855693348365],[null,0.697819314641745,1,1,0.803237858032379],[null,0.499278499278499,1,0.618610747051114,1],[null,0.883870967741936,1,0.616438356164384,0.871212121212121],[null,0.372614107883817,0.516949152542373,1,0.0974358974358975],[null,0.87043795620438,1,0.122065727699531,0.494949494949495],[null,0.0172413793103448,1,1,0.0220994475138122],[null,0.620212765957447,1,0.890909090909091,1],[null,0.283138918345705,1,0.590116279069767,null],[1,0.915766738660907,1,0.942857142857143,0.471910112359551],[0.687793427230047,0.596934174932372,0.9581589958159,1,1],[null,0.493518518518519,0.70096463022508,0.878787878787879,0.625437572928822],[0.63013698630137,0.336076817558299,1,0.818181818181818,0.328671328671329],[null,0.84160756501182,0.684210526315789,0.963888888888889,1],[0.976063829787234,null,1,0.377398720682303,0.171511627906977],[0.0285714285714286,0.698630136986301,null,1,0.058695652173913],[0.464095744680851,0.423005565862709,null,0.917808219178082,0.123809523809524],[1,0.70042194092827,null,0.981481481481482,0.981818181818182],[null,0.96377358490566,1,0.947368421052632,0.232774674115456],[1,0.420752565564424,null,0.942622950819672,1],[0.635394456289979,0.944206008583691,1,1,1],[0.00529100529100529,0.27338782924614,0.714285714285714,0.208695652173913,0.058361391694725],[0.261261261261261,0.527450980392157,1,1,0.508379888268156],[0.460992907801418,0.943462897526502,1,null,null],[null,0.76813880126183,1,0.740740740740741,1],[null,0.942257217847769,1,1,1],[null,1,0.523809523809524,null,null],[0,0.583835946924005,1,1,0.992592592592592],[0.753164556962025,0.940959409594096,null,0.8,null],[1,0.40567612687813,null,null,0.786096256684492],[0.117647058823529,0.466275659824047,null,null,0.599221789883268],[0.708955223880597,1,0.217142857142857,1,0.555555555555556],[0.996168582375479,0.650385604113111,1,1,1],[null,0.749090909090909,null,1,null],[null,0.969465648854962,null,null,1],[1,0.127118644067797,null,null,null],[1,1,1,0.987012987012987,null],[1,null,null,1,null],[0.485507246376812,0.0163398692810458,null,null,null],[null,1,null,1,null],[null,0.485623003194888,1,1,null],[null,0.994318181818182,null,null,null]],"text":[[null,"Manufacturer: Boeing<br />Year: 1928<br />Fatality_Rate:  50.0000000",null,null,null],[null,null,null,"Manufacturer: Lockheed<br />Year: 1929<br />Fatality_Rate:  90.0000000",null],[null,"Manufacturer: Boeing<br />Year: 1930<br />Fatality_Rate: 100.0000000",null,"Manufacturer: Lockheed<br />Year: 1930<br />Fatality_Rate: 100.0000000",null],[null,"Manufacturer: Boeing<br />Year: 1931<br />Fatality_Rate: 100.0000000",null,"Manufacturer: Lockheed<br />Year: 1931<br />Fatality_Rate:  70.5882353",null],[null,"Manufacturer: Boeing<br />Year: 1932<br />Fatality_Rate:  77.7777778",null,null,null],[null,"Manufacturer: Boeing<br />Year: 1933<br />Fatality_Rate:  72.7272727",null,"Manufacturer: Lockheed<br />Year: 1933<br />Fatality_Rate: 100.0000000",null],[null,"Manufacturer: Boeing<br />Year: 1934<br />Fatality_Rate: 100.0000000",null,"Manufacturer: Lockheed<br />Year: 1934<br />Fatality_Rate:  85.7142857",null],[null,"Manufacturer: Boeing<br />Year: 1935<br />Fatality_Rate: 100.0000000",null,"Manufacturer: Lockheed<br />Year: 1935<br />Fatality_Rate: 100.0000000",null],[null,"Manufacturer: Boeing<br />Year: 1936<br />Fatality_Rate: 100.0000000",null,"Manufacturer: Lockheed<br />Year: 1936<br />Fatality_Rate: 100.0000000",null],[null,"Manufacturer: Boeing<br />Year: 1937<br />Fatality_Rate:  38.4615385",null,"Manufacturer: Lockheed<br />Year: 1937<br />Fatality_Rate: 100.0000000",null],[null,null,null,"Manufacturer: Lockheed<br />Year: 1938<br />Fatality_Rate:  75.8620690",null],[null,"Manufacturer: Boeing<br />Year: 1939<br />Fatality_Rate: 100.0000000",null,"Manufacturer: Lockheed<br />Year: 1939<br />Fatality_Rate:  81.8181818",null],[null,"Manufacturer: Boeing<br />Year: 1940<br />Fatality_Rate: 100.0000000",null,"Manufacturer: Lockheed<br />Year: 1940<br />Fatality_Rate:  87.2340426",null],[null,null,null,"Manufacturer: Lockheed<br />Year: 1941<br />Fatality_Rate:  83.3333333",null],[null,"Manufacturer: Boeing<br />Year: 1942<br />Fatality_Rate: 100.0000000",null,"Manufacturer: Lockheed<br />Year: 1942<br />Fatality_Rate: 100.0000000",null],[null,"Manufacturer: Boeing<br />Year: 1943<br />Fatality_Rate:  80.2816901",null,"Manufacturer: Lockheed<br />Year: 1943<br />Fatality_Rate: 100.0000000",null],[null,null,null,"Manufacturer: Lockheed<br />Year: 1944<br />Fatality_Rate:  96.1904762",null],[null,"Manufacturer: Boeing<br />Year: 1945<br />Fatality_Rate: 100.0000000",null,"Manufacturer: Lockheed<br />Year: 1945<br />Fatality_Rate:  57.4468085",null],[null,"Manufacturer: Boeing<br />Year: 1946<br />Fatality_Rate: 100.0000000",null,"Manufacturer: Lockheed<br />Year: 1946<br />Fatality_Rate:  75.0000000",null],[null,"Manufacturer: Boeing<br />Year: 1947<br />Fatality_Rate:   0.0000000","Manufacturer: Ilyushin<br />Year: 1947<br />Fatality_Rate:  36.0000000","Manufacturer: Lockheed<br />Year: 1947<br />Fatality_Rate:  78.0000000",null],[null,"Manufacturer: Boeing<br />Year: 1948<br />Fatality_Rate: 100.0000000","Manufacturer: Ilyushin<br />Year: 1948<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1948<br />Fatality_Rate:  63.6842105",null],[null,"Manufacturer: Boeing<br />Year: 1949<br />Fatality_Rate: 100.0000000","Manufacturer: Ilyushin<br />Year: 1949<br />Fatality_Rate:  97.4358974","Manufacturer: Lockheed<br />Year: 1949<br />Fatality_Rate:  84.2857143",null],[null,"Manufacturer: Boeing<br />Year: 1950<br />Fatality_Rate:  77.7777778","Manufacturer: Ilyushin<br />Year: 1950<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1950<br />Fatality_Rate: 100.0000000",null],[null,"Manufacturer: Boeing<br />Year: 1951<br />Fatality_Rate: 100.0000000","Manufacturer: Ilyushin<br />Year: 1951<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1951<br />Fatality_Rate: 100.0000000",null],[null,"Manufacturer: Boeing<br />Year: 1952<br />Fatality_Rate:  80.3030303","Manufacturer: Ilyushin<br />Year: 1952<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1952<br />Fatality_Rate:  80.9523810",null],[null,"Manufacturer: Boeing<br />Year: 1953<br />Fatality_Rate:  68.7500000","Manufacturer: Ilyushin<br />Year: 1953<br />Fatality_Rate:  85.9154930","Manufacturer: Lockheed<br />Year: 1953<br />Fatality_Rate:  57.4324324",null],[null,"Manufacturer: Boeing<br />Year: 1954<br />Fatality_Rate:  77.7777778","Manufacturer: Ilyushin<br />Year: 1954<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1954<br />Fatality_Rate:  80.4651163",null],[null,"Manufacturer: Boeing<br />Year: 1955<br />Fatality_Rate:  42.4242424",null,"Manufacturer: Lockheed<br />Year: 1955<br />Fatality_Rate:  93.2203390",null],[null,"Manufacturer: Boeing<br />Year: 1956<br />Fatality_Rate:   7.2463768","Manufacturer: Ilyushin<br />Year: 1956<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1956<br />Fatality_Rate:  81.0218978",null],[null,"Manufacturer: Boeing<br />Year: 1957<br />Fatality_Rate: 100.0000000","Manufacturer: Ilyushin<br />Year: 1957<br />Fatality_Rate:  95.5555556","Manufacturer: Lockheed<br />Year: 1957<br />Fatality_Rate:          NA",null],[null,"Manufacturer: Boeing<br />Year: 1958<br />Fatality_Rate:  32.9545455","Manufacturer: Ilyushin<br />Year: 1958<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1958<br />Fatality_Rate:  93.1297710",null],[null,"Manufacturer: Boeing<br />Year: 1959<br />Fatality_Rate: 100.0000000","Manufacturer: Ilyushin<br />Year: 1959<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1959<br />Fatality_Rate:  79.6934866",null],[null,null,"Manufacturer: Ilyushin<br />Year: 1960<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1960<br />Fatality_Rate:  92.6356589",null],[null,"Manufacturer: Boeing<br />Year: 1961<br />Fatality_Rate: 100.0000000","Manufacturer: Ilyushin<br />Year: 1961<br />Fatality_Rate:  68.2926829","Manufacturer: Lockheed<br />Year: 1961<br />Fatality_Rate:  89.4894895","Manufacturer: McDonnell<br />Year: 1961<br />Fatality_Rate:  28.3737024"],[null,"Manufacturer: Boeing<br />Year: 1962<br />Fatality_Rate:  99.6363636","Manufacturer: Ilyushin<br />Year: 1962<br />Fatality_Rate:  66.6666667","Manufacturer: Lockheed<br />Year: 1962<br />Fatality_Rate:  83.3333333","Manufacturer: McDonnell<br />Year: 1962<br />Fatality_Rate:  54.7738693"],[null,"Manufacturer: Boeing<br />Year: 1963<br />Fatality_Rate: 100.0000000","Manufacturer: Ilyushin<br />Year: 1963<br />Fatality_Rate:  73.7500000","Manufacturer: Lockheed<br />Year: 1963<br />Fatality_Rate:  81.8181818","Manufacturer: McDonnell<br />Year: 1963<br />Fatality_Rate: 100.0000000"],[null,"Manufacturer: Boeing<br />Year: 1964<br />Fatality_Rate:  82.1656051","Manufacturer: Ilyushin<br />Year: 1964<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1964<br />Fatality_Rate:  94.2857143","Manufacturer: McDonnell<br />Year: 1964<br />Fatality_Rate: 100.0000000"],[null,"Manufacturer: Boeing<br />Year: 1965<br />Fatality_Rate:  50.3865979","Manufacturer: Ilyushin<br />Year: 1965<br />Fatality_Rate:  69.0476190","Manufacturer: Lockheed<br />Year: 1965<br />Fatality_Rate:  73.5537190",null],[null,"Manufacturer: Boeing<br />Year: 1966<br />Fatality_Rate: 100.0000000","Manufacturer: Ilyushin<br />Year: 1966<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1966<br />Fatality_Rate:  75.7446809","Manufacturer: McDonnell<br />Year: 1966<br />Fatality_Rate:  91.1111111"],[null,"Manufacturer: Boeing<br />Year: 1967<br />Fatality_Rate:  70.3389831","Manufacturer: Ilyushin<br />Year: 1967<br />Fatality_Rate:  81.5384615","Manufacturer: Lockheed<br />Year: 1967<br />Fatality_Rate:  63.5294118","Manufacturer: McDonnell<br />Year: 1967<br />Fatality_Rate:  59.3750000"],[null,"Manufacturer: Boeing<br />Year: 1968<br />Fatality_Rate:  49.4699647","Manufacturer: Ilyushin<br />Year: 1968<br />Fatality_Rate:  75.1445087","Manufacturer: Lockheed<br />Year: 1968<br />Fatality_Rate:  99.2481203","Manufacturer: McDonnell<br />Year: 1968<br />Fatality_Rate:   5.9288538"],[null,"Manufacturer: Boeing<br />Year: 1969<br />Fatality_Rate:  62.4074074","Manufacturer: Ilyushin<br />Year: 1969<br />Fatality_Rate:  76.6423358","Manufacturer: Lockheed<br />Year: 1969<br />Fatality_Rate: 100.0000000","Manufacturer: McDonnell<br />Year: 1969<br />Fatality_Rate:  76.7441860"],[null,"Manufacturer: Boeing<br />Year: 1970<br />Fatality_Rate:  36.7816092","Manufacturer: Ilyushin<br />Year: 1970<br />Fatality_Rate:  82.0512821","Manufacturer: Lockheed<br />Year: 1970<br />Fatality_Rate:  95.1871658","Manufacturer: McDonnell<br />Year: 1970<br />Fatality_Rate:  62.5632378"],[null,"Manufacturer: Boeing<br />Year: 1971<br />Fatality_Rate:  61.4886731","Manufacturer: Ilyushin<br />Year: 1971<br />Fatality_Rate:  68.1818182","Manufacturer: Lockheed<br />Year: 1971<br />Fatality_Rate:  99.3055556","Manufacturer: McDonnell<br />Year: 1971<br />Fatality_Rate:  98.0392157"],[null,"Manufacturer: Boeing<br />Year: 1972<br />Fatality_Rate:  59.6000000","Manufacturer: Ilyushin<br />Year: 1972<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1972<br />Fatality_Rate:  59.6638655","Manufacturer: McDonnell<br />Year: 1972<br />Fatality_Rate:  94.1520468"],[null,"Manufacturer: Boeing<br />Year: 1973<br />Fatality_Rate:  62.4605678","Manufacturer: Ilyushin<br />Year: 1973<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1973<br />Fatality_Rate:  97.6190476","Manufacturer: McDonnell<br />Year: 1973<br />Fatality_Rate:  49.2000000"],[null,"Manufacturer: Boeing<br />Year: 1974<br />Fatality_Rate:  58.6785010","Manufacturer: Ilyushin<br />Year: 1974<br />Fatality_Rate:  92.3076923","Manufacturer: Lockheed<br />Year: 1974<br />Fatality_Rate: 100.0000000","Manufacturer: McDonnell<br />Year: 1974<br />Fatality_Rate:  98.2758621"],[null,"Manufacturer: Boeing<br />Year: 1975<br />Fatality_Rate:  68.2326622","Manufacturer: Ilyushin<br />Year: 1975<br />Fatality_Rate:  98.6486486","Manufacturer: Lockheed<br />Year: 1975<br />Fatality_Rate:  52.1621622","Manufacturer: McDonnell<br />Year: 1975<br />Fatality_Rate:  62.5000000"],["Manufacturer: Airbus<br />Year: 1976<br />Fatality_Rate:   2.7131783","Manufacturer: Boeing<br />Year: 1976<br />Fatality_Rate:  77.7542373","Manufacturer: Ilyushin<br />Year: 1976<br />Fatality_Rate:  96.9849246","Manufacturer: Lockheed<br />Year: 1976<br />Fatality_Rate: 100.0000000","Manufacturer: McDonnell<br />Year: 1976<br />Fatality_Rate: 100.0000000"],[null,"Manufacturer: Boeing<br />Year: 1977<br />Fatality_Rate:  89.9893504","Manufacturer: Ilyushin<br />Year: 1977<br />Fatality_Rate:  88.8888889","Manufacturer: Lockheed<br />Year: 1977<br />Fatality_Rate: 100.0000000","Manufacturer: McDonnell<br />Year: 1977<br />Fatality_Rate:  58.3333333"],[null,"Manufacturer: Boeing<br />Year: 1978<br />Fatality_Rate:  57.0200573","Manufacturer: Ilyushin<br />Year: 1978<br />Fatality_Rate:  88.2352941",null,"Manufacturer: McDonnell<br />Year: 1978<br />Fatality_Rate:  34.3855693"],[null,"Manufacturer: Boeing<br />Year: 1979<br />Fatality_Rate:  69.7819315","Manufacturer: Ilyushin<br />Year: 1979<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1979<br />Fatality_Rate: 100.0000000","Manufacturer: McDonnell<br />Year: 1979<br />Fatality_Rate:  80.3237858"],[null,"Manufacturer: Boeing<br />Year: 1980<br />Fatality_Rate:  49.9278499","Manufacturer: Ilyushin<br />Year: 1980<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1980<br />Fatality_Rate:  61.8610747","Manufacturer: McDonnell<br />Year: 1980<br />Fatality_Rate: 100.0000000"],[null,"Manufacturer: Boeing<br />Year: 1981<br />Fatality_Rate:  88.3870968","Manufacturer: Ilyushin<br />Year: 1981<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1981<br />Fatality_Rate:  61.6438356","Manufacturer: McDonnell<br />Year: 1981<br />Fatality_Rate:  87.1212121"],[null,"Manufacturer: Boeing<br />Year: 1982<br />Fatality_Rate:  37.2614108","Manufacturer: Ilyushin<br />Year: 1982<br />Fatality_Rate:  51.6949153","Manufacturer: Lockheed<br />Year: 1982<br />Fatality_Rate: 100.0000000","Manufacturer: McDonnell<br />Year: 1982<br />Fatality_Rate:   9.7435897"],[null,"Manufacturer: Boeing<br />Year: 1983<br />Fatality_Rate:  87.0437956","Manufacturer: Ilyushin<br />Year: 1983<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1983<br />Fatality_Rate:  12.2065728","Manufacturer: McDonnell<br />Year: 1983<br />Fatality_Rate:  49.4949495"],[null,"Manufacturer: Boeing<br />Year: 1984<br />Fatality_Rate:   1.7241379","Manufacturer: Ilyushin<br />Year: 1984<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1984<br />Fatality_Rate: 100.0000000","Manufacturer: McDonnell<br />Year: 1984<br />Fatality_Rate:   2.2099448"],[null,"Manufacturer: Boeing<br />Year: 1985<br />Fatality_Rate:  62.0212766","Manufacturer: Ilyushin<br />Year: 1985<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1985<br />Fatality_Rate:  89.0909091","Manufacturer: McDonnell<br />Year: 1985<br />Fatality_Rate: 100.0000000"],[null,"Manufacturer: Boeing<br />Year: 1986<br />Fatality_Rate:  28.3138918","Manufacturer: Ilyushin<br />Year: 1986<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1986<br />Fatality_Rate:  59.0116279",null],["Manufacturer: Airbus<br />Year: 1987<br />Fatality_Rate: 100.0000000","Manufacturer: Boeing<br />Year: 1987<br />Fatality_Rate:  91.5766739","Manufacturer: Ilyushin<br />Year: 1987<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1987<br />Fatality_Rate:  94.2857143","Manufacturer: McDonnell<br />Year: 1987<br />Fatality_Rate:  47.1910112"],["Manufacturer: Airbus<br />Year: 1988<br />Fatality_Rate:  68.7793427","Manufacturer: Boeing<br />Year: 1988<br />Fatality_Rate:  59.6934175","Manufacturer: Ilyushin<br />Year: 1988<br />Fatality_Rate:  95.8158996","Manufacturer: Lockheed<br />Year: 1988<br />Fatality_Rate: 100.0000000","Manufacturer: McDonnell<br />Year: 1988<br />Fatality_Rate: 100.0000000"],[null,"Manufacturer: Boeing<br />Year: 1989<br />Fatality_Rate:  49.3518519","Manufacturer: Ilyushin<br />Year: 1989<br />Fatality_Rate:  70.0964630","Manufacturer: Lockheed<br />Year: 1989<br />Fatality_Rate:  87.8787879","Manufacturer: McDonnell<br />Year: 1989<br />Fatality_Rate:  62.5437573"],["Manufacturer: Airbus<br />Year: 1990<br />Fatality_Rate:  63.0136986","Manufacturer: Boeing<br />Year: 1990<br />Fatality_Rate:  33.6076818","Manufacturer: Ilyushin<br />Year: 1990<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1990<br />Fatality_Rate:  81.8181818","Manufacturer: McDonnell<br />Year: 1990<br />Fatality_Rate:  32.8671329"],[null,"Manufacturer: Boeing<br />Year: 1991<br />Fatality_Rate:  84.1607565","Manufacturer: Ilyushin<br />Year: 1991<br />Fatality_Rate:  68.4210526","Manufacturer: Lockheed<br />Year: 1991<br />Fatality_Rate:  96.3888889","Manufacturer: McDonnell<br />Year: 1991<br />Fatality_Rate: 100.0000000"],["Manufacturer: Airbus<br />Year: 1992<br />Fatality_Rate:  97.6063830","Manufacturer: Boeing<br />Year: 1992<br />Fatality_Rate:          NA","Manufacturer: Ilyushin<br />Year: 1992<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1992<br />Fatality_Rate:  37.7398721","Manufacturer: McDonnell<br />Year: 1992<br />Fatality_Rate:  17.1511628"],["Manufacturer: Airbus<br />Year: 1993<br />Fatality_Rate:   2.8571429","Manufacturer: Boeing<br />Year: 1993<br />Fatality_Rate:  69.8630137",null,"Manufacturer: Lockheed<br />Year: 1993<br />Fatality_Rate: 100.0000000","Manufacturer: McDonnell<br />Year: 1993<br />Fatality_Rate:   5.8695652"],["Manufacturer: Airbus<br />Year: 1994<br />Fatality_Rate:  46.4095745","Manufacturer: Boeing<br />Year: 1994<br />Fatality_Rate:  42.3005566",null,"Manufacturer: Lockheed<br />Year: 1994<br />Fatality_Rate:  91.7808219","Manufacturer: McDonnell<br />Year: 1994<br />Fatality_Rate:  12.3809524"],["Manufacturer: Airbus<br />Year: 1995<br />Fatality_Rate: 100.0000000","Manufacturer: Boeing<br />Year: 1995<br />Fatality_Rate:  70.0421941",null,"Manufacturer: Lockheed<br />Year: 1995<br />Fatality_Rate:  98.1481481","Manufacturer: McDonnell<br />Year: 1995<br />Fatality_Rate:  98.1818182"],[null,"Manufacturer: Boeing<br />Year: 1996<br />Fatality_Rate:  96.3773585","Manufacturer: Ilyushin<br />Year: 1996<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1996<br />Fatality_Rate:  94.7368421","Manufacturer: McDonnell<br />Year: 1996<br />Fatality_Rate:  23.2774674"],["Manufacturer: Airbus<br />Year: 1997<br />Fatality_Rate: 100.0000000","Manufacturer: Boeing<br />Year: 1997<br />Fatality_Rate:  42.0752566",null,"Manufacturer: Lockheed<br />Year: 1997<br />Fatality_Rate:  94.2622951","Manufacturer: McDonnell<br />Year: 1997<br />Fatality_Rate: 100.0000000"],["Manufacturer: Airbus<br />Year: 1998<br />Fatality_Rate:  63.5394456","Manufacturer: Boeing<br />Year: 1998<br />Fatality_Rate:  94.4206009","Manufacturer: Ilyushin<br />Year: 1998<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 1998<br />Fatality_Rate: 100.0000000","Manufacturer: McDonnell<br />Year: 1998<br />Fatality_Rate: 100.0000000"],["Manufacturer: Airbus<br />Year: 1999<br />Fatality_Rate:   0.5291005","Manufacturer: Boeing<br />Year: 1999<br />Fatality_Rate:  27.3387829","Manufacturer: Ilyushin<br />Year: 1999<br />Fatality_Rate:  71.4285714","Manufacturer: Lockheed<br />Year: 1999<br />Fatality_Rate:  20.8695652","Manufacturer: McDonnell<br />Year: 1999<br />Fatality_Rate:   5.8361392"],["Manufacturer: Airbus<br />Year: 2000<br />Fatality_Rate:  26.1261261","Manufacturer: Boeing<br />Year: 2000<br />Fatality_Rate:  52.7450980","Manufacturer: Ilyushin<br />Year: 2000<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 2000<br />Fatality_Rate: 100.0000000","Manufacturer: McDonnell<br />Year: 2000<br />Fatality_Rate:  50.8379888"],["Manufacturer: Airbus<br />Year: 2001<br />Fatality_Rate:  46.0992908","Manufacturer: Boeing<br />Year: 2001<br />Fatality_Rate:  94.3462898","Manufacturer: Ilyushin<br />Year: 2001<br />Fatality_Rate: 100.0000000",null,null],[null,"Manufacturer: Boeing<br />Year: 2002<br />Fatality_Rate:  76.8138801","Manufacturer: Ilyushin<br />Year: 2002<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 2002<br />Fatality_Rate:  74.0740741","Manufacturer: McDonnell<br />Year: 2002<br />Fatality_Rate: 100.0000000"],[null,"Manufacturer: Boeing<br />Year: 2003<br />Fatality_Rate:  94.2257218","Manufacturer: Ilyushin<br />Year: 2003<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 2003<br />Fatality_Rate: 100.0000000","Manufacturer: McDonnell<br />Year: 2003<br />Fatality_Rate: 100.0000000"],[null,"Manufacturer: Boeing<br />Year: 2004<br />Fatality_Rate: 100.0000000","Manufacturer: Ilyushin<br />Year: 2004<br />Fatality_Rate:  52.3809524",null,null],["Manufacturer: Airbus<br />Year: 2005<br />Fatality_Rate:   0.0000000","Manufacturer: Boeing<br />Year: 2005<br />Fatality_Rate:  58.3835947","Manufacturer: Ilyushin<br />Year: 2005<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 2005<br />Fatality_Rate: 100.0000000","Manufacturer: McDonnell<br />Year: 2005<br />Fatality_Rate:  99.2592593"],["Manufacturer: Airbus<br />Year: 2006<br />Fatality_Rate:  75.3164557","Manufacturer: Boeing<br />Year: 2006<br />Fatality_Rate:  94.0959410",null,"Manufacturer: Lockheed<br />Year: 2006<br />Fatality_Rate:  80.0000000",null],["Manufacturer: Airbus<br />Year: 2007<br />Fatality_Rate: 100.0000000","Manufacturer: Boeing<br />Year: 2007<br />Fatality_Rate:  40.5676127",null,null,"Manufacturer: McDonnell<br />Year: 2007<br />Fatality_Rate:  78.6096257"],["Manufacturer: Airbus<br />Year: 2008<br />Fatality_Rate:  11.7647059","Manufacturer: Boeing<br />Year: 2008<br />Fatality_Rate:  46.6275660",null,null,"Manufacturer: McDonnell<br />Year: 2008<br />Fatality_Rate:  59.9221790"],["Manufacturer: Airbus<br />Year: 2009<br />Fatality_Rate:  70.8955224","Manufacturer: Boeing<br />Year: 2009<br />Fatality_Rate: 100.0000000","Manufacturer: Ilyushin<br />Year: 2009<br />Fatality_Rate:  21.7142857","Manufacturer: Lockheed<br />Year: 2009<br />Fatality_Rate: 100.0000000","Manufacturer: McDonnell<br />Year: 2009<br />Fatality_Rate:  55.5555556"],["Manufacturer: Airbus<br />Year: 2010<br />Fatality_Rate:  99.6168582","Manufacturer: Boeing<br />Year: 2010<br />Fatality_Rate:  65.0385604","Manufacturer: Ilyushin<br />Year: 2010<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 2010<br />Fatality_Rate: 100.0000000","Manufacturer: McDonnell<br />Year: 2010<br />Fatality_Rate: 100.0000000"],[null,"Manufacturer: Boeing<br />Year: 2011<br />Fatality_Rate:  74.9090909",null,"Manufacturer: Lockheed<br />Year: 2011<br />Fatality_Rate: 100.0000000",null],[null,"Manufacturer: Boeing<br />Year: 2012<br />Fatality_Rate:  96.9465649",null,null,"Manufacturer: McDonnell<br />Year: 2012<br />Fatality_Rate: 100.0000000"],["Manufacturer: Airbus<br />Year: 2013<br />Fatality_Rate: 100.0000000","Manufacturer: Boeing<br />Year: 2013<br />Fatality_Rate:  12.7118644",null,null,null],["Manufacturer: Airbus<br />Year: 2014<br />Fatality_Rate: 100.0000000","Manufacturer: Boeing<br />Year: 2014<br />Fatality_Rate: 100.0000000","Manufacturer: Ilyushin<br />Year: 2014<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 2014<br />Fatality_Rate:  98.7012987",null],["Manufacturer: Airbus<br />Year: 2015<br />Fatality_Rate: 100.0000000",null,null,"Manufacturer: Lockheed<br />Year: 2015<br />Fatality_Rate: 100.0000000",null],["Manufacturer: Airbus<br />Year: 2016<br />Fatality_Rate:  48.5507246","Manufacturer: Boeing<br />Year: 2016<br />Fatality_Rate:   1.6339869",null,null,null],[null,"Manufacturer: Boeing<br />Year: 2017<br />Fatality_Rate: 100.0000000",null,"Manufacturer: Lockheed<br />Year: 2017<br />Fatality_Rate: 100.0000000",null],[null,"Manufacturer: Boeing<br />Year: 2018<br />Fatality_Rate:  48.5623003","Manufacturer: Ilyushin<br />Year: 2018<br />Fatality_Rate: 100.0000000","Manufacturer: Lockheed<br />Year: 2018<br />Fatality_Rate: 100.0000000",null],[null,"Manufacturer: Boeing<br />Year: 2019<br />Fatality_Rate:  99.4318182",null,null,null]],"colorscale":[[0,"#132B43"],[0.00529100529100529,"#132C44"],[0.0163398692810458,"#142D46"],[0.0172413793103448,"#142D46"],[0.0220994475138122,"#142E46"],[0.0271317829457364,"#152E47"],[0.0285714285714286,"#152E48"],[0.058361391694725,"#16324C"],[0.058695652173913,"#17324C"],[0.0592885375494071,"#17324C"],[0.072463768115942,"#17344F"],[0.0974358974358975,"#193753"],[0.117647058823529,"#1A3956"],[0.122065727699531,"#1A3A57"],[0.123809523809524,"#1A3A57"],[0.127118644067797,"#1B3A57"],[0.171511627906977,"#1D405F"],[0.208695652173913,"#204465"],[0.217142857142857,"#204566"],[0.232774674115456,"#214769"],[0.261261261261261,"#234B6E"],[0.27338782924614,"#244C70"],[0.283138918345705,"#254E72"],[0.283737024221453,"#254E72"],[0.328671328671329,"#27537A"],[0.329545454545455,"#28547A"],[0.336076817558299,"#28547B"],[0.343855693348365,"#28557C"],[0.36,"#29587F"],[0.367816091954023,"#2A5980"],[0.372614107883817,"#2A5981"],[0.377398720682303,"#2B5A82"],[0.384615384615385,"#2B5B83"],[0.40567612687813,"#2C5E87"],[0.420752565564424,"#2D608A"],[0.423005565862709,"#2E608A"],[0.424242424242424,"#2E608A"],[0.460992907801418,"#306591"],[0.464095744680851,"#306591"],[0.466275659824047,"#316692"],[0.471910112359551,"#316693"],[0.485507246376812,"#326895"],[0.485623003194888,"#326895"],[0.492,"#326997"],[0.493518518518519,"#326997"],[0.49469964664311,"#326997"],[0.494949494949495,"#326997"],[0.499278499278499,"#336A98"],[0.5,"#336A98"],[0.503865979381443,"#336B99"],[0.508379888268156,"#336B99"],[0.516949152542373,"#346C9B"],[0.521621621621622,"#346D9C"],[0.523809523809524,"#346D9C"],[0.527450980392157,"#356E9D"],[0.547738693467337,"#3671A1"],[0.555555555555556,"#3772A2"],[0.570200573065903,"#3874A5"],[0.574324324324324,"#3874A6"],[0.574468085106383,"#3874A6"],[0.583333333333333,"#3875A7"],[0.583835946924005,"#3876A7"],[0.586785009861933,"#3976A8"],[0.590116279069767,"#3976A8"],[0.59375,"#3977A9"],[0.596,"#3977AA"],[0.596638655462185,"#3977AA"],[0.596934174932372,"#3977AA"],[0.599221789883268,"#3A78AA"],[0.614886731391586,"#3B7AAD"],[0.616438356164384,"#3B7AAD"],[0.618610747051114,"#3B7AAE"],[0.620212765957447,"#3B7BAE"],[0.624074074074074,"#3B7BAF"],[0.624605678233438,"#3B7BAF"],[0.625,"#3B7BAF"],[0.625437572928822,"#3B7BAF"],[0.625632377740304,"#3B7BAF"],[0.63013698630137,"#3C7CB0"],[0.635294117647059,"#3C7DB1"],[0.635394456289979,"#3C7DB1"],[0.636842105263158,"#3C7DB1"],[0.650385604113111,"#3D7FB4"],[0.666666666666667,"#3E81B7"],[0.681818181818182,"#3F83BA"],[0.682326621923937,"#3F83BA"],[0.682926829268293,"#3F83BA"],[0.684210526315789,"#3F83BA"],[0.6875,"#4084BB"],[0.687793427230047,"#4084BB"],[0.69047619047619,"#4084BB"],[0.697819314641745,"#4085BD"],[0.698630136986301,"#4085BD"],[0.70042194092827,"#4186BD"],[0.70096463022508,"#4186BD"],[0.703389830508475,"#4186BE"],[0.705882352941177,"#4187BE"],[0.708955223880597,"#4187BF"],[0.714285714285714,"#4288C0"],[0.727272727272727,"#428AC2"],[0.735537190082645,"#438BC4"],[0.7375,"#438BC4"],[0.740740740740741,"#438BC5"],[0.749090909090909,"#448DC6"],[0.75,"#448DC6"],[0.751445086705202,"#448DC7"],[0.753164556962025,"#448DC7"],[0.757446808510638,"#458EC8"],[0.758620689655172,"#458EC8"],[0.766423357664234,"#458FCA"],[0.767441860465116,"#458FCA"],[0.76813880126183,"#458FCA"],[0.777542372881356,"#4691CC"],[0.777777777777778,"#4691CC"],[0.78,"#4691CC"],[0.786096256684492,"#4792CD"],[0.796934865900383,"#4793CF"],[0.8,"#4894D0"],[0.802816901408451,"#4894D1"],[0.803030303030303,"#4894D1"],[0.803237858032379,"#4894D1"],[0.804651162790698,"#4895D1"],[0.80952380952381,"#4895D2"],[0.81021897810219,"#4895D2"],[0.815384615384615,"#4996D3"],[0.818181818181818,"#4996D3"],[0.82051282051282,"#4997D4"],[0.821656050955414,"#4997D4"],[0.833333333333333,"#4A99D6"],[0.84160756501182,"#4B9AD8"],[0.842857142857143,"#4B9AD8"],[0.857142857142857,"#4C9CDB"],[0.859154929577465,"#4C9CDB"],[0.87043795620438,"#4D9EDE"],[0.871212121212121,"#4D9EDE"],[0.872340425531915,"#4D9EDE"],[0.878787878787879,"#4D9FDF"],[0.882352941176471,"#4DA0E0"],[0.883870967741936,"#4EA0E0"],[0.888888888888889,"#4EA1E1"],[0.890909090909091,"#4EA1E2"],[0.894894894894895,"#4EA2E2"],[0.89989350372737,"#4FA2E3"],[0.9,"#4FA2E3"],[0.911111111111111,"#50A4E5"],[0.915766738660907,"#50A5E6"],[0.917808219178082,"#50A5E7"],[0.923076923076923,"#50A6E8"],[0.926356589147287,"#51A6E8"],[0.931297709923664,"#51A7E9"],[0.932203389830508,"#51A7EA"],[0.940959409594096,"#52A8EB"],[0.941520467836257,"#52A8EB"],[0.942257217847769,"#52A8EC"],[0.942622950819672,"#52A9EC"],[0.942857142857143,"#52A9EC"],[0.943462897526502,"#52A9EC"],[0.944206008583691,"#52A9EC"],[0.947368421052632,"#52A9ED"],[0.951871657754011,"#52AAED"],[0.955555555555556,"#53AAEE"],[0.9581589958159,"#53ABEF"],[0.961904761904762,"#53ABEF"],[0.96377358490566,"#53ACF0"],[0.963888888888889,"#53ACF0"],[0.969465648854962,"#54ACF1"],[0.969849246231156,"#54ADF1"],[0.974358974358974,"#54ADF2"],[0.976063829787234,"#54ADF2"],[0.976190476190476,"#54ADF2"],[0.980392156862745,"#55AEF3"],[0.981481481481482,"#55AEF3"],[0.981818181818182,"#55AEF3"],[0.982758620689655,"#55AEF4"],[0.986486486486486,"#55AFF4"],[0.987012987012987,"#55AFF4"],[0.992481203007519,"#55B0F6"],[0.992592592592592,"#55B0F6"],[0.993055555555556,"#55B0F6"],[0.994318181818182,"#56B0F6"],[0.996168582375479,"#56B0F6"],[0.996363636363636,"#56B0F6"],[1,"#56B1F7"]],"type":"heatmap","showscale":false,"autocolorscale":false,"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":48.9497716894977},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Fatality Rate by Manufacturer","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,5.6],"tickmode":"array","ticktext":["Airbus","Boeing","Ilyushin","Lockheed","McDonnell"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["Airbus","Boeing","Ilyushin","Lockheed","McDonnell"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Manufacturer","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[1922.9,2024.1],"tickmode":"array","ticktext":["1925","1950","1975","2000"],"tickvals":[1925,1950,1975,2000],"categoryorder":"array","categoryarray":["1925","1950","1975","2000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"Year","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"1b542c273af":{"x":{},"y":{},"fill":{},"type":"heatmap"}},"cur_data":"1b542c273af","visdat":{"1b542c273af":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

> This graph is showing the top manufacturers, and the fatality rates per year. The darker the color, the lower the fatality rate. I mention this because I want to compare Boeing and Lockheed Martin. They are two major companies and as you can see Lockheeds fatality rate is higher. (I guess that is a good thing, as boeing supplies the commericial aircraft.)

<br>

---

<br>

# Future Works

I really enjoyed exploring this dataset, and I wish I had more time. Some ideas that I wish to add:

1. exploring how the `operator` of the flight could have effected the outcome of the flight. I noticed there was a lot of military operators, both foreign and domestic. 
2. use data mining to explore the `summary` of the crash to see how many were truly accidents vs. planned attacks.

<br>

---

<br>
