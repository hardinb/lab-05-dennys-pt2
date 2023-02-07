Lab 04 - La Quinta is Spanish for next to Denny’s, Pt. 2
================
Ben Hardin
2/7/2023

### Load packages and data

``` r
library(tidyverse) 
library(dsbox) 
```

``` r
states <- read_csv("data/states.csv")
dennys <- dennys
laquinta <- laquinta
```

### Exercise 1

First, let’s make a dataframe that only has Denny’s locations in Alaska.
Then, we can get the number of rows, which shows that there are 3
Denny’s in Alaska.

``` r
dennys_ak <- dennys %>%
  filter(state == "AK")
nrow(dennys_ak)
```

    ## [1] 3

### Exercise 2

Now, let’s do the same for LaQuinta locations. It seems there are 2
Laquintas in Alaska.

``` r
laquinta_ak <- laquinta %>%
  filter(state == "AK")
nrow(laquinta_ak)
```

    ## [1] 2

### Exercise 3

There are 3 Denny’s and 2 LaQuintas, so there are 3\*2 = 6 distances
between Denny’s and LaQuinta locations in Alaska. Let’s join the two
datasets we created, so we can calculate these distances.

``` r
dn_lq_ak <- full_join(dennys_ak, laquinta_ak, by = "state")
dn_lq_ak
```

    ## # A tibble: 6 × 11
    ##   address.x      city.x state zip.x longi…¹ latit…² addre…³ city.y zip.y longi…⁴
    ##   <chr>          <chr>  <chr> <chr>   <dbl>   <dbl> <chr>   <chr>  <chr>   <dbl>
    ## 1 2900 Denali    Ancho… AK    99503   -150.    61.2 3501 M… "\nAn… 99503   -150.
    ## 2 2900 Denali    Ancho… AK    99503   -150.    61.2 4920 D… "\nFa… 99709   -148.
    ## 3 3850 Debarr R… Ancho… AK    99508   -150.    61.2 3501 M… "\nAn… 99503   -150.
    ## 4 3850 Debarr R… Ancho… AK    99508   -150.    61.2 4920 D… "\nFa… 99709   -148.
    ## 5 1929 Airport … Fairb… AK    99701   -148.    64.8 3501 M… "\nAn… 99503   -150.
    ## 6 1929 Airport … Fairb… AK    99701   -148.    64.8 4920 D… "\nFa… 99709   -148.
    ## # … with 1 more variable: latitude.y <dbl>, and abbreviated variable names
    ## #   ¹​longitude.x, ²​latitude.x, ³​address.y, ⁴​longitude.y

### Exercise 4

There are 6 rows and 11 columns in the new dataset. The rows represent
the pairings of Denny’s and LaQuinta locations in Alaska. The columns
represent the variables. There is only one variable for state, because
that is the variable we joined the Denny’s and LaQuinta datasets on.
Then, we have two copies of every other location variable, with an added
letter (either x or y), which tells us whether the observation came from
the Denny’s dataset (x) or Laquinta dataset (y)

### Exercise 5

The mutate function in tidyverse lets us add a new variable to our
dataset.

``` r
haversine <- function(long1, lat1, long2, lat2, round = 3) {
  # convert to radians
  long1 = long1 * pi / 180
  lat1  = lat1  * pi / 180
  long2 = long2 * pi / 180
  lat2  = lat2  * pi / 180
  
  R = 6371 # Earth mean radius in km
  
  a = sin((lat2 - lat1)/2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1)/2)^2
  d = R * 2 * asin(sqrt(a))
  
  return( round(d,round) ) # distance in km
}
```

### Exercise 6

Alrighty, now that we have the two functions we need to do so, let’s
calculate the distances between each Denny’s and LaQuinta.

``` r
dn_lq_ak <- dn_lq_ak %>%
  mutate(distance = haversine(longitude.x, latitude.x, longitude.y, latitude.y))
```

### Exercise 7

Now we can calculate the minimum distance of a LaQuinta from each
Denny’s.

``` r
dn_lq_mindist <- dn_lq_ak %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))
```

### Exercise 8

Let’s summarize the distance between each Denny’s in Alaska and each
one’s nearest LaQuinta Inn. The average distance from a given Denny’s to
a LaQuinta Inn is 4.4. For any given Denny’s in Alaska, you are at most
about 6 units(?) away from LaQuinta Inn; and if you’re at the luckiest
Denny’s of all, you are only 2 units away from a LaQuinta.

``` r
dn_lq_mindist %>%
  summary(closest)
```

    ##   address.x            closest     
    ##  Length:3           Min.   :2.035  
    ##  Class :character   1st Qu.:3.616  
    ##  Mode  :character   Median :5.197  
    ##                     Mean   :4.410  
    ##                     3rd Qu.:5.598  
    ##                     Max.   :5.998
