install.packages('nycflights13')
library(nycflights13)

dim(flights)
head(flights)

# filter ()
flights %>%
    filter(month == 1, day == 1)
# equivalent to: flights[flights$months == 1 & flights$day == 1, ]

flights %>%
    filter(month == 1 | month ==2)

# slice() to select rows by position
flights %>%
    slice(1:10)

# arrange()
flights %>%
    arrange(year, month, day)

flights %>%
    arrange(desc(arr_delay))

# select()
flights %>%
    select(year, month, day)

flights %>%
    select(year:day)

flights %>%
    select(-(year:day))

flights %>%
    select(tail_num = tailnum)

# rename(), since select() drops all variables not explicitly mentioned,
# use rename() to rename variables instead
flights %>%
    rename(tail_num = tailnum)

# distinct() to return unique values in the table
flights %>% 
    select(tailnum) %>%
    distinct()

flights %>%
    select(origin, dest) %>%
    distinct()

# mutate()
flights %>%
    mutate(gain = arr_delay - dep_delay) %>%
    mutate(speed = distance/air_time * 60) %>%

flights %>%
    mutate(gain = arr_delay - dep_delay) %>%
    mutate(gain_per_hour = gain/(air_time/60))

# transmute() to only keep new variables
flights %>%
    transmute(gain = arr_delay - dep_delay, gain_per_hour = gain/(air_time/60))

# summarise()
flights %>%
    summarise(delay = mean(dep_delay, na.rm = T))

# sample_n() and sample_frac()
# use replace = T to perform a bootstrap sample
# use weight argumetn to weight the sample
flights %>%
    sample_n(10)

flights %>%
    sample_frac(0.01)

# group_by()
delay <- flights %>%
    group_by(tailnum) %>%
    summarise(count = n(), 
              dist = mean(distance, na.rm = T), 
              delay = mean(arr_delay, na.rm = T)) %>%
    filter(count > 20, dist < 2000)

ggplot(data = delay, aes(x=dist, y=delay)) +
    geom_point(aes(size = count), alpha = 1/2) +
    geom_smooth() +
    scale_size_area()

# useful dplyr functions
# n(): the number of observation in the current group
# n_distinct(x): counts the number of unique values in x
# first(x), last(x), and nth(x, n): similar to x[i], x[length(x)], and 
# x[n]

flights %>%
    group_by(dest) %>%
    summarise(planes = n_distinct(tailnum),
              flights = n())

# multiple group_by()s
# wrap the statement with () to show the output after assignment
daily <- group_by(flights, year, month, day)
(per_day <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year <- summarize(per_month, flights = sum(flights)))

# chaining
flights %>%
    group_by(year, month, day) %>%
    summarize(
        arr = mean(arr_delay, na.rm = T),
        dep = mean(dep_delay, na.rm = T)
    ) %>%
    filter(arr > 30 | dep > 30)