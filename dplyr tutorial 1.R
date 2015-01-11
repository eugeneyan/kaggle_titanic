### dplyr tutorial 1: http://seananderson.ca/2014/09/13/dplyr-intro.html
library(data.table)
library(dplyr)

### download data
pantheria <-
    "http://esapubs.org/archive/ecol/E090/184/PanTHERIA_1-0_WR05_Aug2008.txt"
download.file(pantheria, destfile = "mammals.txt")

# read and clean data
mammals <- read.table("mammals.txt", sep = "\t", header = TRUE, 
                      stringsAsFactors = FALSE)
names(mammals) <- sub("X[0-9._]+", "", names(mammals))
names(mammals) <- sub("MSW05_", "", names(mammals))
mammals <- dplyr::select(mammals, Order, Binomial, AdultBodyMass_g, 
                         AdultHeadBodyLen_mm, HomeRange_km2, LitterSize)
names(mammals) <- gsub("([A-Z])", "_\\L\\1", names(mammals), perl = TRUE)
names(mammals) <- gsub("^_", "", names(mammals), perl = TRUE)
mammals[mammals == -999] <- NA
names(mammals)[names(mammals) == "binomial"] <- "species"
mammals <- dplyr::tbl_df(mammals) # for prettier printing
# setDT(mammals) # tbl_df and data.table don't seem to play well together

# select lets you subset by colummns, like subset()
select(mammals, litter_size) 
select(mammals, adult_head_body_len_mm, litter_size) 
select(mammals, -litter_size) 
select(mammals, contains('body'))
select(mammals, starts_with('adult'))
select(mammals, 1:3)

# filter lets you subset by rows
filter(mammals, adult_body_mass_g > 1e7)[ , 1:3]
filter(mammals, species == "Balaena mysticetus")
filter(mammals, order == "Carnivora" & adult_body_mass_g < 200)

# arrange lets you order rows by one or more columns
arrange(mammals, adult_body_mass_g)[ , 1:3]
arrange(mammals, desc(adult_body_mass_g))[ , 1:3]
arrange(mammals, order, adult_body_mass_g)[ , 1:3]

# mutate lets you add new column
mutate(mammals, adult_body_mass_kg = adult_body_mass_g / 1000)[, c(1, 2, 5:8)]
mutate(mammals, g_per_mm = adult_body_mass_g / adult_head_body_len_mm))[, c(1, 2, 5:8)]
mutate(mammals, 
       g_per_mm = adult_body_mass_g / adult_head_body_len_mm,
       kg_per_mm = g_per_mm / 1000) [, c(1, 2, 5:8)]

# summarise lets your calculate summary statistics
# use with group_by 
summarise(mammals, mean_mass = mean(adult_body_mass_g, na.rm = TRUE))
head(summarise(group_by(mammals, order),
               mean_mass = mean(adult_body_mass_g, na.rm = TRUE))
     
# pipes take the output from one function and feed it to the first argument of the next function
mammals %>% arrange(adult_body_mass_g)
mammals %>%
    group_by(order) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
mammals %>%
    mutate(mass_to_length = adult_body_mass_g / adult_head_body_len_mm) %>%
    arrange(desc(mass_to_length)) %>%
    select(species, mass_to_length)
mammals %>% 
    group_by(order) %>%
    summarise(median_litter = median(litter_size, na.rm = TRUE)) %>%
    filter(median_litter > 3) %>%
    arrange(desc(median_litter)) %>%
    select(order, median_litter)

