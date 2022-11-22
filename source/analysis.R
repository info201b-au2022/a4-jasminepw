## Loading packages ----
library(tidyverse)
library(plotly)
library(reshape2)
library(leaflet)

# The functions might be useful for A4
source("~/Documents/info201/assignments/a4-jasminepw/source/a4-helpers.R")

## Loading data ----
#----------------------------------------------------------------------------#
# Variables:
# incarceration_df: a the dataframe with the incarceration trends from the
#                   Vera Institute
# map_df: a dataframe with map data to plot maps
# female_prison_pop: a dataframe with 2016 data of female prison populations
#----------------------------------------------------------------------------#
# Calls the function get_data() to load incarceration trends data
incarceration_df <- get_data()

# Calls the function get_map_data() to load the U.S. counties map data
map_df <- get_map_data()

# A dataframe with the 2016 data of female prison populations
female_prison_pop <- incarceration_df %>%
  filter(year == 2016) %>%
  select(
    female_prison_pop,
    state,
    county_name,
    year,
    black_female_prison_pop,
    white_female_prison_pop,
    fips
  ) %>%
  drop_na()

## Section 2  ----
#----------------------------------------------------------------------------#
# Variables:
# avg_black_jail_rate: is the average jail rate of Black people 
# avg_white_jail_rate: is the average jail rate of white people
# avg_latinx_jail_rate: is the average jail rate of Latinx people
# avg_aapi_jail_rate: is the average jail rate of AAPI people
# avg_native_jail_rate: is the average jail rate of Native American people
# Note: All the rates and data is from 2016.
#----------------------------------------------------------------------------#
# Calculates the average jail rate of Black people in 2016
avg_black_jail_rate <- incarceration_df %>%
  filter(year == 2016) %>%
  pull(black_jail_pop_rate) %>%
  mean(na.rm = TRUE) %>%
  round(3)

# Calculates the average jail rate of white people in 2016
avg_white_jail_rate <- incarceration_df %>%
  filter(year == 2016) %>%
  pull(white_jail_pop_rate) %>%
  mean(na.rm = TRUE) %>%
  round(3)

# Calculates the average jail rate of Latinx people in 2016
avg_latinx_jail_rate <- incarceration_df %>%
  filter(year == 2016) %>%
  pull(latinx_jail_pop_rate) %>%
  mean(na.rm = TRUE) %>%
  round(3)

# Calculates the average jail rate of AAPI people in 2016
avg_aapi_jail_rate <- incarceration_df %>%
  filter(year == 2016) %>%
  pull(aapi_jail_pop_rate) %>%
  mean(na.rm = TRUE) %>%
  round(3)

# Calculates the average jail rate of Native American people in 2016
avg_native_jail_rate <- incarceration_df %>%
  filter(year == 2016) %>%
  pull(native_jail_pop_rate) %>%
  mean(na.rm = TRUE) %>%
  round(3)

## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# get_year_jail_pop(): function that returns a dataframe of  prison population 
#                      of every year from 1970 to 2018
# plot_jail_pop_for_us(): function that plots the data from get_year_jail_pop()
#                         into a bar chart
#----------------------------------------------------------------------------#
# This function returns a dataframe of the jail populations in the U.S. from
# 1970 to 2018
get_year_jail_pop <- function() {
  year_pop <- incarceration_df %>%
    group_by(year) %>%
    summarize(year_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(year_pop)
}

# This function returns the plot of the above dataframe of the jail populations 
# in the U.S. from 1970 to 2018
plot_jail_pop_for_us <- function() {
  pop_plot <- ggplot(get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = year_jail_pop)) +
    labs(
      x = "Year",
      y = "Total Jail Population",
      title = "Increase of Jail Population in U.S. (1970-2018)"
    ) +
    scale_y_continuous(labels = scales::comma)
  return(pop_plot)
}

## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
# get_jail_pop_by_states(states): a function that returns a dataframe of the
#                                 given states' yearly jail populations
# plot_jail_pop_by_states(states): a function that returns a line chart of the
#                                  given states' yearly jail populations
#----------------------------------------------------------------------------#
# Wrangles data from incarceration dataframe to collect the yearly jail
# populations of the given states, returns dataframe
get_jail_pop_by_states <- function(states) {
  by_states <- incarceration_df %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarize(year_jail_pop = sum(total_pop, na.rm = TRUE))
  return(by_states)
}

# Creates the line chart of the given states' yearly jail populations, returns
# plot
plot_jail_pop_by_states <- function(states) {
  growth_states_plot <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = year_jail_pop, color = state)) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      x = "Year",
      y = "Jail Population Per Year",
      title = "Yearly Growth of Prison Population by State (1970-2018)",
      color = "State"
    )
  return(growth_states_plot)
}

## Section 5  ----
#----------------------------------------------------------------------------#
# Variable comparison that reveals potential patterns of inequality:
# Comparing Black and White Female Prison Populations
# get_female_prison_pop(): a function that returns a dataframe of 2016 data
#                          that shows top 5 counties with greatest female prison
#                          populations and comparing Black and white female
#                          populations
# plot_female_prison_pops(): a function that plots the dataframe from the 
#                            above function into a grouped bar chart
#----------------------------------------------------------------------------#
# Wrangles 2016 data to find top five counties with greatest female prison
# populations, returns dataframe
get_female_prison_pop <- function() {
  greater_than <- female_prison_pop %>%
    filter(black_female_prison_pop > white_female_prison_pop) %>%
    top_n(n = 5, wt = female_prison_pop) %>%
    unite(location, county_name:state, sep = ", ") %>%
    select(location, white_female_prison_pop, black_female_prison_pop) %>%
    rename(
      "White" = white_female_prison_pop,
      "Black" = black_female_prison_pop
    )
  df_melt <- melt(greater_than, id.vars = "location")
  df_melt <- df_melt %>%
    rename(
      "Location" = location,
      "Race" = variable,
      "Population" = value
    )
  return(df_melt)
}

# Creates grouped bar chart with the dataframe of the above function, returns
# grouped bar chart that compares Black and white female prison population
plot_female_prison_pops <- function() {
  plot <- ggplot(get_female_prison_pop()) +
    geom_col(
      mapping = aes(x = Location, y = Population, fill = Race),
      position = "dodge"
    ) +
    labs(
      title = "Female Prison Populations by Race in 2016",
      caption = "Top 5 counties with the greatest female
               prison populations in 2016.",
      x = "Location (County, State)",
      y = "Population"
    )
  return(plot)
}

## Section 6  ----
#----------------------------------------------------------------------------#
# A map shows potential patterns of inequality that vary geographically:
# Map of Black Female Prison Populations in the U.S. in 2016
# get_map_data(): a function that returns a dataframe of the 2016 mapping data 
#                 of Black female prison populations
# plot_map(): a function that returns the map plot of Black Female Prison 
#             Populations in the U.S. in 2016
#----------------------------------------------------------------------------#
# Wrangles data to find the 2016 Black female prison populations by county,
# returns dataframe
get_map_data <- function() {
  get_data <- left_join(female_prison_pop, map_df, by = "fips")
  get_data <- get_data %>%
    select(-county_name, -state, -year) %>%
    select(fips, state_name, county, black_female_prison_pop, lat, lng) %>%
    mutate(
      radius = (black_female_prison_pop / max(black_female_prison_pop) * 5)^2
    )
  return(get_data)
}

# Creates interactive map that shows the 2016 Black female prison populations 
# by county, returns plot map
plot_map <- function() {
  prison_pop_map <- leaflet(get_map_data()) %>%
    addTiles() %>%
    addCircleMarkers(
      lat = ~lat,
      lng = ~lng,
      popup = ~county,
      stroke = FALSE,
      radius = ~radius,
      fillOpacity = 0.2
    )
  return(prison_pop_map)
}