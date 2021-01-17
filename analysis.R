library(tidyverse)
data <- read.csv("incarceration_trends.csv")


#######################################
# P1: 5 pieces of summary information
#######################################

# 1. number of features and elements
num_features <- length(data)
num_elements <- (length(data) * length(data$year))

# 2. range of years
range_years <- range(data$year)
diff_years <- range_years[2] - range_years[1]

# 3. number of different counties
num_counties <- length(unique(data$county_name))

# 4. range of prison rate for each race after 2000
range_prison_rates <- data %>%
  filter(year >= 2000) %>%
  summarize(white_range = range(white_prison_pop_rate, na.rm = TRUE),
            black_range = range(black_prison_pop_rate, na.rm = TRUE),
            latinx_range = range(latinx_prison_pop_rate, na.rm = TRUE),
            native_range = range(native_prison_pop_rate, na.rm = TRUE),
            aapi_range = range(aapi_prison_pop_rate, na.rm = TRUE))

# 5. proportion of non-NA values after 2000 and in 2016
after_2000 <- data %>%
  filter(year >= 2000)

prop_valid_data_after_2000 <- round((sum(!is.na(after_2000)) /
  (length(after_2000) * length(after_2000$year))) * 100)

in_2016 <- data %>%
  filter(year == 2016)

prop_valid_data_in_2016 <- round((sum(!is.na(in_2016)) /
  (length(in_2016) * length(in_2016$year))) * 100)


##############################
#P2: Trends over time chart
##############################

# My chart compares the mean prison rate of each racial group
# for each year after and including 2000 in Washington state.

# Getting the mean prison rate for each race
compare_prison_rate <- data %>%
  filter(year >= 2000) %>%
  filter(state == "WA") %>%
  group_by(year) %>%
  summarize(avg_white_prison_rate =
              mean(white_prison_pop_rate, na.rm = TRUE),
            avg_black_prison_rate =
              mean(black_prison_pop_rate, na.rm = TRUE),
            avg_latinx_prison_rate =
              mean(latinx_prison_pop_rate, na.rm = TRUE),
            avg_native_prison_rate =
              mean(native_prison_pop_rate, na.rm = TRUE),
            avg_aapi_prison_rate =
              mean(aapi_prison_pop_rate, na.rm = TRUE)) %>%
  slice(- (18:19)) %>%
  gather(key = "Race", value = "Average Prison Rate", -year)

# Plotting the chart
part2 <- ggplot(compare_prison_rate, aes(x = year, y = `Average Prison Rate`)) +
  geom_line(aes(color = Race)) +
  scale_color_hue(labels = c("Asian/Pacific Islander", "Black",
                             "Latinx", "Native", "White")
  ) +
  labs(
    title = "Mean Incarceration Rate by Race in Washington (2000-2018)",
    subtitle = "Based on 2000-2018 data from Washington counties",
    x = "Year",
    y = "Mean Incarceration Rate Per 100,000",
    color = "Race",
    caption = "Source: Vera Institute Incarceration Trends dataset (2020)")
  
###########################################
#P3: Continuous variable comparison chart
###########################################

# My chart compares the distribution of the proportion of
# the county population of a racial group vs. their
# respective incarceration rate in said county.

# Getting proportion of total population for each race
compare_prop_pop <- data %>%
  filter(year == 2016) %>%
  summarize(black = (black_pop_15to64 / total_pop_15to64) * 100,
            white = (white_pop_15to64 / total_pop_15to64) * 100,
            aapi = (aapi_pop_15to64 / total_pop_15to64) * 100,
            latinx = (latinx_pop_15to64 / total_pop_15to64) * 100,
            native = (native_pop_15to64 / total_pop_15to64) * 100) %>%
  gather(key = race, value = prop)

# Merging proportion data with prison rate for each race
compare_prop_with_prison_rate <- data %>%
  filter(year == 2016) %>%
  summarize(black = black_prison_pop_rate,
            white = white_prison_pop_rate,
            aapi = aapi_prison_pop_rate,
            latinx = latinx_prison_pop_rate,
            native = native_prison_pop_rate) %>%
  gather(key = race, value = rate) %>%
  mutate(compare_prop_pop)

# Plotting the chart
part3a <- ggplot(compare_prop_with_prison_rate,
       aes(x = prop, y = rate, color = race)) +
  geom_point() +
  scale_y_log10() +
  scale_color_hue(labels = c("Asian/Pacific Islander", "Black",
                             "Latinx", "Native", "White")) +
  labs(
    title = "Proportion of County Population vs. Incarceration Rate by Race",
    subtitle = "Based on 2016 data from counties within the United States",
    x = "Percentage of Total County Population (ages 15-64)",
    y = "Incarceration Rate Per 100,000",
    color = "Race")

# Plotting the same chart as above, faceted by race
part3b <- ggplot(compare_prop_with_prison_rate) +
  geom_point(
    mapping = aes(x = prop, y = rate, color = race),
    alpha = .6
  ) +
  facet_wrap(
    ~race,
    labeller = labeller(
      race = c(aapi = "Asian/Pacific Islander",
               black = "Black",
               latinx = "Latinx",
               native = "Native",
               white = "White"))
  ) +
  scale_y_log10() +
  labs(
    x = "Percentage of Total County Population (ages 15-64)",
    y = "Incarceration Rate Per 100,000",
    color = "Race",
    caption = "Source: Vera Institute Incarceration Trends dataset (2020)"
  ) +
  theme(legend.position = "none")


###########
#P4: Map
###########

# My chart compares the median difference in black and white
# incarceration rates by state. The shade of each state represents
# the rate difference on a scale of 0 - 3000.

# Getting median rate difference per state
state_data <- data %>%
  filter(year == 2016) %>%
  group_by(state) %>%
  # converting state abbreviation to state name
  mutate(state = tolower(state.name[match(state, state.abb)])) %>%
  summarize(diff = median(
    black_jail_pop_rate - white_jail_pop_rate, na.rm = TRUE))

# Getting the map shape data
state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(state_data, by = "state")

# Minimalist theme as defined in textbook
minimalist_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

# Plotting the chart
part4 <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long,
                  y = lat,
                  group = group,
                  fill = diff),
    color = "white",
    size = .1
  ) +
  coord_map() +
  scale_fill_viridis_c(
    trans = "sqrt",
    alpha = .7
  ) +
  labs(
    title =
      "Median Difference in Black and White Incarceration Rate Per 100,000 by State",
    subtitle =
      "Based on 2016 data from counties within the United States",
    fill = "Median Difference",
    caption =
      "Source: Vera Institute Incarceration Trends dataset (2020)"
  ) +
  minimalist_theme
