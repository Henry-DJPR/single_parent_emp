# packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  readabs,
  tidyverse,
  readxl
)



# Download data
fm1_path <- download_abs_data_cube(
  catalogue_string = "labour-force-australia-detailed", 
  cube = "FM1", 
  path = normalizePath("./data")
)




# Parse data 
fm1 <- read_excel(
  path = fm1_path,
  sheet = "Data 1",
  skip = 3
)




# Rename
fm1 <- fm1 %>% 
  rename(
    date = Month,
    sex = Sex, 
    single_parent = `Relationship in household`,
    state = `State and territory (STT): ASGS (2011)`,
    employed_ft = `Employed full-time ('000)`,
    employed_pt = `Employed part-time ('000)`,
    unemployed = `Unemployed total ('000)`,
    nilf = `Not in the labour force (NILF) ('000)`
  ) %>% 
  mutate(
    employed = employed_ft + employed_pt,
    labour_force = employed_ft + employed_pt + unemployed,
    population = employed_ft + employed_pt + unemployed + nilf
    )

value_vars = c(
  "employed_ft", 
  "employed_pt", 
  "employed", 
  "unemployed", 
  "nilf",
  "employed",
  "labour_force",
  "population"
  )




# Generate parent categories
fm1 <- fm1 %>% 
  mutate(
    single_parent = recode(
      single_parent,
      `Husband, wife or partner; With children under 15` = 
        "Partnered with dependant children",
      `Husband, wife or partner; With no children under 15 and with dependent students` = 
        "Partnered with dependant children",
      `Husband, wife or partner; With non-dependent children only` = 
        "Partnered with no dependant children",
      `Lone parent; With children under 15` = 
        "Single with dependant children",
      `Lone parent; With no children under 15 and with dependent students` = 
        "Single with dependant children",
      `Lone parent; With non-dependent children only` = 
        "Single with no dependant children",
      .default = "Other"
      
    )
  )



# Filter & aggregate over sex dimension
fm1 <- fm1 %>% 
  filter(state == "Victoria", single_parent != "Other") %>% 
  group_by(date, single_parent) %>% 
  summarise(across(all_of(value_vars), sum, na.rm = TRUE)) %>% 
  ungroup()




# Generate employment rates
fm1 <- fm1 %>% 
  mutate(
    rate_unemployment = unemployed / labour_force,
    rate_participation = labour_force / population,
    rate_emp_to_pop = employed / population
  )




# Download & tidy total employment rates
rates <- c(
  rate_unemployment_total = "A84423690X",
  rate_participation_total = "A84423691A",
  rate_emp_to_pop_total = "A84423692C"
)

vic_total <- read_abs(series_id = rates) %>% 
  mutate(
    series = names(rates)[match(series_id, rates)],
    value = value / 100
  ) %>% 
  pivot_wider(date, names_from = series, values_from = value)




# Join Vic total rates & create differences
fm1 <- fm1 %>% 
  left_join(vic_total) %>% 
  mutate(
    rate_unemployment_diff = rate_unemployment - rate_unemployment_total,
    rate_participation_diff = rate_participation - rate_participation_total,
    rate_emp_to_pop_diff = rate_emp_to_pop - rate_emp_to_pop_total
  )




# Plot unemployment
start_date <- as.Date("2019-01-01")
end_date <- max(fm1$date)

fm1 %>% 
  filter(date >= start_date & date <= end_date) %>% 
  ggplot(aes(date, rate_unemployment_diff)) +
  geom_area() +
  facet_wrap(~single_parent) +
  ylab("Difference from Vic unemployment rate (%pt)")

ggsave("output/unemployment difference.png")




# plot participation
fm1 %>% 
  filter(date >= start_date & date <= end_date) %>% 
  ggplot(aes(date, rate_participation_diff)) +
  geom_area() +
  facet_wrap(~single_parent) +
  ylab("Difference from Vic participation rate (%pt)")

ggsave("output/participation difference.png")




# plot employment to population
fm1 %>% 
  filter(date >= start_date & date <= end_date) %>% 
  ggplot(aes(date, rate_emp_to_pop_diff)) +
  geom_area() +
  facet_wrap(~single_parent) +
  ylab("Difference from Vic emp to pop rate (%pt)")


ggsave("output/employment to population difference.png")