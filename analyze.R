library(pacman)
p_load(tidyverse, magrittr, lubridate, readxl, broom, janitor, censusapi)
p_load_gh("walkerke/tidycensus")

data_dir = here::here("data")

state_fips <- fips_codes %>% select(state, state_code, state_name) %>%
  distinct() %>%
  bind_rows(tibble(state = "US", state_code = "00", state_name = "United States"))

use_data <- read_excel(file.path(data_dir, "sales_annual.xlsx"), skip = 2,
                       na = c("", "NA"),
                       col_names = c("year", "state", "sector",
                                     "residential", "commercial", "industrial",
                                     "transportation", "other", "total"),
                       col_types = c("numeric", "text", "text",
                                     rep("numeric", 6))) %>%
  mutate(sector = factor(sector)) %>% left_join(state_fips, by = "state")

emission_factors <- read_excel(file.path(data_dir, "egrid2016_summarytables.xlsx"),
                               "Table 3", skip = 3) %>%
  clean_names() %>% rename(state = x1) %>% mutate(e_factor = co2e / 2200) %>%
  filter(! is.na(state))

use_data <- use_data %>%
  left_join(select(emission_factors, state, e_factor), by = "state") %>%
  mutate(emissions = residential * e_factor)

state_pop_file <- file.path(data_dir, "state_pop_1.Rds")

if (file.exists(state_pop_file)) {
  state_pop <- read_rds(state_pop_file)
} else {
  state_pop <- getCensus("pep/int_charagegroups", 2000,
                         vars=c("POP", "DATE_", "DATE_DESC", "GEONAME"),
                         region="state:*") %>%
    as_tibble() %>% clean_names()
  write_rds(state_pop, state_pop_file)
}

sp1 <- state_pop %>%
  mutate(date = date_desc %>% str_replace_all("([0-9]) *[A-Za-z].*$", "\\1") %>% mdy) %>%
  filter(date != "2000-07-01") %>%
  mutate(year = year(date), pop = as.numeric(pop)) %>%
  select(year, state_code = state, pop)

state_pop_2_file <- file.path(data_dir, "state_pop_2.Rds")

if (file.exists(state_pop_2_file)) {
  state_pop_2 <- read_rds(state_pop_2_file)
} else {
  county_pop_2 <- map_df(fips, ~getCensus("pep/int_charagegroups", 1990,
                                         vars=c("POP", "YEAR", "STATE", "COUNTY", "AGEGRP",
                                                "RACE_SEX", "HISP"),
                                         region="county:*", regionin=str_c("state:", .x))) %>%
    as_tibble() %>% clean_names() %>% mutate(pop = as.numeric(pop))
  state_pop_2 <- county_pop_2 %>% group_by(year, state) %>%
    summarize(pop = sum(pop)) %>% ungroup() %>%
    mutate(year = 1900 + as.integer(year)) %>%
    filter(year >= 1990) %>% rename(state_code = state)

  write_rds(state_pop_2, state_pop_2_file)
}

sp2 <- state_pop_2

state_pop_3_file <- file.path(data_dir, "state_pop_3.Rds")

if (file.exists(state_pop_3_file)) {
  state_pop_3 <- read_rds(state_pop_3_file)
} else {
  state_pop_3 <- get_estimates("state", "population", time_series = TRUE) %>%
    clean_names() %>% rename(state = geoid)
  write_rds(state_pop_3, state_pop_3_file)
}

sp3 <- state_pop_3 %>% filter(date > 3) %>%
  mutate(year = 2010 + date - 3, date = make_date(year, 7, 1)) %>%
  spread(key = variable, value = value) %>% clean_names() %>%
  select(year, state_code = state, pop)

sp <- bind_rows(sp1, sp2, sp3)

sp_us <- sp %>% group_by(year) %>%
  summarize(state_code = "00", pop = sum(pop))

sp <- bind_rows(sp, sp_us)

df <- left_join(use_data, sp, by = c("state_code", "year")) %>%
  mutate(per_capita_use = residential / pop,
         per_capita_emissions = emissions / pop) %>%
  filter(sector == "Total Electric Industry")

us_total <- df %>% filter(state != "US") %>% group_by(year, sector) %>%
  summarize(residential = sum(residential),
            emissions = sum(emissions),
            pop = sum(pop),
            per_capita_use = residential / pop,
            per_capita_emissions = emissions / pop)

fits <- us_total %>%
  mutate(early = lm(per_capita_use ~ year, data = .,
                    subset = year < 2007) %>%
           augment(newdata = tibble(year = year)) %$% .fitted,
         full = lm(per_capita_use ~ poly(year, 2), data = .) %>%
           augment(newdata = tibble(year = year)) %$% .fitted) %>%
  select(year, early, full) %>%
  gather(-year, key = key, value = per_capita_use) %>%
  mutate(Fit = factor(key, levels = c("early", "full"),
                      labels = c("Linear extrapolation from pre-2007 data",
                                 "Quadratic fit to all data")))

line_levels <- levels(fits$Fit)

line_values <- set_names(c("dashed", "solid"), line_levels)

ggplot(us_total, aes(x = year, y = per_capita_use)) +
  geom_point() +
  geom_line(aes(color = Fit, linetype = Fit), data = fits, size = 1) +
  scale_color_brewer(palette = "Dark2", name = NULL) +
  scale_linetype_manual(values = line_values, name = NULL) +
  labs(x = "Year",
       y = "MWh per year",
       title = "Per Capita Electricity Consumption") +
  theme_bw(base_size = 16) +
  theme(legend.position = c(0.07, 0.9),
        legend.justification = c(0,0.5),
        legend.key.height = unit(0.5, "lines"),
        legend.key.width = unit(2.5, "lines"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.title = element_blank())

ggsave(filename = "electricity_figure.png", dpi = 600, width = 6, height = 6,
       units = "in")
ggsave(filename = "electricity_figure.pdf", dpi = 600, width = 6, height = 6,
       units = "in")

emissions_change <- df %>% filter(state != "US") %>%
  select(year, state, sector, per_capita_use, e_factor, pop) %>%
  group_by(state, sector) %>%
  mutate(early = lm(per_capita_use ~ year, data = ., subset = year < 2007) %>%
           augment(newdata = tibble(year = year)) %$% .fitted,
         full = lm(per_capita_use ~ poly(year, 2), data = .) %>%
           augment(newdata = tibble(year = year)) %$% .fitted,
         delta = early - full,
         delta_total = pop * delta) %>%
  top_n(1, year) %>%
  mutate(delta = delta * e_factor, delta_total = delta_total * e_factor) %>%
  ungroup() %>%
  group_by(sector) %>%
  summarize(delta_total = sum(delta_total), pop = sum(pop),
            delta_mean = delta_total / pop) %>%
  ungroup()
