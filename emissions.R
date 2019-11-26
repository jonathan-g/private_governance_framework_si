library(pacman)
p_load(tidyverse, magrittr, lubridate, readxl, broom, janitor)

data_dir = here::here("data")

egrid <- read_excel(file.path(data_dir, "egrid2016_data.xlsx"), "PLNT16",
                    skip = 1, na = c("", "NA", "N/A", "na", "n/a")) %>%
  clean_names()

renewables <- egrid %>%
  filter(pstatabb %in% c("NC", "VA"), plfuelct %in% c("SOLAR", "WIND")) %>%
  select(pstatabb, plfuelct, capfac, namepcap, plngenan, plco2eqa, plc2erta)

renew_factor <- renewables %>% select(namepcap, plngenan) %>%
  na.omit() %>% summarize_all(sum) %>%
  mutate(cap_factor = plngenan / (24 * 366 * namepcap))

natgas <- egrid %>%
  filter(pstatabb %in% c("NC", "VA"), plfuelct %in% c("GAS")) %>%
  select(pstatabb, plfuelct, capfac, namepcap, plngenan, plco2eqa, plc2erta)

natgas_factor <- natgas %>% select(namepcap, plngenan, plco2eqa) %>%
  na.omit() %>% summarize_all(sum) %>%
  mutate(cap_factor = plngenan / (24 * 366 * namepcap),
         e_factor = plco2eqa / plngenan)

message("Emissions reduction is ",
        scales::comma(750 * 24 * 365 * renew_factor$cap_factor * natgas_factor$e_factor,
              accuracy = 1000),
        " metric tons CO2e")
