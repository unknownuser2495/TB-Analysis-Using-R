library(tidyverse)

who


who_tidy <- who %>%
  pivot_longer(new_sp_m014:newrel_f65, names_to = "group", values_to = "cases") %>%
  mutate(group = str_remove(group, "new_")) %>%
  mutate(group = str_remove(group, "new")) %>%
  separate(group, into = c("diagnosis", "group"), sep = "_") %>%
  separate(group, into = c("gender", "age_group"), sep = 1) %>%
  filter(!is.na(cases))

# this is how it could be done removing missing values as part of the pivot and
# using a more sophisticated regular expression to remove the new_? prefix in a
# single step
who_tidy <- who %>%
  pivot_longer(starts_with("new"), names_to = "group", values_to = "cases", values_drop_na = TRUE) %>%
  mutate(group = str_remove(group, "^new_?")) %>%
  separate(group, into = c("diagnosis", "group"), sep = "_") %>%
  separate(group, into = c("gender", "age_group"), sep = 1)

# alternative using str_replace_all to get groups into suitable state for single
# separate operation
who_tidy <- who %>%
  pivot_longer(starts_with("new"), names_to = "group", values_to = "cases", values_drop_na = TRUE) %>%
  mutate(group = str_replace_all(group, c("newrel" = "rel", "new_" = "", "_m" = "_m_", "_f" = "_f_"))) %>%
  separate(group, into = c("diagnosis", "gender", "age_group"), sep = "_")

# alternative using names_pattern in pivot_longer (need to know regular
# expressions for this) avoiding need for separate operation
who_tidy <- who %>%
  pivot_longer(
    starts_with("new"),
    names_pattern = "new_?(.*)_(.)(.*)",
    names_to = c("diagnosis", "gender", "age_group"),
    values_to = "cases",
    values_drop_na = TRUE
  )

# convert the age group into a factor and change the levels to a more
# human-readable form
# (could also use factor or recode_factor for this)
who_tidy <- mutate(who_tidy, age_group = fct_recode(age_group, "0-14" = "014", "15-24" = "1524", "25-34" = "2534", "35-44" = "3544", "45-54" = "4554", "55-64" = "5564", "65+" = "65"))

who_tidy


who_tidy %>%
  filter(country %in% c("Afghanistan", "Brazil", "China")) %>%
  filter(year %in% c(1999, 2000)) %>%
  group_by(country, year) %>%
  summarise(cases = sum(cases))

who_tidy <- who_tidy %>%
  group_by(country, year, age_group) %>%
  summarise(cases = sum(cases))

who_tidy %>%
  filter(country == "United Kingdom of Great Britain and Northern Ireland") %>%
  ggplot() +
  geom_line(mapping = aes(x = year, y = cases, colour = age_group))

