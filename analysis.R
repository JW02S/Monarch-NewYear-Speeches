library(pdftools)
library(tidyverse)

files <- list.files(
  "~/Desktop/nyårstaler",
  pattern = "\\.pdf$",
  full.names = TRUE
)

speech_texts <- map(files, pdf_text)
basename(files)
speech_data <- tibble(
  file = basename(files),
  text = map_chr(speech_texts, ~ paste(.x, collapse = "\n"))
)

head(speech_data)

cat(substr(speech_data$text[1], 1, 500))

speech_data$year <- coalesce(
  str_extract(speech_data$file, "(19|20)[0-9]{2}"),
  str_extract(speech_data$text, "(19|20)[0-9]{2}")
)

head(speech_data[, c("file", "year")])

duplicated(speech_data$year)

speech_data %>%
  count(year) %>%
  filter(n > 1)

speech_data %>%
  filter(year == "2026") %>%
  select(file)

speech_data <- speech_data %>%
  mutate(year = as.integer(year)) %>%
  arrange(year)

head(speech_data[, c("year", "file")])

tail(speech_data[, c("year", "file")])

speech_data <- speech_data %>%
  mutate(groenland = str_count(str_to_lower(text), "grønland|grønlænd"))

speech_data <- speech_data %>%
  mutate(faeroerne = str_count(
    str_to_lower(text),
    "færø|færing|faero|faering"
  ))

speech_data %>%
  select(year, faeroerne) %>%
  filter(faeroerne > 0)

speech_data %>%
  select(year, file, faeroerne) %>%
  filter(faeroerne == 0)

speech_data %>%
  filter(year == 1948) %>%
  pull(text) %>%
  str_extract_all("(?i).{0,40}(øerne|fær|faer).{0,40}") 

speech_data <- speech_data %>%
  mutate(rigsfaellesskab = str_count(
    str_to_lower(text),
    "rigsfællesskab|rigsfaellesskab"
  ))

speech_data <- speech_data %>%
  mutate(kongeriget = str_count(
    str_to_lower(text),
    "kongerig"
  ))

speech_data %>%
  select(year, rigsfaellesskab, kongeriget) %>%
  filter(rigsfaellesskab > 0 | kongeriget > 0)

speech_data %>%
  filter(rigsfaellesskab > 0 | kongeriget > 0) %>%
  transmute(
    year,
    context = str_extract_all(
      text,
      "(?i).{0,80}(rigsfællesskab|rigsfaellesskab|kongerig).{0,80}"
    )
  ) %>%
  unnest_longer(context)

speech_data <- speech_data %>%
  mutate(
    faellesskab_total = rigsfaellesskab + kongeriget
  )

speech_data %>%
  select(year, groenland, faeroerne, rigsfaellesskab, kongeriget, faellesskab_total) %>%
  tail(10)

speech_data %>%
  select(year, groenland, faeroerne, rigsfaellesskab, kongeriget, faellesskab_total) %>%
  print(n = 78)

speech_data %>%
  summarise(
    groenland_total = sum(groenland),
    faeroerne_total = sum(faeroerne),
    rigsfaellesskab_total = sum(rigsfaellesskab),
    kongeriget_total = sum(kongeriget)
  )

speech_data %>%
  select(year, groenland, faeroerne) %>%
  pivot_longer(
    cols = c(groenland, faeroerne),
    names_to = "place",
    values_to = "mentions"
  ) %>%
  mutate(
    place = recode(
      place,
      groenland = "Greenland",
      faeroerne = "Faroe Islands"
    )
  ) %>%
  ggplot(aes(x = year, y = mentions, color = place)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Mentions of Greenland and the Faroe Islands in New Year's Speeches",
    x = "Year",
    y = "Number of mentions",
    color = "Place"
  ) +
  theme_minimal()

speech_data %>%
  select(year, rigsfaellesskab, kongeriget) %>%
  pivot_longer(
    cols = c(rigsfaellesskab, kongeriget),
    names_to = "concept",
    values_to = "mentions"
  ) %>%
  mutate(
    concept = recode(
      concept,
      rigsfaellesskab = "Rigsfællesskabet",
      kongeriget = "Kongeriget"
    )
  ) %>%
  ggplot(aes(x = year, y = mentions, color = concept)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Use of 'Rigsfællesskabet' and 'Kongeriget' in New Year's Speeches",
    x = "Year",
    y = "Number of mentions",
    color = "Concept"
  ) +
  theme_minimal()

speech_data %>%
  filter(kongeriget > 0) %>%
  transmute(
    year,
    context = str_extract_all(
      text,
      "(?i).{0,100}kongerig.{0,100}"
    )
  ) %>%
  unnest_longer(context)

speech_data %>%
  filter(rigsfaellesskab > 0) %>%
  transmute(
    year,
    context = str_extract_all(
      text,
      "(?i).{0,100}(rigsfællesskab|rigsfaellesskab).{0,100}"
    )
  ) %>%
  unnest_longer(context)

speech_data %>%
  transmute(
    matches = str_extract_all(
      str_to_lower(text),
      "grønland[a-zæøå]*|grønlænd[a-zæøå]*"
    )
  ) %>%
  unnest_longer(matches) %>%
  count(matches, sort = TRUE)

speech_data %>%
  transmute(
    matches = str_extract_all(
      str_to_lower(text),
      "færø[a-zæøå]*|færing[a-zæøå]*|faero[a-zæøå]*|faering[a-zæøå]*"
    )
  ) %>%
  unnest_longer(matches) %>%
  count(matches, sort = TRUE)

speech_data <- speech_data %>%
  mutate(
    monarch = case_when(
      year <= 1971 ~ "Frederik IX",
      year >= 1972 & year <= 2023 ~ "Margrethe II",
      year >= 2024 ~ "Frederik X"
    )
  )

speech_data %>%
  count(monarch)

speech_data %>%
  group_by(monarch) %>%
  summarise(
    speeches = n(),
    greenland_total = sum(groenland),
    faroe_total = sum(faeroerne),
    greenland_per_speech = mean(groenland),
    faroe_per_speech = mean(faeroerne)
  )

speech_data %>%
  group_by(monarch) %>%
  summarise(
    speeches = n(),
    speeches_greenland = sum(groenland > 0),
    speeches_faroe = sum(faeroerne > 0),
    pct_greenland = mean(groenland > 0) * 100,
    pct_faroe = mean(faeroerne > 0) * 100
  )

speech_data <- speech_data %>%
  mutate(
    visit_faroe = if_else(
      year %in% c(
        1949, 1953, 1959, 1972, 1977, 1982, 1987,
        1990, 1995, 2001, 2005, 2010, 2016, 2021
      ),
      1, 0
    )
  )

speech_data %>%
  filter(visit_faroe == 1) %>%
  select(year, faeroerne)

speech_data %>%
  group_by(visit_faroe) %>%
  summarise(
    speeches = n(),
    average_faroe_mentions = mean(faeroerne),
    median_faroe_mentions = median(faeroerne)
  )

speech_data <- speech_data %>%
  mutate(
    visit_greenland = if_else(
      year %in% c(
        1952, 1960, 1968, 1975, 1979, 1982,
        1986, 1991, 1997, 2000, 2004, 2008,
        2011, 2015, 2021, 2024
      ),
      1, 0
    )
  )

speech_data %>%
  filter(visit_greenland == 1) %>%
  select(year, groenland)

speech_data %>%
  group_by(visit_greenland) %>%
  summarise(
    speeches = n(),
    average_greenland_mentions = mean(groenland),
    median_greenland_mentions = median(groenland)
  )

speech_data %>%
  filter(monarch == "Margrethe II") %>%
  group_by(visit_greenland) %>%
  summarise(
    speeches = n(),
    average_greenland_mentions = mean(groenland),
    median_greenland_mentions = median(groenland)
  )

speech_data %>%
  filter(monarch == "Margrethe II") %>%
  group_by(visit_faroe) %>%
  summarise(
    speeches = n(),
    average_faroe_mentions = mean(faeroerne),
    median_faroe_mentions = median(faeroerne)
  )

speech_data %>%
  filter(monarch == "Margrethe II") %>%
  group_by(visit_greenland) %>%
  summarise(
    average_mentions = mean(groenland)
  ) %>%
  mutate(
    visit = if_else(
      visit_greenland == 1,
      "Official visit",
      "No official visit"
    )
  ) %>%
  ggplot(aes(x = visit, y = average_mentions)) +
  geom_col() +
  labs(
    title = "Mentions of Greenland during Margrethe II's reign",
    subtitle = "Comparison of years with and without an official visit",
    x = NULL,
    y = "Average mentions per New Year's speech"
  ) +
  theme_minimal()

visit_comparison <- bind_rows(
  
  speech_data %>%
    filter(monarch == "Margrethe II") %>%
    group_by(visit_greenland) %>%
    summarise(average_mentions = mean(groenland)) %>%
    mutate(
      place = "Greenland",
      visit = if_else(
        visit_greenland == 1,
        "Official visit",
        "No official visit"
      )
    ),
  
  speech_data %>%
    filter(monarch == "Margrethe II") %>%
    group_by(visit_faroe) %>%
    summarise(average_mentions = mean(faeroerne)) %>%
    mutate(
      place = "Faroe Islands",
      visit = if_else(
        visit_faroe == 1,
        "Official visit",
        "No official visit"
      )
    )
)

visit_comparison <- bind_rows(
  
  speech_data %>%
    filter(monarch == "Margrethe II") %>%
    group_by(visit_greenland) %>%
    summarise(average_mentions = mean(groenland)) %>%
    mutate(
      place = "Greenland",
      visit = if_else(
        visit_greenland == 1,
        "Official visit",
        "No official visit"
      )
    ),
  
  speech_data %>%
    filter(monarch == "Margrethe II") %>%
    group_by(visit_faroe) %>%
    summarise(average_mentions = mean(faeroerne)) %>%
    mutate(
      place = "Faroe Islands",
      visit = if_else(
        visit_faroe == 1,
        "Official visit",
        "No official visit"
      )
    )
)

ggplot(
  visit_comparison,
  aes(x = place, y = average_mentions, fill = visit)
) +
  geom_col(position = "dodge") +
  labs(
    title = "Mentions of Greenland and the Faroe Islands during Margrethe II's reign",
    subtitle = "Comparison of years with and without an official visit",
    x = NULL,
    y = "Average mentions per New Year's speech",
    fill = "Year"
  ) +
  theme_minimal()

speech_data %>%
  select(year, groenland, faeroerne) %>%
  pivot_longer(
    cols = c(groenland, faeroerne),
    names_to = "place",
    values_to = "mentions"
  ) %>%
  group_by(place) %>%
  slice_max(mentions, n = 10, with_ties = TRUE) %>%
  arrange(place, desc(mentions))

speech_data %>%
  select(year, groenland) %>%
  arrange(desc(groenland)) %>%
  slice_head(n = 10)

speech_data %>%
  filter(year == 1978) %>%
  transmute(
    context = str_extract_all(
      text,
      "(?i).{0,250}(grønland|grønlænd[a-zæøå]*).{0,250}"
    )
  ) %>%
  unnest_longer(context)

speech_data %>%
  filter(year %in% c(2014, 2017, 2020)) %>%
  transmute(
    year,
    context = str_extract_all(
      text,
      "(?i).{0,300}(grønland|grønlænd[a-zæøå]*).{0,300}"
    )
  ) %>%
  unnest_longer(context)

speech_data %>%
  filter(year %in% c(2014, 2017, 2020)) %>%
  transmute(
    year,
    context = str_extract_all(
      text,
      "(?i).{0,300}(grønland|grønlænd[a-zæøå]*).{0,300}"
    )
  ) %>%
  unnest_longer(context) %>%
  print(n = Inf)

speech_data %>%
  select(year, groenland, faeroerne) %>%
  pivot_longer(
    cols = c(groenland, faeroerne),
    names_to = "place",
    values_to = "mentions"
  ) %>%
  mutate(
    place = recode(
      place,
      groenland = "Greenland",
      faeroerne = "Faroe Islands"
    )
  ) %>%
  ggplot(aes(x = year, y = mentions, color = place)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  labs(
    title = "Mentions of Greenland and the Faroe Islands in Royal New Year's Speeches",
    subtitle = "1948–2025",
    x = "Year",
    y = "Number of mentions",
    color = "Place"
  ) +
  theme_minimal()

decade_data <- speech_data %>%
  mutate(decade = floor(year / 10) * 10) %>%
  group_by(decade) %>%
  summarise(
    Greenland = mean(groenland),
    `Faroe Islands` = mean(faeroerne)
  ) %>%
  pivot_longer(
    cols = c(Greenland, `Faroe Islands`),
    names_to = "place",
    values_to = "average_mentions"
  )

decade_data

ggplot(
  decade_data,
  aes(x = decade, y = average_mentions, color = place)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = seq(1940, 2020, 10)
  ) +
  labs(
    title = "Mentions of Greenland and the Faroe Islands by Decade",
    subtitle = "Average mentions per Royal New Year's Speech, 1948–2025",
    x = "Decade",
    y = "Average number of mentions",
    color = "Place"
  ) +
  theme_minimal()

# Linear trend over time: Greenland
model_greenland <- lm(groenland ~ year, data = speech_data)

summary(model_greenland)


# Linear trend over time: Faroe Islands
model_faroe <- lm(faeroerne ~ year, data = speech_data)

summary(model_faroe)

speech_data <- speech_data %>%
  mutate(
    landsmaend = str_count(
      str_to_lower(text),
      "landsmand|landsmænd|vore landsmænd|danske landsmænd"
    ),
    folk_befolkning = str_count(
      str_to_lower(text),
      "grønlandske folk|grønlands befolkning|færøske folk|færøernes befolkning"
    ),
    tre_lande = str_count(
      str_to_lower(text),
      "tre lande|danmark, færøerne og grønland|grønland og færøerne"
    )
  )

speech_data %>%
  select(year, monarch, landsmaend, folk_befolkning, tre_lande) %>%
  filter(landsmaend > 0 | folk_befolkning > 0 | tre_lande > 0) %>%
  print(n = Inf)

speech_data %>%
  transmute(
    year,
    monarch,
    context = str_extract_all(
      text,
      "(?i).{0,200}(grønland|grønlænd|færø|færing).{0,200}"
    )
  ) %>%
  unnest_longer(context) %>%
  filter(!is.na(context)) %>%
  arrange(year) %>%
  print(n = Inf)

speech_data <- speech_data %>%
  mutate(
    # "vore landsmænd", "landsmænd" osv.
    landsmaend = str_count(
      str_to_lower(text),
      "landsmand|landsmænd"
    ),
    
    # færing, færinger, færingerne osv.
    faeringer = str_count(
      str_to_lower(text),
      "færing"
    ),
    
    # grønlænder, grønlændere, grønlænderne osv.
    groenlaendere = str_count(
      str_to_lower(text),
      "grønlænd"
    ),
    
    # det færøske folk
    faeroesk_folk = str_count(
      str_to_lower(text),
      "færøske folk"
    ),
    
    # det grønlandske folk
    groenlandsk_folk = str_count(
      str_to_lower(text),
      "grønlandske folk"
    ),
    
    # færøske/grønlandske samfund
    faeroesk_samfund = str_count(
      str_to_lower(text),
      "færøske samfund"
    ),
    
    groenlandsk_samfund = str_count(
      str_to_lower(text),
      "grønlandske samfund"
    )
  )

speech_data %>%
  summarise(
    landsmaend = sum(landsmaend),
    faeringer = sum(faeringer),
    groenlaendere = sum(groenlaendere),
    faeroesk_folk = sum(faeroesk_folk),
    groenlandsk_folk = sum(groenlandsk_folk),
    faeroesk_samfund = sum(faeroesk_samfund),
    groenlandsk_samfund = sum(groenlandsk_samfund)
  )

speech_data %>%
  select(
    year, monarch,
    landsmaend,
    faeringer,
    groenlaendere,
    faeroesk_folk,
    groenlandsk_folk,
    faeroesk_samfund,
    groenlandsk_samfund
  ) %>%
  filter(
    landsmaend > 0 |
      faeringer > 0 |
      groenlaendere > 0 |
      faeroesk_folk > 0 |
      groenlandsk_folk > 0 |
      faeroesk_samfund > 0 |
      groenlandsk_samfund > 0
  ) %>%
  print(n = Inf)

speech_data %>%
  filter(landsmaend > 0) %>%
  transmute(
    year,
    monarch,
    context = str_extract_all(
      text,
      "(?i).{0,250}(landsmand|landsmænd).{0,250}"
    )
  ) %>%
  unnest_longer(context) %>%
  arrange(year) %>%
  print(n = Inf, width = Inf)

speech_data %>%
  filter(landsmaend > 0) %>%
  transmute(
    year,
    monarch,
    context = str_extract_all(
      text,
      "(?i).{0,250}(landsmand|landsmænd).{0,250}"
    )
  ) %>%
  unnest_longer(context) %>%
  filter(
    str_detect(
      str_to_lower(context),
      "grønland|grønlands|færø|færøerne"
    )
  ) %>%
  select(year, monarch, context) %>%
  arrange(year) %>%
  print(n = Inf, width = Inf)

speech_data %>%
  filter(landsmaend > 0) %>%
  transmute(
    year,
    monarch,
    context = str_extract_all(
      text,
      "(?i).{0,250}(landsmand|landsmænd).{0,250}"
    )
  ) %>%
  unnest_longer(context) %>%
  filter(
    str_detect(
      str_to_lower(context),
      "grønland|grønlands|færø|færøerne"
    )
  ) %>%
  select(year, monarch, context) %>%
  print(n = Inf, width = Inf)

speech_data %>%
  filter(monarch == "Margrethe II") %>%
  transmute(
    year,
    context = str_extract_all(
      text,
      "(?i).{0,200}(grønland|grønlandsk|grønlænd|færø|færing).{0,300}"
    )
  ) %>%
  unnest_longer(context) %>%
  filter(!is.na(context)) %>%
  arrange(year) %>%
  print(n = Inf, width = Inf)

speech_data %>%
  summarise(
    speeches = n(),
    first_year = min(year),
    last_year = max(year),
    missing_greenland = sum(is.na(groenland)),
    missing_faroe = sum(is.na(faeroerne))
  )

speech_data %>%
  filter(year == 1948) %>%
  select(year, monarch, groenland, faeroerne)

speech_data %>%
  summarise(
    speeches = n(),
    first_year = min(year),
    last_year = max(year),
    missing_years = 78 - n_distinct(year)
  )

speech_data %>%
  summarise(
    missing_greenland = sum(is.na(groenland)),
    missing_faroe = sum(is.na(faeroerne))
  )

speech_data %>%
  summarise(
    total_greenland = sum(groenland),
    total_faroe = sum(faeroerne),
    average_greenland = mean(groenland),
    average_faroe = mean(faeroerne)
  )

model_greenland <- lm(groenland ~ year, data = speech_data)

summary(model_greenland)

model_faroe <- lm(faeroerne ~ year, data = speech_data)

summary(model_faroe)

speech_data %>%
  group_by(monarch) %>%
  summarise(
    speeches = n(),
    avg_greenland = mean(groenland),
    avg_faroe = mean(faeroerne),
    total_greenland = sum(groenland),
    total_faroe = sum(faeroerne)
  )

speech_data %>%
  mutate(decade = floor(year / 10) * 10) %>%
  group_by(decade) %>%
  summarise(
    speeches = n(),
    avg_greenland = mean(groenland),
    avg_faroe = mean(faeroerne)
  )

speech_data %>%
  select(year, monarch, groenland, faeroerne) %>%
  arrange(desc(groenland + faeroerne)) %>%
  slice_head(n = 15)

speech_data %>%
  filter(year == 1978) %>%
  pull(text)

speech_data %>%
  filter(year == 1978) %>%
  pull(text) %>%
  str_extract_all(
    "(?i).{0,250}(grønland|grønlandsk|grønlænd|færøerne|færøsk|færing).{0,250}"
  ) %>%
  unlist()

speech_data %>%
  filter(year == 1978) %>%
  pull(text) %>%
  str_extract(
    "(?s)Gensynet med Færøerne.*?ønsker jeg for alle i Grønland\\."
  )

speech_data %>%
  filter(year == 2014) %>%
  pull(text) %>%
  str_extract_all(
    "(?i).{0,250}(grønland|grønlandsk|grønlænd|færøerne|færøsk|færing).{0,250}"
  ) %>%
  unlist()

speech_data %>%
  filter(year == 1997) %>%
  pull(text) %>%
  str_extract_all(
    "(?i).{0,250}(grønland|grønlandsk|grønlænd|færøerne|færøsk|færing).{0,250}"
  ) %>%
  unlist()

speech_data %>%
  select(
    year,
    monarch,
    landsmaend,
    faeringer,
    groenlaendere,
    faeroesk_folk,
    groenlandsk_folk,
    faeroesk_samfund,
    groenlandsk_samfund
  ) %>%
  filter(
    landsmaend > 0 |
      faeringer > 0 |
      groenlaendere > 0 |
      faeroesk_folk > 0 |
      groenlandsk_folk > 0 |
      faeroesk_samfund > 0 |
      groenlandsk_samfund > 0
  ) %>%
  arrange(year) %>%
  print(n = Inf)

speech_data %>%
  filter(str_detect(text, regex("landsmænd", ignore_case = TRUE))) %>%
  mutate(
    context = str_extract_all(
      text,
      regex(".{0,150}landsmænd.{0,150}", ignore_case = TRUE)
    )
  ) %>%
  select(year, monarch, context) %>%
  unnest_longer(context) %>%
  print(n = Inf, width = Inf)

speech_data %>%
  filter(str_detect(text, regex("landsmænd", ignore_case = TRUE))) %>%
  mutate(
    context = str_extract_all(
      text,
      regex(".{0,150}landsmænd.{0,150}", ignore_case = TRUE)
    )
  ) %>%
  select(year, context) %>%
  unnest_longer(context) %>%
  filter(
    str_detect(
      context,
      regex("Grønland|Færøerne", ignore_case = TRUE)
    )
  ) %>%
  print(n = Inf, width = Inf)

speech_data %>%
  filter(year == 1971) %>%
  pull(text) %>%
  str_extract(
    regex(
      ".{0,200}vore landsmænd i Grønland og på.{0,300}",
      ignore_case = TRUE
    )
  )

speech_data %>%
  filter(year == 1971) %>%
  pull(text) %>%
  str_extract(
    regex(
      "(?s).{0,200}vore landsmænd i Grønland og på.{0,300}",
      ignore_case = TRUE
    )
  )

speech_data %>%
  filter(year == 1972) %>%
  pull(text) %>%
  str_extract(
    regex(
      "(?s).{0,200}det grønlandske og.{0,300}",
      ignore_case = TRUE
    )
  )

speech_data %>%
  filter(year >= 1972, year <= 1981) %>%
  transmute(
    year,
    context = str_extract_all(
      text,
      regex(
        "(?s).{0,150}(det grønlandske folk|det færøske folk|grønlændere|færinger).{0,200}",
        ignore_case = TRUE
      )
    )
  ) %>%
  unnest_longer(context) %>%
  filter(!is.na(context)) %>%
  print(n = Inf, width = Inf)

speech_data %>%
  filter(year == 1979) %>%
  transmute(
    context = str_extract_all(
      text,
      regex(
        "(?s).{0,300}(det grønlandske folk|det færøske folk|grønlændere|færinger).{0,400}",
        ignore_case = TRUE
      )
    )
  ) %>%
  unnest_longer(context) %>%
  print(n = Inf, width = Inf)

speech_data %>%
  filter(year == 1979) %>%
  pull(text) %>%
  str_extract(
    regex(
      "(?s).{0,200}hjemmestyreloven.{0,800}",
      ignore_case = TRUE
    )
  )

speech_data %>%
  filter(year == 1981) %>%
  pull(text) %>%
  str_extract(
    regex(
      "(?s).{0,200}(det færøske folk|det grønlandske folk).{0,700}",
      ignore_case = TRUE
    )
  )

speech_data %>%
  filter(year == 1981) %>%
  pull(text) %>%
  str_extract(
    regex(
      "(?s).{0,250}(det færøske folk|det grønlandske folk|færinger|grønlændere).{0,700}",
      ignore_case = TRUE
    )
  )

speech_data %>%
  filter(year >= 1982, year <= 1999) %>%
  transmute(
    year,
    context = str_extract_all(
      text,
      regex(
        "(?s).{0,120}(det færøske folk|det grønlandske folk|det færøske samfund|det grønlandske samfund|færingerne|grønlænderne).{0,180}",
        ignore_case = TRUE
      )
    )
  ) %>%
  unnest_longer(context) %>%
  filter(!is.na(context)) %>%
  print(n = Inf, width = Inf)

speech_data %>%
  filter(year == 1987) %>%
  pull(text) %>%
  str_extract(
    regex(
      "(?s).{0,250}Danske Rige.{0,700}",
      ignore_case = TRUE
    )
  )

speech_data %>%
  filter(year == 1990) %>%
  pull(text) %>%
  str_extract(
    regex(
      "(?s).{0,250}både det grønlandske og.{0,500}",
      ignore_case = TRUE
    )
  )

speech_data %>%
  filter(year == 1997) %>%
  pull(text) %>%
  str_extract(
    regex(
      "(?s).{0,250}det færøske samfund.{0,1200}",
      ignore_case = TRUE
    )
  )

speech_data %>%
  filter(year >= 2000) %>%
  transmute(
    year,
    monarch,
    context = str_extract_all(
      text,
      regex(
        "(?s).{0,120}(det færøske folk|det grønlandske folk|det færøske samfund|det grønlandske samfund|færingerne|grønlænderne|færinger|grønlændere).{0,180}",
        ignore_case = TRUE
      )
    )
  ) %>%
  unnest_longer(context) %>%
  filter(!is.na(context)) %>%
  print(n = Inf, width = Inf)

speech_data %>%
  filter(year == 2025) %>%
  transmute(
    context = str_extract_all(
      text,
      regex(
        "(?s).{0,250}(færinger|grønlændere|færingerne|grønlænderne).{0,500}",
        ignore_case = TRUE
      )
    )
  ) %>%
  unnest_longer(context) %>%
  print(n = Inf, width = Inf)

speech_data %>%
  filter(year == 2025) %>%
  pull(text) %>%
  str_extract_all(
    regex(
      "(?s).{0,300}(færinger|grønlændere|færingerne|grønlænderne).{0,700}",
      ignore_case = TRUE
    )
  ) %>%
  unlist()

speech_data %>%
  mutate(
    word_count = str_count(text, "\\S+")
  ) %>%
  summarise(
    avg_words = mean(word_count),
    min_words = min(word_count),
    max_words = max(word_count)
  )

speech_data %>%
  mutate(
    word_count = str_count(text, "\\S+")
  ) %>%
  mutate(
    decade = floor(year / 10) * 10
  ) %>%
  group_by(decade) %>%
  summarise(
    speeches = n(),
    avg_words = mean(word_count)
  )

speech_data %>%
  mutate(
    word_count = str_count(text, "\\S+"),
    greenland_per_1000 = groenland / word_count * 1000,
    faroe_per_1000 = faeroerne / word_count * 1000,
    decade = floor(year / 10) * 10
  ) %>%
  group_by(decade) %>%
  summarise(
    speeches = n(),
    avg_greenland_per_1000 = mean(greenland_per_1000),
    avg_faroe_per_1000 = mean(faroe_per_1000)
  )

speech_normalized <- speech_data %>%
  mutate(
    word_count = str_count(text, "\\S+"),
    greenland_per_1000 = groenland / word_count * 1000,
    faroe_per_1000 = faeroerne / word_count * 1000
  )

model_greenland_normalized <- lm(
  greenland_per_1000 ~ year,
  data = speech_normalized
)

summary(model_greenland_normalized)

speech_normalized <- speech_data %>%
  mutate(
    word_count = str_count(text, "\\S+"),
    greenland_per_1000 = groenland / word_count * 1000,
    faroe_per_1000 = faeroerne / word_count * 1000
  )

model_greenland_normalized <- lm(
  greenland_per_1000 ~ year,
  data = speech_normalized
)

summary(model_greenland_normalized)
  
  model_faroe_normalized <- lm(
    faroe_per_1000 ~ year,
    data = speech_normalized
  )
  
  summary(model_faroe_normalized)
