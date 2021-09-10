library(tidyverse)
#Tratamentos

olympics <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv ")

base_paises <- olympics %>% 
  select(id, noc, medal, year, season, sport) %>%
  filter(season == "Summer") %>% 
  group_by(noc, medal) %>% 
  summarise(observ = n(),
            .groups = "drop") %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = medal,
    values_from = observ
  ) %>% 
  select(noc, Gold, Silver, Bronze) %>% 
  mutate(n_medals = Gold+Silver+Bronze) %>%
  filter(!is.na(n_medals)) %>% 
  arrange(-Gold, -Silver, -Bronze) %>% 
  rename("Sigla do país"=noc, "Ouro" = Gold, "Prata" = Silver, "Total de medalhas" = n_medals) %>% 
  mutate("Posição" = 1:94)

base_atletas <- olympics %>% 
  select(id, name, noc, medal, year, season, sport) %>%
  filter(season == "Summer") %>% 
  group_by(id, name, noc, medal) %>% 
  summarise(observ = n(),
            .groups = "drop") %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = medal,
    values_from = observ
  ) %>% 
  select(id, name, noc, Gold, Silver, Bronze) %>% 
  mutate(n_medals = Gold+Silver+Bronze) %>%
  filter(!is.na(n_medals)) %>% 
  arrange(-Gold, -Silver, -Bronze) %>% 
  rename("Nome"=name,"Sigla do país"=noc, "Ouro" = Gold, "Prata" = Silver, "Total de medalhas" = n_medals) %>% 
  mutate("Posição" = 1:515) %>% 
  select(`Posição`, `Sigla do país`, `Total de medalhas`)
