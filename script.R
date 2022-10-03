library(dplyr)
library(tidyr)
library(janitor)
library(openxlsx)
library(ggplot2)
library(ggpmisc)

rm(list = ls())

PercRedCitt <- read.xlsx("PercRedPensCittadinanza.xlsx") %>%
  mutate(Provincia = toupper(Provincia))

Elect2022 <- read.csv("Politiche2022_Scrutini_Camera_Italia.csv", sep = ";") %>%
  select(Provincia = PROVINCIA,
         Comune = COMUNE,
         Collegio = COLLEGIO.UNINOMINALE,
         Cognome = COGNOME,
         Nome = NOME,
         Partito = LISTA,
         Votanti = VOTANTI.TOTALI,
         Elettori = ELETTORI.TOTALI,
         Voti = VOTI.CANDIDATO)

Voters <- Elect2022 %>%
  group_by(Provincia, Comune, Collegio) %>%
  summarise(Votanti = first(Votanti)) %>%
  group_by(Provincia, Comune) %>%
  summarise(Votanti = sum(Votanti)) %>%
  group_by(Provincia) %>%
  summarise(Votanti = sum(Votanti))

Votes <- Elect2022 %>%
  group_by(Provincia, Comune, Cognome, Nome) %>%
  summarise(Partito = first(Partito),
            Voti = first(Voti)) %>%
  group_by(Partito, Provincia) %>%
  summarise(Voti = sum(Voti))

dataset <- Votes %>%
  left_join(Voters, by = "Provincia") %>%
  mutate(percVotes = Voti/Votanti) %>%
  full_join(PercRedCitt, by = "Provincia")

dataset %>%
  filter(Partito == "MOVIMENTO 5 STELLE") %>%
  #as.data.frame()
  ggplot(aes(x = PercRdC, y = percVotes, color = Zona)) +
  stat_poly_line(formula = y ~ x, se = F) +
  stat_poly_eq(use_label(c("eq", "R2")),formula = y ~ x) +
  #geom_smooth(formula = y ~ x, method = "lm") +
  geom_point() +
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = scales::percent)+
  theme_bw() +
  labs(x = "Percentuale percettori Redd/Pensione di cittadinanza",
       y = "Percentuale voto Camera Movimento 5 stelle",
       title = "Voti Camera dei deputati - Sep/2022")

dataset %>%
  filter(Partito == "MOVIMENTO 5 STELLE") %>%
  #as.data.frame()
  ggplot(aes(x = PercRdC, y = percVotes)) +
  stat_poly_line(formula = y ~ x, se = T) +
  stat_poly_eq(use_label(c("eq", "R2")),formula = y ~ x) +
  #geom_smooth(formula = y ~ x, method = "lm") +
  geom_point() +
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(labels = scales::percent)+
  theme_bw() +
  labs(x = "Percentuale percettori Redd/Pensione di cittadinanza",
       y = "Percentuale voto Camera Movimento 5 stelle",
       title = "Voti Camera dei deputati - Sep/2022")

dataset <- dataset %>%
  filter(Partito == "MOVIMENTO 5 STELLE")

summary(lm(formula = percVotes ~ PercRdC, data = dataset))
