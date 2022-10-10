library(dplyr)
library(tidyr)
library(janitor)
library(openxlsx)
library(ggplot2)
library(ggpmisc)
library(ggpubr)

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

p4 <- dataset %>%
  filter(Partito == "MOVIMENTO 5 STELLE") %>%
  group_by(Zona) %>%
  do({model = lm(percVotes ~ PercRdC, data = .);
  data.frame(Obs = nrow(model$model),
             Alpha = coef(model)[1], 
             Beta = coef(model)[2],
             R2 = summary(model)$r.squared)}) %>%
  pivot_longer(-c(Obs, Zona), names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = Obs, y = Value, color = Zona)) +
  geom_point() +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(title = "Risultati per zona",
       subtitle = "medesimo set",
       y = "")

p2 <- dataset %>%
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

p1 <- dataset %>%
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

fullReg <- lm(formula = percVotes ~ PercRdC, data = dataset)

mean(abs(fullReg$residuals))
fullReg$coefficients
summary(fullReg)

dataset %>%
  group_by(Zona) %>%
  summarise(Obs = n())
  
### TEST ######################
Obs <- seq(from = 10, to = 120, by = 2)
Obs <- seq(from = 10, to = 300000, by = 1000)

set.seed(1234)
if(exists("regset")) rm(regset)
for(Ob in Obs) {
  print(Ob)
  x = runif(Ob, min = 0, max = 0.06)
  e = rnorm(Ob, mean = mean(fullReg$residuals), sd = sd(fullReg$residuals))
  y = fullReg$coefficient[1] + fullReg$coefficient[2] * x + e
  
  linreg = lm(y~x)

  df <- data.frame(Obs = Ob, R2 = summary(linreg)$r.squared, Alpha = linreg$coefficient[1], Beta = linreg$coefficient[2])
  
  if(exists("regset")) regset <- rbind(regset, df) else regset <- df
}

################################################
hlines <- data.frame(Metric = c("R2", "Alpha", "Beta"),
                     Value = c(NA, fullReg$coefficient[1], fullReg$coefficient[2]),
                     stringsAsFactors = F)

p3 <- regset %>%
  filter(Obs > 1000) %>%
    pivot_longer(-Obs, names_to = "Metric", values_to = "Value") %>%
    ggplot() +
    geom_hline(data = hlines, aes(yintercept = Value), color = "Red") +
    geom_point(aes(x = Obs, y = Value)) +
    facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
    theme_bw() +
    labs(subtitle = paste0("error ~ ", "N(", round(mean(fullReg$residuals),4), ", ", round(sd(fullReg$residuals),4) ,")"),
         title = paste0("y = ",  round(fullReg$coefficient[1],4), " + ", round(fullReg$coefficient[2],4), "*x + error"),
         y = "")

ggarrange(p1, p2, p3, p4, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
