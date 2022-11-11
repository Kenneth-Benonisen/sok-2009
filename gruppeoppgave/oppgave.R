

# Koden tilhører kandidatnummer: 13, 73, 31, 4, 58

# Pakker.
library(tidyverse)
library(janitor)
library(car)

# Henter ned datasettet fra kilden.
githubURL <- "https://github.com/uit-sok-2009-h22/uit-sok-2009-h22.github.io/blob/main/filer/Bukta_data_v2.Rdata?raw=true"
load(url(githubURL))

# Kode for a kunne bruke norske bokstaver.
Sys.setlocale(locale="no_NO")

# cleaning names with janitor. 
Bukta_data <- Bukta_data %>% 
  clean_names()

#opg 1  ------------------------------------------------------------------------

# Opprettet tabell for dag.
bukta_dag <- Bukta_data %>% 
  group_by(ar, dag) %>% 
  summarize(
    inntekt = sum(antall * pris),
    gjester = max(gjester),
    inntekt_per_gjest = inntekt / gjester
  )

tibble(bukta_dag) # viser datasettet.

# Opprettet tabell for år.
bukta_ar <- bukta_dag %>% 
  group_by(ar) %>% 
  summarise(
    inntekt = sum(inntekt),
    gjester = sum((gjester)),
    inntekt_per_gjest = inntekt / gjester
  )

tibble(bukta_ar) # viser datasettet.

# Plotter graf, fordelt inntekt på dag og år.
bukta_dag %>%
  rename(Dag = dag) %>% 
  ggplot(aes(ar, inntekt, fill = Dag)) +
  geom_col(position = "dodge", ) +
  labs(x = "", y = "Inntekt") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::comma)

# Plotter graf, samlet inntekt hver dag og fordelt på år.
bukta_ar %>%
  ggplot(aes(ar, inntekt, fill = ar)) +
  geom_col(position = "dodge", ) +
  labs(x = "", y = "Samlet inntekt") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::comma) #breaks = seq(0,300000, 20000))



#opg 2.1  ----------------------------------------------------------------------

# Dataframe for solgte produkter, fordelt på år.
bukta_produkt_ar <- Bukta_data %>% 
  group_by(ar, produkt) %>% 
  summarise(
    inntekt_produkt = sum(antall * pris),
    antall = sum(antall)
    )

tibble(bukta_produkt_ar) # viser datasettet

# Plotter graf for samlet inntekt av produkter.
bukta_produkt_ar %>% 
  ggplot(aes(produkt, inntekt_produkt, fill = produkt)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Produkt", y = "Samlet inntekt") +
  scale_y_continuous(labels = scales::comma) #breaks = seq(0,300000, 20000))


# Tabell for solgte produkter, fordelt på år og dag.
bukta_produkt_dag <- Bukta_data %>%
  group_by(ar,dag, produkt) %>% 
  summarise(
    inntekt_produkt = sum(antall * pris),
    antall = sum(antall)
    )
 
tibble(bukta_produkt_dag) # viser datasettet

# Plotter graf av inntektene til produktene fordelt på år. 
bukta_produkt_dag %>%
  rename(Produkt = produkt,
         År = ar) %>% 
  ggplot(aes(y = inntekt_produkt, x = Produkt,  fill = År)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Produkt", y = "Inntekt") +
  scale_y_continuous(labels = scales::comma)


#opg 2.2  ----------------------------------------------------------------------

# dataframe for salg pr time.
bukta_salg_pr_time <- Bukta_data %>% 
  group_by(dato, produkt) %>% 
  summarize(
    inntekt = sum(antall * pris),
    antall = sum(antall)
  )

# t.test.
pairwise.t.test(bukta_salg_pr_time$inntekt,
                bukta_salg_pr_time$produkt,
                p.adjust.method = "holm")

#opg 3.1  ----------------------------------------------------------------------

# Plotter inntekt fordelt på år og dag.
Bukta_data %>%
  group_by(per15min, ar, dag)%>% 
  summarize(inntekt = sum(antall * pris)) %>%
  rename(År = ar) %>% 
  ggplot(aes(per15min, inntekt, col = År)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Per 15 min", y = "Inntekt") +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(~dag)

#opg 3.2 -----------------------------------------------------------------------

# Dataframe for data per kvarter.
per15 <- Bukta_data %>%
  mutate(inntekt = sum(antall * pris)) %>% 
  group_by(per15min, ar, dag)%>% 
  summarize(
    inntekt = sum(inntekt),
    gjester = unique(gjester),
    nedbør = unique(nedbor),
    temp = unique(luft_temperatur),
    solskin = unique(solskin),
    vind = unique(vind)
  )

# Regresjonsanalyse.
regresjon <-  lm(inntekt ~ ar + dag + per15min, data = per15)
summary(regresjon)

linearHypothesis(regresjon, "ar2017=ar2018")
linearHypothesis(regresjon, "ar2017=ar2019")
linearHypothesis(regresjon, "ar2018=ar2019")
linearHypothesis(regresjon, "dagFredag=dagLordag")

#opg 4  ------------------------------------------------------------------------

# Regresjonsanalyse.
regresjon2 <-  lm(inntekt ~ nedbør + temp + solskin + vind + gjester + dag + ar + per15min, data = per15)
summary(regresjon2)


#opg 5  ------------------------------------------------------------------------

# Dataframe for qpois.
lam <- Bukta_data %>%
  filter(time == 20,
         produkt == "Pilsner",
         dag == "Lordag") %>% 
  group_by(dato) %>% 
  summarize(antall = sum(antall))%>% 
  summarize(mean(antall))

# kjører qpois. 
qpois(0.95, lambda=lam[[1]])
