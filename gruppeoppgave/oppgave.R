
# Pakker
library(tidyverse)
library(janitor)
library(car)




# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")



# cleaning names with janitor. 
Bukta_data <- Bukta_data %>% 
  clean_names()


#opg 1
# Lag en tabell over total inntekt per år, deltakere per år og inntekt per deltaker. 
# Lag en tilsvarende tabell, men nå bryt samme data opp i år og dager. 
# Lag grafer av disse tabellene (stolpediagram), hvor du viser inntekt per år og inntekt per år og dag
# Fra disse to tabellene, ser det ut til å være noe forskjeller mellom årene og/eller dagene for hvert år?
# Vi kan ennå ikke si noe om det er statistisk signifikant, men ser du noen trender i dataene? 

bukta_dag <- Bukta_data %>% 
  group_by(ar, dag) %>% 
  summarize(
    inntekt = sum(antall * pris),
    gjester = max(gjester),
    inntekt_per_gjest = inntekt / gjester
  )

bukta_ar <- bukta_dag %>% 
  group_by(ar) %>% 
  summarise(
    inntekt = sum(inntekt),
    gjester = sum((gjester)),
    inntekt_per_gjest = inntekt / gjester
  )


# kommenter funnene

bukta_dag %>%
  ggplot(aes(ar, inntekt, fill = dag)) +
  geom_col(position = "dodge", ) +
  labs(x = "", y = "Inntekt") +
  theme_classic() +
  scale_y_continuous(labels = scales::comma)

bukta_ar %>%
  ggplot(aes(ar, inntekt, fill = ar)) +
  geom_col(position = "dodge", ) +
  labs(x = "", y = "Inntekt") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::comma) #breaks = seq(0,300000, 20000))



#opg 2.1
#  Lag en tabell over total inntekt for hver produkt, og plott også resultatet i et stolpediagram
#  Hvilket produkt virker til å bringe inn mest inntekter og hvilket bringer inn minst?
# Lag en ny tabell over total inntekt for hvert produkt per år. Igjen plott resultatet i et stolpediagram. 

# Ser det ut til å være noe forskjell i distribusjonen av kjøp av drikke varer 
# mellom forskjellige år. Altså er et produkt mer eller mindre populært noen år?

# Dataframe for solgte produkter, fordelt på år.
bukta_produkt_ar <- Bukta_data %>% 
  group_by(ar, produkt) %>% 
  summarise(
    inntekt_produkt = sum(antall * pris),
    antall = sum(antall)
    )

# Samlet inntekt av produkter. 
bukta_produkt_ar %>% 
  ggplot(aes(produkt, inntekt_produkt, fill = produkt)) +
  geom_col() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x = "Produkt", y = "Inntekt") +
  scale_y_continuous(labels = scales::comma) #breaks = seq(0,300000, 20000))


# Dataframe for solgte produkter, fordelt på år og dag.
bukta_produkt_dag <- Bukta_data %>%
  group_by(ar,dag, produkt) %>% 
  summarise(
    inntekt_produkt = sum(antall * pris),
    antall = sum(antall)
    )

# Aggregert produkter fordelt på år 
bukta_produkt_dag %>%
  rename(Produkt = produkt) %>% 
  ggplot(aes(ar, inntekt_produkt, fill = Produkt)) +
  geom_col(position = "dodge") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "År", y = "Inntekt") +
  scale_y_continuous(labels = scales::comma) #breaks = seq(0,300000, 20000))


bukta_produkt_dag %>%
  rename(Produkt = produkt) %>% 
  ggplot(aes(y = inntekt_produkt, x = Produkt,  fill = ar)) +
  geom_col(position = "dodge") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "Produkt", y = "Inntekt") +
  scale_y_continuous(labels = scales::comma)


#  kommenter funnene.


#opg 2.2
#  Finn total inntekt for alle produktene per time.
#  Gjennomfør en parvis t-test mellom produktene og inntekt, korriger p-verdiene med metoden Holm.
#  Hvilke produkter tjener Bukta festivalen mer eller mindre på, og hvilke er relativt lik? 
#  Vi forutsetter et signifikansnivå på 5% (𝛼 =0.05) i alle oppgavene. 

bukta_salg_pr_time <- Bukta_data %>% 
  group_by(dato, produkt) %>% 
  summarize(
    inntekt = sum(antall * pris),
    antall = sum(antall)
  )

pairwise.t.test(bukta_salg_pr_time$inntekt,
                bukta_salg_pr_time$produkt,
                p.adjust.method = "holm")

# ved et sigifikantnivå under 5% observere vi
# verdier lik 1 er relativt lik (wine, bukta beer, other beer lik 1 )
# cider er relativt lik (0.7)
# ytterpunkt pilsner (2e-16) og non alco (1.8e-07) 
# Ytterpunktene er veldig signifikant men kan ikke si noe om det er positivt eller negativ retning for inntjening



#opg 3.1
# Lag 3 grafer som viser sammenhengen mellom total inntekt og hver 15 min av festivalen.
# Lag en graf for torsdag men en linje for hvert år, en graf for fredag med en linje for hvert år 
# og en graf for lørdag med en line for hvert år.
# Hvilke trender ser vi i de forskjellige grafene og er det noe forskjell mellom år eller dager? 

plot_torsdag <- Bukta_data %>%
  filter(dag == "Torsdag") %>% 
  group_by(per15min, ar) %>% 
  summarize(inntekt = sum(antall * pris)) %>%
  rename(År = ar) %>% 
  ggplot(aes(per15min, inntekt, col = År)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Torsdag", x = "Per 15 min", y = "Inntekt") +
  #scale_y_continuous(labels = scales::comma, breaks = seq(0,100000, 10000)) +
  xlim(0,6) +
  ylim(0,100000)

# plot_torsdag
 
plot_fredag <- Bukta_data %>%
  filter(dag == "Fredag") %>% 
  group_by(per15min, ar) %>% 
  summarize(inntekt = sum(antall * pris)) %>%
  rename(År = ar) %>% 
  ggplot(aes(per15min, inntekt, col = År)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Fredag",x = "Per 15 min", y = "Inntekt") +
  #scale_y_continuous(labels = scales::comma, breaks = seq(0,100000, 10000))
  ylim(0,100000)

# plot_fredag


plot_lørdag <- Bukta_data %>%
  filter(dag == "Lordag") %>% 
  group_by(per15min, ar) %>% 
  summarize(inntekt = sum(antall * pris)) %>%
  rename(År = ar) %>% 
  ggplot(aes(per15min, inntekt, col = År)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Lørdag", x = "Per 15 min", y = "Inntekt") +
#  scale_y_continuous(labels = scales::comma, breaks = seq(0,100000, 10000))
  ylim(0,100000)

# plot_lørdag

# funksjon til multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  
  if (is.null(layout)) {
    
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    
    for (i in 1:numPlots) {
      
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(plot_torsdag, plot_fredag, plot_lørdag, cols=1)


#opg 3.2
# Aggregér datasettet ned til total inntekt per 15min, for hver dag og år (dette datasettet skal ha 240 rader).
# Kommentér resultatene til modellen. 
# slik denne regresjonen er satt opp går det ikke an å si noe om forskjellig inntekt mellom fredag og lørdag, 
# det er kun mulig å si om inntekten er forskjellig fra 2016 og de andre årene.
# Gjennomfør en test for å se om det er forskjell i inntekt mellom dagene, og mellom årene 2017, 2018 og 2019.
rm(per15)

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

# tibble(per15)

regresjon <-  lm(inntekt ~ ar + dag + per15min, data = per15)
summary(regresjon)



linearHypothesis(regresjon, "ar2017=ar2018")
linearHypothesis(regresjon, "ar2017=ar2019")
linearHypothesis(regresjon, "ar2018=ar2019")
linearHypothesis(regresjon, "dagFredag=dagLordag")

# Kommentér resultatene til modellen.




#opg 4
# Ledelsen i Buktafestivalen er bekymret for at det dårlige været i Tromsø påvirker salget av drikkevarer. 
# De ønsker at du gjennomfører en test av dette. 
# Gjennomfør en lineær regresjon hvor du har aggregert total inntekt per 15 min.
# I tillegg til vær legger vi til antall gjester, tid, dag og år for å passe på at forskjell som kun skyldes deltakere per år ikke 
# fanges opp i vær-variablene. 
# va kan du rapporterer til Buktafestivalens styre? Hvor sikker er du på disse resultatene? 


regresjon2 <-  lm(inntekt ~ nedbør + temp + solskin + vind + gjester + dag + ar + per15min, data = per15)
summary(regresjon2)




#opg 5
# For hver festival går et stort band på scenen på lørdag klokken 21:00. Dette gjør at 
# pilssalget klokken 20:00-21:00 er det høyeste under hele festivalen. 
# For å forbedrede seg til pils-rushet ber Buktafestivalens styreleder deg
# om å predikere hvor mye pils (i antall enheter) de må gjøre klart til denne timen. 
# Siden dette er observasjoner over et tidsintervall kan vi bruke Poisson fordelingen. 
# Bruk gjennomsnittlig antall solgte pils mellom klokken 20:00 - 21:00. 
# Hvor mange pils må gjøres klart slik at du er 95% sikker på at det ikke blir bestilt mer enn dette. 

lam <- Bukta_data %>%
  filter(time == 20,
         produkt == "Pilsner",
         dag == "Lordag") %>% 
  group_by(dato) %>% 
  summarize(antall = sum(antall))%>% 
  summarize(mean(antall))

qpois(0.95, lambda=lam[[1]])
