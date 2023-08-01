library(fpp3)
load("D:/PolyMtl/MTH8304/devoir/Devoir5/dev5_tmax.RData") ## adapter le chemin
set.seed(2190903) # remplacer par votre matricule

## Generation de tmax.series.train:
ind.cell <- sample(ncol(tmax.train), 1) ## selection de maille
tmax.series <- tsibble(
  Date = tmax.train[,1],
  Observation = tmax.train[,ind.cell+1],
  index = Date
)

## Generation de tmax.series.test:
ind.cell.test <- sample(ncol(tmax.test),1)
tmax.series.test <- tsibble(
  Date = tmax.test[,1],
  Observation = tmax.test[,ind.cell.test+1],
  index = Date
)

############################## Etape 1##########################################

## graphique temporel
autoplot(tmax.series, Observation, linewidth = 1.2)+
  labs(title = "Evolution de la temp",y = "degres C", x = "mois") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        text = element_text(size=20))

## deux types de graphiques saisonniers
tmax.series |> gg_season(Observation, labels = "both", linewidth = 1.2) +
  labs(title= "Evolution des temp. par rapport au mois de chaque année", y = "degres C",x = "mois")+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
                                           text = element_text(size=20))
tmax.series |> gg_subseries(Observation) + theme(text = element_text(size=20))

## graphique de lag: tester plusieurs lags en changeant la valeur de l’argument "lags"
tmax.series |> gg_lag(Observation, geom = "point", lags = 3) +
  theme(text = element_text(size=20),legend.title=element_blank())

## graphique d’auto-corrélation ACF
tmax.series |> ACF(Observation, lag_max = 24) |> autoplot()

############################## Etape 2##########################################

## Décomposition STL stockée dans l’objet tmax.stl
tmax.series |> model(STL(Observation ~ trend(window = 21) + season(window = 13), robust = TRUE)) |>
  components() -> tmax.stl

## Graphique standard de la décomposition
autoplot(tmax.stl)

## Graphique de l’ACF de la composante résiduelle
tmax.stl |> ACF(remainder, lag_max = 24) |> autoplot()

############################## Etape 3##########################################

## Méthode naïve saisonnière stocké dans l’objet tmax.benchmark
tmax.benchmark <- tmax.series |> model('Naive saison.' = SNAIVE(Observation))

## Prévision sur 12 mois avec le modèle naïf
tmax.benchmark.fc <- tmax.benchmark |> forecast(h = 12)

## Graphique avec intervalle de prévision 95%
tmax.benchmark.fc |> autoplot(tmax.series, level = 95, linewidth = 1.2) +
  autolayer(tmax.series.test, Observation,linewith = 1.2, linetype = 2)+
  autolayer(tmax.series, Observation, linewidth = 1.2)+
  labs(y = "degres C", x = "mois") + theme(text = element_text(size=12))

tmax.benchmark.fc |> accuracy(tmax.series.test)

