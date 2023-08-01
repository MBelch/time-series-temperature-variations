library(urca)
## L’objet tmax.stl.fc va servir pour la suite de la modélisation:
tmax.stl.fc <- tmax.stl |> select(-.model)

############################## Etape 1##########################################
## Méthode naïve: analyse graphique des résidus
tmax.stl.fc |> model(NAIVE(season_adjust)) |> gg_tsresiduals()

## Création d’un objet contenant les résidus
tmax.sadj.naive <- tmax.stl.fc |> model(NAIVE(season_adjust)) |> augment()

## Test portmanteau de Ljung-Box avec 10 lags
tmax.sadj.naive |> features(.innov, ljung_box, lag = 10)

#===============================> Méthode de Holt
AAN=ETS(season_adjust ~ error("A") + trend("A") + season("N"))

## Méthode naïve: analyse graphique des résidus
tmax.stl.fc |> model(AAN) |> gg_tsresiduals()

## Création d’un objet contenant les résidus
tmax.sadj.AAN <- tmax.stl.fc |> model(AAN) |> augment()

## Test portmanteau de Ljung-Box avec 10 lags
tmax.sadj.AAN |> features(.innov, ljung_box, lag = 10)

##===============================> Modèle ARIMA

## Méthode naïve: analyse graphique des résidus
tmax.stl.fc |> model(ARIMA(season_adjust)) |> gg_tsresiduals()

## Création d’un objet contenant les résidus
tmax.sadj.ARIMA <- tmax.stl.fc |> model(ARIMA(season_adjust)) |> augment()

## Test portmanteau de Ljung-Box avec 10 lags
tmax.sadj.ARIMA |> features(.innov, ljung_box, lag = 10)

############################## Etape 2##########################################

## Méthode naïve: prévision de la série dé-saisonnalisée
tmax.stl.fc |> model(NAIVE(season_adjust)) |> forecast(h = 12) |> autoplot(tmax.stl.fc)

## Création d’un objet contenant le modèle de prévision basé sur la décomposition STL
fit.tmax.stl <- tmax.series |>
  model(stlf = decomposition_model(
    STL(Observation ~ trend(window = 21) + season(window = 13), robust = TRUE),
    NAIVE(season_adjust)))
## Pour afficher les paramètres du modèle
report(fit.tmax.stl)

## Prévision de la série complète avec le modèle basé sur la décomposition STL
fit.tmax.stl |> forecast(h = 12) |> autoplot(tmax.series, level = 95)

## Calcul des mesures de perfomance sur la série de test
fit.tmax.stl |> forecast(h = 12) |> accuracy(tmax.series.test)

#==========================> Pour ARIMA:
## Méthode ARIMA: prévision de la série dé-saisonnalisée
tmax.stl.fc |> model(ARIMA(season_adjust)) |> forecast(h = 12) |> autoplot(tmax.stl.fc)

## Création d’un objet contenant le modèle de prévision basé sur la décomposition STL
fit.tmax.stl <- tmax.series |>
  model(stlf = decomposition_model(
    STL(Observation ~ trend(window = 21) + season(window = 13), robust = TRUE),
    ARIMA(season_adjust)))
## Pour afficher les paramètres du modèle
report(fit.tmax.stl)

## Prévision de la série complète avec le modèle basé sur la décomposition STL
fit.tmax.stl |> forecast(h = 12) |> autoplot(tmax.series, level = 95)

## Calcul des mesures de perfomance sur la série de test
fit.tmax.stl |> forecast(h = 12) |> accuracy(tmax.series.test)

#=======================> pour ETS:
## Méthode ARIMA: prévision de la série dé-saisonnalisée
tmax.stl.fc |> model(AAN) |> forecast(h = 12) |> autoplot(tmax.stl.fc)

## Création d’un objet contenant le modèle de prévision basé sur la décomposition STL
fit.tmax.stl <- tmax.series |>
  model(stlf = decomposition_model(
    STL(Observation ~ trend(window = 21) + season(window = 13), robust = TRUE),
    AAN))
## Pour afficher les paramètres du modèle
report(fit.tmax.stl)

## Prévision de la série complète avec le modèle basé sur la décomposition STL
fit.tmax.stl |> forecast(h = 12) |> autoplot(tmax.series, level = 95)

## Calcul des mesures de perfomance sur la série de test
fit.tmax.stl |> forecast(h = 12) |> accuracy(tmax.series.test)

##==================> saisonal naive
## Création d’un objet contenant le modèle de prévision basé sur la décomposition STL
fit.tmax.stl <- tmax.series |>
  model(stlf = decomposition_model(
    STL(Observation ~ trend(window = 21) + season(window = 13), robust = TRUE),
    SNAIVE(season_adjust)))

fit.tmax.stl |> forecast(h = 12) |> accuracy(tmax.series.test)

