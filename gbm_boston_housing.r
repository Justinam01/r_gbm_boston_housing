# Chargement des librairies
library(tidyverse)
library(gbm)

# Chargement des données
data <- read.csv("./housing.csv")

# Aperçu des données
head(data)

# Statistiques descriptives
summary(data)

# Vérification des valeurs manquantes
print(sum(is.na(data)))

# Séparation des données en ensemble d'entraînement et de test
set.seed(123)
index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[index, ]
test_data <- data[-index, ]


summary(train_data)
# Ajustement du modèle GBM
gbm_model <- gbm(
  formula = MEDV ~ ., # medv est la variable cible
  data = train_data,
  distribution = "gaussian", # Régression pour des valeurs continues
  n.trees = 1000, # Nombre d'arbres
  interaction.depth = 3, # Profondeur des arbres
  shrinkage = 0.01, # Taux d'apprentissage
  cv.folds = 5, # Validation croisée
  n.cores = NULL, # Utiliser tous les cœurs disponibles
  verbose = FALSE
)


# Affichage du modèle
print(gbm_model)
summary(gbm_model)

#Evaluation du modèle:

# Prédictions sur l'ensemble de test
predictions <- predict(gbm_model, newdata = test_data, n.trees = gbm_model$n.trees)

# Calcul de l'erreur quadratique moyenne (MSE)
mse <- mean((predictions - test_data$MEDV)^2)

# Calcul de la racine carrée de l'erreur quadratique moyenne (RMSE)
rmse <- sqrt(mse)
cat("La racine carrée de l'erreur quadratique moyenne: ", round(rmse, 2))


# Calcul du coefficient de determination R²
ss_res <- sum((predictions - test_data$MEDV)^2) #somme des carrées des residus
ss_tot <- sum((mean(train_data$MEDV) - test_data$MEDV)^2) #somme des carrées totaux
r_squared <- 1 - (ss_res / ss_tot)

cat("Le coefficient de determination R² est:", r_squared, "\n")


