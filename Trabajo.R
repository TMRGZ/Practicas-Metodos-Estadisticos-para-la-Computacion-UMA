library(tidyverse)

########################################
######### Funciones necesarias #########

#### Calcular ajuste lineal
# df: DataFrame
# y: Variable Dependiente/explicada
# x: Variable/s Independiente/explicatoria
####
linearAdjust <- function(df, y, x) {
  lm(str_c(y, "~", str_c(x, collapse = "+")), df)
}

#### Calcular Regresiones lineales con R2 asociado
# df: DataFrame
# y: Variable Dependiente/explicada
# x: Variable/s Independiente/explicatoria
####
regresionesLineales <- function(df, y, x) { # Preparar función para usar el apply automaticamente
  mod <- linearAdjust(df, y, x)
  list(Ajuste = mod, R2 = summary(mod)$r.squared)
}

#### Imprimir gráfica de dispersión junto al ajuste lineal
# mod: Modelo lineal
####
graficaDispRegr <- function(mod) {
  print(ggplot(mod$model, aes_string(x = names(mod$model)[2], y = names(mod$model)[1])) +
          geom_point() +
          stat_smooth(method = "lm", col = "red") +
          labs(title = paste("R2 = ", signif(summary(mod)$r.squared, 5),
                             "Intercept =", signif(mod$coef[[1]], 5),
                             " Slope =", signif(mod$coef[[2]], 5))))
}

#### Separador de datos por porcentajes
# datos: Conjunto de datos
# pEntrenamiento: Porcentaje dedicado a entrenamiento
# pTest: Porcentaje restante dedicado a tests
####
separarSets <- function(datos, pEntrenamiento, pTest) {
  rDF <- seq_len(nrow(datos))
  rTrain <- sample(rDF, pEntrenamiento * length(rDF))
  rAux <- setdiff(rDF, rTrain)
  rTest <- sample(rAux, pTest * length(rAux))
  rVal <- setdiff(rAux, rTest)

  list(train = datos[rTrain,], test = datos[rTest,], val = datos[rVal,])
}

#### Calcular MSE, Varianza Y, R2, R2 ajustado manualmente
# df: DataFrame
# mod: Modelo lineal
# y: Variable Dependiente/explicada
####
calcR2 <- function(df, mod, y) {
  MSE <- mean((df[[y]] - predict.lm(mod, df))^2)
  varY <- mean(df[[y]]^2) - mean(df[[y]])^2
  R2 <- 1 - MSE / varY
  aR2 <- 1 - (1 - R2) * (nrow(df) - 1) / (nrow(df) - mod$rank)
  tibble(MSE = MSE, varY = varY, R2 = R2, aR2 = aR2)
}

#### Calcular modelo, MSE, Varianza Y, R2, R2 ajustado con los datos separados
# dfTrain: DataFrame entrenamiento
# dfTest: DataFrame testeo
# y: Variable Dependiente/explicada
# x: Variable/s Independiente/explicatoria
####
calcModR2 <- function(dfTrain, dfTest, y, x) {
  mod <- linearAdjust(dfTrain, y, x)
  calcR2(dfTest, mod, "IMC")$aR2
}

#### Calcular el mejor ajuste interativamente
# dfTrain: DataFrame entrenamiento
# dfTest: DataFrame testeo
# varPos: Variables de los datos
####
encontrarMejorAjuste <- function(dfTrain, dfTest, varPos) {
  bestVars <- character(0)
  aR2 <- 0

  repeat {
    aR2v <- map_dbl(varPos, ~calcModR2(dfTrain, dfTest, y = "IMC", c(bestVars, .)))
    i <- which.max(aR2v)
    aR2M <- aR2v[i]

    if (aR2M <= aR2) break

    #cat(sprintf("%1.4f %s\n", aR2M, varPos[i])) # DEBUG
    aR2 <- aR2M
    bestVars <- c(bestVars, varPos[i])
    varPos <- varPos[-i]
  }

  mod <- linearAdjust(dfTrain, "IMC", bestVars)
  list(vars = bestVars, mod = mod)
}

######### Funciones necesarias #########
########################################

# Punto 1: Cargar CSV como tibble asegurando factores
datos <- read_csv("Datos.csv",
                  col_types = cols(
                    .default = col_double(),
                    sexo = col_factor(),
                    dietaEsp = col_factor(),
                    nivEstPad = col_integer(),
                    nivEstudios = col_integer(),
                    nivIngresos = col_integer(),
                    edad = col_integer()
                  )) # Leer CSV asegurando factores

# Punto 2: Añadir columna IMC
#datos <- datos %>% mutate(nuevoIMC = peso / (altura^2)) # Añadir la columna calculada IMC al dataframe, comentado al estar ya calculado

# Punto 3: Filtrar NA
datos <- datos %>% drop_na() # Eliminar filas que contengan NA

# Punto 4: Calcular medias y desviaciones típicas de las variables
datos.varNumericas <- datos[, sapply(datos, is.numeric)]
medias <- colMeans(datos.varNumericas)
desviaciones <- apply(datos.varNumericas, 2, sd)

# Punto 5: hacer regresion IMC con el resto de variables menos altura y peso
datos.varExplicatorias <- datos.varNumericas[-c(1, 2, 3)] # Conseguir variables utiles para la regresión
varPred <- colnames(datos.varExplicatorias)
coeficientesRegresion <- varPred %>% map(regresionesLineales, df = datos, y = "IMC")

# Punto 6: Graficos dispersion en numericas + regresion y box-plots en cualitativas
#     Crear PDF de BoxPlots
pdf("graficasBox.pdf")
print(ggplot(datos, aes(x = sexo, y = IMC)) + geom_boxplot())
print(ggplot(datos, aes(x = dietaEsp, y = IMC)) + geom_boxplot())
dev.off()
#     Crear PDF de Dispersiones
pdf("graficasDispersion.pdf")
for (i in seq_along(varPred)) {
  graficaDispRegr(coeficientesRegresion[[i]]$Ajuste)
}
dev.off()

# Punto 7: Separar los datos en entrenamiento 60%, test 20% y validacion 20%
datos.separados <- separarSets(datos, 0.6, 0.5)

# Punto 8: Selecciona cual de las 11 variables explica mejor IMC
ar2 <- varPred %>% map_dbl(calcModR2, dfTrain = datos.separados$train, dfTest = datos.separados$test, y = "IMC")
bestVar <- varPred[which.max(ar2)]

# Punto 9: Selecciona un modelo óptimo lineal de regresión
bestMod <- encontrarMejorAjuste(datos.separados$train, datos.separados$test, varPred)

# Punto 10: Evalua el resultado en el conjunto de validación
modEval <- calcR2(datos.separados$val, bestMod$mod, "IMC")

# Punto 11: Expresa tus conclusiones
print("Mejor variable individual: ")
bestVar
print("Mejor ajuste lineal")
bestMod$mod
print("Evaluacion del modelo")
modEval



