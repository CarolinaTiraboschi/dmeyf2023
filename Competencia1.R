# Librerías necesarias
require("data.table")
require("rpart")
require("ROCR")
require("ggplot2")


# Poner la carpeta de la materia de SU computadora local
setwd("D:/Documentos2/MaestríaDS/DM-EF")
# Poner sus semillas
semillas <- c(194113, 162677, 841213, 415147, 314597)

# Cargamos el dataset
dataset <- fread("./datasets/competencia_01.csv")

lo# Nos quedamos solo con el 202101
dataset <- dataset[foto_mes == 202103]
# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  clase_ternaria == "BAJA+2",
  "evento",
  "noevento"
)]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]

# Seteamos nuestra primera semilla
set.seed(semillas[1])

# Particionamos de forma estratificada
in_training <- caret::createDataPartition(dataset$clase_binaria,
                                          p = 0.70, list = FALSE) 
dtrain  <-  dataset[in_training, ]
dtest   <-  dataset[-in_training, ]
resultados_grid_search <- data.table()

# Complete los valores que se van a combinar para cada parámetro a explorar

for (cp in c(-0.5,-10,-5)) { 
  for (md in c(7, 15, 30, 2)) {
    for (ms in c(200, 3000, 2000, 350)) {
      for (mb in c(10, as.integer(ms / 2))) {
        
        t0 <- Sys.time()
        gan_semillas <- c()
        for (s in semillas) {
          set.seed(s)
          in_training <- caret::createDataPartition(dataset[,
                                                            get("clase_binaria")],
                                                    p = 0.70, list = FALSE)
          dtrain  <-  dataset[in_training, ]
          dtest   <-  dataset[-in_training, ]
          
          modelo <- rpart(clase_binaria ~ .,
                          data = dtrain,
                          xval = 0,
                          cp = cp,
                          minsplit = ms,
                          minbucket = mb,
                          maxdepth = md)
          
          pred_testing <- predict(modelo, dtest, type = "prob")
          gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3
          
          gan_semillas <- c(gan_semillas, gan)
        }
        tiempo <-  as.numeric(Sys.time() - t0, units = "secs")
        
        resultados_grid_search <- rbindlist(list(
          resultados_grid_search,
          data.table(
            tiempo = tiempo,
            cp = cp,
            mb = mb,
            ms = ms,
            md = md,
            gan = mean(gan_semillas)) # se puede agregar el sd?
        ))
      }
    }
  }
}

# Visualizo los parámetros de los mejores parámetros
print(resultados_grid_search[gan == max(gan), ])


# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("D:/Documentos2/MaestríaDS/DM-EF") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/competencia_01.csv")

dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = dtrain, # los datos donde voy a entrenar
  xval = 0,
  cp = -5, # esto significa no limitar la complejidad de los splits
  minsplit = 2000, # minima cantidad de registros para que se haga el split
  minbucket =7 , # tamaño minimo de una hoja
  maxdepth = 10
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
  object = modelo,
  newdata = dapply,
  type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
       file = "./exp/KA2001/K101_005.csv",
       sep = ","
)
