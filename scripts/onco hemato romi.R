library(readxl)
PEPE_OncoHemato <- read_excel("PEPE_OncoHemato.xlsx", 
                              col_types = c("numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "text", "numeric", 
                                            "text", "numeric", "text", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "text", "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "text", "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "text", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric"))
View(PEPE_OncoHemato)



datos<-PEPE_OncoHemato


nombrescol<-colnames(datos)
print(nombrescol)



library(tidyverse)

df_analisis <- datos %>%
  select(
    # --- VARIABLE RESPUESTA (Y) ---
    muerte_uti = MUERTO_UTI,
    
    # --- DEMOGRÁFICOS ---
    edad = EDAD,
    sexo_masc = SEXO_masc,
    
    # --- HEMATOLOGÍA / INMUNIDAD ---
    lma_lla = `EnfeHemato_LMA/LLA`,
    tx_alo = TALLO_si,
    neutropenia = Neutropenia_ingreso,
    
    # --- SCORES Y SOPORTE ---
    saps_2 = `SAPS II`,
    sofa = `SOFA 24`,
    arm = `ARM inicio`,      # <--- ¡AQUÍ FALTABA LA COMA!
    
    # --- LAS VARIABLES NUEVAS ---
    lactato = `Ac Lact`,
    dx_ingreso = DxUTI,
    falla_renal = `Falla renal` # Esta es la última, así que NO lleva coma
  )

# Verificamos
glimpse(df_analisis)


# contar NAs en cada variable
colSums(is.na(df_analisis))


# --- PASO 0: Instalar librerías (solo si no las tienes) ---
if (!require("mice")) install.packages("mice")
library(tidyverse)
library(mice)

# --- PASO 1: Selección de Variables ---
# Sacamos las variables con demasiados NAs (lactato, falla_renal) y la redundante (saps_2)
df_limpieza <- df_analisis %>%
  select(-lactato, -falla_renal, -saps_2)

# --- PASO 2: Imputación Robusta (MICE) ---
# Usamos method = 'pmm' (Predictive Mean Matching). 
# Esto asegura que los valores imputados sean valores REALES observados en otros pacientes 
# (ej: números enteros) y no decimales matemáticos imposibles.

# m=1 es suficiente porque solo son 3 datos, pero el purista usaría m=5 para ver variabilidad.
# Para este paso práctico, generamos 1 base de datos completa.
imputacion <- mice(df_limpieza, m = 1, method = 'pmm', seed = 123, printFlag = FALSE)
df_imputado <- complete(imputacion)

# --- PASO 3: Etiquetado y Factores ---
# Ahora que la base está completa, le ponemos los nombres bonitos
df_final <- df_imputado %>%
  mutate(
    # Convertimos Diagnóstico a Factor con etiquetas
    dx_ingreso = factor(dx_ingreso,
                        levels = c(1, 2, 3, 4, 5),
                        labels = c("Sepsis/Shock", 
                                   "Insuf. Resp.", 
                                   "Neurológico", 
                                   "Otras compl.", 
                                   "Post-Qx")
    ),
    # Convertimos Sexo a Factor
    sexo_masc = factor(sexo_masc, 
                       levels = c(0, 1), 
                       labels = c("Femenino", "Masculino")),
    
    # Aseguramos que variables binarias sean factores también (buena práctica purista)
    muerte_uti = factor(muerte_uti, levels = c(0,1), labels = c("Vivo", "Muerto")),
    arm = factor(arm, levels = c(0,1), labels = c("No", "Si")),
    lma_lla = factor(lma_lla, levels = c(0,1), labels = c("No", "Si")),
    tx_alo = factor(tx_alo, levels = c(0,1), labels = c("No", "Si")),
    neutropenia = factor(neutropenia, levels = c(0,1), labels = c("No", "Si"))
  )

# --- VERIFICACIÓN FINAL ---
print("NAs restantes:")
colSums(is.na(df_final)) # ¡Debería ser TODO CERO!

print("Resumen de SOFA (ahora completo):")
summary(df_final$sofa)

glimpse(df_final)

install.packages("broom.helpers")
library(tidyverse)
library(gtsummary)

# Seleccionamos las variables exactas que me pasaste
tabla_univariada <- df_final %>%
  select(
    muerte_uti,    # Variable Respuesta (Outcome)
    edad,          # Predictoras...
    sexo_masc,
    lma_lla,
    tx_alo,
    neutropenia,
    sofa,
    arm,
    dx_ingreso
  ) %>%
  tbl_uvregression(
    method = glm,                           # Regresión lineal generalizada
    y = muerte_uti,                         # Definimos la Y
    method.args = list(family = binomial),  # Definimos que es LOGÍSTICA
    exponentiate = TRUE,                    # Para obtener Odds Ratios (OR)
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  bold_p(t = 0.05) %>%       # Negrita a los significativos
  add_n(location = "level")  # Muestra cuántos pacientes hay en cada categoría

# Mostrar la tabla
tabla_univariada



library(car) # Necesario para funcion vif()

# 1. ARREGLAR LA VARIABLE dx_ingreso
# Juntamos Post-Qx con Otras complicaciones para evitar el error
df_multivariado <- df_final %>%
  mutate(dx_ingreso = fct_collapse(dx_ingreso,
                                   "Otras/PostQx" = c("Otras compl.", "Post-Qx") 
                                   # Mantenemos Sepsis, Insuf Resp y Neuro como están
  ))

# 2. MODELO PREVIO PARA CHEQUEAR COLINEALIDAD (VIF)
# Hacemos un modelo logístico con todas juntas SOLO para ver si se "pelean"
modelo_vif <- glm(
  muerte_uti ~ edad + sexo_masc + lma_lla + tx_alo + neutropenia + sofa + arm + dx_ingreso,
  data = df_multivariado,
  family = binomial
)

# 3. VER EL VIF
vif(modelo_vif)



modelo_final <- glm(
  muerte_uti ~ edad + sexo_masc + lma_lla + tx_alo + neutropenia + sofa + arm + dx_ingreso,
  data = df_multivariado, # Usamos la base con el dx arreglado
  family = binomial
)

# TABLA FINAL LISTA PARA PUBLICAR
tabla_final <- tbl_regression(
  modelo_final, 
  exponentiate = TRUE, # OR
  pvalue_fun = ~style_pvalue(.x, digits = 3)
) %>%
  bold_p(t = 0.05) %>%
  add_global_p() %>% # Agrega valor p global para variables categóricas (dx_ingreso)
  bold_labels() %>%
  italicize_levels()

# Mostrar
tabla_final



# Instalar si no lo tenés (es rápido)
install.packages("ggstats")
library(ggplot2)
library(ggstats)

library(ggstats)

ggcoef_model(
  modelo_final,
  exponentiate = TRUE,           # OR
  # exclude_intercept = TRUE,    <-- BORRAMOS ESTA LINEA
  variable_labels = c(
    edad = "Edad",
    sexo_masc = "Sexo Masculino",
    lma_lla = "Leucemia (LMA/LLA)",
    tx_alo = "Transplante Alogénico",
    neutropenia = "Neutropenia",
    sofa = "Score SOFA",
    arm = "ARM",
    dx_ingreso = "Diagnóstico"
  )
) +
  labs(title = "Factores de Riesgo (Multivariado)", x = "Odds Ratio (log)") +
  theme_minimal()



##### tabla comparativa uni multivariado

library(gtsummary)

# 1. Generamos la Tabla Univariada (CRUDA)
# Usamos df_multivariado para que coincida la categoría de dx "Otras/PostQx"
t_univariada <- df_multivariado %>%
  select(muerte_uti, edad, sexo_masc, lma_lla, tx_alo, neutropenia, sofa, arm, dx_ingreso) %>%
  tbl_uvregression(
    method = glm,
    y = muerte_uti,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    hide_n = TRUE # Ocultamos N para que entre mejor en la pantalla
  )

# 2. Generamos la Tabla Multivariada (AJUSTADA)
# Usamos el modelo_final que ya creaste
t_multivariada <- tbl_regression(
  modelo_final,
  exponentiate = TRUE,
  pvalue_fun = ~style_pvalue(.x, digits = 3)
) %>%
  add_global_p() # Para el p-valor global del diagnóstico

# 3. FUSIONAMOS LAS DOS (La Magia)
tabla_comparativa <- tbl_merge(
  tbls = list(t_univariada, t_multivariada),
  tab_spanner = c("**Univariado (Crudo)**", "**Multivariado (Ajustado)**") # Títulos de cabecera
) %>%
  bold_labels() %>%
  italicize_levels()

# Mostrar tabla
tabla_comparativa
#################################### validacion del modelo
##comprobacion de la linealidad con el logit

# Probamos linealidad para 'edad' y 'sofa'
df_linealidad <- df_multivariado %>%
  mutate(
    log_edad = log(edad) * edad,
    log_sofa = log(sofa + 1) * sofa # Sumamos 1 por si hay SOFA 0
  )

modelo_box_tidwell <- glm(
  muerte_uti ~ edad + log_edad + sofa + log_sofa + sexo_masc + arm + tx_alo, 
  data = df_linealidad, 
  family = binomial
)

summary(modelo_box_tidwell)



#############validacion curva roc y calibracion
install.packages("pROC")

library(pROC)

# Extraemos los valores observados (Y) y los predichos (Probabilidades) 
# directamente desde el objeto del modelo para asegurar que midan lo mismo.
observados <- modelo_final$y
predichos <- fitted(modelo_final)

# Calculamos la curva ROC
roc_final <- roc(observados, predichos, ci=TRUE)

################hosmer lemensow

library(ResourceSelection)
hl <- hoslem.test(modelo_final$y, fitted(modelo_final), g=10)
hl

# Imprimimos el resultado (AUC e Intervalo de Confianza)
print(roc_final)

# Graficamos con estilo para publicación
plot(roc_final, 
     main=paste("Curva ROC para Mortalidad en UTI\nAUC:", round(roc_final$auc, 2)),
     col="#2c7fb8", 
     lwd=3, 
     identity.lty=2, 
     identity.col="grey",
     print.auc=TRUE,
     print.auc.x=0.5, print.auc.y=0.4) # Ubicación del texto del AUC


#####################

install.packages("rms")
library(rms)

# 1. Ajustamos el modelo con la función lrm (Logistic Regression Model) de rms
# Esta función es necesaria para poder usar la función 'calibrate'
fit_rms <- lrm(muerte_uti ~ edad + sexo_masc + lma_lla + tx_alo + neutropenia + sofa + arm + dx_ingreso, 
               data = df_multivariado, x=TRUE, y=TRUE)

# 2. Realizamos la validación por Bootstrap (400 repeticiones es lo estándar)
# Esto evalúa qué tan estable es la calibración
calib <- calibrate(fit_rms, method="boot", B=400)

# 3. Graficamos
plot(calib, 
     xlab="Probabilidad Predicha (Modelo)", 
     ylab="Probabilidad Observada (Real)",
     main="Curva de Calibración del Modelo Multivariado",
     sub=FALSE)

##################################

# Calculamos la distancia de Cook
cooks_d <- cooks.distance(modelo_final)

# Graficamos
plot(cooks_d, type="h", main="Distancia de Cook", ylab="Influencia")
abline(h = 4/nrow(df_multivariado), col="red", lty=2) # Punto de corte sugerido




###### analisis de sensibilidad con influyentes


# Calculamos el corte
corte_cook <- 4 / nrow(df_multivariado)

# Identificamos las filas influyentes
influyentes <- which(cooks.distance(modelo_final) > corte_cook)

# Vemos sus datos clínicos
df_multivariado[influyentes, ] %>%
  select(muerte_uti, edad, sofa, arm, tx_alo, dx_ingreso)


# Modelo sin influyentes
modelo_sin_outliers <- glm(
  muerte_uti ~ edad + sexo_masc + lma_lla + tx_alo + neutropenia + sofa + arm + dx_ingreso,
  data = df_multivariado[-influyentes, ], # Excluimos las filas
  family = binomial
)

# Comparamos los ORs de ambos modelos lado a lado
library(gtsummary)
t1 <- tbl_regression(modelo_final, exponentiate = TRUE)
t2 <- tbl_regression(modelo_sin_outliers, exponentiate = TRUE)

tbl_merge(
  list(t1, t2),
  tab_spanner = c("**Modelo Original**", "**Sin Influyentes**")
)


######k-fold

library(caret)
library(tidyverse)

# 1. Preparamos el dataset usando tus niveles actuales: "Vivo" y "Muerto"
df_cv_final <- df_multivariado %>%
  select(muerte_uti, edad, sexo_masc, lma_lla, tx_alo, neutropenia, sofa, arm, dx_ingreso) %>%
  drop_na() %>%
  # Nos aseguramos de que sea factor y mantenga TUS etiquetas
  mutate(muerte_uti = factor(muerte_uti, levels = c("Vivo", "Muerto")))

# 2. Verificación (Debe mostrar los conteos reales ahora)
print("Conteo de pacientes:")
print(table(df_cv_final$muerte_uti))

# 3. Configuración y ejecución del k-fold
set.seed(123)
control_cv <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary # Esto calculará el AUC basado en "Vivo" vs "Muerto"
)

modelo_kfold <- train(
  muerte_uti ~ ., 
  data = df_cv_final, 
  method = "glm", 
  family = "binomial",
  trControl = control_cv,
  metric = "ROC"
)

print(modelo_kfold)




#############matriz de confusion


# 1. Extraemos la realidad del modelo (solo los casos que el modelo usó)
# modelo_final$y nos da 0 y 1, por eso lo re-etiquetamos
realidad <- factor(modelo_final$y, levels = c(0, 1), labels = c("Vivo", "Muerto"))

# 2. Obtenemos las probabilidades y las convertimos en clase (Corte 0.5)
probabilidades <- fitted(modelo_final)
prediccion <- factor(ifelse(probabilidades > 0.5, "Muerto", "Vivo"), 
                     levels = c("Vivo", "Muerto"))

# 3. Ahora sí, la longitud es idéntica. Generamos la matriz:
library(caret)
matriz_final <- confusionMatrix(prediccion, realidad)
print(matriz_final)



library(gtsummary)

tabla_final <- tbl_regression(modelo_final, exponentiate = TRUE) %>%
  add_n(location = "level") %>%
  add_nevent(location = "level") %>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_header(label = "**Variable**", estimate = "**OR Ajustado**", ci = "**IC 95%**") %>%
  modify_caption("**Tabla 2. Análisis multivariado de factores asociados a mortalidad en UTI.**")

# Ver la tabla en R
tabla_final

# OPCIONAL: Si quieres guardarla directamente en un Word:
# library(flextable)
# tabla_final %>% as_flex_table() %>% flextable::save_as_docx(path = "Tabla_Resultados_Paper.docx")