#TRABAJO FINAL MÓDULO R. Lucía Calderón Sosinowicz
#-------------------------------------------------------------------------------

#Preparación
#En caso de ser necesario instalar los siguientes paquetes
#install.packages("tidyverse")
#install.packages("eph")
#install.packages("ggplot2")
#install.packages("weights")
#install.packages("car")
#install.packages("lmtest")
#install.packages("glmnet")
#install.packages("caret") Paquete para las funciones train/test
#install.packages("dplyr")
#install.packages("patchwork") para poner juntos los gráficos de observados vs predichos

library(tidyverse)
library(eph)
library(ggplot2)
library(weights)
library(car)
library(lmtest)
library(glmnet)
library(caret)
library(dplyr)
library(patchwork)
library(forcats)


eph_raw <- get_microdata(year = 2026, trimester = 1, type = "individual")

#Organizo la rama de actividad usando la función del paquete eph
eph_rama <- organize_caes(base = eph_raw)

#Filtrado de datos
eph <- eph_rama |>
  #Creo la variable de menores de 6 años
  group_by(CODUSU, NRO_HOGAR) |>
  mutate(
    edad_limpia = ifelse(CH06 < 0, NA, CH06),
    menores_6 = any(edad_limpia < 6, na.rm = TRUE),
    edad_cuadrado = edad_limpia^2,
  ) |>
  ungroup() |>
  # Filtro 
  filter(CAT_OCUP == 3, ESTADO == 1, PP3E_TOT > 0) |>
  #transformo la variable de ocupación del trabajador
  mutate( 
    ocupacion_str = str_pad(as.character(PP04D_COD), 5, pad = "0"),
    # Extraigo el primer dígito
    ocupacion_grupo = str_sub(ocupacion_str, 1, 1),
    # La convierto en factor
    ocupacion_grupo = as.factor(ocupacion_grupo)
    ) |>
    
  #Selección de las variables de interés
  select(
    CODUSU, NRO_HOGAR, COMPONENTE, #Identificación
    horas_trabajadas = PP3E_TOT,     # Variable dependiente
    edad = CH06,                 # Variable explicativa
    edad_cuadrado,               #Variable explicativa
    sexo = CH04,                 # Variable explicativa
    nivel_ed = NIVEL_ED,         # Variable explicativa
    rama_actividad = caes_seccion_cod, # Variable explicativa
    ocupacion_trabajador = ocupacion_grupo, # Variable explicativa
    menores_6,             # Variable explicativa
    ingresos_no_lab = T_VI, # Variable explicativa
    PONDERA                      # Ponderador poblacional para el EDA
  )

#Chequeo y limpieza de base
colSums(is.na(eph)) #No hay NA explícitos

#-------------------------------------------------------------------------------
#Análisis variable dependiente: cantidad de horas trabajadas
summary(eph$horas_trabajadas)

#Tabla de frecuencias.
table(eph$horas_trabajadas)

#Elimino las observaciones que tengan mas de 80 hs trabajadas
eph <- eph |>
  filter(horas_trabajadas <=80)

summary(eph$horas_trabajadas)

#Boxplot
ggplot(eph, aes(y = horas_trabajadas)) +
  # Uso weight para ponderar el boxplot por la población expandida de la EPH
  geom_boxplot(aes(weight = PONDERA), fill = "#2b5c8f", alpha = 0.7, outlier.color = "red", outlier.shape = 16) +
  labs(
    title = "Detección de Valores Extremos en Horas Trabajadas",
    subtitle = "Ocupación principal (Población asalariada ponderada)",
    y = "Horas trabajadas por semana",
    x = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

#Histograma agrupado
ggplot(eph, aes(x = horas_trabajadas, weight = PONDERA)) +
  #intervalos de 5 horas
  geom_histogram(binwidth = 5, fill = "#2b5c8f", color = "white", alpha = 0.85, boundary = 0) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 80)) +
  labs(
    title = "Distribución de Horas Semanales Trabajadas",
    subtitle = "Agrupado en intervalos de 5 horas",
    x = "Horas trabajadas por semana", 
    y = "Población ponderada"
  ) +
  theme_minimal()

#-------------------------------------------------------------------------------
#Análisis variable independiente: edad
summary(eph$edad)

#Filtro solo edad activa de trabajo convencional
eph <- eph |>
  filter(edad >= 18 & edad <= 65)

summary(eph$edad)

#Boxplot edad
ggplot(eph, aes(y = edad)) +
  # Boxplot ponderado por la EPH
  geom_boxplot(aes(weight = PONDERA), fill = "turquoise", alpha = 0.7, outlier.color = "red", outlier.shape = 16) +
  labs(
    title = "Diagrama de Caja: Distribución de la Edad",
    subtitle = "Población ponderada (18 a 65 años)",
    y = "Edad (años)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

#Histograma agrupado
ggplot(eph, aes(x = edad, weight = PONDERA)) +
  geom_histogram(binwidth = 5, fill = "turquoise", color = "white", alpha = 0.85, boundary = 0) +
  labs(
    title = "Distribución Etaria de la Población Analizada",
    subtitle = "Población ponderada (18 a 65 años)",
    x = "Edad (años)",
    y = "Población ponderada"
  ) +
  theme_minimal()

#CORRELACIONES
#Sólo entre edad y horas trabajadas porque son variables numéricas ambas
wtd.cor(eph$edad, eph$horas_trabajadas, weight = eph$PONDERA)
wtd.cor(eph$edad^2, eph$horas_trabajadas, weight = eph$PONDERA)
#Bajas, hay que ver cómo se complementan en el modelo OLS

#-------------------------------------------------------------------------------
#Análisis variable independiente: sexo
eph <- eph |>
  mutate(
    sexo_label = case_when(
      sexo == 1 ~ "Varón",
      sexo == 2 ~ "Mujer",
      TRUE ~ as.character(sexo)
    ),
    sexo_label = as.factor(sexo_label)
  )

summary(eph$sexo_label)

#Gráfico de barras ponderado
df_porcentajes <- eph |>
  group_by(sexo_label) |>
  summarise(pob_pond = sum(PONDERA, na.rm = TRUE)) |>
  mutate(
    porcentaje = pob_pond / sum(pob_pond) * 100,
    etiqueta = sprintf("%.1f%%", porcentaje)
  )

ggplot(df_porcentajes, aes(x = sexo_label, y = pob_pond, fill = sexo_label)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = etiqueta), vjust = -0.5, size = 4.5, fontface = "bold", color = "#122338") +
  scale_fill_manual(values = c("#e07a5f", "#2b5c8f")) +
  labs(
    title = "Composición por Sexo de la Población Analizada",
    subtitle = "Población ponderada y distribución porcentual (18 a 65 años)",
    x = "",
    y = "Población ponderada"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

#-------------------------------------------------------------------------------
#Análisis de variable independiente: nivel educativo
eph$nivel_ed <- as.factor(eph$nivel_ed)
summary(eph$nivel_ed) #No hay datos con categoria 9 (no sabe, no contesta)

#Gráfico de barras ponderado
df_educacion <- eph |>
  group_by(nivel_ed) |>
  summarise(pob_pond = sum(PONDERA, na.rm = TRUE)) |>
  mutate(
    porcentaje = pob_pond / sum(pob_pond) * 100,
    etiqueta = sprintf("%.1f%%", porcentaje)
  ) |>
  arrange(desc(pob_pond))

ggplot(df_educacion, aes(x = reorder(nivel_ed, pob_pond), y = pob_pond)) +
  geom_col(fill = "khaki", alpha = 0.85) +
  geom_text(aes(label = etiqueta), hjust = -0.1, size = 3.5, fontface = "bold", color = "#122338") +
  coord_flip() +
  labs(
    title = "Distribución según Nivel Educativo",
    subtitle = "Población ponderada y distribución porcentual (18 a 65 años)",
    x = "",
    y = "Población ponderada"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

#-------------------------------------------------------------------------------
#Análisis variable independiente: rama actividad
eph |>
  group_by(rama_actividad) |>
  summarise(
    Casos_Muestra = n(),
    Pob_Ponderada = sum(PONDERA, na.rm = TRUE)
  ) |>
  mutate(
    Porcentaje = (Pob_Ponderada / sum(Pob_Ponderada)) * 100
  ) |>
  arrange(desc(Pob_Ponderada)) |>
  print(n = Inf)
  #Recodifico, hay muchas categorias con poco peso

eph <- eph |>
  mutate(
    rama_agrupada = case_when(
      rama_actividad %in% c("G", "C", "O", "P", "T", "Q", "F", "I", "H", "N") ~ rama_actividad,
      TRUE ~ "Otras ramas"
    )
  )

eph |>
  group_by(rama_agrupada) |>
  summarise(
    Casos_Muestra = n(),
    Pob_Ponderada = sum(PONDERA, na.rm = TRUE)
  ) |>
  mutate(
    Porcentaje = (Pob_Ponderada / sum(Pob_Ponderada)) * 100
  ) |>
  arrange(desc(Pob_Ponderada)) |>
  print(n = Inf)

#Gráfico de barras agrupado
df_ramas_agrupadas <- eph |>
  group_by(rama_agrupada) |>
  summarise(pob_pond = sum(PONDERA, na.rm = TRUE)) |>
  mutate(
    porcentaje = pob_pond / sum(pob_pond) * 100,
    etiqueta = sprintf("%.1f%%", porcentaje)
  ) |>
  arrange(desc(pob_pond))

ggplot(df_ramas_agrupadas, aes(x = reorder(rama_agrupada, pob_pond), y = pob_pond)) +
  geom_col(fill = "palevioletred1", alpha = 0.85) +
  geom_text(aes(label = etiqueta), hjust = -0.1, size = 3.5, fontface = "bold", color = "#122338") +
  coord_flip() +
  labs(
    title = "Distribución según Rama de Actividad",
    subtitle = "Población ocupada ponderada (18 a 65 años)",
    x = "",
    y = "Población ponderada"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
#-------------------------------------------------------------------------------
#Análisis variable independiente: ocupación del trabajador
summary(eph$ocupacion_trabajador)

#Gráfico de barras ponderado
df_ocupacion <- eph |>
  group_by(ocupacion_trabajador) |> 
  summarise(pob_pond = sum(PONDERA, na.rm = TRUE)) |>
  mutate(
    porcentaje = pob_pond / sum(pob_pond) * 100,
    etiqueta = sprintf("%.1f%%", porcentaje)
  ) |>
  arrange(desc(pob_pond))

ggplot(df_ocupacion, aes(x = reorder(ocupacion_trabajador, pob_pond), y = pob_pond)) +
  geom_col(fill = "wheat3", alpha = 0.85) +
  geom_text(aes(label = etiqueta), hjust = -0.1, size = 3.5, fontface = "bold", color = "#122338") +
  coord_flip() +
  labs(
    title = "Distribución según Categoría Ocupacional",
    subtitle = "Población ocupada ponderada (18 a 65 años)",
    x = "",
    y = "Población ponderada"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

#-------------------------------------------------------------------------------
#Análisis variable independiente: menores de 6 años
summary(eph$menores_6)

#Gráfico de barras ponderado
df_menores <- eph |>
  group_by(menores_6) |>
  summarise(pob_pond = sum(PONDERA, na.rm = TRUE)) |>
  mutate(
    porcentaje = pob_pond / sum(pob_pond) * 100,
    etiqueta = sprintf("%.1f%%", porcentaje)
  )

ggplot(df_menores, aes(x = menores_6, y = pob_pond, fill = menores_6)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = etiqueta), vjust = -0.5, size = 4.5, fontface = "bold", color = "#122338") +
  scale_fill_manual(values = c("palegreen", "steelblue1")) +
  labs(
    title = "Presencia de Menores de 6 años en el Hogar",
    subtitle = "Población ponderada y distribución porcentual (18 a 65 años)",
    x = "",
    y = "Población ponderada"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

#-------------------------------------------------------------------------------
#Análisis variable independiente: ingresos no laborales
summary(eph$ingresos_no_lab)

colSums(eph == -9, na.rm = TRUE) #238 valores faltante en ingresos no laborables

eph <- eph |>
  # Reemplazo el -9 por NA en las columnas numéricas
  mutate(across(where(is.numeric), ~ na_if(.x, -9))) |>
  #Elimino los NA
  drop_na()

#Histograma
ggplot(eph, aes(x = ingresos_no_lab, weight = PONDERA)) +
  geom_histogram(bins = 30, fill = "#2b5c8f", color = "white", alpha = 0.85) +
  labs(
    title = "Distribución de Ingresos No Laborales",
    subtitle = "Población ponderada (18 a 65 años)",
    x = "Ingresos no laborales",
    y = "Población ponderada"
  ) +
  theme_minimal()

#Como hay mucha proporción de 0 la recodifico
eph <- eph |>
  mutate(
    ingresos_no_lab_bin = case_when(
      ingresos_no_lab > 0 ~ "Sí percibe",
      ingresos_no_lab == 0 | is.na(ingresos_no_lab) ~ "No percibe",
      TRUE ~ "No percibe"
    )
  )

#Gráfico de barras ponderado
df_ingresos_bin <- eph |>
  group_by(ingresos_no_lab_bin) |>
  summarise(pob_pond = sum(PONDERA, na.rm = TRUE)) |>
  mutate(
    porcentaje = pob_pond / sum(pob_pond) * 100,
    etiqueta = sprintf("%.1f%%", porcentaje)
  )

ggplot(df_ingresos_bin, aes(x = ingresos_no_lab_bin, y = pob_pond, fill = ingresos_no_lab_bin)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = etiqueta), vjust = -0.5, size = 4.5, fontface = "bold", color = "#122338") +
  scale_fill_manual(values = c("palegreen", "steelblue1")) + 
  labs(
    title = "Percepción de Ingresos No Laborales",
    subtitle = "Población ponderada y distribución porcentual (18 a 65 años)",
    x = "",
    y = "Población ponderada"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

#-------------------------------------------------------------------------------
#Aplicación de técnicas
#Modelo estimado: OLS con variables base
modelo_ols <- lm(
  horas_trabajadas ~ edad + edad_cuadrado + sexo_label + nivel_ed + menores_6 + ocupacion_trabajador + rama_agrupada + ingresos_no_lab_bin,
  data = eph,
  weights = PONDERA
)

summary(modelo_ols)

#Diagnóstico del modelo
par(mfrow = c(2, 2))
plot(modelo_ols)
par(mfrow = c(1, 1))

bptest(modelo_ols)
vif(modelo_ols)

#Modelos de regularización base
set.seed(7109)
n <- nrow(eph)
train_index <- sample(1:n, size = 0.8 * n)

eph_train_corto <- eph[train_index, ]
eph_test_corto  <- eph[-train_index, ]

#Fórmula base
formula_corto <- horas_trabajadas ~ edad + edad_cuadrado + sexo_label + nivel_ed + menores_6 + ocupacion_trabajador + rama_agrupada + ingresos_no_lab_bin

#Matriz de diseño
x_completa_corto <- model.matrix(formula_corto, data = eph)[, -1]

x_train_corto <- x_completa_corto[train_index, ]
x_test_corto  <- x_completa_corto[-train_index, ]

y_train_corto <- eph$horas_trabajadas[train_index]
y_test_corto  <- eph$horas_trabajadas[-train_index]


#Ajuste de modelos
#OLS
modelo_ols_corto <- lm(formula_corto, data = eph_train_corto)
#RIDGE
set.seed(7109)
cv_ridge_corto <- cv.glmnet(x_train_corto, y_train_corto, alpha = 0, nfolds = 10)
lambda_opt_ridge_corto <- cv_ridge_corto$lambda.min
modelo_ridge_corto <- glmnet(x_train_corto, y_train_corto, alpha = 0, lambda = lambda_opt_ridge_corto)
#LASSO
set.seed(7109)
cv_lasso_corto <- cv.glmnet(x_train_corto, y_train_corto, alpha = 1, nfolds = 10)
lambda_opt_lasso_corto <- cv_lasso_corto$lambda.min
modelo_lasso_corto <- glmnet(x_train_corto, y_train_corto, alpha = 1, lambda = lambda_opt_lasso_corto)

coef(modelo_ridge_corto)
coef(modelo_lasso_corto)

#Predicciones y MSE
pred_ols_corto   <- predict(modelo_ols_corto, newdata = eph_test_corto)
pred_ridge_corto <- predict(cv_ridge_corto, newx = x_test_corto, s = "lambda.min")
pred_lasso_corto <- predict(cv_lasso_corto, newx = x_test_corto, s = "lambda.min")

mse_ols_corto   <- mean((y_test_corto - pred_ols_corto)^2)
mse_ridge_corto <- mean((y_test_corto - as.numeric(pred_ridge_corto))^2)
mse_lasso_corto <- mean((y_test_corto - as.numeric(pred_lasso_corto))^2)

resultados_mse_corto <- data.frame(
  Modelo = c("OLS Corto", "Ridge Corto", "Lasso Corto"),
  MSE_Test = c(mse_ols_corto, mse_ridge_corto, mse_lasso_corto)
)

print("--- RESULTADOS MSE EN TEST (MODELO BASE) ---")
print(resultados_mse_corto)

#Gráfico valores observados vs predichos
p_ols_corto   <- as.numeric(pred_ols_corto)
p_ridge_corto <- as.numeric(pred_ridge_corto)
p_lasso_corto <- as.numeric(pred_lasso_corto)

df_grafico_corto <- data.frame(
  Observados = rep(y_test_corto, 3),
  Predichos  = c(p_ols_corto, p_ridge_corto, p_lasso_corto),
  Modelo     = rep(c("OLS Corto", "Ridge Corto", "Lasso Corto"), each = length(y_test_corto))
)

df_grafico_corto$Modelo <- factor(df_grafico_corto$Modelo, levels = c("OLS Corto", "Ridge Corto", "Lasso Corto"))

grafico_obs_pred_corto <- ggplot(df_grafico_corto, aes(x = Observados, y = Predichos)) +
  geom_point(alpha = 0.15, color = "#2b5c8f", size = 1) +  
  geom_abline(intercept = 0, slope = 1, color = "#e74c3c", linetype = "dashed", linewidth = 1) +
  coord_cartesian(xlim = c(0, 70), ylim = c(10, 60)) +  
  facet_wrap(~ Modelo, ncol = 3) +
  theme_bw(base_size = 11) +
  labs(
    title = "Valores Observados vs. Predichos en Test (Especificación Corta)",
    subtitle = "Comparación de desempeño predictivo: OLS Corto vs. Ridge vs. Lasso",
    x = "Horas Trabajadas Observadas",
    y = "Horas Trabajadas Predichas"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

print(grafico_obs_pred_corto)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#SEGUNDA PARTE
#Nuevas variables
eph_larga <- eph

eph_larga <- eph_larga %>%
  left_join(
    eph_raw %>% select(CODUSU, NRO_HOGAR, COMPONENTE, 
                       estado_civil=CH07,
                       asistencia_edu=CH10,
                       AGLOMERADO), 
    by = c("CODUSU", "NRO_HOGAR", "COMPONENTE")
  )


#Análisis variable independiente: estado civil
summary(eph_larga$estado_civil) #No hay valores faltantes

eph_larga$estado_civil <- as.factor(eph_larga$estado_civil)

eph_larga <- eph_larga %>%
  mutate(estado_civil = fct_recode(estado_civil,
                                      "Unido" = "1",
                                      "Casado" = "2",
                                      "Separado/divorciado" = "3",
                                      "Viudo" = "4",
                                      "Soltero" = "5"))

#Gráfico de barras ponderado
df_estado <- eph_larga |>
  group_by(estado_civil) |>
  summarise(pob_pond = sum(PONDERA, na.rm = TRUE)) |>
  mutate(
    porcentaje = pob_pond / sum(pob_pond) * 100,
    etiqueta = sprintf("%.1f%%", porcentaje)
  )

ggplot(df_estado, aes(x = reorder(estado_civil, pob_pond), y = pob_pond)) +
  geom_col(fill = "orange", alpha = 0.85) +
  geom_text(aes(label = etiqueta), hjust = -0.1, size = 3.5, fontface = "bold", color = "#122338") +
  coord_flip() +
  labs(
    title = "Distribución según Estado Civil",
    subtitle = "Población ocupada ponderada (18 a 65 años)",
    x = "",
    y = "Población ponderada"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))


#Análisis variable independiente: asistencia a establecimiento educativo
summary(eph_larga$asistencia_edu) #No hay valores faltantes

eph_larga <- eph_larga |>
  mutate(
    asistencia_edu = case_when(
      asistencia_edu %in% c("2", "3") ~ "No asiste",
      TRUE ~ "Asiste"
    )
  )

eph_larga$asistencia_edu <- as.factor(eph_larga$asistencia_edu)

#Gráfico de barras ponderado
df_asistencia <- eph_larga |>
  group_by(asistencia_edu) |>
  summarise(pob_pond = sum(PONDERA, na.rm = TRUE)) |>
  mutate(
    porcentaje = pob_pond / sum(pob_pond) * 100,
    etiqueta = sprintf("%.1f%%", porcentaje)
  )

ggplot(df_asistencia, aes(x = asistencia_edu, y = pob_pond, fill = asistencia_edu)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = etiqueta), vjust = -0.5, size = 4.5, fontface = "bold", color = "#122338") +
  scale_fill_manual(values = c("palegreen", "steelblue1")) +
  labs(
    title = "Asistencia a establecimientos educativos",
    subtitle = "Población ponderada y distribución porcentual (18 a 65 años)",
    x = "",
    y = "Población ponderada"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

#Análisis variable independiente: aglomerado
summary(eph_larga$AGLOMERADO) #Por su estructura en la EPH 
                              #no hay valores faltantes

eph_larga$AGLOMERADO <- as.factor(eph_larga$AGLOMERADO)

#Gráfico de barras ponderado
df_aglomerado <- eph_larga |>
  group_by(AGLOMERADO) |>
  summarise(pob_pond = sum(PONDERA, na.rm = TRUE)) |>
  mutate(
    porcentaje = pob_pond / sum(pob_pond) * 100,
    etiqueta = sprintf("%.1f%%", porcentaje)
  )

ggplot(df_aglomerado, aes(x = reorder(AGLOMERADO, pob_pond), y = pob_pond, fill = AGLOMERADO)) +
  geom_col(alpha = 0.85) +
  scale_fill_viridis_d(option = "plasma") +
  geom_text(aes(label = etiqueta), hjust = -0.1, size = 3.5, fontface = "bold", color = "#122338") +
  coord_flip() +
  labs(
    title = "Distribución según Aglomerado",
    subtitle = "Población ocupada ponderada (18 a 65 años)",
    x = "",
    y = "Población ponderada"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) #

#-------------------------------------------------------------------------------
#Segunda parte:técnicas
#OLS en modelo extendido
modelo_ols_largo <- lm(
  horas_trabajadas ~ edad + edad_cuadrado + sexo_label + nivel_ed + menores_6 + ocupacion_trabajador + rama_actividad + ingresos_no_lab_bin+
    estado_civil + asistencia_edu + AGLOMERADO + edad*sexo_label + menores_6*sexo_label + nivel_ed*edad ,
  data = eph_larga,
  weights = PONDERA
)
  
summary(modelo_ols_largo)
AIC(modelo_ols, modelo_ols_largo)
vif(modelo_ols_largo)


#Modelos de regularización extendidos
set.seed(7109)
n <- nrow(eph_larga)
train_index <- sample(1:n, size = 0.8 * n)

eph_train_largo <- eph_larga[train_index, ]
eph_test_largo  <- eph_larga[-train_index, ]

formula_largo <- horas_trabajadas ~ edad + edad_cuadrado + sexo_label + nivel_ed + 
  menores_6 + ocupacion_trabajador + rama_actividad + ingresos_no_lab_bin + 
  estado_civil + asistencia_edu + AGLOMERADO + 
  edad * sexo_label + menores_6 * sexo_label + nivel_ed * edad

x_completa_largo <- model.matrix(formula_largo, data = eph_larga)[, -1]

x_train_largo <- x_completa_largo[train_index, ]
x_test_largo  <- x_completa_largo[-train_index, ]

y_train_largo <- eph$horas_trabajadas[train_index]
y_test_largo  <- eph$horas_trabajadas[-train_index]

# Ajuste de modelos
#OLS LARGO
modelo_ols_largo_2 <- lm(formula_largo, data = eph_train_largo)
#RIDGE LARGO
set.seed(7109)
cv_ridge_largo <- cv.glmnet(x_train_largo, y_train_largo, alpha = 0, nfolds = 10)
lambda_opt_ridge_largo <- cv_ridge_largo$lambda.min
modelo_ridge_largo <- glmnet(x_train_largo, y_train_largo, alpha = 0, lambda = lambda_opt_ridge_largo)
#LASSO LARGO
set.seed(7109)
cv_lasso_largo <- cv.glmnet(x_train_largo, y_train_largo, alpha = 1, nfolds = 10)
lambda_opt_lasso_largo <- cv_lasso_largo$lambda.min
modelo_lasso_largo <- glmnet(x_train_largo, y_train_largo, alpha = 1, lambda = lambda_opt_lasso_largo)

coef(modelo_ridge_largo)
coef(modelo_lasso_largo)


#Predicciones y MSE
pred_ols_largo   <- predict(modelo_ols_largo_2, newdata = eph_test_largo)
pred_ridge_largo <- predict(cv_ridge_largo, newx = x_test_largo, s = "lambda.min")
pred_lasso_largo <- predict(cv_lasso_largo, newx = x_test_largo, s = "lambda.min")

mse_ols_largo   <- mean((y_test_largo - pred_ols_largo)^2)
mse_ridge_largo <- mean((y_test_largo - as.numeric(pred_ridge_largo))^2)
mse_lasso_largo <- mean((y_test_largo - as.numeric(pred_lasso_largo))^2)

resultados_mse_largo <- data.frame(
  Modelo = c("OLS Largo", "Ridge Largo", "Lasso Largo"),
  MSE_Test = c(mse_ols_largo, mse_ridge_largo, mse_lasso_largo)
)

print("--- RESULTADOS MSE EN TEST (MODELO LARGO) ---")
print(resultados_mse_largo)


# Gráfico valores observados vs predichos (Modelo Largo)
p_ols_largo   <- as.numeric(pred_ols_largo)
p_ridge_largo <- as.numeric(pred_ridge_largo)
p_lasso_largo <- as.numeric(pred_lasso_largo)

df_grafico_largo <- data.frame(
  Observados = rep(y_test_largo, 3),
  Predichos  = c(p_ols_largo, p_ridge_largo, p_lasso_largo),
  Modelo     = rep(c("OLS Largo", "Ridge Largo", "Lasso Largo"), each = length(y_test_largo))
)

df_grafico_largo$Modelo <- factor(df_grafico_largo$Modelo, levels = c("OLS Largo", "Ridge Largo", "Lasso Largo"))

grafico_obs_pred_largo <- ggplot(df_grafico_largo, aes(x = Observados, y = Predichos)) +
  geom_point(alpha = 0.15, color = "#2b5c8f", size = 1) +  
  geom_abline(intercept = 0, slope = 1, color = "#e74c3c", linetype = "dashed", linewidth = 1) +
  coord_cartesian(xlim = c(0, 70), ylim = c(10, 60)) +  
  facet_wrap(~ Modelo, ncol = 3) +
  theme_bw(base_size = 11) +
  labs(
    title = "Valores Observados vs. Predichos en Test (Especificación Extendida)",
    subtitle = "Comparación de desempeño predictivo: OLS Largo vs. Ridge vs. Lasso",
    x = "Horas Trabajadas Observadas",
    y = "Horas Trabajadas Predichas"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

print(grafico_obs_pred_largo)