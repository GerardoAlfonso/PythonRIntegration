
# Actividad 1: Proyecto de Python

###  Elección del conjunto de datos

# El conjunto de datos seleccionado hace referencia a la calidad del sueño de la poblacion en general, el cual incluye diferentes variables categoricas y numericas referentes a estilo de vidad, salud y generales descriptivas.

# El tema de la calidad del sueño es muy poco debatido por el publico en general y este puede tener una alta repercusion ya sea positiva o negativa en la calidad de vida de la poblacion en general, con esta investigacion se pretende establecer la relacion entre algunas variables sobre estilo de vida y la calidad de sueño de la muestra de la poblacion tomada y asi determinar cuales son las causas que influyen en la calidad del sueño o si la calidad del sueño en si misma puede influir en otros aspectos de la calidad de vida.

# el dataset fue descargado de 
# https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset


library(ggplot2)
library(plyr)
library(dplyr)
library(readr)
library(plotly)

#Se instala Reticulete
library(reticulate)

#Importando librerias de Python
#Se trabajara en el entorno virutal default que se creo
pd <- import("pandas")

#Lectura con Pandas
RdfArchivo <- pd$read_csv('Sleep_health_and_lifestyle_dataset.csv')
#Convertimos el DataFrame de R a DataFrame de Pandas con la funcion r_to_py()

PydfArchivo <- r_to_py(RdfArchivo)
#Se imprimen las clases de los DataFrames Uno de R y el Otro Pandas
print(class(RdfArchivo))
print(class(PydfArchivo))


# Cargar el dataset
ds <- read_csv("Sleep_health_and_lifestyle_dataset.csv")
print(ds)

# Descripción general del dataset
summary(ds)

# Conteo de valores nulos
colSums(is.na(ds))

# Eliminar registros con valores NULL
ds <- na.omit(ds)

# Información del dataset
str(ds)

# Crear histogramas para todas las columnas numéricas
ds %>% 
  select_if(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) + 
  facet_wrap(~key, scales = 'free') + 
  geom_histogram(bins=5) + 
  theme_minimal()

# Distribucion de la poblacion por Genero
gender_counts <- table(ds$Gender)
pie(gender_counts, labels = names(gender_counts), 
    col = c("lightblue", "pink"), main = "Distribucion por Genero")

# Distribucion de la calidad del sueño por Genero
ggplot(ds, aes(x = Gender, y = `Quality of Sleep`, fill = Gender)) +
  geom_violin() +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "pink")) +
  labs(title = "Distribucion de la calidad del sueño por Genero") +
  theme_minimal()

# Distribucion de la calidad del sueño por edad y genero
ggplot(ds, aes(x = Age, y = `Quality of Sleep`, color = Gender)) +
  geom_point(position = position_jitter(width = 0.2), size = 2) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Distribucion de la calidad del sueño por edad", x = "Edad", y = "Calidad del sueño") +
  theme_minimal()

# Distribucion de la duracion del sueño
ggplot(ds, aes(x = `Sleep Duration`)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  geom_density(aes(y = ..density..), color = "red") +
  labs(title = "Distribucion de la duracion del sueño") +
  theme_minimal()

# Distribucion de la calidad del sueño
ggplot(ds, aes(x = `Quality of Sleep`)) +
  geom_bar() +
  labs(title = "Distribucion de la calidad del sueño") +
  theme_minimal()

# Diagrama de dispersión: Edad y Duración del sueño
ggplot(ds, aes(x = Age, y = `Sleep Duration`)) +
  geom_point() +
  labs(title = "Duracion del sueño por Edad") +
  theme_minimal()

# Calidad de sueño y BMI
ggplot(ds, aes(x = `BMI Category`, y = `Quality of Sleep`)) +
  geom_boxplot() +
  labs(title = "Calidad del Sueño por Categoría de IMC", x = "Categoría de IMC", y = "Calidad del Sueño") +
  theme_minimal()

# Distribucion del nivel de estres
plot_ly(ds, labels = ~`Stress Level`, type = 'pie') %>%
  layout(title = 'Distribucion del nivel de estres')

# Distribucion de la poblacion por Clasificacion del IMC
ggplot(ds, aes(x = `BMI Category`)) +
  geom_bar() +
  labs(title = "Distribucion por BMI (Body Mass Index)") +
  theme_minimal()


# Concluciones

# * Se realizo un análisis de tipo Exploratorio del dataset para conocer a detalle las distintas variables y como estas pueden afectar la calidad del sueño de la poblacion de muestra, las cuales se pueden medir en funcion de: Edad, Nivel de Estres, Clasificacion del Indice de Masa Corporal, Cantidad de pasos que la persona realiza de forma diaria, Genero etc.


# * Al validar la relación entre las diferentes variables, en este dateset podemos ver una correlación positiva lo cual significa que cada una de las variables afecta de manera directa a la variable objetivo del ejercicio que es la calidad del sueño, identificando factores como un mayor nivel de BMI (Clasificacion del Indice de masa corporal) o la edad afectan negativamente a la calidad del sueño de la poblacion.