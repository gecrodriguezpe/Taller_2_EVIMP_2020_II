############################################
# Taller 2 evaluación de impacto: Modelo RCT

# Bibliotecas importantes
##
library(tidyverse)
library(readxl)
library(MASS)
library(stargazer)

# Directorio de trabajo y manipulación base de datos 
##
setwd("~/Documents/GitHub/semestre5/Talleres_evaluax_impacto/Taller_2_EVIMP_2020_II/Datos")

datos = read_xlsx("Datos Taller II.xlsx") # importo base de excel 
# glimpse(datos) # visualizar base de datos

# Transformo la base de datos de tal forma de que las variables categóricas se encuentren 
# en la categoría correcta
datos = datos %>% 
  mutate(hogar = factor(hogar), escuela = factor(escuela), tamano_hogar = factor(tamano_hogar),
         rural = factor(rural), mujer_cabeza = factor(mujer_cabeza), treatment = factor(treatment))

attach(datos)

# glimpse(datos) # visualizar base de datos

# Punto 3
##

treatment_0 = datos %>% 
  filter(treatment == 0) %>% 
  mutate(escuela = factor(escuela))

treatment_1 = datos %>% 
  filter(treatment == 1) %>%  
  mutate(escuela = factor(escuela))

table_rural = table(treatment, rural)
table_muj_cabeza = table(treatment, mujer_cabeza)

summary(datos)

# Punto 4
## 

# Regresión con controles 
impacto_salud = lm(outcome ~ treatment + ingreso_hogar
                   + tamano_hogar + rural + mujer_cabeza 
                   + escuela, data = datos); summary(impacto_salud)

# Diferencia de medias

media_treated = treatment_1 %>% 
  summarize(media_treated = mean(outcome))

media_treated

media_untreated = treatment_0 %>% 
  summarize(media_untreated = mean(outcome))

media_untreated

dif_means = as.double(media_treated - media_untreated)
dif_means

grafica = datos %>%
  ggplot(aes(x = treatment, y = outcome)) +
  geom_boxplot() +
  theme_classic()

grafica

# Punto 5
##

stargazer(impacto_salud, type="latex",keep.stat=c("n","rsq", "f"),
           omit = c(3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 18), style = "AER")

# Punto 7
##

# Filtro la muestra original con solo las escuelas que tienen pacientes ya sea tratados o no tratados
sub_muestra = datos %>% 
  filter(escuela == 5 | escuela == 6 | escuela == 7 | escuela == 8 | escuela == 9 | escuela == 10)

impacto_salud_sub_muestra = lm(outcome ~ treatment + ingreso_hogar
                   + tamano_hogar + rural + mujer_cabeza 
                   + escuela, data = sub_muestra); summary(impacto_salud_sub_muestra)

# Punto 8
##

impacto_notas = lm(nota_hijos ~ treatment + ingreso_hogar
                   + tamano_hogar + rural + mujer_cabeza
                   + escuela, data = datos); summary(impacto_notas)

stargazer(impacto_notas, type="latex",keep.stat=c("n","rsq", "f"),
          omit = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18), style = "AER")
