
### Increasing Sleep


########### RCT's

# Librerias

#install.packages("devtools")
#library(devtools)
#install_github("isidorogu/RCT")
#library(Matching)
library(RCT)
library(stargazer)
library(tidyverse)
library(dplyr)
library(sandwich)
library(fixest)
library(pacman)
library(EnvStats)
library(kableExtra)
library(haven)
library(MatchIt)




#### 1) Tablas de Balance

# setwd("d:/Users/guillermo_reza/Desktop/Maestría/Semestre Ene-Jun 2022/Econometría Aplicada II/Tarea 1 y Tarea 2")
baseline <- read.csv("baseline.csv")


# Experimento con el T_nap


# Se seleccionan 10 variables de datos basales. Se cambian los nombres de
# algunas variables.

# Las variables seleccionadas se relacionan con los datos basales de los grupos.
# De la base de datos se consideraron "time_in_office", "age", "female",
# "education", "sleep_night", "children", "typing_time_hr", "earnings",
# "tot_earnings", "productivity", "T_nap". Estas variables no están relacionadas
# con un potencial efecto sobre el tratamiento, es decir, que las variables
# no son propias del grupo de tratamiento y que el grupo de control tuviera
# valores nulos o en ceros.



baseline <- baseline %>% 
  mutate(age = age_,
         female = female_,
         education = education_,
         children = no_of_children_,
         )

variables<- data.frame(time_in_office = baseline$time_in_office, 
                       age=baseline$age, 
                       female=baseline$female,
                       education=baseline$education, 
                       sleep_night=baseline$sleep_night,
                       children=baseline$children, 
                       happy=baseline$happy,
                       health_bsl=baseline$health_bsl,
                       d1=baseline$d1,
                       absences=baseline$absences_baseline,
                       T_nap=baseline$T_nap )



# Tabla de Balance Individual

tabla1<-balance_table(variables,treatment = "T_nap")

stargazer(as.data.frame(tabla1),type = "text" ,summary=FALSE)

# En esta tabla se obtienen los promedios de cada una de las variables
# seleccionadas. En la primera columna se encuentra el promedio para las
# observaciones de control y la segunda columna el promedio de las observaciones
# que corresponden al tratamiento a partir del noveno día. En la última
# columna está el valor p que corresponde a la diferencia de medias. 
# Como puede observarse, todos los valores p son mayores al 5%, lo cual
# significa que la hipótesis nula que no existen diferencias entre el grupo
# de tratamiento y el grupo de control no se rechaza para todas las variables.
# Sin embargo, para las variables "earnings", "tot_earnings" y "productivity"
# el valor p es menor al 10% lo cual nos daría motivos para poder rechazar
# la hipótesis nula que no hay diferencia entre los grupos de tratamiento y 
# control para estas 3 variables.


# Tabla de Balance Conjunta

balance_conjunto <- balance_regression(variables,treatment = "T_nap")

stargazer(as.data.frame(balance_conjunto$regression_tables),
          type="text",summary=FALSE)

# Cuando se considera la prueba conjunta de las variables por medio de una
# regresión por mínimos cuadrados, ninguna variable es estadísticamente 
# significativa, lo cual, muestra que no hay diferencia entre los dos grupos.
# Los valores p en esta regresión se encuentran a la izquierda del valor z de
# la significancia estadística al 10%. Se concluye que no hay diferencias
# estadísticamente significativas entre el grupo de control y el grupo de 
# tratamiento.
# Las variables están balanceadas en las dos tablas al menos al 5%,



#### 2) Efectos de Tratamiento

## endline <- read.csv("endline.csv")  o attach(endline)

endline <- read.csv("endline.csv")

media_trata <- endline %>% filter(T_nap == 1, !is.na(productivity)) %>% 
  pull(productivity) %>% mean()


media_contro <- endline %>% filter(T_nap == 0, !is.na(productivity)) %>% 
  pull(productivity) %>% mean()


var_trata <- endline %>% filter(T_nap == 1, !is.na(productivity)) %>% 
  pull(productivity) %>% var()


var_contro <- endline %>% filter(T_nap == 0, !is.na(productivity)) %>% 
  pull(productivity) %>% var()


### a) Estimadores ATE de Neyman sobre la productividad

yiT_2a <- endline$productivity[endline$T_nap==1]
yiC_2a <- endline$productivity[endline$T_nap==0]

## Medias de T_nap 

myiT_2a <- mean(yiT_2a,na.rm = TRUE)
myiC_2a <- mean(yiC_2a,na.rm = TRUE)

# Estadístico T

ate_neyman_2a <- myiT_2a - myiC_2a

tau_2a <- abs(ate_neyman_2a)

# Varianza del Estadístico T

vartau_2a <-var(endline$productivity[endline$T_nap==1],na.rm = TRUE)/length(yiT_2a) +
  var(endline$productivity[endline$T_nap==0],na.rm = TRUE)/length(yiC_2a)

# Valor p

tstat_2a <- tau_2a/sqrt(vartau_2a)
      
ate_neyman_2ap <-2*(1-pnorm(tstat_2a))


# Otra forma de obtener el valor p

ate_ney_pv <- t.test(
  x = yiT_2a,
  y = yiC_2a,
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)

Tabla_Neyman <- c("media tratamiento", "media control","Neyman", "varianza", "valor-p")
Productividad <- c(myiT_2a,myiC_2a,ate_neyman_2a,vartau_2a,ate_ney_pv$p.value)
tabla_ney_2a <- cbind(Tabla_Neyman,Productividad)
stargazer(tabla_ney_2a,type = "text")

# Si se prueba como variable de resultado la productividad reportada,
# El valor p del estimador de Neyman es 0.33817, por lo tanto no es significativo
# Se concluye que no hay un efecto significativo en la productividad antes y 
# después del tratamiento.



### b) OLS sin controles, heteroscedasticos

ols_2b <- lm(productivity~T_nap,data = endline)

summary(ols_2b)

cov_2b <- vcovHC(ols_2b, type = "HC")

robust.se_2b <- sqrt(diag(cov_2b))


stargazer(ols_2b,se=list(robust.se_2b),
          column.labels = c("OLS S/C"),type="text")





### c) OLS con controles, heteroscedasticos

endline <- endline %>% 
  mutate(age = age_,
         female = female_,
         education = education_,
         children = no_of_children_,
  )

## La primera variable que elegí como control fue age con la idea que la edad
## está relacionada con la productividad de forma negativa. Es decir, a mayor
## edad, menor productividad, aunque podría aumentar en cierto rango y disminuir
## en otro.

## La segunda variable es educación y considera el supuesto que entre mayor sea
## la educación o mayor sea el número de años, mayor será la productividad. Una
## relación positiva.

## La tercera variable son las ganancias de la persona. Esta variable marca la
## diferencia entre los tipos de trabajo con respecto a su productividad.

## La siguiente variable es el tiempo de dormir reportado, esta variable tiene
## que ver con un sustituto de la siesta en el trabajo. Si la variable es 
## estadísticamente significativa sería porque aumente la productividad por
## no estar cansado.

## Finalmente, el número de hijos es la variable que refleja hasta cierta forma
## un patrón de sueño diferente entre las personas observadas.


ols_2c <- lm(productivity~T_nap + age + education + female + health_bsl + 
               children, data = endline)

summary(ols_2c)

cov_2c <- vcovHC(ols_2c, type = "HC")

robust.se_2c <- sqrt(diag(cov_2c))


stargazer(ols_2c,se=list(robust.se_2c),
          column.labels = c("OLS Controles"),type = "text")




### d) Tabla con formatos de regresión

ols_2a <- lm(productivity~T_nap + 0,data = endline)


## Tablas con errores estándar en paréntesis

stargazer(ols_2a, ols_2b, ols_2c,se=list(sqrt(vartau_2a),robust.se_2b, 
                                        robust.se_2c), 
          column.labels = c("Neyman","OLS S/C","OLS Controles"), type = "text")


## Tablas con valores p en paréntesis

stargazer(ols_2a,ols_2b, ols_2c, se=list(sqrt(vartau_2a), robust.se_2b ,type="text",
                                         robust.se_2c), 
          column.labels = c("Neyman","OLS S/C","OLS Controles"),
          report=('vc*p'))

## Del total de variables de control que se añadieron al modelo, solamente edad, 
## educación, ganancias y número de hijos se obtuvo que son estadísticamente 
## significativos al menos al 10%. Esto quiere decir que sin ellos, 
## los resultados del efecto promedio están sobre estimados. Al tener esta
## diferencia se puede observar que el modelo con controles es el que tiene 
## mejor prueba F.






### e) Estimador ATE para nap_time_mins, sleep_report, happy, cognitive 
###    y typing_time_hr. Se trabaja con el modelo de controles. 


## nap_time_mins

ols_2e1 <- lm(nap_time_mins~T_nap + age + education + female + health_bsl + 
                children, data = endline)

summary(ols_2e1)

cov_2e1 <- vcovHC(ols_2e1, type = "HC")

robust.se_2e1 <- sqrt(diag(cov_2e1))


stargazer(ols_2e1,se=list(robust.se_2e1),
          column.labels = c("OLS Controles") ,type="text")



## sleep_report

ols_2e2 <- lm(sleep_report~T_nap + age + education + female + health_bsl + 
                children, data = endline)

summary(ols_2e2)

cov_2e2 <- vcovHC(ols_2e2, type = "HC")

robust.se_2e2 <- sqrt(diag(cov_2e2))


stargazer(ols_2e2,se=list(robust.se_2e2),
          column.labels = c("OLS Controles") ,type="text")





## Happy

ols_2e3 <- lm(happy~T_nap  + age + education + female + health_bsl + 
                children, data = endline)

summary(ols_2e3)

cov_2e3 <- vcovHC(ols_2e3, type = "HC")

robust.se_2e3 <- sqrt(diag(cov_2e3))


stargazer(ols_2e3, se=list(robust.se_2e3),
          column.labels = c("OLS Controles") ,type="text")




## Cognitive

## Para este ejercicio se normaliza [0,1] cada uno de los scores que obtuvo 
## la observación i para corsi, hf y pvt. De tal manera que el score más bajo
## de corsi representa el 0 y la más alta el 1. Con esta normalización los 3
## scores son comparables. El siguiente paso es obtener la media de estos scores
## que están dentro del [0,1]. No es necesario poner una ponderación diferenciada
## por lo tanto, La variable cognitive es el promedio de los 3 scores normalizados.

endline <- endline %>%
  filter(!is.na(corsi_measure),!is.na(hf_measure),!is.na(pvt_measure)) %>%
  mutate(c_corsi = abs(corsi_measure / sqrt(sum(corsi_measure^2))),
         c_hf = abs(hf_measure / sqrt(sum(hf_measure^2))),
         c_pvt = abs(pvt_measure / sqrt(sum(pvt_measure^2))))

endline <- endline %>% mutate(cognitive = (c_corsi+c_hf+c_pvt)/3 )

ols_2e4 <- lm(cognitive~T_nap  + age + education + female + health_bsl + 
                children, data = endline)

summary(ols_2e4)

cov_2e4 <- vcovHC(ols_2e4, type = "HC")

robust.se_2e4 <- sqrt(diag(cov_2e4))


stargazer(ols_2e4, se=list(robust.se_2e4),
          column.labels = c("OLS Controles") ,type="text")


## typing_time_hr


ols_2e5 <- lm(typing_time_hr~T_nap  + age + education + female + health_bsl + 
                children, data = endline)

summary(ols_2e5)

cov_2e5 <- vcovHC(ols_2e5, type = "HC")

robust.se_2e5 <- sqrt(diag(cov_2e5))


stargazer(ols_2e5, se=list(robust.se_2e5),
          column.labels = c("OLS Controles") ,type="text")




## Regresiones en una tabla (CAMBIAR REGRESIÖN A LA 5: ols_2e4)

stargazer(ols_2e1,ols_2e2,ols_2e3,ols_2e4,ols_2e5, 
          se=list(robust.se_2e1,robust.se_2e2,robust.se_2e3,robust.se_2e4,
                  robust.se_2e5),type="text")





#### 3) Fischer Exact Test - Bootstrap


## Se define la hipótesis nula como: el tratamiento de las siestas
## no tiene efecto alguno sobre la productividad para las personas.

## Se considera la productividad (productivity) como la variable dependiente
## y se define la variable T_nap como la asignación de tratamiento.




### a) Prueba FET para la hipótesis nula


## Para realizar el FET utilizaremos la función twoSamplePermutationTestLocation
## del paquete EnvStats



x_3a<-endline$productivity[endline$T_nap==1]
y_3a<-endline$productivity[endline$T_nap==0]

FET <- twoSamplePermutationTestLocation(x_3a, y_3a,alternative="two.sided",
                                 seed = 123,
                                 mu1.minus.mu2 = 0,
                                 n.permutations = 1000)


head(FET$stat.dist,250)

FET$p.value
(Distribucion_FET <- FET$stat.dist)


hist(Distribucion_FET,xlab = "Permutaciones",col = "gray",ylab = "Valor de la estimación")


pregunta_3a <- cbind(list("media",mean(Distribucion_FET),"P-Value",FET$p.value))

stargazer(pregunta_3a)

mean(Distribucion_FET)



### b) Valores P value para los estimadores.

pregunta_3a <- c("P-Value",FET$p.value)

ate_neyman_2ap <-2*(1-pnorm(tstat_2a))

OLS_nocontrol <- 0.328 

OLS_control <- 0.971


Estimadores <- c("FET", "Neyman", "OLS Sin Control", "OLS Control")
Valores_P <- c(FET$p.value,ate_neyman_2ap, OLS_nocontrol , OLS_control)


tabla_pvalue <- cbind(Estimadores,Valores_P)


## Tabla de valores p comparados
stargazer(tabla_pvalue,type = "text")




#### 4) Estratificación

## En este ejercicio se realiza la estratificación por ganancias y horas 
## de sueño. El umbral es la media de las variables. Se forman 4 grupos a
## los cuáles se espera que tengan observaciones de tratamiento y control.



## Los umbrales son las medianas


ganancias_med <- median(endline$earnings, na.rm = TRUE)

sueno_med <- median(endline$sleep_night, na.rm = TRUE)



### a) Estratificación


## Se crean 4 grupos, ganancias bajas con personas poco sueño
## ganancias bajas con personas mucho sueño, ganancias altas con poco sueño
## y ganancias altas con mucho sueño


## Se crean los grupos

## Se crea una variable indicadora en la base de datos de endline para
## diferenciar entre grupos.


endline<-endline %>% 
  mutate(estrato = case_when(earnings <= ganancias_med & sleep_night <= sueno_med ~ 1,
                             earnings <= ganancias_med & sleep_night > sueno_med ~ 2,
                             earnings > ganancias_med & sleep_night <= sueno_med ~ 3,
                             earnings > ganancias_med & sleep_night > sueno_med ~ 4,
                             TRUE ~ 0))


## Número de observaciones

## Grupo GANANCIAS BAJAS - POCO SUEÑO


n_gb_ps_c <- nrow(endline %>% filter(estrato == 1 & T_nap == 0))

n_gb_ps_t <- nrow(endline %>% filter(estrato == 1 & T_nap == 1))


## Grupo GANANCIAS BAJAS - MUCHO SUEÑO

n_gb_ms_c <- nrow(endline %>% filter(estrato == 2 & T_nap == 0))

n_gb_ms_t <- nrow(endline %>% filter(estrato == 2 & T_nap == 1))


## Grupo GANANCIAS ALTAS - POCO SUEÑO

n_ga_ps_c <- nrow(endline %>% filter(estrato == 3 & T_nap == 0))

n_ga_ps_t <- nrow(endline %>% filter(estrato == 3 & T_nap == 1))


## Grupo GANANCIAS ALTAS - MUCHO SUEÑO

n_ga_ms_c <- nrow(endline %>% filter(estrato == 4 & T_nap == 0))

n_ga_ms_t <- nrow(endline %>% filter(estrato == 4 & T_nap == 1))


Estratos<- c("Estrato","Intervención","Observaciones")
est1_control <- c("Ganancias Bajas, Poco Sueño","Control",n_gb_ps_c)
est1_tratamiento <- c("Ganancias Bajas, Poco Sueño","Tratamiento",n_gb_ps_t)
est2_control <- c("Ganancias Bajas, Mucho Sueño","Control",n_gb_ms_c)
est2_tratamiento <- c("Ganancias Bajas, Mucho Sueño","Tratamiento",n_gb_ms_t)
est3_control <- c("Ganancias Altas, Poco Sueño","Control",n_ga_ps_c)
est3_tratamiento <- c("Ganancias Altas, Poco Sueño","Tratamiento",n_ga_ps_t)
est4_control <- c("Ganancias Altas, Mucho Sueño","Control",n_ga_ms_c)
est4_tratamiento <- c("Ganancias Altas, Mucho Sueño","Tratamiento",n_ga_ms_t)





tabla_inciso4a <-rbind(Estratos,est1_control,est1_tratamiento,est2_control,est2_tratamiento,
                       est3_control,est3_tratamiento,est4_control,est4_tratamiento)

stargazer(tabla_inciso4a,type = "text")





###### nap_time_mins

n_endline_nap <- endline %>% filter(!is.na(nap_time_mins)) %>% nrow()

## Grupo GANANCIAS BAJAS - POCO SUEÑO

n_gb_ps <- nrow(endline %>% filter(estrato == 1))

## Medias

n_gb_ps_c_nap <- nrow(endline %>% filter(estrato == 1 & T_nap== 0))

nap_gb_ps_c <- endline %>%
  filter(estrato == 1 & T_nap == 0) %>%
  filter(!is.na(nap_time_mins)) %>% 
  pull(nap_time_mins) %>%
  mean()

n_gb_ps_t_nap <- nrow(endline %>% filter(estrato == 1 & T_nap== 1))

nap_gb_ps_t <- endline %>%
  filter(estrato == 1 & T_nap == 1) %>%
  filter(!is.na(nap_time_mins)) %>% 
  pull(nap_time_mins) %>%
  mean()

diff1_nap <- nap_gb_ps_t - nap_gb_ps_c

pond_diff1_nap <- (n_gb_ps/n_endline_nap)*diff1_nap

# Varianzas

var_nap_gb_ps_c <- endline %>%
  filter(estrato == 1 & T_nap == 0) %>%
  filter(!is.na(nap_time_mins)) %>% 
  pull(nap_time_mins) %>%
  var()

var_nap_gb_ps_t <- endline %>%
  filter(estrato == 1 & T_nap == 1) %>%
  filter(!is.na(nap_time_mins)) %>% 
  pull(nap_time_mins) %>%
  var()

var_nap_gb_ps <- (var_nap_gb_ps_c/n_gb_ps_c_nap) +
  (var_nap_gb_ps_t/n_gb_ps_t_nap)


## Grupo GANANCIAS BAJAS - MUCHO SUEÑO

n_gb_ms <- nrow(endline %>% filter(estrato == 2))

# Medias

n_gb_ms_c_nap <- nrow(endline %>% filter(estrato == 2 & T_nap== 0))

nap_gb_ms_c <- endline %>%
  filter(estrato == 2 & T_nap == 0) %>%
  filter(!is.na(nap_time_mins)) %>% 
  pull(nap_time_mins) %>%
  mean()

n_gb_ms_t_nap <- nrow(endline %>% filter(estrato == 2 & T_nap== 1))

nap_gb_ms_t <- endline %>%
  filter(estrato == 2 & T_nap == 1) %>%
  filter(!is.na(nap_time_mins)) %>% 
  pull(nap_time_mins) %>%
  mean()

diff2_nap <- nap_gb_ms_t - nap_gb_ms_c

pond_diff2_nap <- (n_gb_ms/n_endline_nap)*diff2_nap


# Varianzas

var_nap_gb_ms_c <- endline %>%
  filter(estrato == 2 & T_nap == 0) %>%
  filter(!is.na(nap_time_mins)) %>% 
  pull(nap_time_mins) %>%
  var()

var_nap_gb_ms_t <- endline %>%
  filter(estrato == 2 & T_nap == 1) %>%
  filter(!is.na(nap_time_mins)) %>% 
  pull(nap_time_mins) %>%
  var()

var_nap_gb_ms <- (var_nap_gb_ms_c/n_gb_ms_c_nap) +
                        (var_nap_gb_ms_t/n_gb_ms_t_nap)



## Grupo GANANCIAS ALTAS - POCO SUEÑO

n_ga_ps <- nrow(endline %>% filter(estrato == 3))

# Medias

n_ga_ps_c_nap <- nrow(endline %>% filter(estrato == 3 & T_nap== 0))

nap_ga_ps_c <- endline %>%
  filter(estrato == 3 & T_nap == 0) %>%
  filter(!is.na(nap_time_mins)) %>% 
  pull(nap_time_mins) %>%
  mean()

n_ga_ps_t_nap <- nrow(endline %>% filter(estrato == 3 & T_nap== 1))

nap_ga_ps_t <- endline %>%
  filter(estrato == 3 & T_nap == 1) %>%
  filter(!is.na(nap_time_mins)) %>% 
  pull(nap_time_mins) %>%
  mean()

diff3_nap <- nap_ga_ps_t - nap_ga_ps_c

pond_diff3_nap <- (n_ga_ps/n_endline_nap)*diff3_nap


# Varianzas

var_nap_ga_ps_c <- endline %>%
  filter(estrato == 3 & T_nap == 0) %>%
  filter(!is.na(nap_time_mins)) %>% 
  pull(nap_time_mins) %>%
  var()

var_nap_ga_ps_t <- endline %>%
  filter(estrato == 3 & T_nap == 1) %>%
  filter(!is.na(nap_time_mins)) %>% 
  pull(nap_time_mins) %>%
  var()

var_nap_ga_ps <- (var_nap_ga_ps_c/n_ga_ps_c_nap) +
                        (var_nap_ga_ps_t/n_ga_ps_t_nap)



## Grupo GANANCIAS ALTAS - MUCHO SUEÑO

n_ga_ms <- nrow(endline %>% filter(estrato == 4))

# Medias

n_ga_ms_c_nap <- nrow(endline %>% filter(estrato == 4 & T_nap== 0))

nap_ga_ms_c <- endline %>%
  filter(estrato == 4 & T_nap == 0) %>%
  filter(!is.na(nap_time_mins)) %>% 
  pull(nap_time_mins) %>%
  mean()

n_ga_ms_t_nap <- nrow(endline %>% filter(estrato == 4 & T_nap== 1))

nap_ga_ms_t <- endline %>%
  filter(estrato == 4 & T_nap == 1) %>%
  filter(!is.na(nap_time_mins)) %>% 
  pull(nap_time_mins) %>%
  mean()


diff4_nap <- nap_ga_ms_t - nap_ga_ms_c

pond_diff4_nap <- (n_ga_ms/n_endline_nap)*diff4_nap


# Varianzas

var_nap_ga_ms_c <- endline %>%
  filter(estrato == 4 & T_nap == 0) %>%
  filter(!is.na(nap_time_mins)) %>% 
  pull(nap_time_mins) %>%
  var()

var_nap_ga_ms_t <- endline %>%
  filter(estrato == 4 & T_nap == 1) %>%
  filter(!is.na(nap_time_mins)) %>% 
  pull(nap_time_mins) %>%
  var()

var_nap_ga_ms <- (var_nap_ga_ms_c/n_ga_ms_c_nap) +
                        (var_nap_ga_ms_t/n_ga_ms_t_nap)


var_nap <- sum(var_nap_gb_ps*(n_gb_ps/n_endline_nap)^2,
               var_nap_gb_ms*(n_gb_ms/n_endline_nap)^2,
               var_nap_ga_ps*(n_ga_ps/n_endline_nap)^2,
               var_nap_ga_ms*(n_ga_ms/n_endline_nap)^2)




Estrato_NAP <- c("Estrato","ATE Estrato NAP","Errores","Observaciones","ATE Ponderado")
nap_dif1 <- c("Ganancias Bajas, Poco Sueño", diff1_nap,sqrt(var_nap_gb_ps),n_gb_ps,pond_diff1_nap)
nap_dif2 <- c("Ganancias Bajas, Mucho Sueño", diff2_nap,sqrt(var_nap_gb_ms),n_gb_ms,pond_diff2_nap)
nap_dif3 <- c("Ganancias Altas, Poco Sueño", diff3_nap,sqrt(var_nap_ga_ps),n_ga_ps,pond_diff3_nap)
nap_dif4 <- c("Ganancias Altas, Mucho Sueño", diff4_nap,sqrt(var_nap_ga_ms),n_ga_ms,pond_diff4_nap)

ATE_ponderado_NAP <- c("","",sqrt(var_nap),"",pond_diff1_nap+pond_diff2_nap+pond_diff3_nap+pond_diff4_nap)

tabla_estrato_NAP <- rbind(Estrato_NAP,nap_dif1,nap_dif2,nap_dif3,nap_dif4,ATE_ponderado_NAP)
stargazer(tabla_estrato_NAP, type = "text")



###### sleep_report

n_endline_slp <- endline %>% filter(!is.na(sleep_report)) %>% nrow()

## Grupo GANANCIAS BAJAS - POCO SUEÑO

n_gb_ps <- nrow(endline %>% filter(estrato == 1))

# Medias

n_gb_ps_c_slp <- nrow(endline %>% filter(estrato == 1 & T_nap== 0))

slp_gb_ps_c <- endline %>%
  filter(estrato == 1 & T_nap == 0) %>%
  filter(!is.na(sleep_report)) %>% 
  pull(sleep_report) %>%
  mean()

n_gb_ps_t_slp <- nrow(endline %>% filter(estrato == 1 & T_nap== 1))

slp_gb_ps_t <- endline %>%
  filter(estrato == 1 & T_nap == 1) %>%
  filter(!is.na(sleep_report)) %>% 
  pull(sleep_report) %>%
  mean()

diff1_slp <- slp_gb_ps_t - slp_gb_ps_c

pond_diff1_slp <- (n_gb_ps/n_endline_slp)*diff1_slp


# Varianzas

var_slp_gb_ps_c <- endline %>%
  filter(estrato == 1 & T_nap == 0) %>%
  filter(!is.na(sleep_report)) %>% 
  pull(sleep_report) %>%
  var()

var_slp_gb_ps_t <- endline %>%
  filter(estrato == 1 & T_nap == 1) %>%
  filter(!is.na(sleep_report)) %>% 
  pull(sleep_report) %>%
  var()

var_slp_gb_ps <- (var_slp_gb_ps_c/n_gb_ps_c_slp) +
                        (var_slp_gb_ps_t/n_gb_ps_t_slp)



## Grupo GANANCIAS BAJAS - MUCHO SUEÑO

n_gb_ms <- nrow(endline %>% filter(estrato == 2))

# Medias

n_gb_ms_c_slp <- nrow(endline %>% filter(estrato == 2 & T_nap== 0))

slp_gb_ms_c <- endline %>%
  filter(estrato == 2 & T_nap == 0) %>%
  filter(!is.na(sleep_report)) %>% 
  pull(sleep_report) %>%
  mean()

n_gb_ms_t_slp <- nrow(endline %>% filter(estrato == 2 & T_nap== 1))

slp_gb_ms_t <- endline %>%
  filter(estrato == 2 & T_nap == 1) %>%
  filter(!is.na(sleep_report)) %>% 
  pull(sleep_report) %>%
  mean()

diff2_slp <- slp_gb_ms_t - slp_gb_ms_c

pond_diff2_slp <- (n_gb_ms/n_endline_slp)*diff2_slp


# Varianzas

var_slp_gb_ms_c <- endline %>%
  filter(estrato == 2 & T_nap == 0) %>%
  filter(!is.na(sleep_report)) %>% 
  pull(sleep_report) %>%
  var()

var_slp_gb_ms_t <- endline %>%
  filter(estrato == 2 & T_nap == 1) %>%
  filter(!is.na(sleep_report)) %>% 
  pull(sleep_report) %>%
  var()

var_slp_gb_ms <- (var_slp_gb_ms_c/n_gb_ms_c_slp) +
                        (var_slp_gb_ms_t/n_gb_ms_t_slp)



## Grupo GANANCIAS ALTAS - POCO SUEÑO

n_ga_ps <- nrow(endline %>% filter(estrato == 3))

# Medias

n_ga_ps_c_slp <- nrow(endline %>% filter(estrato == 3 & T_nap== 0))

slp_ga_ps_c <- endline %>%
  filter(estrato == 3 & T_nap == 0) %>%
  filter(!is.na(sleep_report)) %>% 
  pull(sleep_report) %>%
  mean()

n_ga_ps_t_slp <- nrow(endline %>% filter(estrato == 3 & T_nap== 1))

slp_ga_ps_t <- endline %>%
  filter(estrato == 3 & T_nap == 1) %>%
  filter(!is.na(sleep_report)) %>% 
  pull(sleep_report) %>%
  mean()

diff3_slp <- slp_ga_ps_t - slp_ga_ps_c

pond_diff3_slp <- (n_ga_ps/n_endline_slp)*diff3_slp


# Varianzas

var_slp_ga_ps_c <- endline %>%
  filter(estrato == 3 & T_nap == 0) %>%
  filter(!is.na(sleep_report)) %>% 
  pull(sleep_report) %>%
  var()

var_slp_ga_ps_t <- endline %>%
  filter(estrato == 3 & T_nap == 1) %>%
  filter(!is.na(sleep_report)) %>% 
  pull(sleep_report) %>%
  var()

var_slp_ga_ps <- (var_slp_ga_ps_c/n_ga_ps_c_slp) +
                        (var_slp_ga_ps_t/n_ga_ps_t_slp)



## Grupo GANANCIAS ALTAS - MUCHO SUEÑO

n_ga_ms <- nrow(endline %>% filter(estrato == 4))

# Medias

n_ga_ms_c_slp <- nrow(endline %>% filter(estrato == 4 & T_nap== 0))

slp_ga_ms_c <- endline %>%
  filter(estrato == 4 & T_nap == 0) %>%
  filter(!is.na(sleep_report)) %>% 
  pull(sleep_report) %>%
  mean()

n_ga_ms_t_slp <- nrow(endline %>% filter(estrato == 4 & T_nap== 1))

slp_ga_ms_t <- endline %>%
  filter(estrato == 4 & T_nap == 1) %>%
  filter(!is.na(sleep_report)) %>% 
  pull(sleep_report) %>%
  mean()


diff4_slp <- slp_ga_ms_t - slp_ga_ms_c

pond_diff4_slp <- (n_ga_ms/n_endline_slp)*diff4_slp


# Varianzas

var_slp_ga_ms_c <- endline %>%
  filter(estrato == 4 & T_nap == 0) %>%
  filter(!is.na(sleep_report)) %>% 
  pull(sleep_report) %>%
  var()

var_slp_ga_ms_t <- endline %>%
  filter(estrato == 4 & T_nap == 1) %>%
  filter(!is.na(sleep_report)) %>% 
  pull(sleep_report) %>%
  var()

var_slp_ga_ms <- (var_slp_ga_ms_c/n_ga_ms_c_slp) +
                        (var_slp_ga_ms_t/n_ga_ms_t_slp)


var_slp <- sum(var_slp_gb_ps*(n_gb_ps/n_endline_slp)^2,
               var_slp_gb_ms*(n_gb_ms/n_endline_slp)^2,
               var_slp_ga_ps*(n_ga_ps/n_endline_slp)^2,
               var_slp_ga_ms*(n_ga_ms/n_endline_slp)^2)

Estrato_SLP <- c("Estrato","ATE Estrato SLP","Errores","Observaciones","ATE Ponderado")
slp_dif1 <- c("Ganancias Bajas, Poco Sueño", diff1_slp,sqrt(var_slp_gb_ps),n_gb_ps,pond_diff1_slp)
slp_dif2 <- c("Ganancias Bajas, Mucho Sueño", diff2_slp,sqrt(var_slp_gb_ms),n_gb_ms,pond_diff2_slp)
slp_dif3 <- c("Ganancias Altas, Poco Sueño", diff3_slp,sqrt(var_slp_ga_ps),n_ga_ps,pond_diff3_slp)
slp_dif4 <- c("Ganancias Altas, Mucho Sueño", diff4_slp,sqrt(var_slp_ga_ms),n_ga_ms,pond_diff4_slp)

ATE_ponderado_SLP <- c("","",sqrt(var_slp),"",pond_diff1_slp+pond_diff2_slp+pond_diff3_slp+pond_diff4_slp)

tabla_estrato_SLP <- rbind(Estrato_SLP,slp_dif1,slp_dif2,slp_dif3,slp_dif4,ATE_ponderado_SLP)
stargazer(tabla_estrato_SLP,type = "text")




###### happy

n_endline_hpy <- endline %>% filter(!is.na(happy)) %>% nrow()

## Grupo GANANCIAS BAJAS - POCO SUEÑO

n_gb_ps <- nrow(endline %>% filter(estrato == 1))

# Medias

n_gb_ps_c_hpy <- nrow(endline %>% filter(estrato == 1 & T_nap== 0))

hpy_gb_ps_c <- endline %>%
  filter(estrato == 1 & T_nap == 0) %>%
  filter(!is.na(happy)) %>% 
  pull(happy) %>%
  mean()

n_gb_ps_t_hpy <- nrow(endline %>% filter(estrato == 1 & T_nap== 1))

hpy_gb_ps_t <- endline %>%
  filter(estrato == 1 & T_nap == 1) %>%
  filter(!is.na(happy)) %>% 
  pull(happy) %>%
  mean()

diff1_hpy <- hpy_gb_ps_t - hpy_gb_ps_c

pond_diff1_hpy <- (n_gb_ps/n_endline_hpy)*diff1_hpy


# Varianzas

var_hpy_gb_ps_c <- endline %>%
  filter(estrato == 1 & T_nap == 0) %>%
  filter(!is.na(happy)) %>% 
  pull(happy) %>%
  var()

var_hpy_gb_ps_t <- endline %>%
  filter(estrato == 1 & T_nap == 1) %>%
  filter(!is.na(happy)) %>% 
  pull(happy) %>%
  var()

var_hpy_gb_ps <- (var_hpy_gb_ps_c/n_gb_ps_c_hpy) +
                        (var_hpy_gb_ps_t/n_gb_ps_t_hpy)



## Grupo GANANCIAS BAJAS - MUCHO SUEÑO

n_gb_ms <- nrow(endline %>% filter(estrato == 2))

# Medias

n_gb_ms_c_hpy <- nrow(endline %>% filter(estrato == 2 & T_nap== 0))

hpy_gb_ms_c <- endline %>%
  filter(estrato == 2 & T_nap == 0) %>%
  filter(!is.na(happy)) %>% 
  pull(happy) %>%
  mean()

n_gb_ms_t_hpy <- nrow(endline %>% filter(estrato == 2 & T_nap== 1))

hpy_gb_ms_t <- endline %>%
  filter(estrato == 2 & T_nap == 1) %>%
  filter(!is.na(happy)) %>% 
  pull(happy) %>%
  mean()

diff2_hpy <- hpy_gb_ms_t - hpy_gb_ms_c

pond_diff2_hpy <- (n_gb_ms/n_endline_hpy)*diff2_hpy


# Varianzas

var_hpy_gb_ms_c <- endline %>%
  filter(estrato == 2 & T_nap == 0) %>%
  filter(!is.na(happy)) %>% 
  pull(happy) %>%
  var()

var_hpy_gb_ms_t <- endline %>%
  filter(estrato == 2 & T_nap == 1) %>%
  filter(!is.na(happy)) %>% 
  pull(happy) %>%
  var()

var_hpy_gb_ms <- (var_hpy_gb_ms_c/n_gb_ms_c_hpy) +
                        (var_hpy_gb_ms_t/n_gb_ms_t_hpy)




## Grupo GANANCIAS ALTAS - POCO SUEÑO

n_ga_ps <- nrow(endline %>% filter(estrato == 3))

# Medias

n_ga_ps_c_hpy <- nrow(endline %>% filter(estrato == 3 & T_nap== 0))

hpy_ga_ps_c <- endline %>%
  filter(estrato == 3 & T_nap == 0) %>%
  filter(!is.na(happy)) %>% 
  pull(happy) %>%
  mean()

n_ga_ps_t_hpy <- nrow(endline %>% filter(estrato == 3 & T_nap== 1))

hpy_ga_ps_t <- endline %>%
  filter(estrato == 3 & T_nap == 1) %>%
  filter(!is.na(happy)) %>% 
  pull(happy) %>%
  mean()

diff3_hpy <- hpy_ga_ps_t - hpy_ga_ps_c

pond_diff3_hpy <- (n_ga_ps/n_endline_hpy)*diff3_hpy


# Varianzas

var_hpy_ga_ps_c <- endline %>%
  filter(estrato == 3 & T_nap == 0) %>%
  filter(!is.na(happy)) %>% 
  pull(happy) %>%
  var()

var_hpy_ga_ps_t <- endline %>%
  filter(estrato == 3 & T_nap == 1) %>%
  filter(!is.na(happy)) %>% 
  pull(happy) %>%
  var()

var_hpy_ga_ps <- (var_hpy_ga_ps_c/n_ga_ps_c_hpy) +
                        (var_hpy_ga_ps_t/n_ga_ps_t_hpy)



## Grupo GANANCIAS ALTAS - MUCHO SUEÑO

n_ga_ms <- nrow(endline %>% filter(estrato == 4))

# Medias

n_ga_ms_c_hpy <- nrow(endline %>% filter(estrato == 4 & T_nap== 0))

hpy_ga_ms_c <- endline %>%
  filter(estrato == 4 & T_nap == 0) %>%
  filter(!is.na(happy)) %>% 
  pull(happy) %>%
  mean()

n_ga_ms_t_hpy <- nrow(endline %>% filter(estrato == 4 & T_nap== 1))

hpy_ga_ms_t <- endline %>%
  filter(estrato == 4 & T_nap == 1) %>%
  filter(!is.na(happy)) %>% 
  pull(happy) %>%
  mean()


diff4_hpy <- hpy_ga_ms_t - hpy_ga_ms_c

pond_diff4_hpy <- (n_ga_ms/n_endline_hpy)*diff4_hpy


# Varianzas

var_hpy_ga_ms_c <- endline %>%
  filter(estrato == 4 & T_nap == 0) %>%
  filter(!is.na(happy)) %>% 
  pull(happy) %>%
  var()

var_hpy_ga_ms_t <- endline %>%
  filter(estrato == 4 & T_nap == 1) %>%
  filter(!is.na(happy)) %>% 
  pull(happy) %>%
  var()

var_hpy_ga_ms <- (var_hpy_ga_ms_c/n_ga_ms_c_hpy) +
                        (var_hpy_ga_ms_t/n_ga_ms_t_hpy)

var_hpy <- sum(var_hpy_gb_ps*(n_gb_ps/n_endline_hpy)^2,
               var_hpy_gb_ms*(n_gb_ms/n_endline_hpy)^2,
               var_hpy_ga_ps*(n_ga_ps/n_endline_hpy)^2,
               var_hpy_ga_ms*(n_ga_ms/n_endline_hpy)^2)


Estrato_HAPPY <- c("Estrato","ATE Estrato HAPPY","Errores","Observaciones","ATE Ponderado")
hpy_dif1 <- c("Ganancias Bajas, Poco Sueño", diff1_hpy,sqrt(var_hpy_gb_ps),n_gb_ps,pond_diff1_hpy)
hpy_dif2 <- c("Ganancias Bajas, Mucho Sueño", diff2_hpy,sqrt(var_hpy_gb_ms),n_gb_ms,pond_diff2_hpy)
hpy_dif3 <- c("Ganancias Altas, Poco Sueño", diff3_hpy,sqrt(var_hpy_ga_ps),n_ga_ps,pond_diff3_hpy)
hpy_dif4 <- c("Ganancias Altas, Mucho Sueño", diff4_hpy,sqrt(var_hpy_ga_ms),n_ga_ms,pond_diff4_hpy)

ATE_ponderado_HAPPY <- c("","",sqrt(var_hpy),"",pond_diff1_hpy+pond_diff2_hpy+pond_diff3_hpy+pond_diff4_hpy)

tabla_estrato_HAPPY <- rbind(Estrato_HAPPY,hpy_dif1,hpy_dif2,hpy_dif3,hpy_dif4,ATE_ponderado_HAPPY)
stargazer(tabla_estrato_HAPPY,type = "text")



###### cognitive

n_endline_cog <- endline %>% filter(!is.na(cognitive)) %>% nrow()

## Grupo GANANCIAS BAJAS - POCO SUEÑO

n_gb_ps <- nrow(endline %>% filter(estrato == 1))

# Medias

n_gb_ps_c_cog <- nrow(endline %>% filter(estrato == 1 & T_nap== 0))

cog_gb_ps_c <- endline %>%
  filter(estrato == 1 & T_nap == 0) %>%
  filter(!is.na(cognitive)) %>% 
  pull(cognitive) %>%
  mean()

n_gb_ps_t_cog <- nrow(endline %>% filter(estrato == 1 & T_nap== 1))

cog_gb_ps_t <- endline %>%
  filter(estrato == 1 & T_nap == 1) %>%
  filter(!is.na(cognitive)) %>% 
  pull(cognitive) %>%
  mean()

diff1_cog <- cog_gb_ps_t - cog_gb_ps_c

pond_diff1_cog <- (n_gb_ps/n_endline_cog)*diff1_cog


# Varianzas

var_cog_gb_ps_c <- endline %>%
  filter(estrato == 1 & T_nap == 0) %>%
  filter(!is.na(cognitive)) %>% 
  pull(cognitive) %>%
  var()

var_cog_gb_ps_t <- endline %>%
  filter(estrato == 1 & T_nap == 1) %>%
  filter(!is.na(cognitive)) %>% 
  pull(cognitive) %>%
  var()

var_cog_gb_ps <- (var_cog_gb_ps_c/n_gb_ps_c_cog) +
  (var_cog_gb_ps_t/n_gb_ps_t_cog)



## Grupo GANANCIAS BAJAS - MUCHO SUEÑO

n_gb_ms <- nrow(endline %>% filter(estrato == 2))

# Medias

n_gb_ms_c_cog <- nrow(endline %>% filter(estrato == 2 & T_nap== 0))

cog_gb_ms_c <- endline %>%
  filter(estrato == 2 & T_nap == 0) %>%
  filter(!is.na(cognitive)) %>% 
  pull(cognitive) %>%
  mean()

n_gb_ms_t_cog <- nrow(endline %>% filter(estrato == 2 & T_nap== 1))

cog_gb_ms_t <- endline %>%
  filter(estrato == 2 & T_nap == 1) %>%
  filter(!is.na(cognitive)) %>% 
  pull(cognitive) %>%
  mean()

diff2_cog <- cog_gb_ms_t - cog_gb_ms_c

pond_diff2_cog <- (n_gb_ms/n_endline_cog)*diff2_cog


# Varianzas

var_cog_gb_ms_c <- endline %>%
  filter(estrato == 2 & T_nap == 0) %>%
  filter(!is.na(cognitive)) %>% 
  pull(cognitive) %>%
  var()

var_cog_gb_ms_t <- endline %>%
  filter(estrato == 2 & T_nap == 1) %>%
  filter(!is.na(cognitive)) %>% 
  pull(cognitive) %>%
  var()

var_cog_gb_ms <- (var_cog_gb_ms_c/n_gb_ms_c_cog) +
  (var_cog_gb_ms_t/n_gb_ms_t_cog)




## Grupo GANANCIAS ALTAS - POCO SUEÑO

n_ga_ps <- nrow(endline %>% filter(estrato == 3))

# Medias

n_ga_ps_c_cog <- nrow(endline %>% filter(estrato == 3 & T_nap== 0))

cog_ga_ps_c <- endline %>%
  filter(estrato == 3 & T_nap == 0) %>%
  filter(!is.na(cognitive)) %>% 
  pull(cognitive) %>%
  mean()

n_ga_ps_t_cog <- nrow(endline %>% filter(estrato == 3 & T_nap== 1))

cog_ga_ps_t <- endline %>%
  filter(estrato == 3 & T_nap == 1) %>%
  filter(!is.na(cognitive)) %>% 
  pull(cognitive) %>%
  mean()

diff3_cog <- cog_ga_ps_t - cog_ga_ps_c

pond_diff3_cog <- (n_ga_ps/n_endline_cog)*diff3_cog


# Varianzas

var_cog_ga_ps_c <- endline %>%
  filter(estrato == 3 & T_nap == 0) %>%
  filter(!is.na(cognitive)) %>% 
  pull(cognitive) %>%
  var()

var_cog_ga_ps_t <- endline %>%
  filter(estrato == 3 & T_nap == 1) %>%
  filter(!is.na(cognitive)) %>% 
  pull(cognitive) %>%
  var()

var_cog_ga_ps <- (var_cog_ga_ps_c/n_ga_ps_c_cog) +
  (var_cog_ga_ps_t/n_ga_ps_t_cog)




## Grupo GANANCIAS ALTAS - MUCHO SUEÑO

n_ga_ms <- nrow(endline %>% filter(estrato == 4))

# Medias

n_ga_ms_c_cog <- nrow(endline %>% filter(estrato == 4 & T_nap== 0))

cog_ga_ms_c <- endline %>%
  filter(estrato == 4 & T_nap == 0) %>%
  filter(!is.na(cognitive)) %>% 
  pull(cognitive) %>%
  mean()

n_ga_ms_t_cog <- nrow(endline %>% filter(estrato == 4 & T_nap== 1))

cog_ga_ms_t <- endline %>%
  filter(estrato == 4 & T_nap == 1) %>%
  filter(!is.na(cognitive)) %>% 
  pull(cognitive) %>%
  mean()


diff4_cog <- cog_ga_ms_t - cog_ga_ms_c

pond_diff4_cog <- (n_ga_ms/n_endline_cog)*diff4_cog


# Varianzas

var_cog_ga_ms_c <- endline %>%
  filter(estrato == 4 & T_nap == 0) %>%
  filter(!is.na(cognitive)) %>% 
  pull(cognitive) %>%
  var()

var_cog_ga_ms_t <- endline %>%
  filter(estrato == 4 & T_nap == 1) %>%
  filter(!is.na(cognitive)) %>% 
  pull(cognitive) %>%
  var()

var_cog_ga_ms <- (var_cog_ga_ms_c/n_ga_ms_c_cog) +
  (var_cog_ga_ms_t/n_ga_ms_t_cog)


var_cog <- sum(var_cog_gb_ps*(n_gb_ps/n_endline_cog)^2,
               var_cog_gb_ms*(n_gb_ms/n_endline_cog)^2,
               var_cog_ga_ps*(n_ga_ps/n_endline_cog)^2,
               var_cog_ga_ms*(n_ga_ms/n_endline_cog)^2)



Estrato_COG <- c("Estrato","ATE Estrato COGNITIVE","Errores","Observaciones","ATE Ponderado")
cog_dif1 <- c("Ganancias Bajas, Poco Sueño", diff1_cog,sqrt(var_cog_gb_ps),n_gb_ps,pond_diff1_cog)
cog_dif2 <- c("Ganancias Bajas, Mucho Sueño", diff2_cog,sqrt(var_cog_gb_ms),n_gb_ms,pond_diff2_cog)
cog_dif3 <- c("Ganancias Altas, Poco Sueño", diff3_cog,sqrt(var_cog_ga_ps),n_ga_ps,pond_diff3_cog)
cog_dif4 <- c("Ganancias Altas, Mucho Sueño", diff4_cog,sqrt(var_cog_ga_ms),n_ga_ms,pond_diff4_cog)

ATE_ponderado_COG <- c("","",sqrt(var_cog),"",pond_diff1_cog+pond_diff2_cog+pond_diff3_cog+pond_diff4_cog)

tabla_estrato_COG <- rbind(Estrato_COG,cog_dif1,cog_dif2,cog_dif3,cog_dif4,ATE_ponderado_COG)
stargazer(tabla_estrato_COG,type = "text")





###### typing_time_hr

n_endline_typ <- endline %>% filter(!is.na(typing_time_hr)) %>% nrow()

## Grupo GANANCIAS BAJAS - POCO SUEÑO

n_gb_ps <- nrow(endline %>% filter(estrato == 1))

# Medias

n_gb_ps_c_typ <- nrow(endline %>% filter(estrato == 1 & T_nap== 0))

typ_gb_ps_c <- endline %>%
  filter(estrato == 1 & T_nap == 0) %>%
  filter(!is.na(typing_time_hr)) %>% 
  pull(typing_time_hr) %>%
  mean()

n_gb_ps_t_typ <- nrow(endline %>% filter(estrato == 1 & T_nap== 1))

typ_gb_ps_t <- endline %>%
  filter(estrato == 1 & T_nap == 1) %>%
  filter(!is.na(typing_time_hr)) %>% 
  pull(typing_time_hr) %>%
  mean()

diff1_typ <- typ_gb_ps_t - typ_gb_ps_c

pond_diff1_typ <- (n_gb_ps/n_endline_typ)*diff1_typ


# Varianzas

var_typ_gb_ps_c <- endline %>%
  filter(estrato == 1 & T_nap == 0) %>%
  filter(!is.na(typing_time_hr)) %>% 
  pull(typing_time_hr) %>%
  var()

var_typ_gb_ps_t <- endline %>%
  filter(estrato == 1 & T_nap == 1) %>%
  filter(!is.na(typing_time_hr)) %>% 
  pull(typing_time_hr) %>%
  var()

var_typ_gb_ps <- (var_typ_gb_ps_c/n_gb_ps_c_typ) +
  (var_typ_gb_ps_t/n_gb_ps_t_typ)




## Grupo GANANCIAS BAJAS - MUCHO SUEÑO

n_gb_ms <- nrow(endline %>% filter(estrato == 2))

# Medias

n_gb_ms_c_typ <- nrow(endline %>% filter(estrato == 2 & T_nap== 0))

typ_gb_ms_c <- endline %>%
  filter(estrato == 2 & T_nap == 0) %>%
  filter(!is.na(typing_time_hr)) %>% 
  pull(typing_time_hr) %>%
  mean()

n_gb_ms_t_typ <- nrow(endline %>% filter(estrato == 2 & T_nap== 1))

typ_gb_ms_t <- endline %>%
  filter(estrato == 2 & T_nap == 1) %>%
  filter(!is.na(typing_time_hr)) %>% 
  pull(typing_time_hr) %>%
  mean()

diff2_typ <- typ_gb_ms_t - typ_gb_ms_c

pond_diff2_typ <- (n_gb_ms/n_endline_typ)*diff2_typ


# Varianzas

var_typ_gb_ms_c <- endline %>%
  filter(estrato == 2 & T_nap == 0) %>%
  filter(!is.na(typing_time_hr)) %>% 
  pull(typing_time_hr) %>%
  var()

var_typ_gb_ms_t <- endline %>%
  filter(estrato == 2 & T_nap == 1) %>%
  filter(!is.na(typing_time_hr)) %>% 
  pull(typing_time_hr) %>%
  var()

var_typ_gb_ms <- (var_typ_gb_ms_c/n_gb_ms_c_typ) +
  (var_typ_gb_ms_t/n_gb_ms_t_typ)



## Grupo GANANCIAS ALTAS - POCO SUEÑO

n_ga_ps <- nrow(endline %>% filter(estrato == 3))

# Medias

n_ga_ps_c_typ <- nrow(endline %>% filter(estrato == 3 & T_nap== 0))

typ_ga_ps_c <- endline %>%
  filter(estrato == 3 & T_nap == 0) %>%
  filter(!is.na(typing_time_hr)) %>% 
  pull(typing_time_hr) %>%
  mean()

n_ga_ps_t_typ <- nrow(endline %>% filter(estrato == 3 & T_nap== 1))

typ_ga_ps_t <- endline %>%
  filter(estrato == 3 & T_nap == 1) %>%
  filter(!is.na(typing_time_hr)) %>% 
  pull(typing_time_hr) %>%
  mean()

diff3_typ <- typ_ga_ps_t - typ_ga_ps_c

pond_diff3_typ <- (n_ga_ps/n_endline_typ)*diff3_typ


# Varianzas

var_typ_ga_ps_c <- endline %>%
  filter(estrato == 3 & T_nap == 0) %>%
  filter(!is.na(typing_time_hr)) %>% 
  pull(typing_time_hr) %>%
  var()

var_typ_ga_ps_t <- endline %>%
  filter(estrato == 3 & T_nap == 1) %>%
  filter(!is.na(typing_time_hr)) %>% 
  pull(typing_time_hr) %>%
  var()

var_typ_ga_ps <- (var_typ_ga_ps_c/n_ga_ps_c_typ) +
  (var_typ_ga_ps_t/n_ga_ps_t_typ)



## Grupo GANANCIAS ALTAS - MUCHO SUEÑO

n_ga_ms <- nrow(endline %>% filter(estrato == 4))

# Medias

n_ga_ms_c_typ <- nrow(endline %>% filter(estrato == 4 & T_nap== 0))

typ_ga_ms_c <- endline %>%
  filter(estrato == 4 & T_nap == 0) %>%
  filter(!is.na(typing_time_hr)) %>% 
  pull(typing_time_hr) %>%
  mean()

n_ga_ms_t_typ <- nrow(endline %>% filter(estrato == 4 & T_nap== 1))

typ_ga_ms_t <- endline %>%
  filter(estrato == 4 & T_nap == 1) %>%
  filter(!is.na(typing_time_hr)) %>% 
  pull(typing_time_hr) %>%
  mean()


diff4_typ <- typ_ga_ms_t - typ_ga_ms_c

pond_diff4_typ <- (n_ga_ms/n_endline_typ)*diff4_typ


# Varianzas

var_typ_ga_ms_c <- endline %>%
  filter(estrato == 4 & T_nap == 0) %>%
  filter(!is.na(typing_time_hr)) %>% 
  pull(typing_time_hr) %>%
  var()

var_typ_ga_ms_t <- endline %>%
  filter(estrato == 4 & T_nap == 1) %>%
  filter(!is.na(typing_time_hr)) %>% 
  pull(typing_time_hr) %>%
  var()

var_typ_ga_ms <- (var_typ_ga_ms_c/n_ga_ms_c_typ) +
  (var_typ_ga_ms_t/n_ga_ms_t_typ)


var_typ <- sum(var_typ_gb_ps*(n_gb_ps/n_endline_typ)^2,
               var_typ_gb_ms*(n_gb_ms/n_endline_typ)^2,
               var_typ_ga_ps*(n_ga_ps/n_endline_typ)^2,
               var_typ_ga_ms*(n_ga_ms/n_endline_typ)^2)



Estrato_TYP <- c("Estrato","ATE Estrato TYPING","Errores","Observaciones","ATE Ponderado")
typ_dif1 <- c("Ganancias Bajas, Poco Sueño", diff1_typ,sqrt(var_typ_gb_ps),n_gb_ps,pond_diff1_typ)
typ_dif2 <- c("Ganancias Bajas, Mucho Sueño", diff2_typ,sqrt(var_typ_gb_ms),n_gb_ms,pond_diff2_typ)
typ_dif3 <- c("Ganancias Altas, Poco Sueño", diff3_typ,sqrt(var_typ_ga_ps),n_ga_ps,pond_diff3_typ)
typ_dif4 <- c("Ganancias Altas, Mucho Sueño", diff4_typ,sqrt(var_typ_ga_ms),n_ga_ms,pond_diff4_typ)

ATE_ponderado_TYP <- c("","",sqrt(var_typ),"",pond_diff1_typ+pond_diff2_typ+pond_diff3_typ+pond_diff4_typ)

tabla_estrato_TYP <- rbind(Estrato_TYP,typ_dif1,typ_dif2,typ_dif3,typ_dif4,ATE_ponderado_TYP)
stargazer(tabla_estrato_TYP,type = "text")






#### c) Efectos heterogéneos. Regresión con Interacciones.

endline<-endline %>% 
  mutate( earn_high = ifelse(earnings > ganancias_med,1,0),
          sleep_high = ifelse(sleep_night > sueno_med,1,0))


hetero_nap_4c <- lm(nap_time_mins ~ T_nap + earn_high + sleep_high +
                      T_nap*earn_high + T_nap*sleep_high,data = endline)

hetero_sleep_4c <- lm(sleep_report ~ T_nap + earn_high + sleep_high +
                        T_nap*earn_high + T_nap*sleep_high,data = endline)

hetero_happy_4c <- lm(happy ~ T_nap + earn_high + sleep_high +
                        T_nap*earn_high + T_nap*sleep_high,data = endline)

hetero_cognitive_4c <- lm(cognitive ~ T_nap + earn_high + sleep_high +
                            T_nap*earn_high + T_nap*sleep_high,data = endline)

hetero_typing_4c <- lm(typing_time_hr ~ T_nap + earn_high + sleep_high +
                         T_nap*earn_high + T_nap*sleep_high,data = endline)


stargazer(hetero_nap_4c,hetero_sleep_4c,hetero_happy_4c,hetero_cognitive_4c,
          hetero_typing_4c,type = "text")





#### 5) Atrición

# Se utiliza la variable drop_indicator

### a) Porcentajes de Pérdidas en Tratamiento (T_nap) y Control


atricion_5a_total <- endline %>% 
  group_by(drop_indicator) %>%
  summarize(obs = n()) %>% 
  mutate(porcentaje = (obs/sum(obs))*100) 

tabla_5a_total <-kable(atricion_5a_total,"simple")


atricion_5a <- endline %>% 
  filter(drop_indicator==1) %>% 
  group_by(T_nap) %>%
  summarize(obs = n()) %>% 
  mutate(porcentaje = (obs/sum(obs))*100) 
   
tabla_5a <-kable(atricion_5a,"simple")

stargazer(atricion_5a, type = "text")


### b) Evaluación de Validez Interna y Externa: Tablas de Balance


endline_5b <- endline %>% 
  filter(drop_indicator==0) %>% 
  mutate(age = age_,
         female = female_,
         education = education_,
         children = no_of_children_)
  

variables_5b<- data.frame(time_in_office = endline_5b$time_in_office, 
                       age=endline_5b$age, 
                       female=endline_5b$female,
                       education=endline_5b$education, 
                       sleep_night=endline_5b$sleep_night,
                       children=endline_5b$children, 
                       happy=endline_5b$happy,
                       health_bsl=endline_5b$health_bsl,
                       d1=endline_5b$d1,
                       absences=endline_5b$absences_baseline,
                       T_nap=endline_5b$T_nap )



tabla_5b<-balance_table(variables_5b,treatment = "T_nap")

stargazer(as.data.frame(tabla_5b),summary=FALSE, type = "text")


### c) Conclusión

## Como primer punto se esperaría que la atrición haya afectado de igual forma
## al grupo de tratamiento y al de control. Sin embargo, esto no es así.
## La perdida fue de 105 observaciones que representa un poco más del 25% de 
## la muestra. De estas 105 observaciones el 78% fue del grupo de control o que
## y casi el 22% se perdió del grupo de tratamiento.

## En el primer ejercicio, para cada una de las 10 variables se obtuvo un 
## p-value mayor al 10%. Es decir, la hipótesis nula no se rechaza que 
## significa que no hay diferencia entre las medias . Las covariables están 
## balanceadas, lo cual permite suponer que el tratamiento se asignó 
## de manera aleatoria.

## Posterior a la atrición, no todas las covariables éstán balanceadas.
## Las que pierden balance son el número de hijos(no_of_children) y 
## educación (education). Estas dos variables tienene un p-value menor al 5%.
## Esto significa que hay evidencia estadística para rechazar la hipótesis nula
## de que no hay diferencia en las medias.






#### 6) Lee Bounds


### a) Porcentajes de los perfiles AR, NR, SR y supuesto de monotonicidad.


## Supuesto de monotonicidad

## Bajo este escenario, el supuesto de monotonicidad significa que no existe el 
## perfil de individuos que se les asigna tratamiento (T_nap =1) y dejan el 
## experimento. Es decir, el supuesto de monotonicidad se aplicaría en que la 
## probabilidad de la existencia de Pr(T_nap = 1 & drop_indicator = 1) = 0.


## Tratamiento
total_rows_treat <- endline %>% filter(T_nap == 1) %>% nrow()
total_rows_ts <- endline %>% filter(drop_indicator == 0, T_nap==1) %>% nrow(.)



## Control
total_rows_control <- endline %>% filter(T_nap == 0) %>% nrow()
total_rows_cs <- endline %>% filter(drop_indicator == 0, T_nap==0) %>% nrow(.)


## Always Respondents

AR_share <- total_rows_cs/total_rows_control

## Selective Respondents

SR_share <- ((total_rows_ts/total_rows_treat)-(total_rows_cs/total_rows_control))/(total_rows_ts/total_rows_treat)

## Never Respondents

NR_share <- 1-(AR_share+SR_share)

## Tabla 

Perfil <- c("AR", "SR","NR")
Proporción <- c(AR_share,SR_share,NR_share)

tabla_6a <-cbind(Perfil,Proporción)

stargazer(tabla_6a, type = "text")  
  





### b) Cálculo de los Lee Bounds


## Se calcúla l

l <- total_rows_ts*(1 - SR_share)


## Se calcula la Cota Inferior

prod_desc <- sort(endline$productivity, decreasing = FALSE)

cota_inferior <- sum(prod_desc[1:l])/l


## Se calcula la Cota Superior

prod_ascen <- sort(endline$productivity, decreasing = TRUE)

cota_superior <- sum(prod_ascen[1:l])/l


## Media de la productividad del grupo control de los AR

prod_media_control <- endline %>% 
  filter(drop_indicator==0,T_nap==0) %>% 
  filter(!is.na(productivity)) %>%
  pull(productivity) %>% 
  mean()



## Lee Bounds Productividad

limite_superior = cota_superior-prod_media_control

limite_inferior = cota_inferior-prod_media_control

Lee_bounds <- c("límite inferior","límite superior","cota superior", "cota inferior", "control")
valor <- c(limite_inferior,limite_superior,cota_superior,cota_inferior,prod_media_control)

tabla_6b <- cbind(Lee_bounds,valor)
stargazer(tabla_6b,type = "text") 


## c) Interpretación y relación con el estimador de Neyman

## El valor calculado con el estimador de Neyman entra en este intervalo.
## El estimador de Neyman se calculó a partir de toda la muestra sin considerar
## la atrición y con la base original. Por lo tanto, el estimador de Neyman
## es una estimación del ATE en general considerando los 3 perfiles. Mientras
## que los Lee Bounds solo consideran a los AR. De esta forma, las dos 
## estimaciones son consistentes porque no se contradicen sino que el Neyman
## está dentro del intervalo.

## Por otro lado, el centro del intervalo no necesariamente debe estar centrado
## en el valor del estimador de Neyman o de alguna estimación hecha por un OLS
## porque el intervalo es para un perfil defiido que es el AR y no considera 
## el total de la muestra, es decir, a los demás perfiles. No es necesario que
## el centro del intervalo sea el Neyman pero podría estar y no sería incorrecto.










########## MATCHING


## 1) Coarsened Exact Match

# Utilizando variables Female y Education


CEM_endline <- endline %>% mutate(female=female_,education=education_) %>%
  filter(drop_indicator == 0)

match_CEM <- matchit(T_nap ~ female + education, data = CEM_endline, 
                     method = 'cem', estimand = 'ATE')

summary(match_CEM)

## 296 observaciones fueron emparejadas. De las 414 observaciones quedan 296,
## de las cuales 124 son de control y 172 de tratamiento.


## ATE


CEM_base <- match.data(match_CEM) 

ATE_CEM_t <- CEM_base %>% filter(T_nap == 1) %>%
  summarise(mean_cem = mean(productivity))

ATE_CEM_c <- CEM_base %>% filter(T_nap == 0) %>%
  summarise(mean_cem = mean(productivity))

ATE_CEM <- ATE_CEM_t-ATE_CEM_c

CEM_tabla <- c("media_tratamiento","media_control","ATE")
CEM_valores <- c(ATE_CEM_t,ATE_CEM_c,ATE_CEM)


stargazer(rbind(CEM_tabla,CEM_valores),type =  "text")





### 2) Nearest Neighbor, Mahalanobis


## TOT 1 Nearest Neighbor
NN_baseline <- baseline %>% mutate(female=female_,education=education_) %>%
  filter(drop_indicator == 0)

match_NN1 <- matchit(T_nap ~ female + age + sleep_report,method = 'nearest',
                      estimand = 'ATT',distance = 'mahalanobis',
                      ratio = 1, replace = TRUE, data = NN_baseline )

TOT_NN1_t <- match.data(match_NN1) %>% filter(T_nap == 1) %>%
  summarise(mean_NN1 = mean(productivity))

TOT_NN1_c <- match.data(match_NN1) %>% filter(T_nap == 0) %>%
  summarise(mean_NN1 = mean(productivity))

TOT_NN1 <- TOT_NN1_t-TOT_NN1_c

TOT_NN1_tabla <- c("media_tratamiento","media_control","TOT")
TOT_NN1_valores <- c(TOT_NN1_t,TOT_NN1_c,TOT_NN1)


stargazer(rbind(TOT_NN1_tabla,TOT_NN1_valores),type =  "text")



## TOT 5 Nearest Neighbor

match_NN5 <- matchit(T_nap ~ female + age + sleep_report,method = 'nearest',
                     estimand = 'ATT',distance = 'mahalanobis',
                     ratio = 5, replace = TRUE, data = NN_baseline )

TOT_NN5_t <- match.data(match_NN5) %>% filter(T_nap == 1) %>%
  summarise(mean_NN5 = mean(productivity))

TOT_NN5_c <- match.data(match_NN5) %>% filter(T_nap == 0) %>%
  summarise(mean_NN5 = mean(productivity))

TOT_NN5 <- TOT_NN5_t-TOT_NN5_c

TOT_NN5_tabla <- c("media_tratamiento","media_control","TOT")
TOT_NN5_valores <- c(TOT_NN5_t,TOT_NN5_c,TOT_NN5)


stargazer(rbind(TOT_NN5_tabla,TOT_NN5_valores),type =  "text")



## TOT 10 Nearest Neighbor

match_NN10 <- matchit(T_nap ~ female + age + sleep_report,method = 'nearest',
                     estimand = 'ATT',distance = 'mahalanobis',
                     ratio = 10, replace = TRUE, data = NN_baseline )

TOT_NN10_t <- match.data(match_NN10) %>% filter(T_nap == 1) %>%
  summarise(mean_NN10 = mean(productivity))

TOT_NN10_c <- match.data(match_NN10) %>% filter(T_nap == 0) %>%
  summarise(mean_NN10 = mean(productivity))

TOT_NN10 <- TOT_NN10_t-TOT_NN10_c

TOT_NN10_tabla <- c("media_tratamiento","media_control","TOT")
TOT_NN10_valores <- c(TOT_NN10_t,TOT_NN10_c,TOT_NN10)


stargazer(rbind(TOT_NN10_tabla,TOT_NN10_valores),type =  "text")





### 3) Propensity Score Matching


## a) PSM con diferentes estimaciones

PSM_endline <- endline %>%
  filter(drop_indicator == 0) %>% mutate(age = age_,
                                     female = female_,
                                     education = education_,
                                     children = no_of_children_,
                                     savings = daily_savings) %>%
  select(T_nap,age,female,education,children,time_in_office,
         sleep_night,happy,health_bsl,d1,savings,nap_time_mins,
         productivity,sleep_report,cognitive,typing_time_hr)



psm_mpl <- lm(formula = T_nap ~ age + female + education + children +
                time_in_office, data = PSM_endline)

psm_probit <-glm(formula = T_nap ~ age + female + education + children +
                   time_in_office,family = binomial(link = "probit") ,data = PSM_endline)

psm_logit <-glm(formula = T_nap ~ age + female + education + children +
                   time_in_office,family = binomial(link = "logit") ,data = PSM_endline)

stargazer(psm_mpl,psm_probit,psm_logit,type = "text")


individuo_prom <- PSM_endline %>% summarize_all(funs(mean(., na.rm = T)))

ind_prom <- c( "Predicción PSM")
psm_mpl_pred <- predict(psm_mpl,individuo_prom)
psm_probit_pred <- pnorm(predict(psm_probit,individuo_prom))
psm_logit_pred <- exp(predict(psm_logit,individuo_prom))/(1+exp(predict(psm_logit,individuo_prom)))

stargazer(cbind(ind_prom,psm_mpl_pred,psm_probit_pred,psm_logit_pred),type = "text")



### b) Tablas de reporte con el PSM

# Observaciones

n_tratamiento <- PSM_endline %>% filter(T_nap == 1) %>% nrow()
n_control <- PSM_endline %>% filter(T_nap == 0) %>% nrow()
n <- PSM_endline %>% nrow()

## Modelo de Probabilidad lineal

est_psm_mpl_mean <- PSM_endline %>%
  mutate(pred_mpl = predict(psm_mpl,type = "response")) %>% 
  select(pred_mpl) %>% summarise_all(funs(mean(., na.rm = T)))

est_psm_mpl_var <- PSM_endline %>% 
  mutate(pred_mpl = predict(psm_mpl,type = "response")) %>%
  select(pred_mpl) %>% var()

pond_mpl_t <- (est_psm_mpl_mean/(1-est_psm_mpl_mean))*
  (n_control/n_tratamiento)*(1/n_control)

pond_mpl_c <- ((1-est_psm_mpl_mean)/est_psm_mpl_mean)*
  (n_tratamiento/n_control)*(1/n_tratamiento)

Modelo <- c("Media","varianza","Reponderador T","Reponderador C")
MPL <- c(est_psm_mpl_mean,est_psm_mpl_var,pond_mpl_t,pond_mpl_t)
tabla_mpl <-stargazer(cbind(Modelo,MPL),type = "text")


## Modelo Probit

est_psm_pro_mean <- PSM_endline %>% 
  mutate(pred_pro = pnorm(predict(psm_probit,type = "response"))) %>%
  select(pred_pro) %>% summarise_all(funs(mean(., na.rm = T)))

est_psm_pro_var <- PSM_endline %>%
  mutate(pred_pro = pnorm(predict(psm_probit,type = "response"))) %>%
  select(pred_pro) %>% var()

pond_pro_t <- (est_psm_pro_mean/(1-est_psm_pro_mean))*
  (n_control/n_tratamiento)*(1/n_control)

pond_pro_c <- ((1-est_psm_pro_mean)/est_psm_pro_mean)*
  (n_tratamiento/n_control)*(1/n_tratamiento)

Modelo <- c("Media","varianza","Reponderador T","Reponderador C")
Probit <- c(est_psm_pro_mean,est_psm_pro_var,pond_pro_t,pond_pro_t)
tabla_pro <- stargazer(cbind(Modelo,Probit),type = "text")



## Modelo Logit

est_psm_log_mean <- PSM_endline %>% 
  mutate(pred_log = exp(predict(psm_logit,type = "response"))/
           (1+exp(predict(psm_logit,type = "response")))) %>%
  select(pred_log) %>% summarise_all(funs(mean(., na.rm = T)))

est_psm_log_var <- PSM_endline %>%
  mutate(pred_log = exp(predict(psm_logit,type = "response"))/
           (1+exp(predict(psm_logit,type = "response")))) %>%
  select(pred_log) %>% var()

pond_log_t <- (est_psm_log_mean/(1-est_psm_log_mean))*
  (n_control/n_tratamiento)*(1/n_control)

pond_log_c <- ((1-est_psm_log_mean)/est_psm_log_mean)*
  (n_tratamiento/n_control)*(1/n_tratamiento)

Modelo <- c("Media","varianza","Reponderador T","Reponderador C")
Logit <- c(est_psm_log_mean,est_psm_log_var,pond_log_t,pond_log_t)
tabla_log <- stargazer(cbind(Modelo,Logit), type = "text")


### c) Probit y Tabla de Balance

variable <- c('age_p','female_P','education_P','children_P','office_P',
              'sleep_P','happy_P','health_P','d1_P','savings_P')

endline_probit <- PSM_endline %>% 
  mutate(pred_pro = pnorm(predict(psm_probit,type = "response")),
         pond_pro_t = (pred_pro/(1-pred_pro))*
           (n_control/n_tratamiento)*(1/n_control),
         pond_pro_c = ((1-pred_pro)/pred_pro)*
           (n_tratamiento/n_control)*(1/n_tratamiento))


endline_probit <- endline_probit %>%
  mutate(age_P = ifelse(T_nap==1,pond_pro_t*age,pond_pro_c*age),
         female_P = ifelse(T_nap==1,pond_pro_t*female,pond_pro_c*female),
         education_P = ifelse(T_nap==1,pond_pro_t*education,pond_pro_c*education),
         children_P = ifelse(T_nap==1,pond_pro_t*children,pond_pro_c*children),
         office_P = ifelse(T_nap==1,pond_pro_t*time_in_office,pond_pro_c*time_in_office),
         sleep_P = ifelse(T_nap==1,pond_pro_t*sleep_night,pond_pro_c*sleep_night),
         happy_P = ifelse(T_nap==1,pond_pro_t*happy,pond_pro_c*happy),
         health_P = ifelse(T_nap==1,pond_pro_t*health_bsl,pond_pro_c*health_bsl),
         d1_P = ifelse(T_nap==1,pond_pro_t*d1,pond_pro_c*d1),
         savings_P = ifelse(T_nap==1,pond_pro_t*savings,pond_pro_c*savings))


pro_age_t <- sum(endline_probit$age_P[endline_probit$T_nap==1])
pro_age_c <- sum(endline_probit$age_P[endline_probit$T_nap==0])
pro_age_pv <-  t.test(
  x = endline_probit$age_P[endline_probit$T_nap==1],
  y = endline_probit$age_P[endline_probit$T_nap==0],
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)


pro_fem_t <- sum(endline_probit$female_P[endline_probit$T_nap==1])
pro_fem_c <- sum(endline_probit$female_P[endline_probit$T_nap==0])
pro_fem_pv <-  t.test(
  x = endline_probit$female_P[endline_probit$T_nap==1],
  y = endline_probit$female_P[endline_probit$T_nap==0],
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)


pro_edu_t <- sum(endline_probit$education_P[endline_probit$T_nap==1])
pro_edu_c <- sum(endline_probit$education_P[endline_probit$T_nap==0])
pro_edu_pv <-  t.test(
  x = endline_probit$education_P[endline_probit$T_nap==1],
  y = endline_probit$education_P[endline_probit$T_nap==0],
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)


pro_chil_t <- sum(endline_probit$children_P[endline_probit$T_nap==1])
pro_chil_c <- sum(endline_probit$children_P[endline_probit$T_nap==0])
pro_chil_pv <-  t.test(
  x = endline_probit$children_P[endline_probit$T_nap==1],
  y = endline_probit$children_P[endline_probit$T_nap==0],
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)


pro_off_t <- sum(endline_probit$office_P[endline_probit$T_nap==1])
pro_off_c <- sum(endline_probit$office_P[endline_probit$T_nap==0])
pro_off_pv <-  t.test(
  x = endline_probit$office_P[endline_probit$T_nap==1],
  y = endline_probit$office_P[endline_probit$T_nap==0],
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)


pro_slp_t <- sum(endline_probit$sleep_P[endline_probit$T_nap==1],na.rm = TRUE)
pro_slp_c <- sum(endline_probit$sleep_P[endline_probit$T_nap==0],na.rm = TRUE)
pro_slp_pv <-  t.test(
  x = endline_probit$sleep_P[endline_probit$T_nap==1],
  y = endline_probit$sleep_P[endline_probit$T_nap==0],
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)



pro_hpy_t <- sum(endline_probit$happy_P[endline_probit$T_nap==1])
pro_hpy_c <- sum(endline_probit$happy_P[endline_probit$T_nap==0])
pro_hpy_pv <-  t.test(
  x = endline_probit$happy_P[endline_probit$T_nap==1],
  y = endline_probit$happy_P[endline_probit$T_nap==0],
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)



pro_hel_t <- sum(endline_probit$health_P[endline_probit$T_nap==1])
pro_hel_c <- sum(endline_probit$health_P[endline_probit$T_nap==0])
pro_hel_pv <-  t.test(
  x = endline_probit$health_P[endline_probit$T_nap==1],
  y = endline_probit$health_P[endline_probit$T_nap==0],
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)


pro_d1_t <- sum(endline_probit$d1_P[endline_probit$T_nap==1])
pro_d1_c <- sum(endline_probit$d1_P[endline_probit$T_nap==0])
pro_d1_pv <-  t.test(
  x = endline_probit$d1_P[endline_probit$T_nap==1],
  y = endline_probit$d1_P[endline_probit$T_nap==0],
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)


pro_sav_t <- sum(endline_probit$savings_P[endline_probit$T_nap==1])
pro_sav_c <- sum(endline_probit$savings_P[endline_probit$T_nap==0])
pro_sav_pv <-  t.test(
  x = endline_probit$savings_P[endline_probit$T_nap==1],
  y = endline_probit$savings_P[endline_probit$T_nap==0],
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)


Var <- c("mean_t","mean_c","p_value")
age <- c(pro_age_t,pro_age_c,pro_age_pv$p.value)
female <- c(pro_fem_t,pro_fem_c,pro_fem_pv$p.value)
education <- c(pro_edu_t,pro_edu_c,pro_edu_pv$p.value)
children <- c(pro_chil_t,pro_chil_c,pro_chil_pv$p.value)
office <- c(pro_off_t,pro_off_c,pro_off_pv$p.value)
sleep_night <-c(pro_slp_t,pro_slp_c,pro_slp_pv$p.value)
happy <-c(pro_hpy_t,pro_hpy_c,pro_hpy_pv$p.value)
health <-c(pro_hel_t,pro_hel_c,pro_hel_pv$p.value)
d1 <-c(pro_d1_t,pro_d1_c,pro_d1_pv$p.value)
savings <-c(pro_sav_t,pro_sav_c,pro_sav_pv$p.value)

tabla_probitc <-rbind(Var,age,female,education,children,office,
                      sleep_night,happy,health,d1,savings)


stargazer(tabla_probitc,type = "text")


### d) TOT de variables

endline_probit <- endline_probit %>%
  mutate(productivity_P = ifelse(T_nap==1,pond_pro_t*productivity,pond_pro_c*productivity),
         nap_time_P = ifelse(T_nap==1,pond_pro_t*nap_time_mins,pond_pro_c*nap_time_mins),
         sleep_report_P = ifelse(T_nap==1,pond_pro_t*sleep_report,pond_pro_c*sleep_report),
         happy_P = ifelse(T_nap==1,pond_pro_t*happy,pond_pro_c*happy),
         cognitive_P = ifelse(T_nap==1,pond_pro_t*cognitive,pond_pro_c*cognitive),
         typing_P = ifelse(T_nap==1,pond_pro_t*typing_time_hr,pond_pro_c*typing_time_hr))


## productivity

pro_prod_t <- sum(endline_probit$productivity_P[endline_probit$T_nap==1])
pro_prod_c <- sum(endline_probit$productivity_P[endline_probit$T_nap==0])
TOT_productivity <- pro_prod_t-pro_prod_c
  

## nap_time_mins

pro_nap_t <- sum(endline_probit$nap_time_P[endline_probit$T_nap==1])
pro_nap_c <- sum(endline_probit$nap_time_P[endline_probit$T_nap==0])
TOT_nap_time <- pro_nap_t - pro_nap_c


## sleep_report

pro_slp_rep_t <- sum(endline_probit$sleep_report_P[endline_probit$T_nap==1],na.rm = TRUE)
pro_slp_rep_c <- sum(endline_probit$sleep_report_P[endline_probit$T_nap==0],na.rm = TRUE)
TOT_sleep_report <- pro_slp_rep_t - pro_slp_rep_c


## happy

pro_hpy_t <- sum(endline_probit$happy_P[endline_probit$T_nap==1],na.rm = TRUE)
pro_hpy_c <- sum(endline_probit$happy_P[endline_probit$T_nap==0],na.rm = TRUE)
TOT_happy <- pro_hpy_t - pro_hpy_c


## cognitive

pro_cog_t <- sum(endline_probit$cognitive_P[endline_probit$T_nap==1],na.rm = TRUE)
pro_cog_c <- sum(endline_probit$cognitive_P[endline_probit$T_nap==0],na.rm = TRUE)
TOT_cognitive <- pro_cog_t - pro_cog_c


## typing _time_hr

pro_typ_t <- sum(endline_probit$typing_P[endline_probit$T_nap==1],na.rm = TRUE)
pro_typ_c <- sum(endline_probit$typing_P[endline_probit$T_nap==0],na.rm = TRUE)
TOT_typing <- pro_typ_t - pro_typ_c


Vars <- c("mean_t","mean_c","TOT")
productivity_TOT <- c(pro_prod_t,pro_prod_c,TOT_productivity)
nap_time_mins_TOT <- c(pro_nap_t,pro_nap_c,TOT_nap_time)
sleep_report_TOT <- c(pro_slp_rep_t,pro_slp_rep_c,TOT_sleep_report)
happy_TOT <-c(pro_hpy_t,pro_hpy_c,TOT_happy)
cognitive_TOT <-c(pro_cog_t,pro_cog_c,TOT_cognitive)
typing_TOT <-c(pro_typ_t,pro_typ_c,TOT_typing)


tabla_3c <-rbind(Vars,productivity_TOT,nap_time_mins_TOT,sleep_report_TOT,happy_TOT,cognitive_TOT,
                 typing_TOT)


stargazer(tabla_3c,type = "text")






