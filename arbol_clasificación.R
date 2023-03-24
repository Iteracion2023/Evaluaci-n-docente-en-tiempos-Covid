require(tidyverse)
require(tidymodels)
require(Metrics)
require(rpart.plot)
require(rattle)
require(rpart)
require(baguette)

base <- read.csv2('Evaluacion_docente_2017-2018-2019-2020-2021.csv')

base2 <- base %>% 
  select(COD_DEPE, RURAL_RBD, DOC_GENERO,INSTR_ESC)

base2 <- subset(base2, INSTR_ESC != "N/A") #eliminamos los registros que tienen N/A en INSTR_ESC
base2 <- subset(base2, INSTR_ESC != "-1")  #eliminamos los registros que tienen -1 en INSTR_ESC

# Transformamos las variables a factor:

base3 <- base2 %>% 
  mutate(COD_DEPE = factor(case_when(COD_DEPE == 1 ~ "Municipal DAEM",
                                     COD_DEPE == 2 ~ "Municipal",
                                     COD_DEPE == 5 ~ "Corp. Admin. Deleg.",
                                     COD_DEPE == 6 ~ "Serv. Local Educación")),
         RURAL_RBD = factor(ifelse(RURAL_RBD == 0, "Urbano", "Rural")),
         DOC_GENERO = factor(ifelse(DOC_GENERO == 1, "Hombre", "Mujer")),
         INSTR_ESC = factor(case_when(INSTR_ESC == "I" ~ "Insatisfactorio",
                                      INSTR_ESC == "B" ~ "Básico",
                                      INSTR_ESC == "C" ~ "Competente",
                                      INSTR_ESC == "D" ~ "Destacado", TRUE ~ "Sin escala")))

base3 <- base3[base3$INSTR_ESC != "Sin escala",] #eliminamos los registros que tienen "Sin escala"
# Verificamos que las 4 variables sean factor:
glimpse(base3)

arbol1 <- rpart(INSTR_ESC ~ ., data = base3, method = "class")
print(arbol1)

rpart.plot(arbol1, type = 3, extra = 106)

write.csv(base3, file = "Evaluaciones_escalas.csv", row.names = FALSE)



