
#IMPORTAR set de datos original
setwd("C:/Users/BastianBea/Desktop/Data Science/ownProyects/DB odonto/datasets")
Convencional <- read.csv("Convencional_original.csv")

#RESUMEN del set de datos
head(Convencional)
tail(Convencional)
str(Convencional)
summary(Convencional)
View(Convencional)

# RECODIFICAR VARIABLES
#    renombrar columnas
colnames(Convencional) <- c("Genero", "Edad", "Año", "Grupo", "Eichner", "GI", "GF", "Adh1", "Adh2")

#   dummys
Convencional$Adh2[Convencional$Adh2=="SI"] <- "Si"
Convencional$Adh2 <- factor(Convencional$Adh2)

#   rec GI & GF en nuevas variables _rec
library(car)

Convencional$GI_rec <- Convencional$GI
Convencional$GI_rec<-recode(Convencional$GI_rec,"lo:50 ='Bajo'; 51:56 ='Moderado'; 57:60 ='Alto'")

Convencional$GF_rec <- Convencional$GF
Convencional$GF_rec<-recode(Convencional$GF_rec,"lo:50 ='Bajo'; 51:56 ='Moderado'; 57:60 ='Alto'")

#   convertir Año, Genero, Eichner, GI_rec, GF_rec to factor
Convencional$Año <- as.factor(Convencional$Año)
Convencional$Genero <- as.factor(Convencional$Genero)
Convencional$Eichner <- as.factor(Convencional$Eichner)
Convencional$GI_rec <- as.factor(Convencional$GI_rec)
Convencional$GF_rec <- as.factor(Convencional$GF_rec)

# GUARDAR dataframe depurado
# library(foreign)

#Exportar datos a STATA
# write.dta(Convencional, "C:/Users/BastianBea/Desktop/DB odonto/datasets/convencional_rec.dta")

#Exportar datos a CSV
write.csv(Convencional, file = "Convencional_rec.csv")

# Guardar datos a RData
save.image(file = "Convencional_rec.RData")

