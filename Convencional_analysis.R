# data set
setwd("C:/Users/BastianBea/Desktop/Data Science/Proyects/DB odonto/datasets Convensional")
Convencional <- read.csv("Convencional_rec.csv")
load("Convencional_rec.RData")

# View data and factor some vars
str(Convencional)
View(Convencional)

Convencional$Eichner <- as.factor(Convencional$Eichner)
Convencional$Genero <- as.factor(Convencional$Genero)
Convencional$Año <- as.factor(Convencional$Año)
summary(Convencional)

# ANALISIS Y GRÁFICOS

# Distribucion por sexo y Edad promedio 
summary(Convencional$Genero)
mean(Convencional$Edad)
# round(mean(Convencional$Edad[Convencional$Genero == "0"]), 2)
# round(mean(Convencional$Edad[Convencional$Genero == "1"]), 2)
tapply(Convencional$Edad, Convencional$Genero, mean)

# Crosstables and graphics
library(descr)
library(ggplot2)
library(extrafont)

# Crosstable Adherencia2 vs Grupo
CrossTable(Convencional$Adh2, Convencional$Grupo, 
           prop.r = FALSE, prop.c =TRUE, prop.t = FALSE, 
           prop.chisq = FALSE, chisq = TRUE, format = "SPSS", 
           expected = FALSE)

# Crosstable Grupo vs sexo
CrossTable(Convencional$Genero, Convencional$Grupo, 
           prop.r = FALSE, prop.c =TRUE, prop.t = FALSE, 
           prop.chisq = FALSE, chisq = TRUE, format = "SPSS", 
           expected = FALSE)

# Crosstable Eichner vs Grupo
CrossTable(Convencional$Eichner, Convencional$Grupo, 
           prop.r = FALSE, prop.c =TRUE, prop.t = FALSE, 
           prop.chisq = FALSE, chisq = TRUE, format = "SPSS", 
           expected = FALSE)

# Crosstable GF_rec vs Grupo
CrossTable(Convencional$GF_rec, Convencional$Grupo, 
           prop.r = FALSE, prop.c =TRUE, prop.t = FALSE, 
           prop.chisq = FALSE, chisq = TRUE, format = "SPSS", 
           expected = FALSE)

# BARRAS Grupo*GF_rec
grupo_GFrec <- ggplot(data= Convencional, aes(x = GF_rec, colour = GF_rec, fill=GF_rec)) + 
  labs(title= "Grupos A y B según Gohai Final recodificado", x="Índice de Gohai Final", y="N° pacientes")

grupo_GFrec + facet_grid(.~Grupo, scales="free") + geom_bar(position = "dodge") +
  theme(plot.title = element_text(colour="DarkBlue", size=20, family="Ubuntu" ),
        legend.position = "none",
        axis.title.x = element_text(size=15, family = "Ubuntu" ),
        axis.title.y= element_text(size=15, family = "Ubuntu" ))

# BOXPLOT GF*Grupo 
p_base <- ggplot(data = Convencional, aes(x = Grupo , y = GF, colour = Grupo)) + 
  labs(title= "Grupos A y B según Gohai Final", x="Grupos", y="Índice de Gohai Final")

p_base + geom_boxplot(size=1.2) +
  theme(plot.title = element_text(colour="DarkBlue", size=20, family="Ubuntu" ),
        legend.position = "none",
        axis.title.x = element_text(size=15, family = "Ubuntu" ),
        axis.title.y= element_text(size=15, family = "Ubuntu" ))

# BOXPLOT GF*Grupo con jitter
p_base + geom_jitter() + geom_boxplot(size=1.2, alpha=0.5) +
  theme(plot.title = element_text(colour="DarkBlue", size=20, family="Ubuntu" ),
        legend.position = "none",
        axis.title.x = element_text(size=15, family = "Ubuntu" ),
        axis.title.y= element_text(size=15, family = "Ubuntu" ))

# Crosstable GI_rec vs Grupo
CrossTable(Convencional$GI_rec, Convencional$Grupo, 
           prop.r = FALSE, prop.c =TRUE, prop.t = FALSE, 
           prop.chisq = FALSE, chisq = TRUE, format = "SPSS", 
           expected = FALSE)

# BARRAS Grupo*GI_rec
grupo_GIrec <- ggplot(data= Convencional, aes(x = GI_rec, colour = GI_rec, fill=GI_rec)) + 
  labs(title= "Grupos A y B según Gohai Inicial recodificado", x="Índice de Gohai Inicial", y="N° pacientes")

grupo_GIrec + facet_grid(.~Grupo, scales="free") + geom_bar(position = "dodge") +
  theme(plot.title = element_text(colour="DarkBlue", size=20, family="Ubuntu" ),
        legend.position = "none",
        axis.title.x = element_text(size=15, family = "Ubuntu" ),
        axis.title.y= element_text(size=15, family = "Ubuntu" ))

# BOXPLOT GI*Grupo 
p_base_gi <- ggplot(data = Convencional, aes(x = Grupo , y = GI, colour = Grupo)) + 
  labs(title= "Grupos A y B según Gohai Inicial", x="Grupos", y="Índice de Gohai Inicial")

p_base_gi + geom_boxplot(size=1.2) +
  theme(plot.title = element_text(colour="DarkBlue", size=20, family="Ubuntu" ),
        legend.position = "none",
        axis.title.x = element_text(size=15, family = "Ubuntu" ),
        axis.title.y= element_text(size=15, family = "Ubuntu" ))

# BOXPLOT GI*Grupo con jitter
p_base_gi + geom_jitter() + geom_boxplot(size=1.2, alpha=0.5) +
  theme(plot.title = element_text(colour="DarkBlue", size=20, family="Ubuntu" ),
        legend.position = "none",
        axis.title.x = element_text(size=15, family = "Ubuntu" ),
        axis.title.y= element_text(size=15, family = "Ubuntu" ))

# Crosstable Eichner vs Adherencia
CrossTable(Convencional$Eichner, Convencional$Adh2, 
           prop.r = FALSE, prop.c =TRUE, prop.t = FALSE, 
           prop.chisq = FALSE, chisq = TRUE, format = "SPSS", 
           expected = FALSE)

# Plot Eichner vs Adherencia
plot_eichner_adh2 <- ggplot(data= Convencional, aes(x = Eichner, fill= Adh2))
plot_eichner_adh2 + geom_bar() +
  labs(title= "Eichner según Adherencia(2)", 
       x="índice de Eichner", y="N° pacientes", fill="Adherencia 2") +
  theme(plot.title = element_text(colour="DarkBlue", size=20, family="Ubuntu" ),
        axis.title.x = element_text(size=15, family = "Ubuntu" ),
        axis.title.y= element_text(size=15, family = "Ubuntu" ))

#OTROS GRÁFICOS
# Edad - GF - Eichner - Género
gf_edad <- ggplot(data = Convencional, aes(x = Edad, y = GF, color = Genero, size = Eichner))
gf_edad + geom_point()

# Edad - GF - Grupo - Eichner
gf_grupo <- ggplot(data = Convencional, aes(x = Edad, y = GF, color = Grupo, size = Eichner))
gf_grupo + geom_point()
