#Datos hidrologicos trabajo exporativo

inp <- read.csv("FDC.csv")
head(inp)

dim(inp)

inp[!complete.cases(inp),]

#newinp <- na.omit(inp)
#en este caso observamos el río Estrella (segunda columna)y el río Banano (tercera columna)y se nos muestra la visualización de las dos series de tiempo de caudal.

plot(
  inp[,2],
  type= "l", col = "#FF6600",
  xlab = ("Fecha"),
  ylab = ("Caudal milimetros por día"), 
  main = ("Grafico de series de  visualización de las dos series de tiempo de caudal.
")
)
lines(inp[ ,3], col = "#66FFFF")

#Promedio de caudales diarios.
summary(inp[,2:3])
#El histograma nos permite observar como se distribuyen los datos del mínimo al máximo.
hist(inp[,2], 
     main =("histograma del río Estrella"),
     xlab = ("Agua por día"),
     ylab = ("Caudal"),
     col = "#99CCFF"
     )
hist(inp[,3],
     main =("histograma del río Banano"),
     xlab = ("Agua por día"),
     ylab = ("Caudal"),
     col = "blue"
     )

names(inp) <- c("fecha", "Estrella", "Banano")

attach(inp)
plot(inp[,2])

Tempdate <- strptime(inp[,1], format = "%d/%m/%Y")

#Series de tiempo a promedios anuales

MAQ_Estrella <- tapply(inp[,2], format(Tempdate, format="%Y"), FUN=sum)

MAQ_Banano <- tapply(inp[,3], format(Tempdate, format="%Y"), FUN=sum)


write.csv(rbind(MAQ_Estrella,MAQ_Banano), file="MAQ.csv")

plot(MAQ_Banano, ylim=c(100,3000), main = "caudal por año (y) y mes (m)",
     ylab = "años", xlab ="meses" )

lines(MAQ_Estrella, col=2)

MMQ_Estrella <- tapply(inp[,2], format(Tempdate, format="%m"), FUN=sum)

MMQ_Banano <- tapply(inp[,3], format(Tempdate, format="%m"), FUN=sum)

#Analisis de correlación

corinp <- cor(inp[,2:3], method= "spearman")

plot(inp[,2], inp[,3],main = "Relación de caudales",ylab = "Río Banano",
     xlab = "Río Estrella")

inp.lm <- lm(inp[,2] ~ inp[,3], data=inp)

summary(inp.lm)

plot(inp.lm)