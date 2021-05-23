inp <- read.csv("FDC.csv")
head(inp)

dim(inp)

inp[!complete.cases(inp),]

#newinp <- na.omit(inp)

plot(inp[,2],type = "l", col= "blue")
lines(inp[,3],col="green")

summary(inp[,2:3])

hist(inp[,2])
hist(inp[,3])

names(inp) <- c("fecha", "Estrella", "Banano")

attach(inp)
plot(inp[,2])

Tempdate <- strptime(inp[,1], format = "%d/%m/%Y")

MAQ_Estrella <- tapply(inp[,2], format(Tempdate, format="%Y"), FUN=sum)

MAQ_Banano <- tapply(inp[,3], format(Tempdate, format="%Y"), FUN=sum)


write.csv(rbind(MAQ_Estrella,MAQ_Banano), file="MAQ.csv")

plot(inp[,2], ylim=c(100,3000))
lines(inp[,3], col=2)

MAQ_Estrella <- tapply(inp[,2], format(Tempdate, format="%m"), FUN=sum)

MAQ_Banano <- tapply(inp[,3], format(Tempdate, format="%m"), FUN=sum)

#Analisis de correlación

corinp <- cor(inp[,2:3], method= "spearman")

plot(inp[,2], inp[,3])

inp.lm <- lm(inp[,2] ~ inp[,3], data=inp)

summary(inp.lm)

plot(inp.lm)

