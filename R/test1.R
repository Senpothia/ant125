TAB<-getMeasures("ant046")
COEFS<-list()
La125kHz<-c()

N40<-TAB[TAB$N==40,]
m <- lm(N40$L~N40$F+I(N40$F^2), data=N40)
COEFS[[1]]<-coef(m)
est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
La125kHz[1]<-est(125)
plot(N40$F, N40$L)
curve(est, 10, 100, col="red", add=TRUE)

N50<-TAB[TAB$N==50,]
m <- lm(N50$L~N50$F+I(N50$F^2), data=N50)
COEFS[[2]]<-coef(m)
est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
La125kHz[2]<-est(125)
plot(N50$F, N50$L)
curve(est, 10, 100, col="red", add=TRUE)


N60<-TAB[TAB$N==70,]
m <- lm(N60$L~N60$F+I(N60$F^2), data=N60)
COEFS[[3]]<-coef(m)
La125kHz[3]<-est(125)
est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
plot(N60$F, N60$L)
curve(est, 10, 100, col="red", add=TRUE)


N70<-TAB[TAB$N==70,]
m <- lm(N70$L~N70$F+I(N70$F^2), data=N70)
COEFS[[4]]<-coef(m)
est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
La125kHz[4]<-est(125)
plot(N70$F, N70$L)
curve(est, 10, 100, col="red", add=TRUE)


N80<-TAB[TAB$N==80,]
m <- lm(N80$L~N80$F+I(N80$F^2), data=N80)
COEFS[[5]]<-coef(m)
est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
La125kHz[5]<-est(125)
plot(N80$F, N80$L)
curve(est, 10, 100, col="red", add=TRUE)


N90<-TAB[TAB$N==90,]
m <- lm(N90$L~N90$F+I(N90$F^2), data=N90)
COEFS[[6]]<-coef(m)
est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
La125kHz[6]<-est(125)
plot(N90$F, N90$L)
curve(est, 10, 100, col="red", add=TRUE)

N100<-TAB[TAB$N==100,]
m <- lm(N100$L~N100$F+I(N100$F^2), data=N100)
COEFS[[7]]<-coef(m)
est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
La125kHz[7]<-est(125)
plot(N100$F, N100$L)
curve(est, 10, 100, col="red", add=TRUE)


N110<-TAB[TAB$N==110,]
m <- lm(N110$L~N110$F+I(N110$F^2), data=N110)
COEFS[[8]]<-coef(m)
est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
La125kHz[8]<-est(125)
plot(N110$F, N110$L)
curve(est, 10, 100, col="red", add=TRUE)

N120<-TAB[TAB$N==120,]
m <- lm(N120$L~N120$F+I(N120$F^2), data=N120)
COEFS[[9]]<-coef(m)
est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
La125kHz[9]<-est(125)
plot(N120$F, N120$L)
curve(est, 10, 100, col="red", add=TRUE)

MAT <- matrix(unlist(COEFS), ncol = 3, byrow = TRUE)

TOURS<-env$turns
D<-data.frame(TOURS, La125kHz)
m <- lm(La125kHz~D$TOURS+I(D$TOURS^2), data=D)
est<-function(F){m$coefficients[1] + m$coefficients[2]*F + m$coefficients[3]*F^2}
plot(D$TOURS, D$La125kHz)
curve(est, 10, 120, col="red", add=TRUE)



