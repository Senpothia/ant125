TAB<-getMeasures("ant046")
COEFSL<-extModels(TAB, 125, TRUE, FALSE)
COEFSR<-extModels(TAB, 125, FALSE, FALSE)
R<-function(N){COEFSR[1] + COEFSR[2]*N + COEFSR[3]*N^2}
Ra<-R(env$turns)
In<-Iant(Ra)
plot(env$turns,In, type = "l", col="blue")
