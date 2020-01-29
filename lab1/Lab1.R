EPI_data <- read.csv(file.choose(), skip = 1, header = TRUE)
EPI_data
attach(EPI_data)
fix(EPI_data)
?fix
###########################################
#exercise for the EPI column for in the EPI_data
EPI
filtEPI <- is.na(EPI)
filtEPI
E <- EPI[!filtEPI]
E
summary(E)
fivenum((E))
stem(E)
hist(E)
#probility histogram
hist(E, seq(30., 95., 1.0), prob=TRUE)
#normal distribute lines
lines(density(E, na.rm = TRUE, bw=1.))
lines(density(E, na.rm = TRUE, bw="SJ"))
# should the rug on the plot
rug(E)

#cumulative density function
plot(ecdf(E), do.points=FALSE, verticals=TRUE)
#quantile-quantile
par(pty="s")
qqnorm(E);qqline(E)
#qq plot
x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for t dsn")
qqline(x)
###############################################
#exercise for the DALY column in the EPI_data
attach(EPI_data)
DALY
is.na(DALY)
filtDALY <- is.na(DALY)
filtDALY
daily <- DALY[!filtDALY]
daily
summary(daily)
fivenum((daily))
stem(daily)
hist(daily)
#probility histogram
hist(daily, seq(30., 95., 1.0), prob=TRUE)
#normal distribute lines
lines(density(daily, na.rm = TRUE, bw=1.))
lines(density(daily, na.rm = TRUE, bw="SJ"))
# should the rug on the plot
rug(daily)

#cumulative density function
plot(ecdf(daily), do.points=FALSE, verticals=TRUE)
#quantile-quantile
par(pty="s")
qqnorm(daily);qqline(daily)
#qq plot
x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for t dsn")
qqline(x)

#box plot and qqplot between two
boxplot(daily, E)
qqplot(daily, E)

########################################
#exercise 2
EPILand <- EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)
summary(Eland)
fivenum((Eland))
stem(Eland)
hist(Eland)
#probility histogram
hist(Eland, seq(30., 95., 1.0), prob=TRUE)
#normal distribute lines
lines(density(Eland, na.rm = TRUE, bw=1.))
lines(density(Eland, na.rm = TRUE, bw="SJ"))
# should the rug on the plot
rug(Eland)

#cumulative density function
plot(ecdf(Eland), do.points=FALSE, verticals=TRUE)
#quantile-quantile
par(pty="s")
qqnorm(Eland);qqline(daily)
#qq plot
x <- seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot for t dsn")
qqline(x)

#box plot and qqplot between two
boxplot(daily, Eland)
qqplot(daily, Eland)



