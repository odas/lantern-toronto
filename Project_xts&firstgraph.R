install.packages(c("xts", "reshape2", "ggplot2"))
#jsonlite is already available in Rstudio.

require(xts)
require(jsonlite)
require(ggplot2)
require(reshape2)

#read files and merge
cad_1m_31052010=read.csv('C:/Users/orpit/Desktop/Lantern/R/LIBOR/CAD1MTD156N.csv')
cad_12m_31052010=read.csv('C:/Users/orpit/Desktop/Lantern/R/LIBOR/CAD12MD156N.csv')
cad_6m_31052010=read.csv('C:/Users/orpit/Desktop/Lantern/R/LIBOR/CAD6MTD156N.csv')
cad_3m_31052010=read.csv('C:/Users/orpit/Desktop/Lantern/R/LIBOR/CAD3MTD156N.csv')

libor_1m <- xts(as.numeric(as.character(cad_1m_31052010$CAD1MTD156N)), as.Date(cad_1m_31052010$DATE, format='%Y-%m-%d'))
libor_1m=na.omit(libor_1m)
libor_3m <- xts(as.numeric(as.character(cad_3m_31052010$CAD3MTD156N)), as.Date(cad_3m_31052010$DATE, format='%Y-%m-%d'))
libor_3m=na.omit(libor_3m)
libor_6m <- xts(as.numeric(as.character(cad_6m_31052010$CAD6MTD156N)), as.Date(cad_6m_31052010$DATE, format='%Y-%m-%d'))
libor_6m=na.omit(libor_6m)
libor_12m <- xts(as.numeric(as.character(cad_12m_31052010$CAD12MD156N)), as.Date(cad_12m_31052010$DATE, format='%Y-%m-%d'))
libor_12m=na.omit(libor_12m)

head(libor_1m)
dim(libor_1m)
head(libor_3m)
dim(libor_3m)
head(libor_6m)
dim(libor_6m)
head(libor_12m)
dim(libor_12m)

libor <- merge(libor_1m, libor_3m, all=FALSE)
libor <- merge(libor, libor_6m, all=FALSE)
libor <- merge(libor, libor_12m, all=FALSE)


#graph IR
zoo.rates <- as.zoo(libor)
tsRainbow <- rainbow(ncol(zoo.rates))
windows(7,4)
plot(x=zoo.rates,col=tsRainbow, main="Historical Interest Rates", ylab="Rates", xlab="Time", plot.type="single", xaxt ="n", yaxt="n")
axis(1)
pts<- pretty(zoo.rates)
axis(2, at=pts, labels=paste(pts, "%", sep=""))
legend(x="topright", legend=colnames(libor), lty=1, col=tsRainbow, cex=.64,bty="n")