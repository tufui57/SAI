<<<<<<< HEAD
### Comparison of indices (Fig.4)

png(filename = "Fig4.png")

par(mfrow = c(1,3))
plot(sai.dat$AUC, sai.dat$euclidian)
plot(sai.dat$AUC, sai.dat$maha)
plot(sai.dat$AUC, sai.dat$mess)

dev.off()

### Elevation vs. of indices (Fig.5)

png(filename = "Fig5.png")

par(mfrow = c(1,4))
plot(sai.dat$elev, sai.dat$AUC)
plot(sai.dat$elev, sai.dat$euclidian)
plot(sai.dat$elev, sai.dat$maha)
plot(sai.dat$elev, sai.dat$mess)

dev.off()
=======
# Load SAI data
sai.dat <- read.csv(".\\SAI_SI.csv")

### Comparison of indices (Fig.4)

par(mfrow = c(1,3))

#plot(sai.dat$AUC, sai.dat$es, xlab = "SAI", ylab = "Euclidian similarity")
plot(sai.dat$AUC, sai.dat$maha, xlab = "SAI", ylab = "Mahalnobis similarity")
plot(sai.dat$AUC, sai.dat$mess, xlab = "SAI", ylab = "MESS")

### Elevation vs. of indices (Fig.5)
par(mfrow = c(1,4))

plot(sai.dat$elev, sai.dat$AUC, xlab = "elevation", ylab = "SAI")
#plot(sai.dat$elev, sai.dat$es, xlab = "elevation", ylab = "Euclidian similarity")
plot(sai.dat$elev, sai.dat$maha, xlab = "elevation", ylab = "Mahalnobis similarity")
plot(sai.dat$elev, sai.dat$mess, xlab = "elevation", ylab = "MESS")
>>>>>>> 2f92bba5853f72a900777d89604a59fbac04f8c9
