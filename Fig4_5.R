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
