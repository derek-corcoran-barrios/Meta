
coord <- read.csv("~/Documents/Art classification proyect/ART/coord.csv")
Art <- read.csv("~/Documents/Art classification proyect/ART/Art.csv")

pca.3d <- cbind(coord$Dim.1, coord$Dim.2, coord$Dim.3, Art$movimiento)
colnames(pca.3d) <- c("X", "Y", "Z", "Mov")


library(rgl)
attach(data.frame(pca.3d))
plot3d(X,Y,Z,col=Mov, size=4, type="s")
if (!rgl.useNULL())
  +     movie3d(spin3d(axis = c(1, 0.2, 0.5)), duration = 20,
                dir = getwd())


