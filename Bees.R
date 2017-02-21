library(vegan)
BeeDiversity <- read.csv("~/Hex/BeeDiversity.csv")
BeesOnly <- BeeDiversity[1:24,-(1:3)]
row.names(BeesOnly) <- BeeDiversity$FieldID[1:24]
BeesManagement <- BeeDiversity[1:24,1:3]

Bees.dist <- vegdist(BeesOnly)
Bees.ano <- anosim(Bees.dist, BeesManagement$Treatment)
summary(Bees.ano)
plot(Bees.ano)

Bees.ano2 <- anosim(Bees.dist, BeesManagement$Crop)
summary(Bees.ano2)
plot(Bees.ano2)

example_NMDS=metaMDS(BeesOnly,k=2,trymax=100)

stressplot(example_NMDS)
ordiplot(example_NMDS,type="n")
orditorp(example_NMDS,display="species",col="red",air=1)
orditorp(example_NMDS,display="sites",cex=1.25,air=0.01)


ordiplot(example_NMDS,type="n")
ordihull(example_NMDS,groups= BeesManagement$Treatment,draw="polygon",col="grey90",label=F)
orditorp(example_NMDS,display="sites",col=ifelse(BeesManagement$Treatment == "treated", "blue", "red"),
         air=0.01,cex=1)


ordiplot(example_NMDS,type="n")
ordihull(example_NMDS,groups= BeesManagement$Crop,draw="polygon",col="grey90",label=F)
orditorp(example_NMDS,display="sites",col=ifelse(BeesManagement$Crop == "soybeans", "blue", ifelse(BeesManagement$Crop == "oldfield","red",ifelse(BeesManagement$Crop == "corn", "yellow", ifelse(BeesManagement$Crop == "grassland","green", ifelse(BeesManagement$Crop == "sunflowers","brown", ifelse(BeesManagement$Crop == "hayfield","orange", "purple")))))),
         air=0.01,cex=1)

geom_mean <- function (spp) {
  spp[spp== 0]<- NA
  results <- exp(rowSums(log(spp), na.rm = TRUE)/rowSums(ifelse(is.na(spp), 0 , 1)))
  hist(results)
  results
}

set.seed(124)
x2<-c(1:50)
y2<- 50*x2 + -1*((x2)^2) + 20 + rnorm(50, 0 , 50)
plot(y2~x2, xlab = "Environmental Variable 1", ylab = "Abundance")
a <- glm(y2~ x2+ I(x2^2))
b <- predict(a, new.data = c(1:50))
lines(b)
