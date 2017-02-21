#Load the data
newDat <- readRDS("newDat.rds")
SK <- readRDS("SK.prairie.banding.rds")


#Load packages
library(lme4)
library(boot)
library(merTools)
library(ggplot2)
library(dplyr)

#Group Birds by year
SK.by.year <- group_by(SK, year)

#Calculate the Observed proportion of youngs by yeat
SK.by.year <- summarise(SK.by.year, prop = weighted.mean(Young, Count.of.Birds), pond.count = mean(pond.count), summerfallow = mean(summerfallow), natural.area = mean(natural.area))
#Set "mean" location
SK.by.year$location <- as.factor(551475)


#fit glmer model
fit1 <- glmer(Young ~ pond.count + (1|year/location), family = "binomial", weight = Count.of.Birds, data = SK)
#Create the bootstrap for the line + ribbon part of the graph
bootfit2 <- bootMer(fit1, FUN=function(x)predict(x, newDat, allow.new.levels=T, type = "response"),nsim=100)
apply(bootfit2$t, 2, sd)
newDat$lci <- apply(bootfit2$t, 2, quantile, 0.025)
newDat$uci <- apply(bootfit2$t, 2, quantile, 0.975)
newDat$Youngprop <- predict(fit1, newDat, allow.new.levels=T, type ="response")

#Pred is the predicted value for the 51 observed points
SK.by.year$pred <- predict(fit1, SK.by.year, allow.new.levels=T, type ="response")

#Create a separate Bootfit for the prediction over observed yearly points
bootfit3 <- bootMer(fit1, FUN=function(x)predict(x, SK.by.year, allow.new.levels=T, type = "response"),nsim=100)

#Use them to get the standard error and standard deviation for each point
SK.by.year$se <- (apply(bootfit3$t, 2, sd)/sqrt(length(bootfit3$t)))
SK.by.year$SD <- apply(bootfit3$t, 2, sd)

#Change the years to numeirc in order to generate the Decadal binning
SK$B.Year <- as.numeric(as.character(SK$B.Year))
SK.by.year$year <- as.numeric(as.character(SK.by.year$year))

#Bin the decades for both databases SK and SK.by.year
SK$decade <- ifelse(SK$B.Year < 1970, as.character("1960s"), ifelse(SK$B.Year >= 1970 & SK$B.Year < 1980, as.character("1970s"), ifelse(SK$B.Year >= 1980 & SK$B.Year < 1990, as.character("1980s"), ifelse(SK$B.Year >= 1990 & SK$B.Year < 2000, as.character("1990s"), ifelse(SK$B.Year >= 2000 & SK$B.Year < 2010, as.character("2000s"), as.character("2010s"))))))
SK.by.year$Decade <- ifelse(SK.by.year$year < 1970, as.character("1960s"), ifelse(SK.by.year$year >= 1970 & SK.by.year$year < 1980, as.character("1970s"), ifelse(SK.by.year$year >= 1980 & SK.by.year$year < 1990, as.character("1980s"), ifelse(SK.by.year$year >= 1990 & SK.by.year$year < 2000, as.character("1990s"), ifelse(SK.by.year$year >= 2000 & SK.by.year$year < 2010, as.character("2000s"), as.character("2010s"))))))

#Create a color palet, in this case it goes from Paleturquoise to blue 4, spanning 6 colors
#for more colors go to http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
cc <- scales::seq_gradient_pal("paleturquoise1", "blue4", "Lab")(seq(0,1,length.out=6))
cc <- scales::seq_gradient_pal("paleturquoise1", "blue4", "Lab")(seq(0,1,length.out=6))

#####GGPLOTS###########
#######################

#non summarised plot binomial with jittering
p1 <- ggplot(data = newDat, aes(x = pond.count, y = Youngprop))+ geom_line(size = 1, color = "black")+ geom_ribbon(aes(ymin = lci, ymax= uci), fill = "red", alpha = 0.5) + xlab("Pond count") + ylab("Proportion of juveniles") +geom_point(data = SK, aes(y = Young, x = pond.count, color = decade), alpha = 0.5,position=position_jitter(width=0, height=0.15))+
  theme(legend.position="bottom") +scale_color_manual(values =cc)

#The selected plot, with points and standard errors
p2 <- ggplot(data = newDat, aes(x = pond.count, y = Youngprop))+ geom_line(size = 1, color = "black")+ geom_ribbon(aes(ymin = lci, ymax= uci), fill = "grey", alpha = 0.5) + xlab("Pond count") + ylab("Proportion of juveniles") +
  theme(legend.position="bottom") + ylim(c(0,1))

p2 <- p2 + geom_errorbar(size = 0.3,inherit.aes = FALSE	, data= SK.by.year, aes(x = pond.count),ymin = (SK.by.year$pred - SK.by.year$SD), ymax =(SK.by.year$pred + SK.by.year$SD), width = 0.07)+geom_point(data = SK.by.year, aes(x = pond.count, y = pred, color = Decade), size = 1.5)+ theme(
  panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA)
)+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

# x = year, y = Porportion of young

p3 <- ggplot(data = newDat, aes(x = as.numeric(as.character(year)), y = Youngprop)) + geom_smooth(data = SK.by.year,aes(x = year, y = prop))+ xlab("Year") + ylab("Proportion of young") +geom_point(data = SK.by.year, aes(x = year, y = prop, color = pond.count), alpha = 0.5)+
  theme(legend.position="bottom") 

#If you want to combine many ggplots you use gridextra

library(gridExtra)

#Just list the names of the plots and add number of columns
grid.arrange(p1, p2, p3, ncol = 2)


######Surface plots

#Load packages
library(caret)
library(lme4)
library(rgl)
library(scatterplot3d)

#Load data
newDat <- readRDS("newDat.rds")
M11z <- readRDS("M11z.rds")
SK <- readRDS("SK.prairie.banding.rds")

#Fit model
fit2 <- glmer(Young ~ summerfallow + natural.area + pond.count + (1|year/location), family = "binomial", weight = Count.of.Birds, data = SK)


#Expand grid makes an all possible combination dataframe to predict on
Surfacegrid <-  expand.grid(summerfallow = seq(from = min(SK$summerfallow), to = max(SK$summerfallow), length.out = 50), natural.area = seq(from = min(SK$natural.area), to = max(SK$natural.area), length.out = 100))

#Fix all other variables to the mean
Surfacegrid$pond.count <- 0.1853
Surfacegrid$year <- as.factor(1987)
Surfacegrid$location <- as.factor(551475)

#Predict the Proportion of youngs for the 3D plot
Surfacegrid$Young <- predict(fit2, Surfacegrid, re.form = NA, type = "response")

#plot moving 3d for personal visualization and gifs
plot3d(x =Surfacegrid$natural.area, y = Surfacegrid$Young, Surfacegrid$summerfallow, col = "red",xlab = "Summerfallow", ylab ="Proportion of youngs", zlab = "Natural Area")


#Actuall 3d surface for plot
scat3d <- scatterplot3d(x =Surfacegrid$natural.area, y = Surfacegrid$summerfallow, Surfacegrid$Young, pch = 1, xlab = "Natural area", ylab = "Summerfallow", zlab = "Proportion of youngs", highlight.3d = TRUE, grid = FALSE, angle = 200, x.ticklabs = seq(from = -1.5, to = 1.5, by = 0.5))


#Add observed points to the plot
scat3d$points3d(x = SK.by.year$natural.area, y = SK.by.year$summerfallow, SK.by.year$prop,
             col="blue", pch=16)

#If you want to test angles this is the loop I made

for (i in seq(from =0, to = 270, by = 2)){
  Title = as.character(i)
  scatterplot3d(x =Surfacegrid$natural.area, y = Surfacegrid$summerfallow, Surfacegrid$Young, color = "red", pch = 1, xlab = "Natural area", ylab = "Summerfallow", zlab = "Proportion of youngs", highlight.3d = TRUE, grid = FALSE, angle = i, main = Title)
}

