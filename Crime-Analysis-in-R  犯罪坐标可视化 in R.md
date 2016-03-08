# Crime-Analysis-in-R  犯罪坐标可视化 in R
UK data until sep 2015
echo "# Crime-Analysis-in-R" >> README.md
git init
git add README.md
git commit -m "first commit"
git remote add origin https://github.com/thelastwhite/Crime-Analysis-in-R.git
git push -u origin master


library(ggplot2)
library(ggmap)
library(magrittr)

geocode("Sheffield")

# imput crime data
mydata1=read.csv("/Users/mac/Desktop/Data Science/INF6027 Introduction to Data Science /Coursework/2015-09/2014-10/2014-10-south-yorkshire-street.csv", header = TRUE, sep = ",") 

# only violent crimes
violent_crimes <- subset(mydata1,
                           Crime.type != "Other theft" &
                           Crime.type != "Bicycle theft" &
                           Crime.type != "Anti-social behaviour" &
                           Crime.type != "Public order" &
                           Crime.type != "Shoplifting" &
                           Crime.type != "Burglary" &
                           Crime.type != "Theft from the person" &
                           Crime.type != "Other crime" &
                           Crime.type != "Criminal damage and arson" 
)

nrow(violent_crimes)

# rank violent crimes
violent_crimes$Crime.type <-
  factor(violent_crimes$Crime.type,
         levels = c("Robbery","Violence and sexual offences", "Vehicle crime",
                    "Drugs","Possession of weapons")
  )
# restrict to downtown
violent_crimes <- subset(violent_crimes,
                         -1.522870  <= Longitude & Longitude <= -1.412879 &
                           53.33560 <= Latitude & Latitude <= 53.42067
)


getGeoCode("Sheffiled")
# get map and bounding box
theme_set(theme_bw(16))
SheffiledMap <- qmap("sheffiled", zoom = 13, color = "bw")

# the bubble chart
library(grid)
SheffiledMap +
  geom_point(aes(x = Longitude, y = Latitude, colour = Crime.type, size = Crime.type), data = violent_crimes) +
  scale_colour_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons")) +
  scale_size_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons"),
                      range = c(1.75,4)) +
  guides(size = guide_legend(override.aes = list(size = 6))) +
  theme(
    legend.key.size = unit(1.8,"lines"),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  ) +
  labs(colour = "Crime.type", size = "Crime.type")


# doing it with qmplot is even easier
qmplot(Longitude, Latitude, data = violent_crimes, maptype = "toner-lite",
       color = Crime.type, size = Crime.type, legend = "right"
) +
  scale_colour_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons")) +
  scale_size_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons"),
                      range = c(1.75,6)) +
  guides(size = guide_legend(override.aes = list(size = 6))) +
  theme(
    legend.key.size = unit(1.8,"lines"),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)
  ) +
  labs(colour = "Crime.type", size = "Crime.type")

# a contour plot
SheffiledMap +
  stat_density2d(aes(x = Longitude, y = Latitude, colour = Crime.type),
                 size = 2, bins = 2, alpha = 3/4, data = violent_crimes) +
  scale_colour_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons")) +
  theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = unit(1.8,"lines"))



sheffield <- get_map("sheffield", zoom = 14)
SheffiledMap <- ggmap(sheffield, extent = "device", legend = "right")

# a filled contour plot...
SheffiledMap  +
  stat_density2d(aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
                 size = 2, bins = 4, data = violent_crimes12, geom = "polygon") +
  scale_fill_gradient("Violent\nCrime\nDensity") +
  scale_alpha(range = c(.4, .75), guide = FALSE) +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))
  
  
  
  # count crime number
  nrow(violent_crimes)


Month <- c("10","11","12","01","02","03","04","05","06","07","08","09")
Numbers <- c(nrow(violent_crimes1),nrow(violent_crimes2),nrow(violent_crimes3),nrow(violent_crimes4),nrow(violent_crimes5),nrow(violent_crimes6),nrow(violent_crimes7),nrow(violent_crimes8),nrow(violent_crimes9),nrow(violent_crimes10),nrow(violent_crimes11),nrow(violent_crimes12))
dataframe1 <- data.frame(Month, Numbers)
dataframe1


ggplot(dataframe1, aes(x = Month, y = Numbers)) + geom_line()

p <- ggplot(dataframe1, aes(x = Month, y = Numbers,group = 1)) + geom_line()
p + ylim(0, max(5000))


#densities
SheffiledMap +
  stat_density2d(aes(x = Longitude, y = Latitude, colour = Crime.type),
                 size = 2, bins = 2, alpha = 3/4, data = violent_crimes) +
  scale_colour_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons")) +
  theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = unit(1.8,"lines")) +
  stat_density2d(aes(x = Longitude, y = Latitude, colour = Crime.type),
                 size = 2, bins = 2, alpha = 3/4, data = violent_crimes2) +
  scale_colour_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons")) +
  theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = unit(1.8,"lines")) +
  stat_density2d(aes(x = Longitude, y = Latitude, colour = Crime.type),
                 size = 2, bins = 2, alpha = 3/4, data = violent_crimes3) +
  scale_colour_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons")) +
  theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = unit(1.8,"lines"))  +
  stat_density2d(aes(x = Longitude, y = Latitude, colour = Crime.type),
                 size = 2, bins = 2, alpha = 3/4, data = violent_crimes4) +
  scale_colour_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons")) +
  theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = unit(1.8,"lines"))  +
  stat_density2d(aes(x = Longitude, y = Latitude, colour = Crime.type),
                 size = 2, bins = 2, alpha = 3/4, data = violent_crimes5) +
  scale_colour_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons")) +
  theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = unit(1.8,"lines")) +
  stat_density2d(aes(x = Longitude, y = Latitude, colour = Crime.type),
                 size = 2, bins = 2, alpha = 3/4, data = violent_crimes6) +
  scale_colour_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons")) +
  theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = unit(1.8,"lines")) +
  stat_density2d(aes(x = Longitude, y = Latitude, colour = Crime.type),
                 size = 2, bins = 2, alpha = 3/4, data = violent_crimes7) +
  scale_colour_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons")) +
  theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = unit(1.8,"lines")) +
  stat_density2d(aes(x = Longitude, y = Latitude, colour = Crime.type),
                 size = 2, bins = 2, alpha = 3/4, data = violent_crimes8) +
  scale_colour_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons")) +
  theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = unit(1.8,"lines")) +
  stat_density2d(aes(x = Longitude, y = Latitude, colour = Crime.type),
                 size = 2, bins = 2, alpha = 3/4, data = violent_crimes9) +
  scale_colour_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons")) +
  theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = unit(1.8,"lines"))  +
  stat_density2d(aes(x = Longitude, y = Latitude, colour = Crime.type),
                 size = 2, bins = 2, alpha = 3/4, data = violent_crimes10) +
  scale_colour_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons")) +
  theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = unit(1.8,"lines"))  +
  stat_density2d(aes(x = Longitude, y = Latitude, colour = Crime.type),
                 size = 2, bins = 2, alpha = 3/4, data = violent_crimes11) +
  scale_colour_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons")) +
  theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = unit(1.8,"lines")) +
  stat_density2d(aes(x = Longitude, y = Latitude, colour = Crime.type),
                 size = 2, bins = 2, alpha = 3/4, data = violent_crimes12) +
  scale_colour_discrete("Crime.type", labels = c("Robbery","Violence and sexual offences", "Vehicle crime","Drugs","Possession of weapons")) +
  theme(
    legend.text = element_text(size = 15, vjust = .5),
    legend.title = element_text(size = 15,face="bold"),
    legend.key.size = unit(1.8,"lines")) 

