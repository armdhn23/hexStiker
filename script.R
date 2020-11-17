library(hexSticker)
library(ggplot2)
library(showtext)
library(ggpubr)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Libre Baskerville", "baskerville")
## Automatically use showtext to render text for future devices
showtext_auto()


# 1. Ari : app.R (#f39c12 / orange)

options(scipen = 999)  # turns of scientific notations like 1e+40

# Read data
email_campaign_funnel <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/email_campaign_funnel.csv")

# X Axis Breaks and Labels 
brks <- seq(-15000000, 15000000, 5000000)
lbls = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")

# Plot
ari <- ggplot(email_campaign_funnel, aes(x = Stage, y = Users, fill = Gender)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  scale_y_continuous(breaks = brks,   # Breaks
                     labels = lbls) + # Labels
  coord_flip() +  # Flip axes
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_brewer(palette = "Oranges")  +# Color palette
  theme_void() + theme_transparent() +
  theme(legend.position = "none")

ari
sticker(ari ,package="app.R", p_size=6, s_x=1, s_y=.85, s_width=1.5, s_height=0.8,
        h_fill="#d45511", h_color="black",
        p_color = "black", p_y = 1.5,
        p_family = "baskerville",
        url = "kerabatdata.com",u_color = "black", u_family = "baskerville",
        u_size = 1, u_angle = 1, u_x = 0.85, u_y = .2,
        filename="ari.png")

#############################
# 2. Rohis : kuro:/neko (font color : #FFF200 style : Baskerville Old Face)
g <- ggplot(mpg, aes(displ)) + scale_fill_brewer(palette = "Spectral")

rohis <- g + geom_histogram(aes(fill=class), 
                        bins=5, 
                        col="black", 
                        size=.1) + 
  theme_void() +
  theme(legend.position = "none")

rohis

sticker(rohis ,package="kuro:/neko", p_size=6, s_x=1, s_y=.85, s_width=1.7, s_height=1,
          h_fill="#FFF200", h_color="black",
          p_color = "black",
          p_family = "baskerville",
          url = "kerabatdata.com",u_color = "black", u_family = "baskerville",
          u_size = 1, u_angle = 1, u_x = 0.8, u_y = .2,
          filename="rohis1.png")


################33
#3. Inda : @IndaNaser_04 (Medium Orchid _ #BA55D3)
# Functions to convert raw data into moving averages
lag <- function(x, k){
  return( c(rep(NA,k), x[1:(length(x) - k)]) )
}
Mov.Avg <- function(x, k){
  temp.df <- data.frame(x = x)
  for(i in 1:k){
    temp.lag <- lag(x, i)
    temp.df[, i + 1] <- temp.lag
  }
  ma <- rowMeans(temp.df)
  return(ma)
}

# Raw data
set.seed(100)
x <- 1:100
y <- vector("numeric", 100)
for(i in 1:100){
  ifelse(i == 1, y[i] <- 10, y[i] <- y[i-1] + (rnorm(1, 0, 0.75) * i) + rnorm(1, 0, 10))
}
test <- data.frame(x = x,
                   y = y,
                   MA5 = Mov.Avg(y, 5),
                   MA10 = Mov.Avg(y, 10))

# Plot point and line data
inda <- ggplot(test, aes(x, y)) + 
  geom_point(size = 0.1) + 
  geom_line(data = test, aes(x, MA5, stat="identity"), color="red") +
  geom_line(data = test, aes(x, MA10, stat="identity"), color="green") +
  theme_gray() + theme(legend.position="none") + theme_void() + theme_transparent() +
  theme(legend.position = "none")
inda

sticker(inda ,package="@IndaNaser_04", p_size=4, s_x=1, s_y=.85, s_width=1.5, s_height=0.8,
        h_fill="#BA55D3", h_color="black",
        p_color = "black",
        p_family = "baskerville",
        url = "kerabatdata.com",u_color = "black", u_family = "baskerville",
        u_size = 1, u_angle = 1, u_x = 0.85, u_y = .2,
        filename="inda.png")


################
#4. Eni : Mantappu Jiwa (plum. #DDA0DD)
# reorder is close to order, but is made to change the order of the factor levels.
mpg$class = with(mpg, reorder(class, hwy, median))

eni <- mpg %>%
  ggplot( aes(x=class, y=hwy, fill=class)) + 
  geom_violin() +
  xlab("class") +
  theme(legend.position="none") + theme_void() + theme_transparent() +
  theme(legend.position = "none")
eni

sticker(eni ,package="Mantappu Jiwa", p_size=5, s_x=1, s_y=.85, s_width=1.5, s_height=0.8,
       h_fill="#DDA0DD", h_color="black",
       p_color = "black",
       p_family = "baskerville",
       url = "kerabatdata.com",u_color = "black", u_family = "baskerville",
       u_size = 1, u_angle = 1, u_x = 0.8, u_y = .2,
       filename="eni.png")

#5. Hanif : (Sejahtera)
library(lubridate)
theme_set(theme_bw())

df <- economics[, c("date", "psavert", "uempmed")]
df <- df[lubridate::year(df$date) %in% c(1967:1981), ]

# labels and breaks for X axis text
brks <- df$date[seq(1, length(df$date), 12)]
lbls <- lubridate::year(brks)

# plot
hanif <- ggplot(df, aes(x=date),alpha=0.8) + 
  geom_area(aes(y=psavert+uempmed, fill="psavert")) + 
  geom_area(aes(y=uempmed, fill="uempmed")) +
  scale_x_date(labels = lbls, breaks = brks) +  # change to monthly ticks and labels
  scale_fill_manual(name="", 
                    values = c("psavert"="red", "uempmed"="white")) +  # line color
  theme(panel.grid.minor = element_blank()) + # turn off minor grid 
  theme_void() + theme_transparent() +
  theme(legend.position = "none")
hanif
sticker(hanif ,package="Sejahtera", p_size=17, s_x=1, s_y=.85, s_width=1.5, s_height=0.8,
        h_color="white",
        p_family = "baskerville",
        url = "kerabatdata.com",u_color = "white", u_family = "baskerville",
        u_size = 4, u_angle = 1, u_x = 0.8, u_y = .2,
        filename="hanif.png")



################
#6. Yusuf : My_System (lightgreen)

yusuf <- ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values=c("green", "navy", "red")) +
  theme_void() + theme_transparent() +
  theme(legend.position = "none")
yusuf

sticker(yusuf ,package="My_System", p_size=17, s_x=1, s_y=.78, s_width=1.3, s_height=0.9,
        h_fill="lightgreen", h_color="black",
        p_color = "black",
        p_family = "baskerville",
        url = "kerabatdata.com",u_color = "black", u_family = "baskerville",
        u_size = 4, u_angle = 1, u_x = 0.8, u_y = .2,
        filename="yusuf1.png")


################
#7. Wawan: 2105spt (#286e73/biru)

data("ToothGrowth")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

e <- ggplot(ToothGrowth, aes(x = dose, y = len))

wawan <- e + 
  geom_boxplot(aes(fill = supp), position = position_dodge(0.9) ) +
  scale_fill_manual(values = c("#f05a0a", "#f5f6fa")) + theme_void() + theme_transparent() +
  theme(legend.position = "none")
wawan
sticker(wawan,
        package="2105spt", p_size=6, p_color = "#f5f6fa",
        s_x=1, s_y=.78, s_width=1.7, s_height=1.5,
        h_fill="#286e73", h_color="#f5f6fa",
        p_family = "baskerville", p_y = 1.5,
        url = "kerabatdata.com",u_color = "#f5f6fa", u_family = "baskerville",
        u_size = 1.2, u_angle = 1, u_x = 0.8, u_y = .2,
        filename="wawan1.png")

################
#8. Batara: Easy
# library
library(tidyverse)
library(viridis)

# Create dataset
data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value1=sample( seq(10,100), 60, replace=T),
  value2=sample( seq(10,100), 60, replace=T),
  value3=sample( seq(10,100), 60, replace=T)
)

# Transform data in a tidy format (long format)
data <- data %>% gather(key = "observation", value="value", -c(1,2)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
nObsType <- nlevels(as.factor(data$observation))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group)*nObsType, ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar*nObsType )
data <- rbind(data, to_add)
data <- data %>% arrange(group, individual)
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- data %>% group_by(id, individual) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
batara <- ggplot(data) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=observation), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  
  ylim(-150,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar()

sticker(batara,
        package="Easy", p_size=6, p_y = 1.6,
        s_x=1, s_y=.78, s_width=1.7, s_height=0.9,
        h_fill="#062a2e", h_color="#c7c72e",
        p_family = "baskerville", p_color = "#c7c72e",
        url = "kerabatdata.com",u_color = "#c7c72e", u_family = "baskerville",
        u_size = 1.2, u_angle = 1, u_x = 0.8, u_y = .2,
        filename="batara.png")


#############
#9.putri : _putrislsabilla (soft pink)
library(reshape2)
mydata <- mtcars[, c(1,3,4,5,6,7)]
head(mydata)

cormat <- round(cor(mydata),2)


melted_cormat <- melt(cormat)
head(melted_cormat)

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
putri <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "black")+
  scale_fill_gradient2(low = "#f5d7dd", high = "#f53b5e", mid = "#f794a7", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() + theme_void() + theme_transparent() +
  theme(legend.position = "none") 
# Print the heatmap
sticker(putri ,package="_putrislsabilla", p_size=5, s_x=1, s_y=.78, s_width=1.7, s_height=0.9,
        h_fill="#f59db0", h_color="black",
        p_color = "black",
        p_family = "baskerville",
        url = "kerabatdata.com",u_color = "black", u_family = "baskerville",
        u_size = 1, u_angle = 1, u_x = 0.8, u_y = .2,
        filename="putri.png")

    #10. indah : indmay29 ( green  )
library(rgdal)
library(sf)
library(rgdal)

indonesia <- read_sf(dsn="data/IDN_adm_shp", layer="IDN_adm1")
jokowi_prabowo <- read.csv('/data/jokowi_prabowo.csv')
str(indonesia)
maya2<- plot((indonesia$geometry))
maya2
ggplot(data = indonesia) + 
  geometry()

maya <- ggplot(indonesia)+
  geom_sf(color = "black", fill = "black")+ 
  theme_void() +
  theme(legend.position = "none")



maya <- g + theme_void() + theme_transparent() +
  theme(legend.position = "none")

maya
sticker(maya ,package="indmay29", p_size=17, s_x=1, s_y=.78, s_width=1.7, s_height=0.9,
        h_fill="#66CDAA", h_color="black",
        p_color = "black",
        p_family = "baskerville",
        url = "kerabatdata.com",u_color = "black", u_family = "baskerville",
        u_size = 4, u_angle = 1, u_x = 0.8, u_y = .2,
         filename="maya2.png")
          