##Clear the R environment, if required
rm(list = ls())

#####################################################################################
#####################Install the libraries, if required##############################
#####################################################################################
if(!require(ggplot2))install.packages("ggplot2")
if(!require(animation))install.packages("animation")
if(!require(data.table))install.packages("data.table")
if(!require(dplyr))install.packages("dplyr")
if(!require(ggthemes))install.packages("ggthemes")


gdp.data <- read.csv("gdp.csv", header = T)
attach(gdp.data)
gdp.data1 <- data.table(gdp.data[order(-X2017),])
gdp.data2 <- gdp.data1[gdp.data1$Country.Name %in% c("United States", "United Kingdom","Germany","Japan","China")]
gdp.data2 <- subset(gdp.data2,select= -c(Country.Code,Indicator.Name,Indicator.Code,X1960,X1961,X1962,X1963,X1964,X1965,X1966,X1967,X1968,X1969,X2018))
names(gdp.data2) <- gsub("X", "", names(gdp.data2))
names(gdp.data2)[names(gdp.data2)=="Country.Name"] <- "Country"
gdp.data2$`2018` <- c(20513*10^9,13457.27*10^9,5070.63*10^9,4029.14*10^9,2808.9*10^9)
new.data <- melt(gdp.data2, id=c("Country"))
colnames(new.data) <- c("Country","Year","GDP")
new.data$GDP <- new.data$GDP/(10^12)
new.data$Year <- as.character(new.data$Year)
new.data$Year <- as.numeric(new.data$Year)

ggplot(new.data, aes(Year, GDP, group=Country, colour=Country))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = c(1970,1978,1986,1994,2002,2010,2018))+
  ylim(0,(21))+
  labs(x='',y='GDP (in Trillions USD)')+
  theme_minimal()+
  theme(legend.title = element_blank())

ani.options(interval=0.22,nmax=500)

saveVideo({
  end_year=2018
  num_years = 48
  for(i in 1:num_years){
    new.data.subset <- new.data %>% filter(Year <= end_year - (num_years-i))
    p <- ggplot(new.data.subset, aes(Year,GDP,group=Country,colour=Country))+
      geom_line(size=1.5)+
      scale_x_continuous(breaks=c(1970,1978,1986,1994,2002,2010,2018))+
      ylim(0,21)+
      labs(x='',y='GDP (in Trillions USD)',title = "The World's 5 Biggest Economies since 1970", caption = "Source: World Development Indicators, The World Bank")+
      theme_economist()+
      theme(legend.title = element_blank(),
            plot.title = element_text(face = "bold", size=16,hjust = 0.5),  
            plot.caption = element_text(face = "italic", size = 9),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 12))
    print(p)  
  }
}, convert = 'gm convert', movie.name = 'GDP_animation.mpeg')
