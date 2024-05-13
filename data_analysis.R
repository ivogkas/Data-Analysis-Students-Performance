library(data.table);library(ggplot2)
# import data set
scores = fread("Pisa mean perfomance scores 2015 Data.csv", header = TRUE)
View(scores)
attach(scores)

# check types
classes = sapply(scores, class)
classes

# create new columns, rename and remove columns
series_code = scores[, `Series Code`]
scores[, `:=` (Discipline = substr(series_code,9,11),
               Gender = substr(series_code,13,14))]
setnames(scores,"2015","Performance")
scores = scores[, -c("Series Name", "Series Code")]

# fill missing values, remove rows, change column type
scores[Gender == "",Gender := 'All']
scores[Performance == "..", .N] #check all columns
scores = scores[Performance != "..",]
scores[, Performance:=as.numeric(Performance)]

# create column Region
unique(scores$`Country Code`)
#eu = c("ALB", "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "GEO", "DEU", "GRE", "HUN",
#       "ISL", "IRL", "ITA", "LVA", "LTU", "LUX", "MKD", "MLT", "MDA", "MNE", "NLD",  "NOR", "POL", "PRT", "RUS" , "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR"  )  
amr = c("ARG", "BRA", "CAN", "CHL", "COL", "CRI",
       "DOM","MEX", "PER", "TTO", "USA", "URY")
afr = c("DZA", "TUN")
ocn = c("AUS", "NZL")
asia = c("HKG", "IDN", "ISR", "JPN", "JOR", "KAZ" , "KOR", 
       "LBN", "MAC", "MYS","QAT", "SGP",  "THA", "ARE", "VNM")

scores[, `:=` (Region = "EU")]
scores[`Country Code` %in% amr,  Region := "AM"]
scores[`Country Code` %in% afr,  Region := "AF"]
scores[`Country Code` %in% ocn,  Region := "OC"]
scores[`Country Code` %in% asia,  Region := "AS"]
View(scores)


#### DATA ANALYSIS

perf_gender = scores[Gender != "All", .(`Mean Performance` = mean(Performance)),
                     by = .(Discipline, Gender)] 
perf_gender
ggplot(perf_gender, aes(x = Gender,  y = `Mean Performance`, fill = Gender)) + 
  geom_bar(stat = "identity",  width = 0.5) + 
  facet_grid(. ~ Discipline) + 
  coord_cartesian(ylim = c(430, 480))  +
  ggtitle("Mean Performance by Gender")+
  theme(plot.title = element_text(hjust = 0.5), legend.position="none")


# create table [Country Name, Discipline, Gender Gap]
gender_dif = scores[Gender != "All", .(`Gender Gap` = diff(Performance)), by = .(`Country Name`, Discipline)]  
gender_dif[`Gender Gap` > 0, Gender := "Male" ]
gender_dif[`Gender Gap` < 0, Gender := "Female" ]
gender_dif[`Gender Gap` > 0 & Discipline == "SCI", .N ] 
gender_dif

# plot 
ggplot(gender_dif, aes(x=`Country Name`, y=`Gender Gap`, fill=Gender)) + geom_bar(stat='identity', width=.5) + facet_grid(. ~ Discipline) +
  geom_hline(yintercept = 0) + ggtitle("Gender Differences in Performance")+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  coord_flip()      


#create table [Country Name, Mean Performance] for European countries
avg_eu_perf = scores[Gender == "All" & Region == "EU", .(`Mean Performance` = mean(Performance)), by = .(`Country Name`)]
avg_eu_perf

#ggplot package for map
map = map_data("world")
setDT(map)
attach(map)

# match some names between mean_eu_perf and map
map[region == "Russia", region := "Russian Federation"]
map[region == "UK", region := "United Kingdom"]
map[region == "Slovakia", region := "Slovak Republic"]
map[region == "North Macedonia", region := "Macedonia, FYR"]
map[region == "USA", region := ""]

# merge data tables
map_eu_perf = avg_eu_perf[map, on = .(`Country Name` == region), allow.cartesian=TRUE ]
c_names = aggregate(cbind(long, lat, group) ~ `Country Name`, data=map_eu_perf, FUN=function(x)mean(range(x)))

ggplot(map_eu_perf, aes(long, lat, group = group))+
  geom_polygon(aes(fill = `Mean Performance` ), color = "white")+
  geom_text(data=c_names, aes(long, lat, label = `Country Name`), size=3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5), 
        legend.position="bottom") +
  labs(title="Mean Performance in Europe")+
  scale_fill_viridis_c(option = "magma", trans = "sqrt") +
  coord_fixed(xlim = c(-25,50), ylim = c(35,70), ratio = 1.3) 
# mean performance ranking
avg_eu_perf[order(`Mean Performance`)]



# create 3 tables [Country Names, Performance by Discipline]
mat_country = scores[Gender == "All" & Discipline == "MAT", .(`Country Name`, Performance)]
setnames(mat_country,"Performance","MAT")
rea_country = scores[Gender == "All" & Discipline == "REA", .(`Country Name`, Performance)]
setnames(rea_country,"Performance","REA")
sci_country = scores[Gender == "All" & Discipline == "SCI", .(`Country Name`, Performance)]
setnames(sci_country,"Performance","SCI")
avg_discipline = mat_country[rea_country, on = .(`Country Name`) ][sci_country, on = . (`Country Name`)]


avg_discipline
avg_discipline[,`:=` (`Mean Performance` = (avg_discipline$MAT + avg_discipline$REA + avg_discipline$SCI)/3),]
avg_discipline = avg_discipline[order(-avg_discipline$`Mean Performance`),] # sort by mean performance 
avg_discipline
library(xtable)
print(xtable(avg_discipline, type = 'latex')) # create latex text



# create 3 tables [Country Names, Performance by Discipline, Region]
mat_country = scores[Gender == "All" & Discipline == "MAT", .(`Country Name`, Performance)]
setnames(mat_country,"Performance","MAT")
rea_country = scores[Gender == "All" & Discipline == "REA", .(`Country Name`, Performance)]
setnames(rea_country,"Performance","REA")
sci_country = scores[Gender == "All" & Discipline == "SCI", .(`Country Name`, Performance, Region)]
setnames(sci_country,"Performance","SCI")
corr_disc = mat_country[rea_country, on = .(`Country Name`) ][sci_country, on = . (`Country Name`)]
corr_disc

p1 = ggplot(corr_disc, aes(x = REA, y = MAT)) +
  geom_point(aes(color = Region)) + 
  geom_smooth(method = 'loess', color = "black", se=F) +
  theme(legend.position="none")
p2 = ggplot(corr_disc, aes(x = REA, y = SCI)) +
  geom_point(aes(color = Region)) + 
  geom_smooth(method = 'loess', color = "black", se=F) +
  theme(legend.position="none")
p3 = ggplot(corr_disc, aes(x = SCI, y = MAT)) +
  geom_point(aes(color = Region)) + 
  geom_smooth(method = 'loess', color = "black", se=F)+
  theme(legend.position="none")
p=plot_grid(p1, p2, p3, ncol=3)

# create one legend 
legend = get_legend(p1 + guides(color = guide_legend(nrow = 1)) + 
                      theme(legend.position = "bottom") )
library(cowplot)
plot_grid(p, legend, ncol=1, rel_heights = c(1, .1))
# find correlation
cor(corr_disc$SCI, corr_disc$REA)



# dot chart
perf_disc = scores[Gender == "All",.(Performance, Discipline),by=.(`Country Name`)]
perf_disc[, `:=` (Mean=mean(Performance)), by =.(`Country Name`)] 
perf_disc
perf_disc = perf_disc[order(-perf_disc$Mean, perf_disc$Performance), ] # sort
perf_disc
perf_disc$`Country Name` = factor(perf_disc$`Country Name`, levels =rev(unique(perf_disc$`Country Name`)), ordered=TRUE)

ggplot(perf_disc, aes(x=`Country Name`, y=Performance)) +
  geom_point(aes(col=Discipline), size=3) +
  geom_segment(aes(x=`Country Name`,
                   xend=`Country Name`,
                   y=min(Performance),
                   yend=max(Performance)),
                   linetype="dashed",
                   size=0.1) + 
  labs(title="Performance by Discipline",
       subtitle="For each country") +
  coord_flip()



#create table [Region, Discipline, Performance]
perf_region = scores[Gender == "All" & Region != "AF" & Region != "OC", .(Performance = mean(Performance)), by = .(Region, Discipline)]
perf_region
#plot lollipop chart
ggplot(perf_region, aes(x=Region, y=Performance)) +
  geom_point(size=2) +
  geom_segment(aes(x=Region,
                   xend=Region,
                   y=0,
                   yend=Performance)) +
  facet_grid(. ~ Discipline) +
  coord_cartesian(ylim = c(400, 500)) +
  labs(title="Performance by Region") +
  theme(plot.title = element_text(hjust = 0.5))



mean(avg_eu_perf$`Mean Performance`)
# create table [Country Name, Region, order(Mean Performance)]
avg_country = scores[Gender == "All", .(`Mean Performance` = mean(Performance)), by = .(`Country Name`, Region)]
avg_country
avg_country = avg_country[order(avg_country$`Mean Performance`), ] # sort
avg_country$`Country Name` = factor(avg_country$`Country Name`, levels = avg_country$`Country Name`) 
avg_country

# create bar chart
ggplot(avg_country, aes(x =`Country Name`, y = `Mean Performance`, fill = Region)) + 
  geom_bar(stat = "identity")  +
  geom_hline(yintercept=mean(avg_country$`Mean Performance`), linetype="dashed", color = "black", size=1) +
  annotate("text", x=10, y=490, label=paste("AVG:", as.character(round(mean(avg_country$`Mean Performance`),2))) ) +
  ggtitle("Mean Performance by Country") +
  theme(axis.title.y=element_blank()) +
  coord_flip(ylim = c(300, 580))


#create table [Region, number_of_countries, percentage]
region_chart = scores[, .(nmb_of_countries = .N/9), by = .(Region)]
region_chart[, `:=` (freq = nmb_of_countries / sum(nmb_of_countries))]
region_chart

#create pie chart plot
ggplot(region_chart, aes(x = "", y=nmb_of_countries, fill = Region)) +
  geom_bar(color = "white", stat = "identity") +
  geom_text(aes(x = 1.6, label = scales::percent(freq, accuracy = .1)),
            position = position_stack(vjust = .5)) +
  theme_void() +
  geom_text(aes(label = nmb_of_countries),
            position = position_stack(vjust = .5)) +
  theme(axis.line = element_blank(),
        plot.title = element_text(hjust = .5)) +
  ggtitle("Pie Chart of Region") + 
  coord_polar("y") 

  

