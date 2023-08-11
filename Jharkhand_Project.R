#shrishti_singh
#20070243031

#Working on Jharkhand Education Dataset: 


#setting working directory:
setwd("~/study-material/R PROGRAMMING/R project Topics/Jharkhand%3A_Education")

library(mapproj)
library(rgdal)
library(rgeos)
library(raster)
library(sf)
library(plotly)
library(dplyr)
library(readr)
library(tmap)
library(tmaptools)
library(leaflet)
library(ggplot2)
library(DataExplorer)
library(cartography)
library(spatial)
library(RColorBrewer)
library(OpenStreetMap)
library(magrittr)
library(maps)
library(ggmap)
library(tidyverse)
library(gganimate)
library(ggthemes)
library(gapminder)
library(reshape2)
library(ggpubr)

#Jharkhand%3A_Education.shp
jharkhand_edu <- readOGR(choose.files(caption = "select shape file", multi = FALSE))

Jharkhand.data<-read.csv('C:\\Users\\shrishti singh\\Documents\\study-material\\R PROGRAMMING\\R project Topics\\Jharkhand%3A_Education.csv')
rank<-read.csv('C:\\Users\\shrishti singh\\Documents\\study-material\\R PROGRAMMING\\R project Topics\\Jharkhand%3A_Education\\data sets\\Literacy_rank.csv')


names(Jharkhand.data)[1]<- "objectid"     #renaming the col-name as objectid

plot(jharkhand_edu, main='Jharkhand') # shape file plotted

jharkhand_shp<- readOGR("Jharkhand%3A_Education.shp")
jharkhand_shp<- jharkhand_shp[1:24,1:1]

#merging of two csv files:    
jharkhand.data<- merge(Jharkhand.data, rank, by.x='objectid', by.y='objectid') 


names(jharkhand.data)[6]<- "std_1_2_who_can_read_letters_word_more"
names(jharkhand.data)[7]<- "std_1_2_who_can_recognize_no.s_or_more"
names(jharkhand.data)[8]<- "std_3_5_who_can_do_subtraction_or_more"
names(jharkhand.data)[9]<- "std_3_5_who_can_do_read_std1_texts_or_more"
names(jharkhand.data)[10]<- "single_classroom_primary_schl"
names(jharkhand.data)[4]<- "6_14_yrs_out_of_school"
names(jharkhand.data)[5]<- "6_14_yrs_in_private_school"
names(jharkhand.data)[11]<- "single_teacher_primary_schl"
names(jharkhand.data)[12]<- "primary_school_with_girls_toilet"
names(jharkhand.data)[13]<- "primary_school_with_boys_toilet"
names(jharkhand.data)[14]<- "primary_school_with_Drinking_water_facility"
names(jharkhand.data)[16]<- "gov_primary_school_with_school_managment_committee"
names(jharkhand.data)[15]<- "gov_primary_school_providing_midDay_meal"
names(jharkhand.data)[17]<- "TransitionRate_primary_to_UpperPrimary"
names(jharkhand.data)[18]<- "Retension_Rate_primaryLevel"
names(jharkhand.data)[19]<- "pupil_teacher_ratio_at_primary_level"
names(jharkhand.data)[20]<- "pupil_teacher_ratio_at_upper_primary_level"
names(jharkhand.data)[21]<- "sc_enrollments_in_primary_school"
names(jharkhand.data)[22]<- "sc_girls_enrollments_in_primary_school"
names(jharkhand.data)[23]<- "st_enrollment_in_primary_school"
names(jharkhand.data)[24]<- "st_girls_enrollment_in_primary_school"


write.csv(jharkhand.data,"C:\\Users\\shrishti singh\\Documents\\study-material\\R PROGRAMMING\\R project Topics\\Jharkhand%3A_Education\\data sets\\jharkhand.data.csv")
write.csv(Jharkhand.data,"C:\\Users\\shrishti singh\\Documents\\study-material\\R PROGRAMMING\\R project Topics\\Jharkhand%3A_Education\\Jharkhand.data.csv")
write.csv(rank.csv,"C:\\Users\\shrishti singh\\Documents\\study-material\\R PROGRAMMING\\R project Topics\\Jharkhand%3A_Education\\rank.csv")


#merging the shapefile and csv file
J.data<- merge(jharkhand_shp, jharkhand.data, by.x='objectid', by.y='objectid')

#jharkhand_map web_map
leaflet(data =J.data) %>% #base plot 
  addTiles() %>% #base map - default is openstreet map 
  addMarkers(lng = ~longitude, 
           lat = ~latitude, 
           label =~district_name,
           labelOptions = labelOptions(noHide = T, direction = "bottom",
                                       style = list(
                                         "color" = "red",
                                         "font-family" = "serif",
                                         "font-style" = "italic",
                                         "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                         "font-size" = "12px",
                                         "border-color" = "rgba(0,0,0,0.5)" )))
#............Jharkhand web map displayed

A1<-tm_shape(J.data) + 
  tm_fill("gov_primary_school_providing_midDay_meal",style = "quantile", palette = "Greens") +
  tm_borders(alpha = .2)+ tm_style("col_blind")+
  tm_layout(title="Mid Day meals provided in gov schls",
            legend.text.size = 0.8, legend.title.size = 1.4, legend.position =c("left","bottom"),
            frame = FALSE)+ tm_text("district_name",scale=.7)
A2<-tm_shape(J.data) + 
  tm_fill("6_14_yrs_in_private_school",style = "quantile", palette = "Reds") +
  tm_borders(alpha = .2)+ tm_style("col_blind")+
  tm_layout(title="6-14 yrs old going private schls ",
            legend.text.size = 0.8, legend.title.size = 1.4, legend.position = c("left","bottom"),
            frame = FALSE)+ tm_text("district_name", shadow=TRUE,scale=.5)
A3<-tm_shape(J.data) + 
  tm_fill("6_14_yrs_out_of_school",style = "quantile", palette = "Blues") +
  tm_borders(alpha = .2)+ tm_style("col_blind")+
  tm_layout(title="6-14 yrs old not going schls",
            legend.text.size = 0.8, legend.title.size = 1.4, legend.position = c("left","bottom"),
            frame = FALSE)+ tm_text("district_name",scale=.7)
A12<-tm_shape(J.data) + 
  tm_fill("gov_primary_school_with_school_managment_committee",style = "quantile", palette = "Purples") +
  tm_borders(alpha = .2)+ tm_style("col_blind")+
  tm_layout(title="gov primary school with school managment committee",
            legend.text.size = 0.8, legend.title.size = 1.4, legend.position = c("left","bottom"),
            frame = FALSE)+ tm_text("district_name",scale=.7)

A4<-tm_shape(J.data) + 
  tm_fill("std_1_2_who_can_read_letters_word_more",style = "quantile", palette = "Purples") +
  tm_borders(alpha = .2)+  tm_style("classic")+
  tm_layout(title="Std:1 & 2 able to read words",
            legend.text.size = 0.8, legend.title.size = 1.4, legend.position = c("left","bottom"),
            frame = FALSE)+tm_text("district_name",scale=.7)
A5<-tm_shape(J.data) + 
  tm_fill("std_1_2_who_can_recognize_no.s_or_more",style = "quantile", palette = "Greens") +
  tm_borders(alpha = .2)+  tm_style("classic")+
  tm_layout(title="Std:1 & 2 able to recognize no.s",
            legend.text.size = 0.8, legend.title.size = 1.4, legend.position = c("left","bottom"),
            frame = FALSE)+ tm_text("district_name",scale=.7)
A6<-tm_shape(J.data) + 
  tm_fill("std_3_5_who_can_do_subtraction_or_more",style = "quantile", palette = "Reds") +
  tm_borders(alpha = .2)+  tm_style("classic")+
  tm_layout(title="Std:3 & 5 able to basic math",
            legend.text.size = 0.8, legend.title.size = 1.4, legend.position = c("left","bottom"),
            frame = FALSE)+ tm_text("district_name",scale=.7)
A7<-tm_shape(J.data) + 
  tm_fill("std_3_5_who_can_do_read_std1_texts_or_more",style = "quantile", palette = "Blues") +
  tm_borders(alpha = .2)+  tm_style("classic")+
  tm_layout(title="Std:3 & 5 able to read texts",
            legend.text.size = 0.8, legend.title.size = 1.4, legend.position = c("left","bottom"),
            frame = FALSE)+ tm_text("district_name",scale=.7)

A8<-tm_shape(J.data) + 
  tm_fill("sc_enrollments_in_primary_school",style = "quantile", palette = "Reds") +
  tm_borders(alpha = .2)+ tm_style("cobalt")+
  tm_layout(title="SC enrollments in primary school",
            legend.text.size = 0.8, legend.title.size = 1.4, legend.position = c("left","bottom"),
            frame = FALSE)+tm_text("district_name",scale=.7)
A9<-tm_shape(J.data) + 
  tm_fill("sc_girls_enrollments_in_primary_school",style = "quantile", palette = "Oranges") +
  tm_borders(alpha = .2)+ tm_style("cobalt")+
  tm_layout(title="SC girls enrollments in primary school",
            legend.text.size = 0.8, legend.title.size = 1.4, legend.position = c("left","bottom"),
            frame = FALSE)+tm_text("district_name",scale=.7)
A10<-tm_shape(J.data) + 
  tm_fill("st_enrollment_in_primary_school",style = "quantile", palette = "Blues") +
  tm_borders(alpha = .2)+ tm_style("cobalt")+
  tm_layout(title="ST enrollments in primary school",
            legend.text.size = 0.8, legend.title.size = 1.4, legend.position = c("left","bottom"),
            frame = FALSE)+tm_text("district_name",scale=.7)
A11<-tm_shape(J.data) + 
  tm_fill("st_girls_enrollment_in_primary_school",style = "quantile", palette = "Greens") +
  tm_borders(alpha = .2)+ tm_style("cobalt")+
  tm_layout(title="ST girls enrollments in primary school",
            legend.text.size = 0.8, legend.title.size = 1.4, legend.position = c("left","bottom"),
            frame = FALSE)+tm_text("district_name",scale=.7)

tmap_arrange(A2,A3,A1,A12)     #output_1
tmap_arrange(A4,A5,A6,A7)     #output_2
tmap_arrange(A8,A9,A10,A11)  #output_3


#Map plots...........
 
#map 1:
 
 #.............male literacy map ............
 
 pal <- colorNumeric("viridis", 
                     domain = J.data$Male_literacy_rate_excluding_0_to_6_age_grp)
 leaflet(J.data)%>%
   addTiles() %>% addPopups(~longitude, ~latitude, ~as.character(J.data$district_name),
 options = popupOptions(minWidth = 5, closeOnClick = FALSE, closeButton = TRUE)) %>%
   addPolygons(
     stroke = FALSE, 
     #fills according to variable of hex colors:
     fillColor = ~pal(Male_literacy_rate_excluding_0_to_6_age_grp),
     fillOpacity = 0.7, 
     #how much to simplify the plot when zooming:
     smoothFactor = 0.5, 
     #changes what happens to the shape when we mouse over it
     highlight = highlightOptions(weight = 5, 
                                  color = "black",
                                  fillOpacity = 0.9,
                                  bringToFront = FALSE)) %>%
   addCircles(data = J.data,
              lng = ~longitude, 
              lat =  ~latitude,
              popup = ~paste(J.data$district_name,": ",round(J.data$Total_literacy_rate_excluding_0_to_6_age_grp,2),
                             sep=""),
              radius = 2) %>% 
   # Add a legend
   addLegend(pal = pal, 
             values = ~Male_literacy_rate_excluding_0_to_6_age_grp, 
             opacity = 0.4, 
             title = "Male Literacy Rate",
             position = "bottomright")  %>%
  
   addTiles() %>% #base map - default is openstreet map 
   addMarkers(lng = ~longitude, 
              lat = ~latitude, 
              label =~Male_literacy_rate_excluding_0_to_6_age_grp,
              labelOptions = labelOptions(noHide = T, direction = "bottom",
              style = list(
                "color" = "red",
                "font-family" = "serif",
                "font-style" = "italic",
                "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                "font-size" = "12px",
                "border-color" = "rgba(0,0,0,0.5)"
              ))) 
   #.......................male literacy rate map displayed............
 
 # map2:
 
 #.............female literacy map ............
 
 pal <- colorNumeric("magma", 
                     domain = J.data$Female_literacy_rate_excluding_0_to_6_age_grp)
 leaflet(J.data)%>%
   addTiles() %>% addPopups(~longitude, ~latitude, ~as.character(J.data$district_name),
                            options = popupOptions(minWidth = 5, closeOnClick = FALSE, closeButton = TRUE)) %>%
   addPolygons(
     stroke = FALSE, 
     #fills according to variable of hex colors:
     fillColor = ~pal(Female_literacy_rate_excluding_0_to_6_age_grp),
     fillOpacity = 0.7, 
     #how much to simplify the plot when zooming:
     smoothFactor = 0.5, 
     #changes what happens to the shape when we mouse over it
     highlight = highlightOptions(weight = 5, 
                                  color = "blue",
                                  fillOpacity = 0.9,
                                  bringToFront = FALSE)) %>%
   addCircles(data = J.data,
              lng = ~longitude, 
              lat =  ~latitude,
              popup = ~paste(J.data$district_name,": ",round(J.data$Female_literacy_rate_excluding_0_to_6_age_grp,2),
                             sep=""),
              radius = 2) %>% 
   # Add a legend
   addLegend(pal = pal, 
             values = ~Female_literacy_rate_excluding_0_to_6_age_grp, 
             opacity = 0.4, 
             title = "Female Literacy Rate",
             position = "bottomright")  %>%
   
   addTiles() %>% #base map - default is openstreet map 
   addMarkers(lng = ~longitude, 
              lat = ~latitude, 
              label =~Female_literacy_rate_excluding_0_to_6_age_grp,
              labelOptions = labelOptions(noHide = T, direction = "bottom",
                                          style = list(
                                            "color" = "red",
                                            "font-family" = "serif",
                                            "font-style" = "italic",
                                            "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          ))) 
 #.......................Female literacy rate map displayed............
 
 # map 3
 #............. Total literacy map ............
 
 pal <- colorNumeric("BuPu", 
                     domain = J.data$Total_literacy_rate_excluding_0_to_6_age_grp)
 leaflet(J.data)%>%
   addTiles() %>% addPopups(~longitude, ~latitude, ~as.character(J.data$district_name),
                            options = popupOptions(minWidth = 5, closeOnClick = FALSE, closeButton = TRUE)) %>%
   addPolygons(
     stroke = FALSE, 
     #fills according to variable of hex colors:
     fillColor = ~pal(Total_literacy_rate_excluding_0_to_6_age_grp),
     fillOpacity = 0.7, 
     #how much to simplify the plot when zooming:
     smoothFactor = 0.5, 
     #changes what happens to the shape when we mouse over it
     highlight = highlightOptions(weight = 5, 
                                  color = "blue",
                                  fillOpacity = 0.9,
                                  bringToFront = FALSE)) %>%
   addCircles(data = J.data,
              lng = ~longitude, 
              lat =  ~latitude,
              popup = ~paste(J.data$district_name,": ",round(J.data$Total_literacy_rate_excluding_0_to_6_age_grp,2),
                             sep=""),
              radius = 2) %>% 
   # Add a legend
   addLegend(pal = pal, 
             values = ~Total_literacy_rate_excluding_0_to_6_age_grp, 
             opacity = 0.4, 
             title = "Total Literacy Rate",
             position = "bottomright")  %>%
   
   addTiles() %>% #base map - default is openstreet map 
   addMarkers(lng = ~longitude, 
              lat = ~latitude, 
              label =~Total_literacy_rate_excluding_0_to_6_age_grp,
              labelOptions = labelOptions(noHide = T, direction = "bottom",
                                          style = list(
                                            "color" = "red",
                                            "font-family" = "serif",
                                            "font-style" = "italic",
                                            "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)"
                                          ))) 
 #.......................Total literacy rate map displayed............
 
 # map4 
 
 #............. Literacy_Rank map ............

 pal <- colorNumeric("Reds",domain = J.data$Rank)
 leaflet(J.data)%>%
   addTiles() %>% addPopups(~longitude, ~latitude, ~as.character(J.data$district_name),
                            options = popupOptions(minWidth = 3, closeOnClick = FALSE, closeButton = FALSE)) %>%
   addPolygons(
     stroke = FALSE, 
     #fills according to variable of hex colors:
     fillColor = ~pal(Rank),
     fillOpacity = 0.7, 
     #how much to simplify the plot when zooming:
     smoothFactor = 0.5, 
     #changes what happens to the shape when we mouse over it
     highlightOptions = highlightOptions(weight = 5, 
                                  color = "blue",
                                  fillOpacity = 0.9,
                                  bringToFront = FALSE)) %>%
   addCircles(data = J.data,
              lng = ~longitude, 
              lat =  ~latitude,
              popup = ~paste(J.data$district_name,": ",round(J.data$Rank),
                             sep=""),
              radius = 2) %>% 
   # Add a legend
   addLegend(pal = pal, 
             values = ~Rank, 
             opacity = 0.4, 
             title = "District Literacy Rank",
             position = "bottomright")  %>%
   
   addTiles() %>% #base map - default is openstreet map 
   addMarkers(lng = ~longitude, 
              lat = ~latitude, 
              label =~Rank, 
              labelOptions = labelOptions(noHide = T, direction = "bottom",
                                          style = list(
                                            "color" = "red",
                                            "font-family" = "serif",
                                            "font-style" = "bold",
                                            "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                            "font-size" = "13px",
                                            "border-color" = "rgba(0,0,0,0.5)" ))) 
   
 #.......................Literacy_Rank rate map displayed............
 
 
 #.........................Basic Data Analysis:
 
 
 #....plot1
 g1<-gapminder%>%
   ggplot(data=jharkhand.data,mapping =  aes(x=(sc_enrollments_in_primary_school + st_enrollment_in_primary_school), y=Total_literacy_rate_excluding_0_to_6_age_grp, color=district_name,size=Rank))+geom_point()
 g1
 
 #.....plot2
 g2<-gapminder%>%
   ggplot(data=jharkhand.data,mapping =  aes(x=Male_literacy_rate_excluding_0_to_6_age_grp))+geom_bar()
 g2

 
#.........plot3
 q1<-filter(jharkhand.data, Rank<11)  #.......Top 10 literacy ranked ..district data
 
 #...............pie_chart displaying top 8 district ranks
 p1<-ggplot(data=q1, aes(x=district_name, y=literacy_percent, fill=district_name))
 p1<-p1+geom_bar(width = 0.5, stat = "identity")
 p1<-p1+geom_text(aes(label=paste0(Rank)),position = position_stack(vjust = 0.3))
 p1<-p1+ theme_classic()
 p1<-p1+ theme(legend.position = "none")
 p1<-p1+ coord_polar()
 p1<-p1+ theme(
 axis.ticks = element_blank(),
 plot.title = element_text(hjust = 0.5, size = 18)) 
 p1<-p1+ggtitle("Top 10 Districts Literacy rate:") 
 p1<-p1+theme_solarized()
 p1
#..............Displayed
 
 q2<-filter(jharkhand.data, Rank>15)
 
# Pie chart with plotly.........plo4....
 p2 <- plot_ly(data = q2, labels = ~q1$district_name, values = ~q1$literacy_percent, 
                            type = 'pie',sizes = c(50, 80), sort= FALSE,
                           marker= list(colors=colors, line = list(color="black", width=2))) %>%
      layout(title=" jharkhand district  having low literacy rate literacy",paper_bgcolor='yellowgreen')

 p2
#................displayed

 #........plot5
 #camparison study: rank & st_girls_enrollment 
 c1<-ggplot(data=jharkhand.data)+geom_point(mapping=aes(x=literacy_percent,y=std_3_5_who_can_do_subtraction_or_more,size=Rank,color=std_3_5_who_can_do_subtraction_or_more)) +theme_economist() +ggtitle("comparison study 1: ")
 c2<-ggplot(data=jharkhand.data)+geom_point(mapping=aes(x=literacy_percent,y=std_3_5_who_can_do_read_std1_texts_or_more,size=Rank,color=std_3_5_who_can_do_read_std1_texts_or_more)) +theme_economist() +ggtitle("comparison study 2: ")
 ggarrange(c1,c2)
#.....displayed combined

 
ggplot(data=jharkhand.data)+geom_point(mapping=aes(x=Rank,y=TransitionRate_primary_to_UpperPrimary))  #..to be used
 
#........plot6
 p1<-plot(J.data$gov_primary_school_providing_midDay_meal , J.data$sc_enrollments_in_primary_school, pch = 8, col="red",
         main = "Jharkhand education", xlab = "gov mid-day meal", ylab = "SC enrollment")
 p1
 
 p2<-plot(J.data$gov_primary_school_providing_midDay_meal , J.data$st_enrollment_in_primary_school, pch = 8, col="red",
         main = "Jharkhand education", xlab = "gov mid-day meal", ylab = "St enrollment")
 p2
 
 write.csv(q1,"C:\\Users\\shrishti singh\\Documents\\study-material\\R PROGRAMMING\\R project Topics\\Jharkhand%3A_Education\\q1.csv")
 write.csv(q2,"C:\\Users\\shrishti singh\\Documents\\study-material\\R PROGRAMMING\\R project Topics\\Jharkhand%3A_Education\\q2.csv")
 
 
 
 
 #.........................END................................
