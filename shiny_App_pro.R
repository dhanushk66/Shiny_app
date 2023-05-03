library(shiny)
library(shinyWidgets)
library(tidyverse)
library(datasets)
library(dbplyr)
library(ggplot2)
library(dplyr)
library(tibble)
library(data.table)
library(ggmosaic)
library(ggforce)
library(ggmap)
library(ggthemes)
library(purrr)
library(keep)
library(readr)
library(maps)
library(mapproj)
library(tmap)
library(leaflet)
library(sf)
data('World')
library(DT)
xfun::session_info('DT')
#library(rnaturalearth)
#library(rnaturalearthdata)
#library(lwgeom)
#library(geos)
#library(ggspatial)



# make sure to change color back background necessary. 
# make it crowded 
# scatter plot, regression, box page & more(usage of tabs)
# more tabs more points 

#git@github.com:dhruvjain1999/Shiny_app_project.git

# important link:
# source link:  https://www.kaggle.com/datasets/ucsusa/active-satellites?datasetId=744
# https://www.esa.int/Enabling_Support/Space_Transportation/Types_of_orbits
# https://en.wikipedia.org/wiki/Geosynchronous_orbit


data=read_csv('Active_Satellites_in_Orbit_Around_Earth.csv')
head(data)
str(data)

# Data cleaning 
# -----------------------------------------------------------------------------
satellite = select(data, select = -c(`Operator/Owner`, `Country of Operator/Owner`,`Eccentricity`,
                                   `Period (Minutes)`,`Power (Watts)`,`Launch Site`,`NORAD Number`,
                                   `Detailed Purpose`))

na_counts <- satellite %>% 
  summarise_all(~ sum(is.na(.)))%>%
  gather()
# View the results
head(na_counts,18)

# replacing NA values with unknown 
satellite$`Type of Orbit` <- ifelse(is.na(satellite$`Type of Orbit`), "unknown", satellite$`Type of Orbit`)

# Remove non-numeric characters from the value column
satellite$`Dry Mass (Kilograms)` <- as.numeric(gsub("[^0-9]", "", satellite$`Dry Mass (Kilograms)`))

# Replacing values by mean or median ?
hist(satellite$`Launch Mass (Kilograms)`)
# the graph is skewed to right so we will use median approach 
hist(satellite$`Dry Mass (Kilograms)`)
# the graph is skewed to right so we will use median approach 

# Calculate the median of the non-missing values
# the median value is 1178.  
median_mass <- median(satellite$`Launch Mass (Kilograms)`, na.rm = TRUE)
# Impute the missing values with the median
satellite$`Launch Mass (Kilograms)`[is.na(satellite$`Launch Mass (Kilograms)`)] <- median_mass
# another variable 
median_mass <- median(satellite$`Dry Mass (Kilograms)`, na.rm = TRUE)
# the median value is 980.  
# Impute the missing values with the median
satellite$`Dry Mass (Kilograms)`[is.na(satellite$`Dry Mass (Kilograms)`)] <- median_mass

# mean 

satellite$`Expected Lifetime (Years)` <- as.numeric(gsub("[^0-9]", "", satellite$`Expected Lifetime (Years)`))
mean_mass <- mean(satellite$`Expected Lifetime (Years)`, na.rm = TRUE)
satellite$`Expected Lifetime (Years)`[is.na(satellite$`Expected Lifetime (Years)`)] <- mean_mass

# omitting rest all values form data
satellite <- na.omit(satellite)

#view(satellite)


# COUNT 
unique(satellite$`Country of Contractor`)
table(satellite$`Country of Contractor`)

# -----------------------------------------------------------------------------
# Data cleaning completed 

# Calculating Na values in data set after cleaning 
na_counts <- satellite %>% 
  summarise_all(~ sum(is.na(.)))%>%
  gather()
# View the results
head(na_counts,18)

# number of rows 
nrow(satellite)
# number of colums 
ncol(satellite)
# name of colums 
colnames(satellite)



## Selecting particular country and finding the pie chart of number of their launches
## Select particular country and showing a scatter plot if their increase over time
## Getting bar plot for thing usage of satellite for purpose
## Finding the average of perigee and apogee for each type of orbit
## The type of users holding their satellite



# ---------------------------------------------------------------------


satellite

library(tidyr)
# Separate date string into day, month, and year columns
df_new <- separate(satellite, col = `Date of Launch`, into = c("month", "day", "year"), sep = "/")
view(df_new)





#------------coding starts from here -------------------------------------

ab <- satellite %>%
  group_by(`Country of Contractor`) %>%
  summarise(No_of_satellite = table(`Country of Contractor`)) %>%
  left_join(satellite, by = "Country of Contractor")

length(table(satellite$`Country of Contractor`))

ab
NROW(ab)
ncol(ab)
view(ab)

view(satellite)
unique(satellite$`Country of Contractor`)

table(satellite$`Country of Contractor`) -> ab
#view(tibble(ab))
#










#------------ another example here -------------------------------------

library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
}

shinyApp(ui, server)
#####------------------------------------------------------------------

world <- ne_countries(scale = "small", returnclass = "sf")
class(world)


ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", 
          subtitle = paste0("(", length(unique(world$name)), " countries)"))


world %>% 
  filter(!is.na(pop_est)) %>% 
  ggplot() +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma") +
  theme(legend.position = "NULL") 


#satellite----------------------------------------------------------

ui <- fluidPage(
  titlePanel("Frequency Plots, Box Plots, and a Table"),
  sidebarLayout(
    sidebarPanel(
      selectInput("DVvar", "Diamond Variables",
                  choices=colnames(satellite))
    ),
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel("Plot1", plotOutput("freqplot")),
                  tabPanel("Plot2", plotOutput("boxplot")),
                  tabPanel("Table", dataTableOutput("mytable"))
      )
    )
  )
)

server <- function(input, output) {
  output$freqplot <- renderPlot({
    ggplot(satellite, mapping = aes(x = .data[[input$DVvar]], color = cut)) +
      geom_freqpoly(binwidth = 0.1) +
      ggtitle("frequency polygons")
  })
  output$boxplot <- renderPlot({
    ggplot(satellite, aes(y = .data[[input$DVvar]], x=`Expected Lifetime (Years)`)) +
      geom_boxplot(fill = "purple") +
      ggtitle("boxplot")
  })
  output$mytable <- renderDataTable(satellite)
}

shinyApp(ui = ui, server = server)


#####################################################################
#  ----------------------------- example ---------------------------
if (require("shiny")) {
  
  
  data(World)
  world_vars <- setdiff(names(World), c("iso_a3", "name", "sovereignt", "geometry"))
  
  
  ui <- fluidPage(
    tmapOutput("map"),
    selectInput("var", "Variable", world_vars)
  )
  
  server <- function(input, output, session) {
    output$map <- renderTmap({
      tm_shape(World) +
        tm_polygons(world_vars[1], zindex = 401)
    })
    
    observe({
      var <- input$var
      tmapProxy("map", session, {
        tm_remove_layer(401) +
          tm_shape(World) +
          tm_polygons(var, zindex = 401)
      })
    })
  }	
  
  
  app <- shinyApp(ui, server)
  if (interactive()) app
}
# ----------------------------------------------------------------------
  
  
## Selecting particular country and finding the pie chart of number of their launches
## Select particular country and showing a scatterplot if their increase over time
## Getting barplot for theing usage of sattllite for purpose
## Finding the average of perigee and apogee for each type of orbit
## The type of users holding their satellite














