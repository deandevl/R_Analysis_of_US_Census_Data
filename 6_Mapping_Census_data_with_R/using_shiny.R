library(shiny)
library(usmap)
library(here)
library(leaflet)
library(RcensusPkg)

output_dir <- file.path(here(), "shapefiles")

# We are looking for variables from the 2019 acs/acs5/profile 
# dataset and looking at percentages of race/ethnic groups (DP05_0071PE(hispanic), 
# DP05_0077PE(white), DP05_0078PE(black), DP05_0079PE(native),DP05_0080PE(asian)).  
# The geographies are tracts of 7 counties in the Minneanpolis., Minnesota area.

minn_henn_fips <- substr(usmap::fips(state = "minnesota", county = "hennepin"),3,5)
minn_ram_fips <- substr(usmap::fips(state = "minnesota", county = "ramsey"),3,5)
minn_ano_fips <- substr(usmap::fips(state = "minnesota", county = "anoka"),3,5)
minn_was_fips <- substr(usmap::fips(state = "minnesota", county = "washington"),3,5)
minn_dak_fips <- substr(usmap::fips(state = "minnesota", county = "dakota"),3,5)
minn_car_fips <- substr(usmap::fips(state = "minnesota", county = "carver"),3,5)
minn_sco_fips <- substr(usmap::fips(state = "minnesota", county = "scott"),3,5)

minn_fips <-  usmap::fips(state = "minnesota")

# We will iterate over the 7 counties in obtaining the race/ethnic data:

regionin_v <- c(
  paste0("state:", minn_fips, "+county:", minn_henn_fips),
  paste0("state:", minn_fips, "+county:", minn_ram_fips),
  paste0("state:", minn_fips, "+county:", minn_ano_fips),
  paste0("state:", minn_fips, "+county:", minn_was_fips),
  paste0("state:", minn_fips, "+county:", minn_dak_fips),
  paste0("state:", minn_fips, "+county:", minn_car_fips),
  paste0("state:", minn_fips, "+county:", minn_sco_fips)
)

data_lst <- regionin_v |>
  purrr::map(\(region_in_val) RcensusPkg::get_vintage_data(
    dataset = "acs/acs5/profile",
    vintage = 2019,
    vars = c("DP05_0071PE","DP05_0077PE","DP05_0078PE","DP05_0079PE","DP05_0080PE"),
    region = "tract",
    regionin = region_in_val
  ))

# Convert the obtained list into a data.table and perform some minor wrangling:
data_dt <- data.table::rbindlist(data_lst) %>%
  data.table::setnames(old = c("DP05_0071PE","DP05_0077PE","DP05_0078PE","DP05_0079PE","DP05_0080PE"),
                       new = c("Hispanic","White","Black","Native","Asian")
  ) %>%
  .[, `:=`(
    Hispanic = as.numeric(Hispanic),
    White = as.numeric(White),
    Black=as.numeric(Black),
    Native=as.numeric(Native),
    Asian=as.numeric(Asian))] 

# Reshape data_dt from wide to long:
data_long_dt <- data.table::melt(
  data = data_dt,
  id.vars = c("NAME","state","county","tract","GEOID"),
  variable.name = "Race",
  value.name = "Percentage"
)

# Get the geometries for the county tracts:
data_tracts_dt <- RcensusPkg::tiger_tracts_sf(
  state = minn_fips,
  output_dir = output_dir,
  vintage = 2019,
  general = T,
  sf_info = T,
  datafile = data_long_dt,
  datafile_key = "GEOID",
  check_na = T
) %>% data.table::as.data.table(.) %>% 
  .[Percentage >= 0.0,]

# Setup the shiny user interface:
groups <- c("Hispanic","White","Black","Native","Asian")
ui <- fluidPage(
  fluidRow(
    column(
      width = 2,
      selectInput(
        inputId = "group",
        label = "Select a group",
        choices = groups
      )
    ),
    column(
      width = 2,
      numericInput(
        inputId = "zoom",
        label = "Zoom level",
        value = 9
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      leaflet::leafletOutput(
        outputId = "map",
        height = "800"
      )
    )
  )
)

# Set up the shiny server function:
server <- function(input,output,session){
  # Reactive function that filters for the selected group in the drop-down menu
  group_to_map_fun <- eventReactive(input$group,{
    data_tracts_dt[Race == input$group,]
  })
  
  # Initialize the map object, centered on the Minneapolis-St. Paul area
  output$map <- leaflet::renderLeaflet({
    pal <- colorNumeric(
      palette = "OrRd",
      domain = group_to_map_fun()$Percentage
    )
    leaflet(options = leafletOptions(zoomControl = F)) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      setView(lng = -93.21,lat = 44.98,zoom = input$zoom) %>%
      addPolygons(
        data = group_to_map_fun()$geometry,
        color = pal(group_to_map_fun()$Percentage),
        weight = 0.5,
        fillOpacity = 0.5,
        smoothFactor = 0.2,
        label = group_to_map_fun()$Percentage
      ) %>%
      addLegend(
        pal = pal,
        values = group_to_map_fun()$Percentage,
        position = "bottomright",
        title = "% of Population"
      )
  })
}

# Start the server:
shinyApp(ui = ui, server = server)
