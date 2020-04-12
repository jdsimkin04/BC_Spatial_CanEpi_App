library(shiny)
library(shinythemes)
library(tidyverse)
library(tmap)
library(tmaptools)
library(sf)
library(viridisLite)
library(leaflet)
library(DT)
# library(crosstalk)


hsda <-     
  st_read("HSDA_2018/HSDA_2018.shp") %>%
  st_simplify(., dTolerance = 1000) %>%
  left_join(., cancer_df_hsda, by = "HSDA_Name") %>% 
  mutate(region = "HSDA") %>% 
  select(-c("HSDA_CD", "HA_CD", "HA_ID", "HA_Name", "HA_Title")) %>% 
  rename(REGION_ID = HSDA_ID, REGION_Title = HSDA_Title, AREA = HSDA_Area,
         POP = HSDA_Pop16, REGION_Name = HSDA_Name)

ha <- 
  st_read("HA_2018/HA_2018.shp") %>%
  st_simplify(., dTolerance = 1000) %>%
  left_join(., cancer_df_ha, by = "HA_Name") %>% 
  mutate(region = "HA") %>% 
  select(-c("HA_CD")) %>%
  rename(REGION_ID = HA_ID, REGION_Title = HA_Title, AREA = HA_Area,
         POP = HA_Pop16, REGION_Name = HA_Name)

lha <- 
  st_read("LHA_2018/LHA_2018.shp") %>%
  st_simplify(., dTolerance = 1000) %>%
  left_join(., cancer_df_lha, by = "LHA_Name") %>% 
  mutate(region = "LHA") %>% 
  select(-c("HSDA_CD", "HSDA_ID", "HSDA_Name",  "HSDA_Title", "HA_CD", "HA_ID", "HA_Name", "HA_Title")) %>%
  rename(REGION_ID = LHA_CD, REGION_Title = LHA_Title, AREA = LHA_Area,
         POP = LHA_Pop16, REGION_Name = LHA_Name)

data_sets <-  list(ha, hsda, lha) #I've thrown the st dfs in a list
data_sets2 <-  list(hsda, lha) #I've thrown the st dfs in a list

cancers <- 
  c("Lung", #These are the list of attributes I would like to choose from to plot
    "Breast", 
    "Colorectal",
    "Prostate",
    "Cervix")

sex <- c("Males", "Females")

year <- c(2017, 2016, 2015, 2014)

ui <- fluidPage(
  theme = shinytheme("yeti"),
  tabsetPanel(
    tabPanel(
      #First Panel
      "Choropleth Mapping",
  titlePanel(strong("Mapping Age-Standardized Cancer Incidence Rates in British Columbia", br(), "(Synthetic Data)")),
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      "Use the dropdown menus below to filter the map by geographic level, sex, cancer type and year.",
      br(),
      br(),
      width = 2,
      selectInput('dataset', 'Geographic level:', choices = c("Health Authority" = "1", "Health Service Delivery Area" = "2", "Local Health Area" = "3")), #Atm I have this as choose one dataframe or the other... ha or hsda
      selectInput("sex_var", "Sex", sex),
      selectInput("cancer_var", "Cancer type", cancers), #Select which cancer to plot... atm the cancer is it's own column. Can pivot_longer to change the dataframes so they are a variable in a column as well if we want to go that approach.. I just haven't figured out how to dplyr::filter in the reactive part of server
      sliderInput("year_var", "Year", min = 2014, max = 2017, value = 2017, 
                  animate = 
                    animationOptions(interval = 2000, loop = F)),
      "Click the Play button above to animate the map over time."
      ),
    mainPanel(
      h4(div("Explore the map by", strong("zooming in and out"), "and", strong("hover and click"), "to find out more information for a given geography.", style = "color: grey;")),
      
      tmapOutput("map", height = 500, #width = "50%" #Output is a tmap
      ),
      strong("Notes:"),
      div("Data are", strong("synthetic"), "(i.e. fake data) and do not represent actual rates", style = "color: grey;"),
      div("ASIR = Age-standardized incidence rate", style = "color: grey;"),
      div("Rates were age-standardized using the direct method to the 2011 Canadian Census Age Structure", style = "color: grey;"),
      br(),
      h4("Data Table"),
      dataTableOutput("table")) #output is a DT datable
  )
  ),
  #Second panel
  tabPanel(
    "Spatial Statistics",
    titlePanel(strong("Testing Spatial Autocorrelation Through Global Moran's test and LISA Analysis", br(), "(Synthetic Data)")),
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        "Use the dropdown menus below to filter the map by geographic level, sex, cancer type and year.",
        br(),
        br(),
        width = 3,
        selectInput('dataset3', 'Geographic level:', choices = c("Health Service Delivery Area" = "1", "Local Health Area" = "2")), #Atm I have this as choose one dataframe or the other... ha or hsda
        selectInput("sex_var3", "Sex", sex),
        selectInput("cancer_var3", "Cancer type", cancers), #Select which cancer to plot... atm the cancer is it's own column. Can pivot_longer to change the dataframes so they are a variable in a column as well if we want to go that approach.. I just haven't figured out how to dplyr::filter in the reactive part of server
        sliderInput("year_var3", "Year", min = 2014, max = 2017, value = 2017, 
                    animate = 
                      animationOptions(interval = 2000, loop = F)),
        "Click the Play button above to animate the map over time.",
        br(),
        h4(strong("Interpreting Spatial Statistics")),
        "The", strong("Global Moran's I test"), "tests for spatial autocorrelation for a set of geographies and attributes (in this case, the ASIR). In other words, the test evalutes whether the pattern of ASIRs across geographies are clustered, dispersed or random.", 
        br(),
        br(),
            "The", strong("Moran's I statistic"), "is an inferential statistic, meaning it is always interpreted within the context of it's null hypothesis - the null states that the ASIR is randomly distrbuted across the geographies.", 
            "When Moran's I is", em("positive"), "the spatial distribution of high and low ASIRs appear clustered.", 
            "When Moran's I is", em("negative"), "the spatial dstribution of high and low ASIRs appear spatially dispersed.", 
        br(),
        br(),
        "When the Global Moran's I test is significant, meaning there is significant spatial autocorrelation, the", strong("Local Moran's test"), "helps us understand exactly which geographical units are similar or different to their surrounding units.",
        br(),
        br(),
            "All analyses used queen weights."
      ),
      mainPanel(
        h4(div("Explore the map by", strong("zooming in and out"), "and", strong("hover and click"), "to find out more information for a given geography.", style = "color: grey;")),
        
        tmapOutput("cluster_map", height = 500, #width = "50%" #Output is a tmap
        ),
        strong("Notes:"),
        div("Data are", strong("synthetic"), "(i.e. fake data) and do not represent actual rates", style = "color: grey;"),
        div("ASIR = Age-standardized incidence rate", style = "color: grey;"),
        div("Rates were age-standardized using the direct method to the 2011 Canadian Census Age Structure", style = "color: grey;"),
        br(),
        h4(strong("Spatial Statistics")),
        br(),
        fluidRow(
          splitLayout(cellWidths = c("20%", "40%", "40%"), tableOutput("moran_table"), plotOutput("moran_plot"), plotOutput("lisa_plot")))
      )
    )
  )#,
  #Third Panel
  # tabPanel(
  #   "Plotting",
  #   sidebarPanel(
  #     "Use the dropdown menus below to filter the map by geographic level, sex, cancer type and year.",
  #     br(),
  #     br(),
  #     width = 2,
  #     selectInput('dataset2', 'Geographic level:', choices = c("Health Authority" = "1", "Health Service Delivery Area" = "2", "Local Health Area" = "3")), #Atm I have this as choose one dataframe or the other... ha or hsda
  #     selectInput("sex_var2", "Sex", sex),
  #     selectInput("cancer_var2", "Cancer type", cancers), #Select which cancer to plot... atm the cancer is it's own column. Can pivot_longer to change the dataframes so they are a variable in a column as well if we want to go that approach.. I just haven't figured out how to dplyr::filter in the reactive part of server
  #     sliderInput("year_var2", "Year", min = 2014, max = 2017, value = 2017)),
  #   mainPanel(
  #     plotOutput("plot")) #output is a ggplot
  # )
  )
)
