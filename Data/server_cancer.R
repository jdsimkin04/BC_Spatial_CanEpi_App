library(shiny)
library(shinythemes)
library(tidyverse)
library(tmap)
library(tmaptools)
library(sf)
library(viridis)
library(viridisLite)
library(leaflet)
library(DT)
library(magrittr)
library(spdep)
library(knitr)
library(kableExtra)
# library(crosstalk)


server <- function(input, output, session) {
  
  datasetInput <- reactive({ 
    data_sets[[as.numeric(input$dataset)]] %>% 
      filter(Sex == input$sex_var,
             Cancer == input$cancer_var,
              Year == input$year_var
             )
  })
  
  output$map <- renderTmap({ 
    
    mymap <-  data_sets[[1]] %>% 
      filter(Sex == sex[1],
             Cancer == cancers[1],
              Year == year[1]
             )
    
    tm_shape(mymap) + 
      tm_polygons("ASIR", zindex = 401,
                  id = "REGION_Name",
                  title = "ASIR per 100,000",
                  style = "sd",
                  alpha = 0.8,
                  palette = "viridis",
                  border.col = "White",
                  lwd = 0.5,
                  popup.vars=c("Rate per 100,000: " = "ASIR", "Population: " = "POP")
      ) +
      tm_layout(outer.margins = 0) +
      tm_view(
        set.view = c(-127, 55, 4.5))
  })
  
  #update map
  
  observe({
    
    mymap <- datasetInput()
    
    if((input$sex_var == "Males" & input$cancer_var %in% c("Cervix", "Breast"))){
      tmapProxy("map", session, {
        tm_remove_layer(401) +
          tm_shape(mymap) +
          tm_polygons(zindex = 401,
                      title = "ASIR per 100,000",
                      style = "sd",
                      alpha = 0.8,
                      id = "REGION_Name",
                      # title = "Cancer",
                      # palette = "viridis",
                      border.col = "White",
                      lwd = 0.5,
                      popup.vars=c("Rate per 100,000: " = "ASIR", "Population: " = "POP")
          ) +
          tm_layout(outer.margins = 0) +
          tm_view(
            set.view = c(-127, 55, 4.5))
        
      })
    }
    
    if(input$sex_var == "Females" & input$cancer_var %in% c("Prostate")){
      tmapProxy("map", session, {
        tm_remove_layer(401) +
          tm_shape(mymap) +
          tm_polygons(zindex = 401,
                      id = "REGION_Name",
                      title = "ASIR per 100,000",
                      style = "sd",
                      alpha = 0.8,
                      # title = "Cancer",
                      # palette = "viridis",
                      border.col = "White",
                      lwd = 0.5,
                      popup.vars=c("Rate per 100,000: " = "ASIR", "Population: " = "POP")
          ) +
          tm_layout(outer.margins = 0) +
          tm_view(
            set.view = c(-127, 55, 4.5))
        
      })
    }
    
    if((input$sex_var == "Males" & !(input$cancer_var %in% c("Cervix", "Breast"))) | (input$sex_var == "Females" & !(input$cancer_var == "Prostate"))){ #if not the above
      tmapProxy("map", session, {
        tm_remove_layer(401) +
          tm_shape(mymap) +
          tm_polygons("ASIR", zindex = 401,
                      id = "REGION_Name",
                      title = "ASIR per 100,000",
                      # title = "Cancer",
                      palette = "viridis",
                      border.col = "White",
                      lwd = 0.5,
                      popup.vars=c("Rate per 100,000: " = "ASIR", "Population: " = "POP")
          ) +
          tm_layout(outer.margins = 0) +
          tm_view(
            set.view = c(-127, 55, 4.5))
        
      })
    }
  })
  
  
  output$table <- renderDataTable({
    mydata <- datasetInput()
    
    mydata %>%
      st_drop_geometry() %>%
      select(REGION_Name, Year, Sex, Cancer, ASIR, POP) %>%
      datatable(.,
                rownames = F,
                colnames = c("Region Name", "Year", "Sex", "Cancer", "ASIR", "Population"),
                extensions = 'Buttons',
                options = list(
                  dom = 'Bfrtip',
                  buttons = list(list(
                    extend = 'collection',
                    buttons = c('csv'),
                    text = 'Download data'
                  )),
                  class = "display"
                ))
  })
  
  datasetInput2 <- reactive({ 
    data_sets[[as.numeric(input$dataset2)]] %>% 
      filter(Sex == input$sex_var2,
             Cancer == input$cancer_var2,
             Year == input$year_var2
      )
  })
  
#Panel for plotting, third panel, left out for now
  # output$plot <- renderPlot({
  #   myplot <- datasetInput2() %>% 
  #     st_drop_geometry()
  #   
  #   # order <- myplot %>%
  #   #   arrange(ASIR) %$%
  #   #   REGION_Name
  # 
  #   # myplot <-
  #   #   myplot %>%
  #   #   mutate(REGION_Name = factor(REGION_Name, levels = c(order)))
  # 
  #   myplot %>% 
  #     ggplot(., aes(x = REGION_Name, y = ASIR, fill = REGION_Name)) +
  #     geom_bar(stat = "identity", colour = "black", size = 0.5) +
  #     labs(x = "Region Name",
  #          y = "Age-standardized incidence rate\nper 100,000",
  #          fill = "") +
  #     scale_fill_viridis_d() +
  #     coord_flip() +
  #     theme_bw(base_size = 14) +
  #     theme(axis.title = element_text(face = "bold"),
  #           # axis.text.x = element_text(angle = 45, hjust = 1),
  #           panel.grid.major.x = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_blank(),
  #           legend.position = "bottom")
  # 
  # })
    
  # Cluster map
  datasetInput3 <- reactive({ #Here I am picking the dataset from the list to then show in the table
    data_sets2[[as.numeric(input$dataset3)]] %>% 
      filter(Sex == input$sex_var3,
             Cancer == input$cancer_var3,
             Year == input$year_var3
      )
  })
  
  output$cluster_map <- renderTmap({

    mymap2 <-  data_sets2[[1]] %>%
      filter(Sex == sex[1],
             Cancer == cancers[1],
             Year == year[1]
      )

    get.ZeroPolicyOption()
    set.ZeroPolicyOption(TRUE)

    nb <-
      poly2nb(mymap2, queen=TRUE)

    lw <-
      nb2listw(nb, style="W", zero.policy=TRUE)

    lmoran <-
      localmoran(mymap2$ASIR, lw,
                 alternative = "greater")

        # padronize the variable and save it to a new column
    mymap2$s_ASIR <- scale(mymap2$ASIR)  %>% as.vector()

    # create a spatially lagged variable and save it to a new column
    mymap2$lag_s_ASIR <- lag.listw(lw, mymap2$s_ASIR)

    # high-high quadrant
    mymap2 <-
      mymap2 %>%
      mutate(Cluster_Type = case_when(
        (s_ASIR >= 0 & lag_s_ASIR >= 0) & (lmoran[, 5] <= 0.05) ~ "high-high",
        (s_ASIR <= 0 & lag_s_ASIR <= 0) & (lmoran[, 5] <= 0.05) ~ "low-low",
        (s_ASIR >= 0 & lag_s_ASIR <= 0) & (lmoran[, 5] <= 0.05) ~ "high-high",
        (s_ASIR <= 0 & lag_s_ASIR >= 0) & (lmoran[, 5] <= 0.05) ~ "high-high",
        TRUE ~ "Not significant"
      )) %>%
      mutate(Cluster_Type = as.factor(Cluster_Type))

        # plotting the map
    tm_shape(mymap2) +
      tm_polygons("Cluster_Type", zindex = 401, 
                  id = "REGION_Name",
                  title = "Cluster Type",
                  # style = "sd",
                  alpha = 0.8,
                  palette = viridis_pal(option = "D")(mymap2 %>% st_drop_geometry() %>% distinct(Cluster_Type) %>% nrow),
                  border.col = "White",
                  lwd = 0.5,
                  popup.vars=c("Rate per 100,000: " = "ASIR", "Population: " = "POP", "Cluster Type" = "Cluster_Type")
      ) +
      tm_layout(outer.margins = 0) +
      tm_view(
        set.view = c(-127, 55, 4.5))
  })

  #update map

  observe({

    mymap2 <- datasetInput3()

    if((input$sex_var3 == "Males" & input$cancer_var3 %in% c("Cervix", "Breast"))){
      tmapProxy("cluster_map", session, {
        tm_remove_layer(401) +
          tm_shape(mymap2) +
          tm_polygons(zindex = 401,
                      # title = "Cluster Type",
                      style = "sd",
                      alpha = 0.8,
                      id = "REGION_Name",
                      # title = "Cancer",
                      # palette = "viridis",
                      border.col = "White",
                      lwd = 0.5,
                      popup.vars=c("Rate per 100,000: " = "ASIR", "Population: " = "POP")
          ) +
          tm_layout(outer.margins = 0) +
          tm_view(
            set.view = c(-127, 55, 4.5))

      })
    }

    if(input$sex_var3 == "Females" & input$cancer_var3 %in% c("Prostate")){
      tmapProxy("cluster_map", session, {
        tm_remove_layer(401) +
          tm_shape(mymap2) +
          tm_polygons(zindex = 401,
                      id = "REGION_Name",
                      # title = "ASIR per 100,000",
                      style = "sd",
                      alpha = 0.8,
                      # title = "Cancer",
                      # palette = "viridis",
                      border.col = "White",
                      lwd = 0.5,
                      popup.vars=c("Rate per 100,000: " = "ASIR", "Population: " = "POP")
          ) +
          tm_layout(outer.margins = 0) +
          tm_view(
            set.view = c(-127, 55, 4.5))

      })
    }

    if((input$sex_var3 == "Males" & !(input$cancer_var3 %in% c("Cervix", "Breast"))) | (input$sex_var3 == "Females" & !(input$cancer_var3 == "Prostate"))){ #if not the above
      get.ZeroPolicyOption()
      set.ZeroPolicyOption(TRUE)
      
      nb <-
        poly2nb(mymap2, queen=TRUE)
      
      lw <-
        nb2listw(nb, style="W", zero.policy=TRUE)
      
      lmoran <-
        localmoran(mymap2$ASIR, lw,
                   alternative = "greater")
      
      # padronize the variable and save it to a new column
      mymap2$s_ASIR <- scale(mymap2$ASIR)  %>% as.vector()
      
      # create a spatially lagged variable and save it to a new column
      mymap2$lag_s_ASIR <- lag.listw(lw, mymap2$s_ASIR)
      
      # high-high quadrant
      mymap2 <-
        mymap2 %>%
        mutate(Cluster_Type = case_when(
          (s_ASIR >= 0 & lag_s_ASIR >= 0) & (lmoran[, 5] <= 0.05) ~ "high-high",
          (s_ASIR <= 0 & lag_s_ASIR <= 0) & (lmoran[, 5] <= 0.05) ~ "low-low",
          (s_ASIR >= 0 & lag_s_ASIR <= 0) & (lmoran[, 5] <= 0.05) ~ "high-high",
          (s_ASIR <= 0 & lag_s_ASIR >= 0) & (lmoran[, 5] <= 0.05) ~ "high-high",
          TRUE ~ "Not significant"
        )) %>%
        mutate(Cluster_Type = as.factor(Cluster_Type))
      
      tmap_mode("view")
      
      tmapProxy("cluster_map", session, {
        tm_remove_layer(401) +
          tm_shape(mymap2) +
          tm_polygons("Cluster_Type", zindex = 401,
                      id = "REGION_Name",
                      title = "Cluster Type",
                      # title = "Cancer",
                      palette = viridis_pal(option = "D")(mymap2 %>% st_drop_geometry() %>% distinct(Cluster_Type) %>% nrow),
                      border.col = "White",
                      lwd = 0.5,
                      popup.vars=c("Rate per 100,000: " = "ASIR", "Population: " = "POP", "Cluster Type" = "Cluster_Type")
          ) +
          tm_layout(outer.margins = 0) +
          tm_view(
            set.view = c(-127, 55, 4.5))

      })
    }
  })
  
  output$moran_table <- function() {

    mydata2 <- datasetInput3()

    if((input$sex_var3 == "Males" & input$cancer_var3 %in% c("Cervix", "Breast"))){
      tibble(
        Indicator = c("Moran's I", "p value"),
        Value = c("-", "-")
      ) %>%
        kable(., 
              format = "html",
              caption = "Global Moran's Test (Monte Carlo Simulation test)") %>%
        kable_styling("striped", full_width = F)
    }

    if(input$sex_var3 == "Females" & input$cancer_var3 %in% c("Prostate")){

      tibble(
        Indicator = c("Moran's I", "p value"),
        Value = c("-", "-")
      ) %>%
        kable(., 
              format = "html",
              caption = "Global Moran's Test (Monte Carlo Simulation test)") %>%
        kable_styling("striped", full_width = F)
    }

    if((input$sex_var3 == "Males" & !(input$cancer_var3 %in% c("Cervix", "Breast"))) | (input$sex_var3 == "Females" & !(input$cancer_var3 == "Prostate"))){ #if not the above

      get.ZeroPolicyOption()
      set.ZeroPolicyOption(TRUE)

      nb <-
        poly2nb(mydata2, queen=TRUE)


      lw <-
        nb2listw(nb, style="W", zero.policy=TRUE)

      # Moran's I test MC simulations
      MC <-
        moran.mc(mydata2$ASIR, lw, nsim=599)

      tibble(
        Indicator = c("Moran's I", "p value"),
        Value = c(
          round(MC$statistic,4),
          round(MC$p.value, 4))
      ) %>%
        kable(., 
              format = "html",
              caption = "Global Moran's Test (Monte Carlo Simulation test)") %>%
        kable_styling("striped", full_width = T)

    }
  }
  
  output$moran_plot <- renderPlot({
    myplot2 <- datasetInput3()
    
    if((input$sex_var3 == "Males" & input$cancer_var3 %in% c("Cervix", "Breast"))){

          }
    
    if(input$sex_var3 == "Females" & input$cancer_var3 %in% c("Prostate")){

          }
    
    if((input$sex_var3 == "Males" & !(input$cancer_var3 %in% c("Cervix", "Breast"))) | (input$sex_var3 == "Females" & !(input$cancer_var3 == "Prostate"))){ #if not the above
      
      get.ZeroPolicyOption()
      set.ZeroPolicyOption(TRUE)
      
      nb <-
        poly2nb(myplot2, queen=TRUE)
      
      
      lw <-
        nb2listw(nb, style="W", zero.policy=TRUE)
      
      MC <- 
        moran.mc(myplot2$ASIR, lw, nsim=599)
      
      df <- 
        tibble(
          I = MC$res
        )
      
      MC_result <- 
        if(MC$p.value < 0.05){
          "Spatial autocorrelation is present"
        }
      else{"Spatial autocorrelation is not present"}
      
      MC_result2 <- 
        if(MC$p.value < 0.05 & MC$statistic > 0){
          "\nand ASIRs are clustered."
        }
      else if(MC$p.value < 0.05 & MC$statistic < 0){
        "\nand ASIRs are dispersed."
      }
      else{"\nand ASIRs are distributed at random."}
          
      ggplot(df, aes(x = I)) +
        geom_density(fill = "grey") +
        geom_vline(xintercept = MC$statistic, col = "black", size = 2) +
        labs(
          title = "Simulated Moran's I values",
          subtitle = paste0(MC_result, MC_result2),
          x = "Moran's I",
          y = "Density",
          caption = "One-sided test for positve Moran's I statistic because we expect clustering and not dispersion."
        ) +
        theme_bw(base_size = 14) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
      
    }
    
    
  })
  
  output$lisa_plot <- renderPlotly({
    myplot2 <- datasetInput3()
    
    if((input$sex_var3 == "Males" & input$cancer_var3 %in% c("Cervix", "Breast"))){
      
    }
    
    if(input$sex_var3 == "Females" & input$cancer_var3 %in% c("Prostate")){
      
      
    }
    
    if((input$sex_var3 == "Males" & !(input$cancer_var3 %in% c("Cervix", "Breast"))) | (input$sex_var3 == "Females" & !(input$cancer_var3 == "Prostate"))){ #if not the above
      
      get.ZeroPolicyOption()
      set.ZeroPolicyOption(TRUE)
      
      nb <-
        poly2nb(myplot2, queen=TRUE)
      
      
      lw <-
        nb2listw(nb, style="W", zero.policy=TRUE)
      
      lmoran <-
        localmoran(myplot2$ASIR, lw,
                   alternative = "greater")
      
      # padronize the variable and save it to a new column
      myplot2$s_ASIR <- scale(myplot2$ASIR)  %>% as.vector()
      
      # create a spatially lagged variable and save it to a new column
      myplot2$lag_s_ASIR <- lag.listw(lw, myplot2$s_ASIR)
      
      # high-high quadrant
      myplot2 <-
        myplot2 %>%
        mutate(Cluster_Type = case_when(
          (s_ASIR >= 0 & lag_s_ASIR >= 0) & (lmoran[, 5] <= 0.05) ~ "high-high",
          (s_ASIR <= 0 & lag_s_ASIR <= 0) & (lmoran[, 5] <= 0.05) ~ "low-low",
          (s_ASIR >= 0 & lag_s_ASIR <= 0) & (lmoran[, 5] <= 0.05) ~ "high-high",
          (s_ASIR <= 0 & lag_s_ASIR >= 0) & (lmoran[, 5] <= 0.05) ~ "high-high",
          TRUE ~ "Not significant"
        )) %>%
        mutate(Cluster_Type = as.factor(Cluster_Type))
      
      legendtitle <- list(yref='paper', xref="paper", y=-0.3, x=0, text = "<b>Cluster Type</b>", showarrow = F, font = list(size = 14))
      
      # myplot2 %>% 
      #   st_drop_geometry() %>% 
      #   ggplot(., aes(x = s_ASIR, y = lag_s_ASIR, colour = Cluster_Type)) + 
      #   geom_point() + 
      #   geom_hline(yintercept = 0, linetype = 'dashed') + 
      #   geom_vline(xintercept = 0, linetype = 'dashed') +
      #   labs(
      #     title = "Local Moran Scatterplot",
      #     y = "Lagged ASIR",
      #        x = "ASIR",
      #     colour = "Cluster Type and\n Significance") +
      #   coord_fixed(ratio = 1) + 
      #   theme_bw(base_size = 14) +
      #   theme(
      #     panel.grid.major = element_blank(),
      #     panel.grid.minor = element_blank()
      #   ) +
      #   scale_colour_viridis_d()
      
      myplot2_stdrop <- 
      myplot2 %>% 
        st_drop_geometry()
      
      plot_ly(data = myplot2_stdrop, x = ~s_ASIR, y = ~lag_s_ASIR, type = 'scatter', mode = 'markers',
              text = ~REGION_Name, color = ~Cluster_Type,
              hovertemplate = paste('<b>Region Name</b>: %{text}'),
              colors = viridis_pal(option = "D")(myplot2 %>% st_drop_geometry() %>% distinct(Cluster_Type) %>% nrow),
              # size = 10,
              marker = list(size = 6,
                            line = list(color = 'black',
                                        width = 1))
      ) %>% 
        layout(title = 'Moran Scatterplot',
               yaxis = list(title = "<b>Lagged ASIR<b>"),
               xaxis = list(title = "<b>ASIR<b>"),
               legend = list(orientation = 'h', y = -0.3),
               annotations = legendtitle)
    }
    
    
  })
  
  
}
