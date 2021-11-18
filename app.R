
library(shiny)
library(shiny.router)
library(sf)
library(sp)
library(tidyverse)
library(rgdal)
library(leaflet)
library(spdep)
library(tmap)

# Global variables defineed which will be used in below functions
csv_file <- NULL
map_file <- NULL
mappy <- NULL
col_headers <- c()
items <- NULL


home_page <- div(
      mainPanel(
          fluidRow(
              h2("Project Motivation"),
              p("An important step in data science is to explore and investigate data before proceeding with any modeling, which helps to identify patterns and summarise the main characteristics of the data. The conventional Exploratory Data Analysis (EDA) investigates the relationship between data variables and how they affect each other. In contrast, Exploratory Spatial Data Analysis (ESDA) is nothing but an extension of EDA exploring how a variable correlates to a geographic location."),
              p("It is an important Geographic Information Science (GIS)-based technique that allows users to go beyond mapping to describe and visualise spatial distributions, identify spatial outliers and discover spatial patterns such as clusters or hot spots, allowing users to translate geospatial data into useful insights for decison making."),
              p("Especially with the latest technological advancements in Unmanned Aerial Vehicles (UAV), remote sensing and the increasing use of GPS-enabled smartphones and devices, the demand for location-based analytics is increasing in every industry with many organisations and multidisciplinary researchers relying on Geospatial Analytics for making decisions and deriving solutions. Therefore, there is a significant demand for free and open-source spatial analysis research infrastructure to enable more users (researchers, students, industry practitioners, non-GIS users) to adopt geospatial analytics and find the value in 'where'.")),
              
            
          
          
          fluidRow(
              h2("Project Objective"),
              p("GeoDa is one such software program built by Professor Luc Anselin and his team which allows users to perform an end-to-end spatial analysis exercise from data transformation, mapping and geovisualisation to ESDA, spatial autocorrelation analysis and finally spatial regression analysis."), 
              p("Therefore, using GeoDa as reference, our project objective is to build a flexible, robust, user-friendly and web-enabled geospatial tool - GeoXplorer - using R shiny application hosted on Shinyapp.io that allows users to conduct exploratory spatial data analysis on different use cases."),
          ),
          
          fluidRow( 
              h2("Application Features"),
              p("Dynamic and interactive tool that allows users to input various datasets to explore and model spatial patterns"),
              p("1. Compute Global Spatial Autocorrelation (GSA) statistics and plot spatial correlogram"),
              p("2. Compute Local Indicator of Spatial Association (LISA) statistics for detecting clusters and outliers"),
              p("3. Compute Getis-Ord's Gi-statistics for detecting hot spot or/and cold spot area")),
           

          fluidRow(
              h2("User Guide"),
              tags$a(href="https://github.com/jackwashere/IS415-GroupProjectWebsite/blob/main/IS415-GeoXplorer-user-guide-v2.pdf", "Click here to be redirected to our user guide!"))
            )
)



# This function is for inputting CSV data 
csvFileUI <- function(id, label = "CSV file") {
    ns <- NS(id)
    tagList(
        fileInput(ns("file"), label))
}

# This is the accompanying server function for inputting CSV data 
csvFileServer <- function(id, stringsAsFactors) {
    moduleServer(
        id,
        function(input, output, session) {
            userFile <- reactive({
                validate(need(input$file, message = FALSE))
                input$file
            })
            
            dataframe <- reactive({
                csv_file <- read_csv(userFile()$datapath)
                col_headers <<- colnames(csv_file)
                
                csv_file <<- dataclean_aspatial(csv_file)
                csv_file
            })
            
            observe({
                msg <- sprintf("File %s was uploaded", userFile()$datapath[1])
                cat(msg, "\n")
            })
            
           return(dataframe)
        }
    )    
}

# This function is for inputting geospatial data 
shpFileUI <- function(id, label = "SHP file") {
    ns <- NS(id)
    tagList(
        fileInput(ns("file"),
                  label = "Select all files (Inclusive of .shp, .dbf, .sbn, .sbx, .shx, .prj)",
                  multiple = TRUE,
                  accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj'))
    )
    
}

# This is the accompanying server function for inputting geospatial data 
shpFileServer <- function(id, stringsAsFactors) {
    moduleServer(
        id,
        function(input, output, session) {
          map_processing <- reactive({
                req(input$file)
                shpdf <- input$file
                tempdirname <- dirname(shpdf$datapath[1])
                # Rename files
                for (i in 1:nrow(shpdf)) {
                   file.rename(
                        shpdf$datapath[i],
                        paste0(tempdirname, "/", shpdf$name[i])
                    )
                   
                    name_splt <- strsplit(shpdf$name, "\\.")[[1]]
                }
                
                map_file <<- readOGR(dsn = tempdirname,
                                    layer =  toString(name_splt[1]),
                                    verbose = FALSE)
                
                map_file <<- dataclean_geospatial(map_file)
                
                
                #if (!is.null(csv_file)){
                #  data_join(map_file,csv_file)
                #}
                    
                return(map_file)
                
            })           
})}

# This function is called from the shpFileServer function for checking the invalid geometries of geospatial data 
dataclean_geospatial <- function (shp_data){
    shp_data = st_as_sf(shp_data)
    # Checking for invalid geometries
    if (length(which(st_is_valid(shp_data) == FALSE)) >0){
      shp_data <- st_make_valid(shp_data)
    }
    return(shp_data)
}

# This function is called from the csvFileServer function for cleaning aspatial data 
dataclean_aspatial <- function (csv_data){
    csv_data <- csv_data%>%drop_na()
    return(csv_data)
}

datajoinUI <- function(id, label = "SHP file") {
  ns <- NS(id)
  tagList(
    uiOutput("spatialjoin"),
    uiOutput("geospatialjoin"),
    submitButton("Submit")
  )
  
}

# This is the accompanying server function for joining data 
data_join_server <- function(id, stringsAsFactors) {
  
    moduleServer(
      id,
        function(input, output, session) {
            # in server()
            mappy_frame <- reactive({
              
              req(input$Selectvariable1)
              variable_Spatial <- input$Selectvariable1
              
              req(input$Selectvariable2)
              variable_Geospatial <- input$Selectvariable2
              
              names(map_file)[names(map_file) == variable_Geospatial] <- variable_Spatial
    
              mappy <<- left_join(map_file, csv_file)
              mappy <<- mappy%>%drop_na()
              return(mappy)  
            })
            
    })
}

# This function is for performing spatial autocorrelation 
Spatial_AC_UI <- function(id) {
    ns <- NS(id)
        #col_headers <<- names(names(csv_file))
    tagList(
        uiOutput("SpatialAutoParam"),
        radioButtons(ns("QORR"),"Select Contiguity", c("Queen","Rook")),
        radioButtons(ns("SpatialAutoTestType"),"Test Type",c("Moran's I","Geary's C")),
        sliderInput(ns("Simulation_count"),
                    label = "Number of Simulations",
                    min = 50,
                    max = 100,
                    value= 10),
        submitButton("Submit")
    )
    
}

# This is the accompanying server function for performing spatial autocorrelation  
Spatial_AC_Server <- function(id, stringsAsFactors) {
    moduleServer(
        id,
        function(input, output, session) {
           
            Spatial_AC_processing <- reactive({
                
                req(input$Variablesdropdown)
                variable_AC <- input$Variablesdropdown
                print(paste("Variable selected: ", variable_AC))
                variable_vector <- mappy[[variable_AC]]
                req(input$QORR)
                Q_OR_K <- input$QORR
                print(paste("Contiguity selected: ", Q_OR_K))
                req(input$SpatialAutoTestType)
                Test_type <- input$SpatialAutoTestType
                print(paste("Test type selected: ", Test_type))
                
                nsim_value <- input$Simulation_count
                print(paste("No. of Simulations selected for Moran's I: ", nsim_value))
                
                
        
                if(is.numeric(variable_vector) == TRUE){
                  if (Q_OR_K == "Queen"){
                    weight_matrix_q <- poly2nb(mappy, 
                                               queen=TRUE)
                    weight_matrix <- nb2listw(weight_matrix_q, 
                                              style="W", 
                                              zero.policy = TRUE)
                    
                    MI_corr <- sp.correlogram(weight_matrix_q, 
                                              variable_vector, 
                                              order=6, 
                                              method="I", 
                                              style="W")
                    Spatial_Correlogram <- MI_corr
                    
                    print("------------------ MI Corr ---------------------------------------------------")
                    print(MI_corr)
                    
                  }
                  else{
                    weight_matrix_r <- poly2nb(mappy, 
                                               queen=FALSE)
                    weight_matrix <- nb2listw(weight_matrix_r, 
                                              style="W", 
                                              zero.policy = TRUE)
                    GC_corr <- sp.correlogram(weight_matrix_r, 
                                              variable_vector, 
                                              order=6, 
                                              method="C", 
                                              style="W")
                    Spatial_Correlogram <- GC_corr
                    
                    print("------------------ GC Corr ---------------------------------------------------")
                    print(GC_corr)
                  }
                  
                  print("------------------ Weight Matrix ---------------------------------------------")
                  print(weight_matrix)
                  
                  
                  
                  if(Test_type == "Moran's I"){
                    
                    print("------------------ Moran's I -------------------------------------------------")
                    moran_results <- moran.test(variable_vector, #hardcoding this
                                                listw=weight_matrix, 
                                                zero.policy = TRUE, 
                                                na.action=na.omit)
                    print(moran_results)
                    
                    print("------------------ Monte Carlo -----------------------------------------------")
                    
                    set.seed(1234)
                    bperm= moran.mc(variable_vector, 
                                    listw=weight_matrix, 
                                    nsim=nsim_value, 
                                    zero.policy = TRUE, 
                                    na.action=na.omit)
                    print(bperm)
                    print(paste("Mean: ",mean(bperm$res[1:nsim_value])))
                    print(paste("Variance: ", var(bperm$res[1:nsim_value])))
                    print("Summary:")
                    print(summary(bperm$res[1:nsim_value]))
                    
                    return(Spatial_Correlogram)
                    
                    
                  }
                  
                  else{
                    
                    print("------------------ Geary's C -------------------------------------------------")
                    geary_results <- geary.test(variable_vector, listw=weight_matrix)
                    print(geary_results)
                   
                    print("------------------ Monte Carlo -----------------------------------------------")
                    
                    set.seed(1234)
                    bperm=geary.mc(variable_vector, 
                                   listw=weight_matrix, 
                                   nsim=nsim_value)
                    print(bperm)
                    print(paste("Mean: ",mean(bperm$res[1:nsim_value])))
                    print(paste("Variance: ", var(bperm$res[1:nsim_value])))
                    print("Summary:")
                    print(summary(bperm$res[1:nsim_value]))
                    
                    
                    
                    return(Spatial_Correlogram)
                  }
                }else
                  print("Error: Variable selected for Global Spatial Autocorrelation is non-numeric, please select a numeric variable")
                
            })           
})}

# This function is for performing Hot & Cold spot analysis 
Hot_Cold_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput("HCParam_1"),
    uiOutput("HCParam_2"),
    selectInput(ns("Distance"), "Distance weight matrix", c(
      "None",
      "Fixed",
      "Adaptive"
    )),
    submitButton("Submit")
  )
  
}

# This is the accompanying server function for performing Hot & Cold spot analysis  
Hot_Cold_Server <- function(id, stringsAsFactors) {
  
  moduleServer(
    id,
    function(input, output, session) {
      Hot_Cold_processing <- reactive({
        req(input$Distance)
        weightmatrix_type <- input$Distance
        
        req(input$Selectvariable_1)
        variable_order <- input$Selectvariable_1
        variable_order_vector <- mappy[[variable_order]]

        
        req(input$Selectvariable_2)
        variable_analysis <- input$Selectvariable_2
        variable_analysis_vector <- mappy[[variable_analysis]]
        
        
        if (is.numeric(variable_analysis_vector) == TRUE){
          
          longitude <- map_dbl(mappy$geometry, ~st_centroid(.x)[[1]]) 
          latitude <- map_dbl(mappy$geometry, ~st_centroid(.x)[[2]])
          coords <- cbind(longitude, latitude)
          req(coords)
          if (weightmatrix_type == "Fixed"){
            
            print("------------------ Determining Cut-off Distance ------------------------------")
            k1 <- knn2nb(knearneigh(coords))
            k1dists <-  unlist(nbdists(k1, coords, longlat = TRUE))
            print(summary(k1dists))
            
            print("------------------ Determining Fixed Distance Weight Matrix ------------------")
           
            max_value <- round(max(k1dists))
            
            fixed_wm <- dnearneigh(coords, 0, max_value, longlat = TRUE)
            fixed_wm_lw <- nb2listw(fixed_wm, style = 'B')
            print(summary(fixed_wm_lw)) #figure out why it is printing out twice
            
            fips <- order(variable_order_vector) #need to let user select this district/Variable
            gi.fixed <- localG(variable_analysis_vector, fixed_wm_lw)
            mappy.gi <- cbind(mappy, as.matrix(gi.fixed)) %>%
              rename(gstat_fixed = as.matrix.gi.fixed.)
            
            variable_analysis_plot <- qtm(mappy, variable_analysis)
            
            Gimap <-tm_shape(mappy.gi) +
              tm_fill(col = "gstat_fixed", 
                      style = "pretty",
                      palette="-RdBu",
                      title = "local Gi") +
              tm_borders(alpha = 0.5)
            
            plotting_maps <- tmap_arrange(variable_analysis_plot, Gimap, asp=1, ncol=2)
            return(plotting_maps)
            
          }
          else{
            
            print("------------------ Determining Adaptive Distance Weight Matrix ---------------")
            knn <- knn2nb(knearneigh(coords, k=8)) #let user choose value of k = 8
            knn_lw <- nb2listw(knn, style = 'B')
            print(summary(knn_lw))
            
            fips <- order(variable_order_vector)
            gi.adaptive <- localG(variable_analysis_vector, knn_lw)
            mappy.gi <- cbind(mappy, as.matrix(gi.adaptive)) %>%
              rename(gstat_adaptive = as.matrix.gi.adaptive.)
            
            variable_analysis_plot <- qtm(mappy, variable_analysis)
            
            Gimap <- tm_shape(mappy.gi) +
              tm_fill(col = "gstat_adaptive", 
                      style = "pretty",
                      palette="-RdBu",
                      title = "local Gi") +
              tm_borders(alpha = 0.5)
            
            plotting_maps <- tmap_arrange(variable_analysis_plot, Gimap, asp=1, ncol=2)
            return(plotting_maps)
            
          }
         
        }
        else{
          print("Error: Variable selected for Hot & Cold spot analysis is non-numeric, please select a numeric variable")
        }
        
        
      })    
      
    })}

Cluster_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput("ClusterParam_1"),
    uiOutput("ClusterParam_2"),
    radioButtons(ns("QORR"),"Select Contiguity", c("Queen","Rook")),
    selectInput(ns("Sig_level"), "Choose Significance level (%)", c(
      "1",
      "5",
      "10"
    )),
    submitButton("Submit")
  )
  
}

Cluster_Server <- function(id, stringsAsFactors) {
  moduleServer(
    id,
    function(input, output, session) {
      
      Cluster_processing <- reactive({
        
        req(input$Selectvariable_1)
        variable_order <- input$Selectvariable_1
        variable_order_vector <- mappy[[variable_order]]
        
        
        req(input$Selectvariable_2)
        variable_analysis <- input$Selectvariable_2
        variable_analysis_vector <- mappy[[variable_analysis]]
        
        
        req(input$QORR)
        Q_OR_K <- input$QORR
        
        req(input$Sig_level)
        Siglevel <- as.numeric(input$Sig_level)/100
        
        
          if (Q_OR_K == "Queen"){
            weight_matrix_q <- poly2nb(mappy, #changed from map_file
                                       queen=TRUE)
            weight_matrix <- nb2listw(weight_matrix_q, 
                                      style="W", 
                                      zero.policy = TRUE)
            
          }
          else{
            weight_matrix_r <- poly2nb(mappy, #changed from map_file
                                       queen=FALSE)
            weight_matrix <- nb2listw(weight_matrix_r, 
                                      style="W", 
                                      zero.policy = TRUE)
          }
        
        if(is.numeric(variable_analysis_vector) == TRUE){
          
          fips <- order(variable_order_vector)
          localMI <- localmoran(variable_analysis_vector, weight_matrix)
          
          Map.localMI <- cbind(mappy,localMI) %>%
            rename(Pr.Ii = Pr.z....E.Ii..)
          
          localMI.map <- tm_shape(Map.localMI) +
            tm_fill(col = "Ii", 
                    style = "pretty", 
                    title = "local moran statistics") +
            tm_borders(alpha = 0.5)
          
          pvalue.map <- tm_shape(Map.localMI) +
            tm_fill(col = "Pr.Ii", 
                    breaks=c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                    palette="-Blues", 
                    title = "local Moran's I p-values") +
            tm_borders(alpha = 0.5)
        
         
        quadrant <- vector(mode="numeric",length=nrow(localMI))
        DV <- variable_analysis_vector - mean(variable_analysis_vector) 
        C_mI <- localMI[,1] - mean(localMI[,1]) 
        quadrant[DV >0 & C_mI>0] <- 4      
        quadrant[DV <0 & C_mI<0] <- 1      
        quadrant[DV <0 & C_mI>0] <- 2
        quadrant[DV >0 & C_mI<0] <- 3
        quadrant[localMI[,5]>Siglevel] <- 0
        
        variable_analysis_plot <- qtm(mappy, variable_analysis)
        
        Map.localMI$quadrant <- quadrant
        colors <- c("#ffffff", "#2c7bb6", "#abd9e9", "#fdae61", "#d7191c")
        clusters <- c("insignificant", "low-low", "low-high", "high-low", "high-high")
        
        LISAmap <- tm_shape(Map.localMI) +
          tm_fill(col = "quadrant", 
                  style = "cat", 
                  palette = colors[c(sort(unique(quadrant)))+1], 
                  labels = clusters[c(sort(unique(quadrant)))+1],
                  popup.vars = c("")) +
          tm_view(set.zoom.limits = c(11,17)) +
          tm_borders(alpha=0.5)
        
        plotting_maps <- tmap_arrange(localMI.map, pvalue.map, variable_analysis_plot, LISAmap, asp =1, ncol=2, nrow = 2)
          
        return(plotting_maps)
         
          }
          
          else{
            print("Error: Variable selected for Cluster analysis is non-numeric, please select a numeric variable")
          }
      })           
    })}

data_page <- div(
    
    titlePanel("Data"),
    p("Add your aspatial and geospatial data in the below subsections before proceeding to perform the relational join"),
    p("Note: Ensure your datasets are pre-cleaned and have a common variable for performing relational join"),
    
    tabsetPanel(
      id="HelloWorld",
      tabPanel("Aspatial",
        sidebarPanel(
          csvFileUI("datafile", "Aspatial data (.csv format)")
        ),
        mainPanel(
          dataTableOutput("table")
        ),
      ),
      
      tabPanel("Geospatial",
        sidebarPanel(
          shpFileUI("mapfile", "Geospatial data (.shp format)")
        ),
        mainPanel(
          plotOutput("Plot")
        )
      ),
      
      tabPanel("Joined Data",
          sidebarPanel(
            datajoinUI("pr_join", "Performing relational join")
               ),
         mainPanel(
           plotOutput("PlotMappy")
         )
      )
    )
)

CA_page <- div(
    titlePanel("Cluster & Outlier Analysis"),
    p("Identify the Clusters and Outliers using the Local Indicator of Spatial Association (LISA) statistical method: Local Moran's I"),
    p("- An outlier: significant and negative if location i is associated with relatively low values in surrounding locations."),
    p("- A cluster: significant and positive if location i is associated with relatively high values in surrounding locations."),
    p("Note: Ensure the analysis variable selected is numeric."),
    sidebarLayout(
      sidebarPanel(
        Cluster_UI("Cluster")
      ),
      mainPanel(
        verticalLayout(
          plotOutput("Plot5", width = "100%"),
          verbatimTextOutput("Plot_e"))
        )))
    

HC_page <- div(
    titlePanel("Hot Spot & Cold Spot Analysis"),
    p("Identify the Clusters and Outliers using spatial statistic method called Getis-Ord Gi* statistic"),
    p("- A hot spot area: significant and positive if location i is associated with relatively high values in surrounding locations."),
    p("- A cold spot area: significant and negative if location i is associated with relatively low values in surrounding locations."),
    p("Note: Ensure the analysis variable selected is numeric."),
    sidebarLayout(
      sidebarPanel(
        Hot_Cold_UI("Hot_Cold1")
        
      ),
      mainPanel(
        verticalLayout(
          plotOutput("Plot4")),
          verbatimTextOutput("Plot3")
          
        
      )
    )
)

SA_page <- div(
    titlePanel("Spatial Autocorrelation"),
    p("Perform global spatial autocorrelation using statistic methods: Moran's I and Geary's C"),
    p("Note: Ensure the analysis variable selected is numeric."),
    sidebarLayout(
        sidebarPanel(
            Spatial_AC_UI("Spatial_AC")
            
        ),
        mainPanel(
          verticalLayout(
            verbatimTextOutput("Plot1"),
            plotOutput("Plot2"))
        )
    )
)


ui <- fluidPage(
    navbarPage(title = "GeoXplorer",
               tabPanel("Home", home_page),
               tabPanel("Data", data_page),
               tabPanel("Cluster Analysis", CA_page),
               tabPanel("Hot & Cold Spot", HC_page),
               tabPanel("Spatial Autocorrelation", SA_page)),
)


server <- function(input, output, session) {
    datafile <- csvFileServer("datafile", stringsAsFactors = FALSE)
    output$table <- renderDataTable({
        datafile()
       
    })
    
    mapfile <- shpFileServer("mapfile", stringsAsFactors = FALSE)
    output$Plot <- renderPlot({
        req(mapfile())
        plot(mapfile(), max.plot = 35)
    })
    
    output$spatialjoin <- renderUI({
      
      ns <- NS("pr_join")
      df <- datafile()
      if (is.null(df)) return(NULL)
      items <<- names(df)
      names(items) <-items
      
      selectInput(ns("Selectvariable1"), "Select the Spatial Variable for relational join",items)
    })
    
    output$geospatialjoin <- renderUI({
      
      ns <- NS("pr_join")
      df <- mapfile()
      if (is.null(df)) return(NULL)
      items <<- names(df)
      names(items) <-items
      
      selectInput(ns("Selectvariable2"), "Select the Geospatial Variable for relational join",items)
    })
    
    joined_data <- data_join_server("pr_join", stringsAsFactors = FALSE)
    #for joined map
    output$PlotMappy <- renderPlot({
      #joined_data()
      req(joined_data())
      plot(joined_data(), max.plot = 35)
    })
    
    
   output$SpatialAutoParam <- renderUI({
        ns <- NS("Spatial_AC")
        df <- datafile()
        if (is.null(df)) return(NULL)
           items <<- names(df)
           names(items) <-items
           
        selectInput(ns("Variablesdropdown"), "Variables:",items)
  })
    
   
    Spatial_AC <- Spatial_AC_Server("Spatial_AC", stringsAsFactors = FALSE)
    
    output$Plot1 <- renderPrint({
        req(Spatial_AC())
        Spatial_AC()
        
    })
    
    output$Plot2 <- renderPlot({
      req(Spatial_AC())
      plot(Spatial_AC())
      
    })
    
    
    output$HCParam_1 <- renderUI({
      ns <- NS("Hot_Cold1")
      df <- datafile()
      if (is.null(df)) return(NULL)
      items <<- names(df)
      names(items) <-items
      print(items)
      selectInput(ns("Selectvariable_1"), "Select the Order Variable",items)
    })
    
    output$HCParam_2 <- renderUI({
      ns <- NS("Hot_Cold1")
      df <- datafile()
      if (is.null(df)) return(NULL)
      items <<- names(df)
      names(items) <-items
      print(items)
      selectInput(ns("Selectvariable_2"), "Select the Analysis Variable",items)
    })
    
    Hot_Cold <- Hot_Cold_Server("Hot_Cold1", stringsAsFactors = FALSE)
    output$Plot3 <- renderPrint({
      req(Hot_Cold())
      Hot_Cold()
      
    })
    
    
    output$Plot4 <- renderPlot({
      Hot_Cold()
    })
    
    
    
    output$ClusterParam_1 <- renderUI({
      ns <- NS("Cluster")
      df <- datafile()
      if (is.null(df)) return(NULL)
      items <<- names(df)
      names(items) <-items
      print(items)
      selectInput(ns("Selectvariable_1"), "Select the Order Variable",items)
    })
    
    output$ClusterParam_2 <- renderUI({
      ns <- NS("Cluster")
      df <- datafile()
      if (is.null(df)) return(NULL)
      items <<- names(df)
      names(items) <-items
      print(items)
      selectInput(ns("Selectvariable_2"), "Select the Analysis Variable",items)
    })
    
    Cluster <- Cluster_Server("Cluster", stringsAsFactors = FALSE)
    output$Plot_e <- renderPrint({
      req(Cluster())
      Cluster()
    })
    
    output$Plot5 <- renderPlot({
      Cluster()
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
