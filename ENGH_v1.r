### ENGH

library(shiny)
library(shinydashboard)
library(ggthemes)
library(ggplot2)


library(dplyr)
library(DT)
library(DBI)
library(data.table)
library(DT)
library(stringr)
library(reshape2)

cod_eq = read.table("C:/Users/Sherm4n/Downloads/codigos_engho2012/ENGHo - Códigos de equipamiento.txt", sep = "|", header = TRUE)
cod_prov = read.table("C:/Users/Sherm4n/Downloads/codigos_engho2012/ENGHo - Provincias.txt", sep = "|", header = TRUE)
cod_reg = read.table("C:/Users/Sherm4n/Downloads/codigos_engho2012/ENGHo - Regiones.txt", sep = "|", header = TRUE)
cod_subreg = read.table("C:/Users/Sherm4n/Downloads/codigos_engho2012/ENGHo - Subregiones.txt", sep = "|", header = TRUE)
cod_bys = read.table("C:/Users/Sherm4n/Downloads/codigos_engho2012/ENGHo_Codigos_de_bienes_y_servicios.txt", sep = "|", header = TRUE)


d_hogares = read.table("C:/Users/Sherm4n/Downloads/bases_datos_engho2012/ENGHo - Hogares.txt", sep="|", header = TRUE)




runApp(host="0.0.0.0", port=5070,list(
  
  ui = dashboardPage(skin="yellow",
                     dashboardHeader(
                       title = "Dash Title"
                       
                     ),
                     dashboardSidebar(
                       sidebarMenu(id = "menu01",
                                   h4("Filters"),
                                   selectizeInput("select_provincia", "Select Provincia", choices = unique(cod_prov$DESCRIPCION)[! unique(cod_prov$DESCRIPCION) %in% c("Nivel central-INDEC")], selected = unique(cod_prov$DESCRIPCION)[! unique(cod_prov$DESCRIPCION) %in% c("Nivel central-INDEC")], multiple = TRUE, options = list(maxItems = 40))

                                   
                       )
                       
                     ),
                     dashboardBody(
                       
                       tabsetPanel(
                         tabPanel("Summary",
                                  h3("Text"),
                                  uiOutput("render_t_hogares_provincia_ui"),
                                  uiOutput("render_df_hogares_provincia_gastos_ui")

                                  )
                                )
                         
                              )
                     
                     ),
  
  
  server = function(input, output) {
    
    selected_prov_desc = reactive({
      
      d = input$select_provincia
      d = data.frame(d)
      # unique.cod_prov.DESCRIPCION.
      # 1               CIUDAD DE BS AS

      
      d = merge(d[,1], cod_prov[,c("DESCRIPCION","PROVINCIA")], by.x = 1, by.y = 1)
      
      d = d[,2]
      d
      
      # print(d)
      
    })
    
    
    df_main = reactive({
      
      
      
      d = d_hogares
      d = d[d$PROVINCIA %in% selected_prov_desc(),]
      d
      
    })
    
    
    df_t_hogares_provincia = reactive({
      
      d = df_main()
      d = merge(data.frame(table(d$PROVINCIA)), cod_prov, by.x = "Var1",by.y = "PROVINCIA")
      d = d[order(d$Freq, decreasing = TRUE),]
      # print(head(d))
      # Var1 Freq  DESCRIPCION REGION SUBREGION
      # 15    6 2321 BUENOS AIRES      2        2C
      d = d[,c("DESCRIPCION", "REGION", "SUBREGION", "Freq")]
      names(d)[4] = "Counts"
      
      d
      
      
      
    })
    
    output$render_t_hogares_provincia = renderDataTable({
      
      d = df_t_hogares_provincia()

      
      
      d
      
    })
    
    output$render_t_hogares_provincia_ui = renderUI({
      
      dataTableOutput("render_t_hogares_provincia")
      
    })
    
    df_t_hogares_provincia_gastos = reactive({
      
      d = df_main()
      # d = ###
      d = aggregate(d, by = list(d$PROVINCIA), FUN=sum)
      d
      
    })
    
    output$render_df_t_hogares_provincia_gastos = renderDataTable({
      
      df_t_hogares_provincia_gastos()
      
    })
    
    output$render_df_hogares_provincia_gastos_ui = renderUI({
      
      dataTableOutput("render_df_t_hogares_provincia_gastos")
      
    })
    
    
  }
))

