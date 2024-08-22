library(shiny)
library(shinydashboard)
library(tidyverse)
library(bslib)
library(plotly)
library(echarts4r)
library(reactable)
library(reactablefmtr)

sales_data_sample <- read_csv("sales_data_sample.csv")
df_poblacion <- read_rds("Estimaciones_y_Proyecciones_Municipales_2015_2035.RData") %>% 
  dplyr::select(-`2015`, -`2016`, -`2017`, -`2018`, -`2019`, -`2020`, -`2021`, -`2022`)

sales_data_sample$ORDERDATE <- as.Date(sales_data_sample$ORDERDATE, format = "%m/%d/%Y")
sales_data_sample$MONTH_FLOOR <- lubridate::floor_date(sales_data_sample$ORDERDATE, unit = "month")

verde <- "forestgreen"
amarillo <- "gold"
rojo <- "firebrick"
naranja <- "orange"
azul <- "royalblue4"
celeste <- "cornflowerblue"
color_spinner <- "#0dc5c1"

# Sales YoY ----
Year_over_Year_Sales <- sales_data_sample %>% 
  dplyr::select(SALES, YEAR_ID) %>% 
  dplyr::group_by(YEAR_ID) %>% 
  dplyr::summarise(SALES_YEAR = sum(SALES), .groups = "drop")

# Country YoY ----
Country_Year_over_Year <- sales_data_sample %>% 
  dplyr::select(SALES, YEAR_ID, COUNTRY) %>% 
  dplyr::group_by(YEAR_ID, COUNTRY) %>% 
  dplyr::summarise(SALES_YEARLY = sum(SALES), .groups = "drop") %>% 
  dplyr::arrange(COUNTRY)

# Sales Monthly
Sales_Monthly <- sales_data_sample %>% 
  dplyr::select(SALES, YEAR_ID, MONTH_ID) %>% 
  dplyr::group_by(YEAR_ID, MONTH_ID) %>% 
  dplyr::summarise(SALES_MONTH = sum(SALES), .groups = "drop")
Sales_Monthly$YEAR_ID <- as.character(Sales_Monthly$YEAR_ID)

# Customer ----
Customer_Year_over_Year <- sales_data_sample %>% 
  dplyr::select(SALES, YEAR_ID, CUSTOMERNAME) %>% 
  dplyr::group_by(YEAR_ID, CUSTOMERNAME) %>% 
  dplyr::summarise(SALES_YEARLY = sum(SALES), .groups = "drop") %>% 
  dplyr::arrange(CUSTOMERNAME) #%>% 
#tidyr::pivot_wider(names_from = YEAR_ID, values_from = SALES_YEARLY, values_fill = 0)

# cantidad de Clientes por ciudad
 Cantidad_Clientes_por_ciudad <- sales_data_sample %>% 
  dplyr::distinct(CUSTOMERNAME, CITY, .keep_all = TRUE) %>% 
  dplyr::select(CUSTOMERNAME, CITY) %>% 
  dplyr::group_by(CITY) %>% 
  dplyr::summarise(Clientes_x_Ciudad = n(), .groups = "drop")
 
 Listado_Clientes_por_ciudad <- sales_data_sample %>% 
   dplyr::distinct(CUSTOMERNAME, CITY, .keep_all = TRUE) %>% 
   dplyr::select(CUSTOMERNAME, CITY) %>% 
   dplyr::group_by(CUSTOMERNAME, CITY) %>% 
   dplyr::summarise(.groups = "drop")

# cual es el producto que mas se vende
 # tengo que sacar la suma de quantityordered por productcode por año
 Quantity_ProducCode_per_year <- sales_data_sample %>% 
   dplyr::select(YEAR_ID, QUANTITYORDERED, PRODUCTCODE) %>% 
   dplyr::group_by(YEAR_ID, PRODUCTCODE) %>% 
   dplyr::summarise(QUANTITY = sum(QUANTITYORDERED), .groups = "drop") 
   
 #
 df_tabla_revenue_producto_cliente <- sales_data_sample %>% 
   dplyr::select(YEAR_ID, MONTH_ID, SALES, CUSTOMERNAME, PRODUCTCODE) %>% 
   dplyr::group_by(YEAR_ID, MONTH_ID, CUSTOMERNAME, PRODUCTCODE) %>% 
   dplyr::summarise(Revenue = sum(SALES), .groups = "drop")
 
 # tengo que sacar la suma de quantityordered por productcode por año por cliente
 # tengo que sacar la suma de quantityordered por productcode por año por mes
 
# cual es la linea que mas se vende
 #por cliente por año
 
 
  


# UI ----
ui <- bslib::page_navbar(
  title = "Sales Dashboard",
  collapsible = FALSE,
  fillable = c("Resumen"),
  # sidebar = FALSE,
  nav_panel("Resumen",
            bslib::layout_columns(
              card(
                card_header("Comparativo ventas año contra año"),
                plotOutput("grafica_Year_over_Year_Sales")
              ),
              card(
                card_header("Comparativo ventas por pais ultimos 3 años"),
                reactableOutput("tabla_comparativo_pais_3_años")
              ),
              card(
                card_header("Comparativo ventas ultimos 3 años"),
                echarts4rOutput("grafica_mes_vrs_mes_ultimos_3_años")
              ),
              col_widths = c(6, 6, 12),
              #row_heights = c(2)
            )
  ),
  nav_panel("Clientes",
            bslib::layout_columns(
              card(
                card_header("Top 5 Clientes este Año"),
                plotOutput("graf_top_5_clientes")
              ),
              card(
                card_header("Comparativo ultimos 3 años Clientes"),
                reactableOutput("tabla_comparivo_top_clientes")
              ),
              fluidRow(selectInput("seleccionar_cliente", "Seleccione Cliente", choices = "")),
              card(
                card_header("Revenue por producto mensual"),
                reactableOutput("tabla_revenue_producto_cliente")
              ),
              col_widths = c(6, 6, 3, 12),
              #row_heights = c(2)
            )
  ),
  nav_panel("Poblacion Guatemala",
            bslib::layout_columns(
              card(
                card_header("Poblacion Guatemala proyeccion 2035 INE"),
                reactableOutput("tabla_poblacion_Guate_2035")
               )#,
              # card(
              #   card_header("Comparativo ultimos 3 años Clientes"),
              #   reactableOutput("tabla_comparivo_top_clientes")
              # ),
              # fluidRow(selectInput("seleccionar_cliente", "Seleccione Cliente", choices = "")),
              # card(
              #   card_header("Revenue por producto mensual"),
              #   reactableOutput("tabla_revenue_producto_cliente")
              # ),
              # col_widths = c(6, 6, 3, 12),
              #row_heights = c(2)
            )
  )
  
)

# SERVER ----
server <- function(input, output, session){
  
  # observes ----
  observe({
    updateSelectInput(session,
                      "seleccionar_cliente", choices = unique(sales_data_sample$CUSTOMERNAME))
  })
  
  # reactive tables ----
  df_tabla_revenue_producto_cliente_reactive <- reactive({
    a <- df_tabla_revenue_producto_cliente %>% 
      dplyr::filter(CUSTOMERNAME == input$seleccionar_cliente)
    a
  })
  
  # output graficas ----
  ## graf Sales YoY ----
  output$grafica_Year_over_Year_Sales <- renderPlot({
    df_grafica_Year_over_Year_Sales <-  Year_over_Year_Sales %>% 
      dplyr::filter(YEAR_ID >= max(Year_over_Year_Sales$YEAR_ID)-1)
    Revenue_this_year <- df_grafica_Year_over_Year_Sales %>% 
      dplyr::pull(SALES_YEAR) %>% 
      max()
    
    df_grafica_Year_over_Year_Sales %>% 
      ggplot2::ggplot() +
      ggplot2::aes(x= as.character(YEAR_ID), y=SALES_YEAR) +
      ggplot2::geom_bar(stat = "identity", fill = celeste) +
      ggplot2::geom_text(aes(label =  paste0("$ ", prettyNum(SALES_YEAR, big.mark = "," , scientific = FALSE  ))), y = Revenue_this_year/2,  size = 5) +
      ggplot2::theme(strip.text = element_text(size = 20),
                     axis.ticks.x = element_blank(), #quito tick eje Y
                     panel.grid.major.y = element_blank(), #quito grilla Y mayor
                     axis.text.x = element_blank(), # quito los numeros del eje Y
                     panel.grid.minor.x = element_blank(), #quito grilla X menor
                     panel.grid.major.x = element_blank(), #quito grilla X mayor
                     panel.grid.minor.y = element_blank(),
                     panel.background = element_blank(),  #quito grilla Y menor
                     axis.text.y =  element_text(size = 20)) +
      ggplot2::coord_flip() +
      xlab("") +
      ylab("Revenue Anual por Ventas")
    
  })
  
  ## graf country YoY ----
  output$tabla_comparativo_pais_3_años <- renderReactable({
    pal_scale <- c("#E9DAEC", "#A270E5", "#43009A")
    
    Country_Year_over_Year_wider <- Country_Year_over_Year %>% 
      tidyr::pivot_wider(names_from = YEAR_ID,
                         values_from = SALES_YEARLY,
                         values_fill = 0)
    Country_Year_over_Year_wider %>% 
      reactable::reactable(searchable = TRUE,
                           showPageSizeOptions = TRUE,
                           theme = reactableTheme(
                             style = list(fontFamily = "sans-serif"),
                             borderColor = "#DADADA"),
                           pageSizeOptions = c(10, 25, 50, 100),
                           defaultPageSize = 10,
                           defaultColDef = colDef(
                             vAlign = "center",
                             align = "center",
                             headerVAlign = "center",
                             style = color_scales(Country_Year_over_Year_wider, span = 2:4, colors = pal_scale)),
                           columns = list(
                             COUNTRY = colDef(
                               name = "Pais",
                               style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1, borderRight = "1px solid #eee"),
                               headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1, borderRight = "1px solid #eee")
                             ),
                             `2003` = colDef(
                               format = colFormat(prefix= "$", separators = TRUE)
                             ),
                             `2004` = colDef(
                               format = colFormat(prefix= "$", separators = TRUE)
                             ),
                             `2005` = colDef(
                               format = colFormat(prefix= "$", separators = TRUE)
                             )
                           ))
  })
  
  ## graf Monthly Sales ----
  
  output$grafica_mes_vrs_mes_ultimos_3_años <- renderEcharts4r({
    Sales_Monthly %>% 
      dplyr::group_by(YEAR_ID) %>% 
      echarts4r::e_charts(x = MONTH_ID) %>% 
      echarts4r::e_line(serie = SALES_MONTH, smooth = TRUE) %>% 
      echarts4r::e_tooltip(trigger = "axis") %>%  # tooltip
      echarts4r::e_legend(bottom = 0) %>% # move legend to the bottom
      echarts4r::e_labels(fontSize = 10) %>% 
      echarts4r::e_y_axis(axisLabel = list(fontSize = 10)) %>%  #coloco tamano de letra a cero para hacer desaparecer los valor del eje Y
      echarts4r::e_hide_grid_lines() %>% 
      echarts4r::e_x_axis(type = "category", axisLabel = list(interval = 0, rotate = 0))
  })
  
  ## graf customers ----
  output$graf_top_5_clientes <- renderPlot({
    a <- Customer_Year_over_Year %>% 
      dplyr::filter(YEAR_ID  == max(YEAR_ID)) %>% 
      dplyr::arrange(desc(SALES_YEARLY)) %>% 
      head(5)
    
    a %>% 
      ggplot2::ggplot() +
      ggplot2::aes(x= reorder(CUSTOMERNAME, SALES_YEARLY), y=SALES_YEARLY) +
      ggplot2::geom_bar(stat = "identity", fill = celeste) +
      ggplot2::geom_text(aes(label =  paste0("$ ", prettyNum(SALES_YEARLY, big.mark = "," , scientific = FALSE  ))), y = max(a$SALES_YEARLY)/2,  size = 5) +
      ggplot2::theme(strip.text = element_text(size = 20),
                     axis.ticks.x = element_blank(), #quito tick eje Y
                     panel.grid.major.y = element_blank(), #quito grilla Y mayor
                     axis.text.x = element_blank(), # quito los numeros del eje Y
                     panel.grid.minor.x = element_blank(), #quito grilla X menor
                     panel.grid.major.x = element_blank(), #quito grilla X mayor
                     panel.grid.minor.y = element_blank(),
                     panel.background = element_blank(),  #quito grilla Y menor
                     axis.text.y =  element_text(size = 20)) +
      ggplot2::coord_flip() +
      xlab("") +
      ylab("Revenue Anual por Ventas")
  })
  
  output$tabla_comparivo_top_clientes <- renderReactable({
    pal_scale <- c("#E9DAEC", "#A270E5", "#43009A")
    
    Customer_Year_over_Year_wider <- Customer_Year_over_Year %>% 
      tidyr::pivot_wider(names_from = YEAR_ID,
                         values_from = SALES_YEARLY,
                         values_fill = 0)
    Customer_Year_over_Year_wider %>% 
      reactable::reactable(searchable = FALSE,
                           showPageSizeOptions = TRUE,
                           theme = reactableTheme(
                             style = list(fontFamily = "sans-serif"),
                             borderColor = "#DADADA"),
                           pageSizeOptions = c(10, 25, 50, 100),
                           defaultPageSize = 10,
                           defaultColDef = colDef(
                             vAlign = "center",
                             align = "center",
                             headerVAlign = "center",
                             style = color_scales(Customer_Year_over_Year_wider, span = 2:4, colors = pal_scale)),
                           columns = list(
                             CUSTOMERNAME = colDef(
                               name = "Cliente",
                               style = list(position = "sticky", left = 0, background = "#fff", zIndex = 1, borderRight = "1px solid #eee"),
                               headerStyle = list(position = "sticky", left = 0, background = "#fff", zIndex = 1, borderRight = "1px solid #eee")
                             ),
                             `2003` = colDef(
                               format = colFormat(prefix= "$", separators = TRUE)
                             ),
                             `2004` = colDef(
                               format = colFormat(prefix= "$", separators = TRUE)
                             ),
                             `2005` = colDef(
                               format = colFormat(prefix= "$", separators = TRUE)
                             )
                             ))
  })
  
  output$tabla_revenue_producto_cliente <- renderReactable({
    
    # sales_data_sample %>% 
    #   dplyr::select(YEAR_ID, MONTH_ID, SALES, CUSTOMERNAME, PRODUCTCODE) %>% 
    #   dplyr::group_by(YEAR_ID, MONTH_ID, CUSTOMERNAME, PRODUCTCODE) %>% 
    #   dplyr::summarise(Revenue = sum(SALES), .groups = "drop") %>% 
    #   dplyr::filter(CUSTOMERNAME == "La Rochelle Gifts") %>% 
    #   dplyr::filter(YEAR_ID == 2005) %>% 
    #   dplyr::select(-YEAR_ID, -CUSTOMERNAME) %>% 
    #   tidyr::pivot_wider(names_from = MONTH_ID,
    #                      values_from = Revenue,
    #                      values_fill = 0)
    
    
    b <- df_tabla_revenue_producto_cliente_reactive() %>%
      
      dplyr::filter(YEAR_ID == max(sales_data_sample$YEAR_ID)) %>% 
      dplyr::select(-YEAR_ID, -CUSTOMERNAME) %>% 
      tidyr::pivot_wider(names_from = MONTH_ID,
                         values_from = Revenue,
                         values_fill = 0)
    b %>% 
      reactable::reactable(searchable = FALSE,
                           showPageSizeOptions = TRUE,
                           theme = reactableTheme(
                             style = list(fontFamily = "sans-serif"),
                             borderColor = "#DADADA"),
                           pageSizeOptions = c(10, 25, 50, 100),
                           defaultPageSize = 10,
                           defaultColDef = colDef(
                             vAlign = "center",
                             align = "center",
                             headerVAlign = "center")
                           )
  })
  
  
  output$tabla_poblacion_Guate_2035 <- renderReactable({
    df_poblacion %>%
      reactable::reactable(
        groupBy = "Categoria",
        searchable = FALSE,
        showPageSizeOptions = TRUE,
        theme = reactableTheme(
          style = list(fontFamily = "sans-serif"),
          borderColor = "#DADADA"),
        pageSizeOptions = c(10, 25, 50, 100),
        defaultPageSize = 10,
        defaultColDef = colDef(
          vAlign = "center",
          align = "center",
          headerVAlign = "center")
      )
  })
  
  
  
}

shinyApp(ui = ui, server = server)