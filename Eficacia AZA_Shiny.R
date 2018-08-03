#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(plotly)

# Load data
efi_data <- read.csv2("Reporte_Eficacia.csv", header = TRUE)
head(efi_data)

#funcion para transformar en 0 los NA "
#fx.cero.na=function(x){
#  ifelse(is.na(x),0,x)
#}

#trasnformar los NA de eficacia.hembras.ovigeras a 0, para despues filtrarlos en el data frame "AZA"
efi_data$eficacia.hembras.ovigeras.2 <- ifelse(is.na(efi_data$eficacia.hembras.ovigeras), 0, efi_data$eficacia.hembras.ovigeras)



### Crear nuevas variables (acs2, Region, mes2 y fecha2)
#-----------------------------------------------------------
#[acs2] -> formato acs_3b
efi_data$acs2 <- paste("acs_", efi_data$acs)

#[Region] -> Mediante un programa for() + if() + else
efi_data$region <- NULL

for (i in 1:length(efi_data$macrozona)) {
  if (efi_data$macrozona[i] <= 5) {
    efi_data$region[i] <- "Los Lagos"
  } else{
    efi_data$region[i]<- "Aysen"
  }
}

table(efi_data$region)
table(efi_data$macrozona)


# [Mes2, fecha.ok y a??o] -> con formato fecha OK
class(efi_data$mes.ini.tto)
#nuevo vector con formato fecha dmy()
efi_data$mes.ini.tto2 <- dmy(efi_data$mes.ini.tto)
class(efi_data$mes.ini.tto2)

#a??o
efi_data$ano <- year(efi_data$mes.ini.tto2)
efi_data$mes <- month(efi_data$mes.ini.tto2)

#formato fecha final
efi_data$fecha.fin<- paste("01", efi_data$mes, efi_data$ano, sep="-")
efi_data$fecha.fin<- dmy(efi_data$fecha.fin)
class(efi_data$fecha.fin)

#Crea bases de datos por producto
AZA <- as.data.frame(efi_data[efi_data$producto=="Azametifos" & efi_data$incluirBD_x=="Incluir en BD1" & efi_data$eficacia.hembras.ovigeras.2 > 0,])


#-----------------
### SHINY
#-----------------

# Define UI for application that draws a histogram
ui <- navbarPage("Proyecto Control Coordinado de Caligus",
                 
                 #Primer Panel (plot general de eficacia)
                 tabPanel("General",
                          titlePanel("Eficacia Tratamientos Azametifos"),
                          
                          plotOutput("general", height = "600px")
                 ),
                 
                 #Segundo Panel (para seleccionar por barrio)
                 tabPanel("ACS",
                          titlePanel("Eficacia Tratamientos Azametifos"),
                          # Sidebar with a slider input for number of bins 
                          sidebarPanel(
                            selectInput(inputId = "var",
                                        label = strong("Selecionar Barrio:"),
                                        choices = unique(unique(AZA$acs2)),
                                        selected = "acs_2"),
                            hr(),
                            #plotOutput("termPlot", height = 500),
                            helpText("Desarrollado por....."),
                            # br() element to introduce extra vertical spacing ----
                            br(),
                            #link pagina web
                            tags$a(href = "https://www.aquabench.com", "AQUABENCH S.A.", target = "_blank")
                            ),
                          
                          # Show a plot of the generated distribution
                          mainPanel(
                            plotlyOutput("eficaciaPlot", height = "600px"),
                            textOutput("texto")
                            )
                          )
               )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  #subset data
  selected_trends <- reactive({
    AZA %>%
      filter(
        acs2 == input$var
        )
  })
  
  # Create plot object the plotOutput function is expecting
   output$eficaciaPlot <- renderPlotly({
     #boxplot(selected_trends()$eficacia.hembras.ovigeras ~ selected_trends()$fecha.fin)
     plot.aza<-ggplot() +
       geom_boxplot(aes(reorder(format(selected_trends()$fecha.fin, "%b-%y"),selected_trends()$fecha.fin),
                        selected_trends()$eficacia.hembras.ovigeras.2), alpha=.6, outlier.alpha = .25, fill="chocolate3")+
       #geom_vline(xintercept = c(12.5, 24.5, 36.5,48.5), colour = "white",size=1.5)+
       scale_y_continuous(labels = percent, limits = c(0,1), breaks = seq(0,1,0.2))+
       #scale_fill_manual(values = c("steelblue", "chocolate3"))+
       #facet_grid(.~ano, scales="free")+
       labs(title="",
            subtitle="",
            x="\nMonth",
            y="Efficacy (%)\n",
            caption="---- Reference line 90% efficacy")+
       geom_hline(yintercept = 0.9, colour="red3", size=.8, lty=1)+
       #theme_minimal()+
       theme(strip.background = element_rect(fill="grey90",color="grey80"),
             strip.text.x = element_text(size=13, color = "black"),
             strip.text.y = element_text(size=13, color = "black"),
             axis.text.x = element_text(size=13, angle=90, hjust = 1, vjust = .5, colour= "black"),
             axis.text.y = element_text(size=13, hjust = 1, vjust = .5, colour= "black"),
             plot.title = element_text(size=15, hjust = 1, colour= "steelblue"),
             plot.subtitle = element_text(size=13, hjust = 1, colour= "darorange2"),
             plot.caption = element_text(size=10, colour = "red3"))+
       theme(legend.position = "NA")
     
     ggplotly(plot.aza)
   })


  # Pull in description
   output$texto <- renderText({
     "---- Reference line 90% efficacy"
     })
   
   
   
   output$general <- renderPlot({
     ggplot(AZA) +
       geom_boxplot(aes(reorder(format(fecha.fin, "%b-%y"),fecha.fin), eficacia.hembras.ovigeras.2), alpha=.6, outlier.alpha = .25, fill="chocolate3")+
       #geom_vline(xintercept = c(12.5, 24.5, 36.5,48.5), colour = "white",size=1.5)+
       scale_y_continuous(labels = percent, limits = c(0,1), breaks = seq(0,1,0.2))+
       #scale_fill_manual(values = c("steelblue", "chocolate3"))+
       facet_grid(.~ano, scales="free")+
       labs(title="Eficacia Tratamientos Azametifos",
            subtitle="Region de Los Lagos y Region de Aysen\n",
            x="\nMes datos",
            y="Eficacia HO (%)\n",
            caption="---- Linea de referencia 90% efficacia\n
            Se han omitido acentos de forma voluntaria")+
       geom_hline(yintercept = 0.9, colour="red3", size=.8, lty=1)+
       #theme_minimal()+
       theme(strip.background = element_rect(fill="grey90",color="grey80"),
             strip.text.x = element_text(size=13, color = "black"),
             strip.text.y = element_text(size=13, color = "black"),
             axis.text.x = element_text(size=13, angle=90, hjust = 1, vjust = .5, colour= "black"),
             axis.text.y = element_text(size=13, hjust = 1, vjust = .5, colour= "black"),
             plot.title = element_text(size=18, hjust = 0.5, colour= "darkblue"),
             plot.subtitle = element_text(size=14, hjust = 0.5, colour= "darkorange2"),
             plot.caption = element_text(size=10, colour = "red3")
             )+
       theme(legend.position = "NA")
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)


