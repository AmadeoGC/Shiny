
library(shiny)
library(shinydashboard)
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


#======================
### SHINY DASHBOARD
#======================

#---------------------------
## 1.- Barra lateral - Sidebar
#---------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    
    # Selector
    selectInput(inputId = "var",
                label = strong("Selecionar Barrio:"),
                choices = unique(AZA$acs2),
                selected = "acs_2",
                width = '95%')
    )
  )




#-------------------
## 2.- Cuerpo
#-------------------
body <- dashboardBody(
  
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("Tab 1", 
                         
                         fluidRow(
                           box(
                             title = "Histograma Basico",
                             status = "primary",
                             plotOutput("grafico1", height = 400)
                           ),
                           
                           box(
                             title = "Histograma ggplot2",
                             plotOutput("grafico2", height = 400)
                           )
                         )
                         
                ), 
                
                tabPanel("Tab 2", h2("Tab 2")), 
                
                tabPanel("Tab 3", h2("Tab 3"))
    )
  )
)



# ---------------------
# 3.- Inteface - UI 
# ---------------------

ui <- dashboardPage(
  dashboardHeader(title = 'Proyecto Control Coordinado de Caligus', titleWidth = 290),
  sidebar,
  body
)


#--------------------
# 4.- Define server 
#--------------------
server <- function(input, output) {
   
  #subset data
  selected_trends <- reactive({
    AZA %>%
      filter(
        acs2 == input$var
        )
  })
  
  # Create plot object the plotOutput function is expecting
   output$grafico1 <- renderPlot({
     #boxplot(selected_trends()$eficacia.hembras.ovigeras ~ selected_trends()$fecha.fin)
     ggplot() +
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
             plot.subtitle = element_text(size=13, hjust = 1, colour= "darkorange2"),
             plot.caption = element_text(size=10, colour = "red3"))+
       theme(legend.position = "NA")
   })


   output$grafico2 <- renderPlot({
     #boxplot(selected_trends()$eficacia.hembras.ovigeras ~ selected_trends()$fecha.fin)
     ggplot() +
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
             plot.subtitle = element_text(size=13, hjust = 1, colour= "darkorange2"),
             plot.caption = element_text(size=10, colour = "red3"))+
       theme(legend.position = "NA")
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)


