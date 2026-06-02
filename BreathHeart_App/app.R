# Title: Breath-Heart Synchronisation: A dataset for the analysis of Respiratory Sinus Arrythmia
# Subtitle: Shiny App
# Author: Tony Estrella
# Sep 2025 - June 2026

library(shiny)
library(shinydashboard)
library(plotly)
library(pracma)
library(tidyverse)

# Data
url_rr <- "https://zenodo.org/records/19115193/files/RR_long_format.csv?download=1"
file_rr <- "RR_long_format.csv"


url_respira <- "https://zenodo.org/records/19115193/files/resp_hrvb.txt?download=1"
file_resp <- "resp_hrvb.txt"



if (!file.exists(file_rr)) {
  download.file(url_rr, destfile = file_rr, mode = "wb")
}
if (!file.exists(file_resp)) {
  download.file(url_respira, destfile = file_resp, mode = "wb")
}


df_rr <- read_csv(file_rr)
df_rr <- df_rr[,-1]

df_respira <- read_delim(file_resp,
                         delim = "\t")


ui <- dashboardPage(
  dashboardHeader(title = "RSA Visualisation"),
  
  # SideBar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem("Continuous RSA", 
               tabName = "contRSA", 
               icon = icon("heart-pulse")),
      menuItem("Peak detection", 
               tabName = "peak", 
               icon = icon("think-peaks")),
      menuItem("Poincaré Plot", 
               tabName = "poincare", 
               icon = icon("chart-column")),
      menuItem("Github",
               icon = icon("github"),
               href = "https://github.com/estrellatonyy")
    )
  ),
  
  # Body ----
  dashboardBody(
    tags$head(tags$style(HTML("
    /* Name */
      .main-header .logo {
        background-color: #07183d !important;
        font-weight: bold;
      }
      /* Top bar */
      .main-header .navbar {
        background-color: #96bfe6 !important;
      }
      /* background */
      .content-wrapper, .right-side {
        background-color: #F8F9FA;
      }
                              "))),
    ## B1: Continuous RSA ---- 
    tabItems(
      tabItem(tabName = "contRSA",
              fluidRow(
                column(6, selectInput("Id",
                                      label = "Participant:",
                                      choices = unique(df_rr$Participante)),
                       align = "center"),
                column(6, selectInput("Sesion",
                                      label = "Session:",
                                      choices = unique(df_rr$Sesion)),
                       align = "center")
              ),
              fluidRow(
                column(12, plotlyOutput("contRSA"),
                       align = "center")
              ),
              fluidRow(
                column(12, plotlyOutput("resp_cont"),
                       align = "center")
              )
      ),
      ## B2: Peak detection ----
      tabItem(tabName = "peak",
              fluidRow(
                column(4, selectInput("Id2",
                                      label = "Participant:",
                                      choices = unique(df_rr$Participante)),
                       align = "center"),
                column(4, selectInput("Sesion2",
                                      label = "Session:",
                                      choices = unique(df_rr$Sesion)),
                       align = "center"),
                column(4, selectInput("respiration2",
                                      label = "Breathing Frequency:",
                                      choices = unique(df_rr$resp)),
                       align = "center")
              ),
              fluidRow(
                column(8, plotOutput("peakdetec")),
                column(4, DT::dataTableOutput("peaksum",
                                              height = "400px"))
              ),
              fluidRow(
                column(12, 
                       h4(textOutput("selec")),
                       style = "margin-top: 15px;
                       margin-bottom: 15px;
                       color: #34495e; text-align: center;")
              ),
              fluidRow(
                column(12, plotOutput("spectral")
                )
              )
      ),
      
      ## B3: Poincare Plots ----
      tabItem(tabName = "poincare",
              fluidRow(
                column(6, selectInput("Id3",
                                      label = "Participant:",
                                      choices = unique(df_rr$Participante)),
                       align = "center"),
                column(6, selectInput("Sesion3",
                                      label = "Session:",
                                      choices = unique(df_rr$Sesion)),
                       align = "center")
              ),
              fluidRow(
                column(12, plotOutput("poincare1"),
                       align = "center")
              )
              
      )
    )
  )
)


server <- function(input, output) {
  
  # Filter 1 - Participant and Session
  filtro <- reactive({df_rr %>% 
      filter(Participante == input$Id,
             Sesion == input$Sesion)})
  filtro_resp <- reactive({df_respira %>% 
      filter(Participante == input$Id,
             Sesion == input$Sesion)})
  
  # Output P1.1: Breathing frequencies and RR intervals ----
  
  ### Y-axis bands based on min and max RR intervals
  y_0res <- reactive({min(filtro()$rr)})
  y_1res <-reactive({max(filtro()$rr)})
  
  ## Continuous Plot ----
  output$contRSA <- renderPlotly({
    filtro() %>% 
      plot_ly(x= ~timestamp, y = ~ rr) %>% 
      add_lines() %>% 
      layout(title = paste("
                           Plot for the participant", 
                           input$Id, "in session", 
                           input$Sesion, sep = " "),
             xaxis = list(title = "",
                          range = c(0, 1200)),
             yaxis = list(title = "RR"),
             shapes = list(
               # Rec 1
               list(
                 type = "rect",
                 fillcolor = "#7ed957",
                 line = list(color ="#7ed957"),
                 opacity = 0.1,
                 x0 = 0,
                 x1 = 120, 
                 y0 = y_0res(),
                 y1 = y_1res()
               ),
               # Rec 2
               list(
                 type = "rect",
                 fillcolor = "#7ed957",
                 line = list(color ="#7ed957"),
                 opacity = 0.1,
                 x0 = 240,
                 x1 = 360, 
                 y0 = y_0res(),
                 y1 = y_1res()
               ),
               # Rec 3
               list(
                 type = "rect",
                 fillcolor = "#7ed957",
                 line = list(color ="#7ed957"),
                 opacity = 0.1,
                 x0 = 480,
                 x1 = 600, 
                 y0 = y_0res(),
                 y1 = y_1res()
               ),
               # Rec 4
               list(
                 type = "rect",
                 fillcolor = "#7ed957",
                 line = list(color ="#7ed957"),
                 opacity = 0.1,
                 x0 = 720,
                 x1 = 840, 
                 y0 = y_0res(),
                 y1 = y_1res()
               ),
               # Rec 5
               list(
                 type = "rect",
                 fillcolor = "#7ed957",
                 line = list(color ="#7ed957"),
                 opacity = 0.1,
                 x0 = 960,
                 x1 = 1080, 
                 y0 = y_0res(),
                 y1 = y_1res()
               )
             ),
             annotations = list(
               list(x = 60, y = y_0res() - 10, text = "Resp 6.5", showarrow = FALSE, font = list(size = 14, color = "black")),
               list(x = 300, y = y_0res() - 10, text = "Resp 6", showarrow = FALSE, font = list(size = 14, color = "black")),
               list(x = 540, y = y_0res() - 10, text = "Resp 5.5", showarrow = FALSE, font = list(size = 14, color = "black")),
               list(x = 780, y = y_0res() - 10, text = "Resp 5", showarrow = FALSE, font = list(size = 14, color = "black")),
               list(x = 1020, y = y_0res() - 10, text = "Resp 4.5", showarrow = FALSE, font = list(size = 14, color = "black"))
             )
      )
  })
  
  # Output P1.2: Breathing frequencies and respiration ----
  
  ### Y-axis bands based on min and max RR intervals
  y_0 <- reactive({min(filtro_resp()$val_resp)})
  y_1 <-reactive({max(filtro_resp()$val_resp)})
  
  output$resp_cont <- renderPlotly({
    filtro_resp() %>% 
      plot_ly(x= ~timestamp, y = ~ val_resp) %>% 
      add_lines() %>% 
      layout(title = "",
             xaxis = list(title = "Time",
                          range = c(0, 1200)),
             yaxis = list(title = "Respiration"),
             shapes = list(
               # Rec 1
               list(
                 type = "rect",
                 fillcolor = "#7ed957",
                 line = list(color ="#7ed957"),
                 opacity = 0.1,
                 x0 = 0,
                 x1 = 120, 
                 y0 = y_0(),
                 y1 = y_1()
               ),
               # Rec 2
               list(
                 type = "rect",
                 fillcolor = "#7ed957",
                 line = list(color ="#7ed957"),
                 opacity = 0.1,
                 x0 = 240,
                 x1 = 360, 
                 y0 = y_0(),
                 y1 = y_1()
               ),
               # Rec 3
               list(
                 type = "rect",
                 fillcolor = "#7ed957",
                 line = list(color ="#7ed957"),
                 opacity = 0.1,
                 x0 = 480,
                 x1 = 600, 
                 y0 = y_0(),
                 y1 = y_1()
               ),
               # Rec 4
               list(
                 type = "rect",
                 fillcolor = "#7ed957",
                 line = list(color ="#7ed957"),
                 opacity = 0.1,
                 x0 = 720,
                 x1 = 840, 
                 y0 = y_0(),
                 y1 = y_1()
               ),
               # Rec 5
               list(
                 type = "rect",
                 fillcolor = "#7ed957",
                 line = list(color ="#7ed957"),
                 opacity = 0.1,
                 x0 = 960,
                 x1 = 1080, 
                 y0 = y_0(),
                 y1 = y_1()
               )
             ),
             annotations = list(
               list(x = 60, y = y_0() - 10, text = "Resp 6.5", showarrow = FALSE, font = list(size = 14, color = "black")),
               list(x = 300, y = y_0() - 10, text = "Resp 6", showarrow = FALSE, font = list(size = 14, color = "black")),
               list(x = 540, y = y_0() - 10, text = "Resp 5.5", showarrow = FALSE, font = list(size = 14, color = "black")),
               list(x = 780, y = y_0() - 10, text = "Resp 5", showarrow = FALSE, font = list(size = 14, color = "black")),
               list(x = 1020, y = y_0() - 10, text = "Resp 4.5", showarrow = FALSE, font = list(size = 14, color = "black"))
             )
      )
  })
  
  # Output P2: Peak Detection ----
  filtro_peak <- reactive({
    df_rr %>% 
      filter(Participante == input$Id2,
             Sesion == input$Sesion2,
             resp == input$respiration2,
             output == 1)
  })
  
  butter<- signal::butter(1, 0.35, type = "low")
  rr_suavizado <- reactive({signal::filtfilt(butter, filtro_peak()$rr)})
  
  ## Plot ----
  output$peakdetec <- renderPlot({
    plot(filtro_peak()$rr, type = "l", col= "gray", 
         ylab = "RR intervals",
         xlab = "Time (s)")
    lines(rr_suavizado(), col = "red")
    legend("topleft", c("Observed", "Butterworth"), col = c("gray", "red"),
           lty = 1)
    title("Observed vs. Butterworth filter")
  }, res = 96)
  
  ## Table ----
  output$peaksum <- DT::renderDataTable({
    
    tab <- findpeaks(rr_suavizado())
    data.frame(N_peaks = seq(1, nrow(tab)),
               Peak_RR = tab[,1],
               begins = tab[,3],
               ends = tab[,4])
  },
  rownames = FALSE,
  options = list(
    scrollY = "350px",
    scrollCollapse = TRUE,
    paging = FALSE,
    searching = TRUE)
  )
  ## BF selection Hz ---- 
  output$selec <- renderText({
    # Nos aseguramos de que haya un valor seleccionado
    req(input$respiration2)
    
    # Convertimos a numérico por si acaso el input viene como string
    resp_min <- as.numeric(input$respiration2)
    
    # Cálculo: Respiraciones por segundo (Hz)
    # Hz = Respiraciones por minuto / 60
    resp_hz <- round(resp_min / 60, 3)
    
    paste0("Breathing Frequency: ", resp_min, " (Expected peak at ", resp_hz, " Hz)")
  })
  
  ## Spectral Plot ----
  rr <- reactive({filtro_peak()$rr})
  fs <- 4
  t <- reactive({cumsum(rr())/1000})
  tiempo <- reactive({seq(0, max(t()), by = 1/fs)})
  rrm <- reactive({spline(t(), rr(), xout =tiempo())$y})
  
  nFFT <- reactive({length(rrm())})
  
  output$spectral <- renderPlot({
    xfft <- abs(fft(signal::hanning(nFFT()) * pracma::detrend(rrm())))^2
    
    s <- 2 * xfft * 8 / 3 / (fs*nFFT())
    
    f <- (0:(nFFT()-1)) / nFFT() * fs 
    
    # 1Hz limitation
    n <- which(f > 0.5)[1] - 1
    df_freq <- as.data.frame(cbind(x_esp = f[1:n],
                                   y_esp = s[1:n]))
    
    peak_hz <- df_freq$x_esp[which.max(df_freq$y_esp)]
    
    df_freq %>% 
      ggplot(aes(x_esp, y_esp))+
      geom_line()+
      theme_minimal()+
      #annotate("text", x = 0.1, y = max(df_freq$y_esp),label = "LF", size = 5, fontface = "italic")+
      annotate("rect", xmin= 0.04, xmax = 0.15,
               ymin = min(df_freq$y_esp), ymax = max(df_freq$y_esp),
               alpha = 0.1, fill = "blue")+
      annotate("rect", xmin= 0.15, xmax = 0.40,
               ymin = min(df_freq$y_esp), ymax = max(df_freq$y_esp),
               alpha = 0.1, fill = "red")+
      labs(title = paste("Spectral plot for participant", input$Id2,
                         "Session", input$Sesion2,
                         "breathing frequency", input$respiration2,
                         "RR peak at", round(peak_hz, 3), "Hz", sep = " "),
           x = "Hz (ms2)",
           y = "")
    
  }, res = 96)
  
  # Output P3: Poincare Plots ----
  filtro_pp12 <- reactive({
    df_rr %>% 
      filter(Participante == input$Id3,
             Sesion == input$Sesion3)
  })
  
  data_pp1 <- reactive({filtro_pp12() %>%
      mutate(RR = lag(rr),
             RR_1 = rr) %>% 
      filter(!is.na(RR), !is.na(RR_1))
  })
  
  
  ## Poincare 1: output ----
  output$poincare1 <- renderPlot({
    data_pp1() %>% 
      ggplot(aes(x = RR, y = RR_1, color = as.factor(output)))+
      geom_point(alpha = 0.3, size = 3)+
      stat_ellipse(type = "norm", level = 0.95, linewidth = 1) +
      scale_color_discrete(name = "Output",
                           labels = c("Free Breathing", "Control Breathing"))+
      scale_x_continuous(limits = c(min(data_pp1()$rr) - 200, max(data_pp1()$rr) + 200),
                         breaks = c(500, 700, 900,
                                    1100, 1300, 1500, 1700))+
      scale_y_continuous(limits = c(min(data_pp1()$rr) - 200, max(data_pp1()$rr) + 200),
                         breaks = c(500, 700, 900,
                                    1100, 1300, 1500, 1700))+
      theme_minimal()+
      labs(title = paste("Poincaré Plot participant", 
                         input$Id3, "session", input$Sesion3, sep = " "),
           x = "RR (ms)",
           y = "RR n+1 (ms)")
  },res = 96)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
