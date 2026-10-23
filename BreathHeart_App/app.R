# Title: Breath-Heart Synchronisation: A dataset for the analysis of Respiratory Sinus Arrythmia
# Subtitle: Shiny App
# Author: Tony Estrella
# Sep 2025 - June 2026 - Oct 2026

library(shiny)
library(shinydashboard)
library(plotly)
library(pracma)
library(tidyverse)

# Data
url_rr <- "https://zenodo.org/records/23257004/files/RR_long_format.csv?download=1"
file_rr <- "RR_long_format.csv"


url_respira <- "https://zenodo.org/records/23257004/files/resp_hrvb.txt?download=1"
file_resp <- "resp_hrvb.txt"

download.file(url_rr, destfile = file_rr, mode = "wb")
download.file(url_respira, destfile = file_resp, mode = "wb")


df_rr <- read_csv(file_rr)
df_respira <- read_delim(file_resp)

# Shiny Web App
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
                                      choices = unique(df_rr$participant_id)),
                       align = "center"),
                column(6, selectInput("Sesion",
                                      label = "Session:",
                                      choices = unique(df_rr$session_id)),
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
                                      choices = unique(df_rr$participant_id)),
                       align = "center"),
                column(4, selectInput("Sesion2",
                                      label = "Session:",
                                      choices = unique(df_rr$session_id)),
                       align = "center"),
                column(4, selectInput("respiration2",
                                      label = "Breathing Frequency:",
                                      choices = unique(df_rr$protocol_step)),
                       align = "center")
              ),
              fluidRow(
                column(8, plotlyOutput("peakdetec",
                                       height = "400px")),
                column(4, DT::dataTableOutput("peaksum",
                                              height = "400px"))
              ),
              fluidRow(
                column(12, 
                       h4(textOutput("p2v_text"),
                          h4(textOutput("selec")),
                          style = "color: #34495e; font-weight: bold;"),
                       style = "margin-top: 12px;
                       margin-bottom: 12px;
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
                                      choices = unique(df_rr$participant_id)),
                       align = "center"),
                column(6, selectInput("Sesion3",
                                      label = "Session:",
                                      choices = unique(df_rr$session_id)),
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
      filter(participant_id == input$Id,
             session_id == input$Sesion)})
  filtro_resp <- reactive({df_respira %>% 
      filter(participant_id == input$Id,
             session_id == input$Sesion)})
  
  # Output P1.1: Breathing frequencies and RR intervals ----
  
  ### Y-axis bands based on min and max RR intervals
  y_0res <- reactive({min(filtro()$rr)})
  y_1res <-reactive({max(filtro()$rr)})
  
  ## Continuous Plot ----
  output$contRSA <- renderPlotly({
    filtro() %>% 
      plot_ly(x= ~timestamps, y = ~ rr) %>% 
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
  y_0 <- reactive({min(filtro_resp()$resp_amplitude)})
  y_1 <-reactive({max(filtro_resp()$resp_amplitude)})
  
  output$resp_cont <- renderPlotly({
    filtro_resp() %>% 
      plot_ly(x= ~timestamps, y = ~ resp_amplitude) %>% 
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

  # findpeaks MATLAB
  detect_peaks_matlab <- function(y, min_dist = 6, min_h = 5) {
    n <- length(y)
    if (n < 3) return(integer(0))
    
    peaks <- which(diff(sign(diff(y))) == -2) + 1
    
    peaks <- peaks[y[peaks] >= min_h]
    if (length(peaks) <= 1) return(peaks)
    
    ord <- order(y[peaks], decreasing = TRUE)
    keep <- rep(TRUE, length(peaks))
    
    for (i in seq_along(ord)) {
      curr <- ord[i]
      if (!keep[curr]) next
      too_close <- which(abs(peaks - peaks[curr]) < min_dist & seq_along(peaks) != curr)
      keep[too_close] <- FALSE
    }
    
    sort(peaks[keep])
  }
  
  # peak2valley.m
  peak2valley_matlab <- function(y, locs) {
    if (is.null(locs) || length(locs) < 2) {
      return(list(peak2peak = numeric(0), locsmin = integer(0)))
    }
    
    locsmin <- integer(0)
    peak2peak <- numeric(0)
    
    for (k in 1:(length(locs) - 1)) {
      sub_y <- y[locs[k]:locs[k + 1]]
      pmi <- which.min(sub_y)
      mi <- sub_y[pmi]
      
      locsmin <- c(locsmin, pmi + locs[k] - 1)
      peak2peak <- c(peak2peak, y[locs[k]] - mi)
    }
    
    list(peak2peak = peak2peak, locsmin = locsmin)
  }
  
  filtro_peak <- reactive({
    req(input$Id2, input$Sesion2, input$respiration2)
    df_rr %>% 
      filter(participant_id == input$Id2,
             session_id == input$Sesion2,
             protocol_step == input$respiration2,
             output == 1)
  })
  
  # Butterworth
  butter_model <- signal::butter(1, 0.35, type = "low")
  
  rr_suavizado <- reactive({
    req(filtro_peak()$rr)
    signal::filtfilt(butter_model, filtro_peak()$rr)
  })
  
  # Plotly signals
  output$peakdetec <- plotly::renderPlotly({
    req(filtro_peak()$rr)
    
    y_raw  <- filtro_peak()$rr
    y_filt <- rr_suavizado()
    x_seq  <- seq_along(y_raw)
    
    plotly::plot_ly() %>%
      plotly::add_lines(
        x = x_seq,
        y = y_raw,
        name = "Observed",
        line = list(color = "#7f8c8d", width = 1.2),
        hoverinfo = "text",
        text = paste0("Sample: ", x_seq, "<br>Observed RR: ", round(y_raw, 2), " ms")
      ) %>%
      plotly::add_lines(
        x = x_seq,
        y = y_filt,
        name = "Butterworth",
        line = list(color = "#e74c3c", width = 2),
        hoverinfo = "text",
        text = paste0("Sample: ", x_seq, "<br>Filtered RR: ", round(y_filt, 2), " ms")
      ) %>%
      plotly::layout(
        title = list(text = "Observed vs. Butterworth Filter", font = list(size = 14)),
        xaxis = list(title = "Samples", showgrid = TRUE),
        yaxis = list(title = "RR intervals (ms)", showgrid = TRUE),
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0.05, y = 1.1)
      )
  })
  
  # Table
  output$peaksum <- DT::renderDataTable({
    req(filtro_peak()$rr)
    
    y_signal <- filtro_peak()$rr
    locs <- detect_peaks_matlab(y_signal, min_dist = 6, min_h = 5)
    
    if (length(locs) == 0) {
      return(data.frame(Mensaje = "No se detectaron picos"))
    }
   
    res_p2v <- peak2valley_matlab(y_signal, locs)
    
    valleys_aligned <- c(res_p2v$locsmin, NA)
    p2v_aligned <- c(round(res_p2v$peak2peak, 2), NA)
    
    data.frame(
      N_peaks     = seq_along(locs),
      Peak_Sample = locs,
      Peak_RR     = round(y_signal[locs], 2),
      Next_Valley = valleys_aligned,
      P2V_Amp     = p2v_aligned
    )
  },
  rownames = FALSE,
  options = list(
    scrollY = "350px",
    scrollCollapse = TRUE,
    paging = FALSE,
    searching = TRUE)
  )
  
  # Text Peak-to-Valley Method
  output$p2v_text <- renderText({
    req(filtro_peak()$rr)
    
    observado <- filtro_peak()$rr
    suavizado <- rr_suavizado()
    
    calc_p2v <- function(signal_data) {
      locs <- detect_peaks_matlab(signal_data, min_dist = 6, min_h = 5)
      res <- peak2valley_matlab(signal_data, locs)
      
      if (length(res$peak2peak) == 0) return(NA)
      round(mean(res$peak2peak), 2)
    }
    
    p2v_obs  <- calc_p2v(observado)
    p2v_filt <- calc_p2v(suavizado)
    
    str_obs  <- ifelse(is.na(p2v_obs), "N/A", paste0(p2v_obs, " ms"))
    str_filt <- ifelse(is.na(p2v_filt), "N/A", paste0(p2v_filt, " ms"))
    
    paste0("Peak-to-Valley method: Observed: ", str_obs, "  |  Filtered: ", str_filt)
  })
  
  # Selec respiration
  output$selec <- renderText({
    req(input$respiration2)
    resp_min <- as.numeric(input$respiration2)
    resp_hz <- round(resp_min / 60, 3)
    paste0("Breathing Frequency: ", resp_min, " (Expected peak at ", resp_hz, " Hz)")
  })
  
  
  ## BF selection Hz ---- 
  output$selec <- renderText({
    req(input$respiration2)
    
    resp_min <- as.numeric(input$respiration2)
    
    resp_hz <- round(resp_min / 60, 3)
    
    paste0("Breathing Frequency: ", resp_min, " (Expected peak at ", resp_hz, " Hz)")
  })
  
  ## Peak-to-Valley (P2V) Method ---- 
  peak2valley_r <- function(y, locs) {
    if (is.null(locs) || length(locs) < 2) {
      return(list(peak2peak = numeric(0), locsmin = integer(0)))
    }
    
    locsmin <- integer(0)
    peak2peak <- numeric(0)
    
    for (k in 1:(length(locs) - 1)) {
      # Ventana entre el pico k y el pico k+1
      sub_y <- y[locs[k]:locs[k + 1]]
      pmi <- which.min(sub_y)
      mi <- sub_y[pmi]
      
      # Índice global del valle: pmi + locs(k) - 1
      locsmin <- c(locsmin, pmi + locs[k] - 1)
      # Amplitud pico a valle: y(locs(k)) - mi
      peak2peak <- c(peak2peak, y[locs[k]] - mi)
    }
    
    list(peak2peak = peak2peak, locsmin = locsmin)
  }
  
  
  
  ## Peak-to-Valley (P2V) TEXT ----
  output$p2v_text <- renderText({
    req(filtro_peak()$rr)
    
    observado <- filtro_peak()$rr
    suavizado <- rr_suavizado()
    
    calc_p2v <- function(signal_data) {
      h_min <- mean(signal_data, na.rm = TRUE)
      

      picos <- pracma::findpeaks(
        signal_data,
        minpeakdistance = 7,         
        minpeakheight   = h_min,     
        threshold       = 10         
      )
      
      if (is.null(picos) || nrow(picos) < 2) {
        return(NA)
      }
      
      # index
      locs <- picos[, 2]
      
      # Peak-to-Valley MATLAB
      res <- peak2valley_r(signal_data, locs)
      
      if (length(res$peak2peak) == 0) return(NA)
      round(mean(res$peak2peak), 2)
    }
    
    p2v_obs  <- calc_p2v(observado)
    p2v_filt <- calc_p2v(suavizado)
    
    str_obs  <- ifelse(is.na(p2v_obs), "N/A", paste0(p2v_obs, " ms"))
    str_filt <- ifelse(is.na(p2v_filt), "N/A", paste0(p2v_filt, " ms"))
    
    paste0("Peak-to-Valley method: Observed: ", str_obs, "  |  Filtered: ", str_filt)
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
      filter(participant_id == input$Id3,
             session_id == input$Sesion3)
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
