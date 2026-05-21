# Title: Breath-Heart Synchronization: A dataset for the analysis of Respiratory Sinus Arrythmia
# Subtitle: Dataset creation from long RR format
# Author: Tony Estrella
# July 2025


# Libraries ----
library(tidyverse)
library(readxl)

# Data ----
df<- read_excel("C:/Users/estre/OneDrive - UAB/5. HRVB DATA/HRV/datos_long.xlsx")

## Dataframe with HRV parameters ----

### Time Domain ----
df_measures <- df %>% 
  group_by(Participante, Sesion, resp) %>% 
  summarise(MeanHR = mean(1000/rr)*60,
            mRR = mean(rr),
            SDNN = sd(rr),
            RMSSD = sqrt(mean(diff(rr)^2)),
            pNN50 = sum(abs(diff(rr)) > 50) / length(diff(rr)) * 100)


### Frequency Domain ----
frequency_indexes <- function(rr) {
  # rr en ms
  fs <- 4  #sampling frequency 4Hz
  
  # time in s
  t <- cumsum(rr) / 1000
  
  # time for resampling
  ti <- seq(0, max(t), by = 1/fs)
  
  # cubic spline resampling
  rrm <- spline(t, rr, xout = ti, method = "natural")$y
  rrm <- as.vector(rrm)
  
  # FFT and spectral density
  NFFT <- length(rrm)
  
  # Hanning window and detrend
  hanning_window <- 0.5 * (1 - cos(2 * pi * (0:(NFFT-1)) / (NFFT-1)))
  detrended_rrm <- pracma::detrend(rrm - mean(rrm))
  
  # FFT
  xfft <- abs(fft(hanning_window * detrended_rrm))^2
  
  # scalar FFT for spectral density
  s <- (2 * xfft * 8/3) / (fs * NFFT)
  
  # frequency index
  f <- (0:(NFFT-1)) / NFFT * fs
  
  # 1Hz 
  n <- which(f > 1)[1] - 1
  if(is.na(n)) n <- length(f)
  cuaesp <- function(s, f, vlf_limit = 0.04, lf_limit = 0.15, hf_limit = 0.4) {
    # Normalisation
    incf <- f[2] - f[1]
    
    # VLF: 0-0.04 Hz
    vlf_idx <- which(f >= 0 & f < vlf_limit)
    pvlf <- sum(s[vlf_idx]) * incf
    
    # LF: 0.04-0.15 Hz
    lf_idx <- which(f >= vlf_limit & f < lf_limit)
    plf <- sum(s[lf_idx]) *incf
    
    # HF: 0.15-0.4 Hz
    hf_idx <- which(f >= lf_limit & f < hf_limit)
    phf <- sum(s[hf_idx]) * incf
    
    # LF/HF ratio
    lfhf <- plf / phf
    
    return(list(pvlf = pvlf, plf = plf, phf = phf, lfhf = lfhf))
  }
  # Spectral indexes
  spectral_results <- cuaesp(s, f, vlf_limit = 0.04, lf_limit = 0.15, hf_limit = 0.4)
  
  pvlf <- spectral_results$pvlf
  plf <- spectral_results$plf
  phf <- spectral_results$phf
  lfhf <- spectral_results$lfhf
  
  # Abs powers
  powers <- c(pvlf, plf, phf)
  names(powers) <- c("VLF", "LF", "HF")
  
  # Nu powers (%)
  total_power <- pvlf + plf + phf
  npowers <- powers / total_power * 100
  names(npowers) <- c("VLF_norm", "LF_norm", "HF_norm")
  
  # Nu powers LF and HF (no VLF)
  lf_hf_total <- plf + phf
  normpowers <- c(plf, phf) / lf_hf_total * 100
  names(normpowers) <- c("LF_nu", "HF_nu")
  
  # peak detection in frequency bands
  # peak in VLF (0-0.04 Hz)
  k1 <- which(f < 0.04)
  if(length(k1) > 0) {
    max_idx <- which.max(s[k1])
    p1 <- f[k1[max_idx]]
  } else {
    p1 <- NA
  }
  
  # peak in LF (0.04-0.15 Hz)
  k2 <- which(f > 0.04 & f < 0.15)
  if(length(k2) > 0) {
    max_idx <- which.max(s[k2])
    p2 <- f[k2[max_idx]]
  } else {
    p2 <- NA
  }
  
  # peak in HF (0.15-0.4 Hz)
  k3 <- which(f > 0.15 & f < 0.4)
  if(length(k3) > 0) {
    max_idx <- which.max(s[k3])
    p3 <- f[k3[max_idx]]
  } else {
    p3 <- NA
  }
  
  peaks <- c(p1, p2, p3)
  names(peaks) <- c("VLF_peak", "LF_peak", "HF_peak")
  
  # Results
  return(list(
    peaks = peaks,
    powers = powers,
    npowers = npowers,
    normpowers = normpowers,
    lfhf = lfhf,
    f = f[1:n],
    s = s[1:n]
  ))
}

# Function for long format ----
process_hrv_data <- function(data) {
  # Group by participante, session and breathing frequency
  results <- data %>%
    group_by(Participante, Sesion, resp) %>%
    do({
      # RR intervals for groups
      rr_intervals <- .$rr
      
      # spectral indexes
      hrv_results <- frequency_indexes(rr_intervals)
      
      # dataframe from results
      data.frame(
        # frequency peaks
        VLF_peak = hrv_results$peaks["VLF_peak"],
        LF_peak = hrv_results$peaks["LF_peak"],
        HF_peak = hrv_results$peaks["HF_peak"],
        
        # Abs power (ms²)
        VLF_power = hrv_results$powers["VLF"],
        LF_power = hrv_results$powers["LF"],
        HF_power = hrv_results$powers["HF"],
        
        # Nu power (%)
        VLF_norm = hrv_results$npowers["VLF_norm"],
        LF_norm = hrv_results$npowers["LF_norm"],
        HF_norm = hrv_results$npowers["HF_norm"],
        
        # Nu power (no VLS)
        LF_nu = hrv_results$normpowers["LF_nu"],
        HF_nu = hrv_results$normpowers["HF_nu"],
        
        # Ratio LF/HF
        LF_HF_ratio = hrv_results$lfhf,
        
        # total power
        Total_power = sum(hrv_results$powers)
      )
    })
  
  return(as.data.frame(results))
}

frecuenciales <- process_hrv_data(df)


# Adding output ----
df_measures_complete <- df_measures %>%
  full_join(frecuenciales, by = c("Participante", "Sesion", "resp")) %>%
  arrange(Participante, Sesion, resp)

output <- rep(c(1,0), 1040)

df_final <- cbind(df_measures_complete, output = output)

# Save dataframe
writexl::write_xlsx(df_final, path = "C:/Users/estre/OneDrive - UAB/5. HRVB DATA/HRV/hrv_indexes.xlsx")


