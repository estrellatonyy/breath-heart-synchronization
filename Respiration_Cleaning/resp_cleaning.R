### Resp Data
library(tidyverse)

df_respira <- read_delim("data/resp_hrvb_1.txt",
                         delim = "\t",
                         col_names = FALSE)


colnames(df_respira)<- c("Participante", "Sesion", "val_resp", "timestamp", "resp", "output" )

df_respira <- df_respira %>% 
  mutate(Sesion= ifelse(Participante == 030, Sesion + 1, Sesion))

df_respira <- df_respira %>% 
  mutate(
    Participante = str_pad(as.character(Participante), width = 3, pad = "0"),
    Sesion = paste0("s", Sesion)
  )

df_clean_resp <- df_respira %>% 
  filter(
    !(Participante == "002" & Sesion == "s6") &
    !(Participante == "004" & Sesion == "s6") &
    !(Participante == "005" & Sesion == "s6") &
    !(Participante == "009" & Sesion == "s4") &  
    !(Participante == "009" & Sesion == "s6") &
    !(Participante == "010" & Sesion == "s6") &
    !(Participante == "011" & Sesion == "s6") &
    !(Participante == "012" & Sesion == "s4") &  
    !(Participante == "013" & Sesion == "s4") &
    !(Participante == "026" & Sesion == "s3") &  
    !(Participante == "030" & Sesion == "s4") &
    !(Participante == "030" & Sesion == "s5") &
    !(Participante == "033" & Sesion == "s6") &
    !(Participante == "034" & Sesion == "s5") 
  )

df_clean_resp %>% 
  filter((Participante == "035" & Sesion == "s2")) %>% 
  summary()


df_clean_resp_1 <- df_clean_resp %>% 
  filter((Participante == "035" & Sesion == "s2")) %>% 
  mutate(val_resp = ifelse(val_resp > 0, -4.74, ifelse(val_resp < -100, -4.74, val_resp)))

plot(df_clean_resp_1$val_resp)


df_clean_resp <- df_clean_resp %>% 
  mutate(val_resp = case_when(
    Participante == "035" & Sesion == "s2" & (val_resp > 0 | val_resp < -100) ~ -4.74,
    TRUE ~ val_resp
  ))


write_tsv(df_clean_resp, "data/resp_hrvb_V2.txt")

