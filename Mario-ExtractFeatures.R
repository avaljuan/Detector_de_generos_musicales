# Cargar paquetes

pacman::p_load(tuneR,seewave,dplyr,tidyverse,tidyr,here, readr)

# Limpio la memoria antes

rm(list = ls())

## Adaptar

genero <- "Reggae" #pon aqui el genere que te ha tocado, primera en mayus
carpeta <- "reggae/" #pon aqui el nombre de la carpeta que contiene los archivos

## Declaramos funciones

importar_audio_normalizado <- function(path) {
  # 1. Cargar
  w <- tuneR::readWave(path)
  
  # 2. Convertir a Mono
  w <- tuneR::mono(w, which = "both")
  
  # 3. Recortar (30 a 120 segundos)
  w <- tuneR::extractWave(w, from = 30, to = 120, xunit = "time")
  
  # 4. Extraer vector para procesar
  x  <- w@left
  fs <- w@samp.rate
  
  # 5. Quitar DC (centrar la onda en 0)
  x <- x - mean(x)
  
  # 8. Construir objeto Wave para uso interno en R
  # Usamos bit = 32 y pcm = FALSE para que R entienda los decimales
  w <- tuneR::Wave(
    left = x, 
    samp.rate = fs, 
    bit = 32, 
    pcm = FALSE
  )
  
  # 7. Normalización a [-1, 1]
  w <- normalize(w)
  
  return(w)
}

#ZCR

zcr_stats <- function(w, ms = 20, ovlp = 50){
  x  <- w@left
  fs <- w@samp.rate
  
  
  wl <- as.integer((ms/1000) * fs)
  Z <- seewave::zcr(x, f = fs, wl = wl, ovlp = ovlp, plot = FALSE)
  zvals <- Z[, "zcr"]
  
  slope <- coef(lm(zvals ~ seq_along(zvals)))[2]
  
  return(c(mean(zvals), sd(zvals)))
}

# Plots ZCR 
plot_dir <- here::here("outputs", "plots_zcr")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

guardar_plot_zcr <- function(w, file_png, ms = 20, ovlp = 50, titulo = "") {
  x  <- w@left
  fs <- w@samp.rate
  
  wl <- as.integer((ms/1000) * fs)
  Z  <- seewave::zcr(x, f = fs, wl = wl, ovlp = ovlp, plot = FALSE)
  zvals <- Z[, "zcr"]
  
  png(file_png, width = 1200, height = 650)
  plot(zvals, type = "l", main = titulo, xlab = "Frames", ylab = "ZCR")
  abline(h = mean(zvals), lty = 2)
  dev.off()
}

# Espectrales

extraer_espectrales <- function(audio){
  fs <- audio@samp.rate
  spec <- meanspec(audio, f=fs, wl=1024, ovlp=50, plot=FALSE)
  
  props <- specprop(spec, f = spec[2,1], plot = FALSE)
  
  cent  <- props$mean
  bw    <- props$sd
  
  freq <- spec[,1] #kHz ojo
  amp  <- spec[,2]
  
  
  # normaliza amplitud a densidad energética
  Pv <- amp^2 / sum(amp^2)
  
  cumP     <- cumsum(Pv)
  idx     <- which(cumP >= 0.85 )[1]
  rolloff_85 <- freq[idx]
  
  E_low  <- sum(amp[freq <= 0.250]^2)
  E_high <- sum(amp[freq > 4.0 & freq <= 20.000]^2)
  
  
  
  
  return(
    c(
    cent*1000,
    bw*1000,
    rolloff_85*1000,
    E_low / E_high
    )
  )
}


## Importamos los datos

names_files <- list.files(path=carpeta, 
                          pattern = "\\.wav$", 
                          ignore.case = TRUE)
paths       <- paste0(carpeta,names_files)
songs_list  <- list()

for(path in paths) {
  songs_list <- append(songs_list,importar_audio_normalizado(path = path))
}

## Sacamos características

avg_energy <- c() #Energía media
sd_energy  <- c()

ZCR_mean   <- c() 
ZCR_sd     <- c()

centroid       <- c() # Centroide espectro
bandwidth      <- c() #  desviacion estandar centroide

rolloff        <- c() # Frecu por debajo de la cual está el 85% de la energía

ratio_graves_agudos <- c() # BER

for (song in songs_list) {
  sound      <- song@left
  
  acv        <- acf(sound, plot = FALSE, type = "covariance", lag.max = 0)
  
  avg_energy <- c(avg_energy,as.numeric(acv$acf[1]))
  sd_energy  <- c(sd_energy,sd(sound^2))
  
  aux        <- zcr_stats(song, ms = 20, ovlp = 50)
  ZCR_mean   <- c(ZCR_mean, aux[1])
  ZCR_sd     <- c(ZCR_sd,   aux[2])
  
  aux                 <- extraer_espectrales(song)
  centroid      <- c(centroid,
                           aux[1]) 
  bandwidth     <- c(bandwidth,
                           aux[2]) 
  rolloff       <- c(rolloff,
                           aux[3]) 
  
  ratio_graves_agudos <- c(ratio_graves_agudos,
                           aux[4])
  
}

# Guardar plots ZCR
for (i in seq_along(songs_list)) {
  song  <- songs_list[[i]]
  fname <- names_files[i]
  
  png_path <- file.path(plot_dir, paste0(tools::file_path_sans_ext(fname), "_zcr.png"))
  
  guardar_plot_zcr(
    song,
    png_path,
    ms = 20,
    ovlp = 50,
    titulo = paste("ZCR -", fname)
  )
}

rm(list = c('acv','aux')) #quitamos esto de la memoria que ya no nos sirve


features <- data.frame(AVG_Energy = avg_energy,
                       SD_Energy  = sd_energy,
                       ZCR_mean = ZCR_mean,
                       ZCR_sd   = ZCR_sd,
                       Centroid = centroid,
                       BW       = bandwidth,
                       RollOff  = rolloff,
                       Ratio_Frec = ratio_graves_agudos 
                      )


# Añadimos nuestro género

features <- features %>% mutate(Genero = genero)

# Exportamos

write.csv(features, "features_reggae.csv", row.names = FALSE)
