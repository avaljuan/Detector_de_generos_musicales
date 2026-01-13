# Cargar paquetes

pacman::p_load(tuneR,seewave,dplyr,tidyverse,tidyr,here, readr)

# Limpio la memoria antes

rm(list = ls())

## Adaptar


genero <- "Rap" #pon aqui el genere que te ha tocado, primera en mayus
#carpeta <- "../MusicaRap/" #pon aqui el nombre de la carpeta que contiene los archivos
carpeta <- "../MusicaRap/"

## Declaramos funciones

importar_audio_normalizado <- function(path) {
  # 1. Cargar
  w <- tuneR::readWave(path)
  
  # 2. Convertir a Mono
  w <- tuneR::mono(w, which = "both")
  
  # 3. Recortar (30 a 120 segundos)
  w <- tuneR::extractWave(w, from = 0, to = 90, xunit = "time")
  
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

#BER
# Conversión Hz -> Bark
hz2bark <- function(f) {
  13 * atan(0.00076 * f) + 3.5 * atan((f / 7500)^2)
}
metrica_BER <- function(signal, fs, wl = 1024, ovlp = 75) {
  
  spec <- spectro(
    signal,
    f = fs,
    wl = wl,
    ovlp = ovlp,
    plot = FALSE,
    norm = FALSE,
    dB = NULL
  )
  
  power_spec <- spec$amp^2
  
  # Frecuencias reales (Hz)
  freqs <- seq(0, fs/2, length.out = nrow(power_spec))
  bark_freq <- hz2bark(freqs)
  
  bark_ranges <- list(
    subbass  = c(1, 3),
    bass     = c(4, 6),
    low_mid  = c(7, 10),
    mid      = c(11, 14),
    high_mid = c(15, 18),
    treble   = c(19, 24)
  )
  
  total_energy <- sum(power_spec)
  
  BER <- numeric(length(bark_ranges))
  names(BER) <- names(bark_ranges)
  
  for (i in seq_along(bark_ranges)) {
    idx <- which(
      bark_freq >= bark_ranges[[i]][1] &
        bark_freq <= bark_ranges[[i]][2]
    )
    
    if (length(idx) > 0) {
      BER[i] <- sum(power_spec[idx, ]) / total_energy
    }
  }
  
  return(BER)
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
  
  # E_low  <- sum(amp[freq <= 0.250]^2)
  # E_high <- sum(amp[freq > 4.0 & freq <= 20.000]^2)
  x <- audio@left
  ber <- metrica_BER(x, fs)
  
  
  return(
    c(
      cent*1000,
      bw*1000,
      rolloff_85*1000,
      ber
      #E_low / E_high
    )
  )
}


#Espectrograma de MEL
espectro_MEL<- function( audio,FS_OBJETIVO ){
  
  
  # Resampling si es necesario
  if (audio@samp.rate != FS_OBJETIVO) {
    if (audio@samp.rate > FS_OBJETIVO) {
      audio <- downsample(audio, FS_OBJETIVO)
    } 
  }
  
  # numcep = 13 es estándar. nbands controla las bandas Mel.
  mel_spect_db <- melfcc(audio, 
                         sr = FS_OBJETIVO, 
                         numcep = 13, 
                         wintime = 0.025, 
                         hoptime = 0.010)
  
  #  Calcular Mean y SD
  mel_mean <- mean(mel_spect_db,na.rm = TRUE)
  mel_sd <- sd(mel_spect_db,na.rm = TRUE)
  return(c(mel_mean, mel_sd))
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

song_names <- c()
avg_energy <- c() #Energía media
sd_energy  <- c()

ZCR_mean   <- c() 
ZCR_sd     <- c()

centroid       <- c() # Centroide espectro
bandwidth      <- c() #  desviacion estandar centroide

rolloff        <- c() # Frecu por debajo de la cual está el 85% de la energía

#ratio_graves_agudos <- c() # BER
BER_subbass  <- c()
BER_bass     <- c()
BER_low_mid  <- c()
BER_mid      <- c()
BER_high_mid <- c()
BER_treble   <- c()

MEL_mean <- c()
MEL_sd <- c()

for (i in 1:length(songs_list)) {
  sound      <- songs_list[[i]]@left
  
  acv        <- acf(sound, plot = FALSE, type = "covariance", lag.max = 0)
  
  song_names <- c(song_names,names_files[i])
  avg_energy <- c(avg_energy,as.numeric(acv$acf[1]))
  sd_energy  <- c(sd_energy,sd(sound^2))
  
  aux        <- zcr_stats(songs_list[[i]], ms = 20, ovlp = 50)
  ZCR_mean   <- c(ZCR_mean, aux[1])
  ZCR_sd     <- c(ZCR_sd,   aux[2])
  
  aux                 <- extraer_espectrales(songs_list[[i]])
  centroid      <- c(centroid,
                     aux[1]) 
  bandwidth     <- c(bandwidth,
                     aux[2]) 
  rolloff       <- c(rolloff,
                     aux[3]) 
  
  #ratio_graves_agudos <- c(ratio_graves_agudos, aux[4])
  
  ber_vals <- aux[4:9]
  
  BER_subbass  <- c(BER_subbass,  ber_vals[1])
  BER_bass     <- c(BER_bass,     ber_vals[2])
  BER_low_mid  <- c(BER_low_mid,  ber_vals[3])
  BER_mid      <- c(BER_mid,      ber_vals[4])
  BER_high_mid <- c(BER_high_mid, ber_vals[5])
  BER_treble   <- c(BER_treble,   ber_vals[6])
  
  aux     <- espectro_MEL(songs_list[[i]],songs_list[[i]]@samp.rate)
  MEL_mean   <- c(MEL_mean, aux[1])
  MEL_sd     <- c(MEL_sd,   aux[2])
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

features <- data.frame(Song_Name= song_names,
                      AVG_Energy = avg_energy,
                       SD_Energy  = sd_energy,
                       ZCR_mean = ZCR_mean,
                       ZCR_sd   = ZCR_sd,
                       Centroid = centroid,
                       BW       = bandwidth,
                       RollOff  = rolloff,
                       BER_subbass  = BER_subbass,
                       BER_bass     = BER_bass,
                       BER_low_mid  = BER_low_mid,
                       BER_mid      = BER_mid,
                       BER_high_mid = BER_high_mid,
                       BER_treble   = BER_treble,
                       MEL_mean = MEL_mean,
                       MEL_sd = MEL_sd
)


# Añadimos nuestro género

features <- features %>% mutate(Genero = genero)


# Exportamos


write.csv(features, "features_rap.csv", row.names = FALSE)


