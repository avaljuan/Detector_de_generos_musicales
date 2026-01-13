pacman::p_load(reticulate)
pacman::p_load(dplyr)
pacman::p_load("tuneR")
pacman::p_load("seewave")
pacman::p_load(readr)

configuracion_librosa <- function(){
  # Instalar Miniconda (esto crea un Python real y funcional para R)
  reticulate::install_miniconda(force = TRUE)
  #
  #  Crear un entorno específico para tu clasificador de audio
  # # Usaremos Python 3.10 porque es el que mejor se lleva con Librosa
  reticulate::conda_create("env_audio", python_version = "3.10")
  use_condaenv("env_audio", required = TRUE)
  #
  reticulate::conda_install(
    envname = "env_audio",
    packages = c("librosa", "numpy", "soundfile", "numba"),
    pip = TRUE
  )
}

#configuracion_librosa()

#Importamos librosa
librosa <- import("librosa")
np <- import("numpy")

calcular_ritmo_avanzado <- function(ruta_archivo) {
  # Cargar con los mismos parámetros que tu función de R
  # mono=TRUE (Un solo canal)
  # duration=90 (Límite de tiempo)
  audio_data <- librosa$load(ruta_archivo, sr = 48000, mono = TRUE, offset=0.0,duration = 90.0)
  
  y <- audio_data[[1]]
  sr <- audio_data[[2]]
  # QUITAR DC OFFSET
  # Restamos la media para centrar la onda en 0.0
  y <- y - mean(y)
  # Onset Strength
  oenv <- librosa$onset$onset_strength(y = y, sr = sr)
  
  # Tempo (BPM)
  tempo_estimado <- librosa$beat$tempo(onset_envelope = oenv, sr = sr)
  bpm <- as.numeric(tempo_estimado)
  
  # Normalización de Octava
  if (bpm > 160) bpm <- bpm / 2
  if (bpm < 65)  bpm <- bpm * 2
  
  # Fuerza del Pulso
  fuerza_pulso <- mean(oenv)
  
  return(list(
    bpm = round(bpm, 2),
    pulse_strength = round(fuerza_pulso, 4)
  ))
}

# Función para extraer mccfs

extraer_mfccs_filtrados <- function(ruta_archivo) {
  # Cargar con los parámetros estándar
  audio_data <- librosa$load(ruta_archivo, sr = 48000, mono = TRUE, duration = 90.0)
  y <- audio_data[[1]]
  sr <- audio_data[[2]]
  y <- y - mean(y)
  
  # Pedimos 9 coeficientes 
  mfccs <- librosa$feature$mfcc(y = y, sr = sr, n_mfcc = as.integer(9))
  
  #  Calculamos la media de cada uno
  mfccs_mean <- rowMeans(mfccs)
  

  mfccs_utiles <- mfccs_mean[2:9]
  

  nombres <- paste0("mfcc_", 2:9)
  res <- as.list(mfccs_utiles)
  names(res) <- nombres
  
  return(res)
}

añadir_features <- function(ruta_csv, carpeta){
  features <- read_csv(ruta_csv)
  nombres_canciones <- list.files(carpeta)
  canciones <- list()
  
  for (i in 1:length(nombres_canciones)) {
    
    ruta_completa <- paste0(carpeta, nombres_canciones[i])
    
    # Extraemos Ritmo (BPM y Pulso)
    res_ritmo <- calcular_ritmo_avanzado(ruta_completa)
    
    #  Extraemos Timbre (MFCCs 2 al 9)
    res_mfccs <- extraer_mfccs_filtrados(ruta_completa)
    
    # Creamos la fila base con los datos de ritmo
    fila_actual <- data.frame(
      Song_Name = nombres_canciones[i], 
      tempo          = res_ritmo$bpm,
      Fuerza_Pulso   = res_ritmo$pulse_strength,
      Genero         = "Reggae",
      stringsAsFactors = FALSE
    )
    
    # Añadimos los MFCCs a la fila usando cbind
    # Convertimos la lista de MFCCs en un dataframe de una fila
    fila_completa <- cbind(fila_actual, as.data.frame(res_mfccs))
    
    # Guardamos en nuestra lista
    canciones[[i]] <- fila_completa
    
    cat("Procesado:", i, "/", length(nombres_canciones), "-", nombres_canciones[i], "\n")
  }
  
  # -Unir odo en el dataframe final 
  df_final <- do.call(rbind, canciones)
  
  df_final$tempo <- round(df_final$tempo,2)
  df_final$TempoLog<-round(log2(df_final$tempo),4)
  
  df_total <- features %>%
    left_join(df_final %>% select(-tempo, -Genero), by = "Song_Name")
  df_total <- df_total %>% 
    select(-Genero, Genero)
  return (df_total)
  
}

df_completo <- añadir_features("features_reggae.csv","../Reggae/")
write.csv(df_completo, "features_reggae.csv", row.names = FALSE)