# Instalo los paquetes necesarios (si aún no los tengo instalados)
# install.packages("googlesheets4")

library(googlesheets4)

# Link al archivo
url="https://docs.google.com/spreadsheets/d/1Kwl4KByOv8q2kXMsgaO3d5QI3vUQ40RCZJgJHhg5bmE/edit?pli=1&gid=580479207#gid=580479207"

# Evito loggeo
gs4_deauth()

# Leo el archivo y almaceno los datos en un data frame
datos <- read_sheet(url, skip = 1, sheet = "tp")

# Veo la estructura del dataset
str(datos)