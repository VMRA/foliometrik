# Leer la ultima direccion en la que se pusieron los datos de salida

if (file.exists("diroutputleaves.txt")) {
     (diroutput <- scan("diroutputleaves.txt", what = "", sep = "\n"))
     if (!dir.exists(diroutput)) diroutput <- "~"
} else {
     diroutput <- "~"
     cat(diroutput, file = "diroutputleaves.txt")
}

#Sistemas de unidades
unitnames <- list("IS" = c("mm", "cm", "dm", "m"),
                  "AS" = c("inches", "feet"), "other")

