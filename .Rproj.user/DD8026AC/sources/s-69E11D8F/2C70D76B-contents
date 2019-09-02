

#____________________________________________
enmarca.img <- function(img) {
     # img es una imagen de la clase Image, de EBImage
     # Enmarca una imagen (le aniade un marco de 4 pixeles de grosor). Esto es
     # util para que no se generen errores cuando el algoritmo que halla el
     # contorno se encuentra con el borde de la imagen, aunque aun luego de esto
     # puede que se sigan generando este tipo de errores, pero se espera que
     # sean menos frecuentes.
     # value es el valor numerico que tendra el marco: blanco (v=1), negro (v=0)
     # u otro
     #I <- array(data = 0.5, dim = c(3, 4, 3)); img <- as.Image(I); img

     (dm <- dim(img))
     (M <- matrix(data = 1, nrow = 1, ncol = dm[2]))
     (M <- c(rbind(M, img@.Data[, , 1], M), rbind(M, img@.Data[, , 2], M),
             rbind(M, img@.Data[, , 3], M)))
     dim(M) <- c(dm[1] + 2, dm[2], 3); img@.Data <- M

     (dm <- dim(img))
     (M <- matrix(data = 1, ncol = 1, nrow = dm[1]))
     (M <- c(cbind(M, img@.Data[, , 1], M), cbind(M, img@.Data[, , 2], M),
             cbind(M, img@.Data[, , 3], M)))
     dim(M) <- c(dm[1], dm[2] + 2, 3); img@.Data <- M

     (dm <- dim(img))
     (M <- matrix(data = 0, nrow = 4, ncol = dm[2]))
     (M <- c(rbind(M, img@.Data[, , 1], M), rbind(M, img@.Data[, , 2], M),
             rbind(M, img@.Data[, , 3], M)))
     dim(M) <- c(dm[1] + 8, dm[2], 3); img@.Data <- M

     (dm <- dim(img))
     (M <- matrix(data = 0, ncol = 4, nrow = dm[1]))
     (M <- c(cbind(M, img@.Data[, , 1], M), cbind(M, img@.Data[, , 2], M),
             cbind(M, img@.Data[, , 3], M)))
     dim(M) <- c(dm[1], dm[2] + 8, 3); img@.Data <- M

     return(img)
}

# ________________________________________
imgarrays <- function(path, channel = "Red") {

     # Las opciones de canales definen cual de las matrices sera escogida
     w <- switch(channel, "Red" = 1, "Green" = 2, "Blue" = 3)

     # Si se proporciona una imagen cargada en lugar de una direccion, no sera
     # necesario leer el archivo de la imagen
     if (class(path) == "Image") {
          return(list(img_rgb = path, img_gray = path@.Data[, , w]))
     } else {
          # de lo contrario se carga la imagen y se enmarca
          (img <- enmarca.img(img = EBImage::readImage(files = path)))
          # tener en cuenta que img_rgb es de la clase imgage y img_gray
          # # es una matriz sin clase
          return(list(img_rgb = img, img_gray = img@.Data[, , w]))
     }}
# ________________________________________
binarize <- function(matrix, threshold = 0.5) {
     EBImage::as.Image(matrix > threshold)
}
# ________________________________________
# ________________________________________
contorno <- function(p, imagematrix){
     # Dado un punto p y una imagen imagematrix, devuelve una lista
     # de valores que definen el contorno
     (x <- round(p))
     (I <- imagematrix)
     #(x <- rev(x))
     #x[1] <- dim(I)[1] - x[1]
     while (abs(I[x[1], x[2]] - I[x[1], (x[2] - 1)]) < 0.1) {
          x[2] <- x[2] - 1}
     a <- 1
     M <- matrix(c(0, -1, -1, -1, 0, 1, 1, 1, 1, 1, 0, -1, -1, -1, 0, 1),
                 2,8, byrow = T)
     M <- cbind(M[,8],M,M[,1])
     X <- 0; Y <- 0
     x1 <- x[1]; x2 <- x[2]
     SS <- NA; S <- 6


     while ((any(c(X[a],Y[a]) != c(x1,x2) ) | length(X) < 3) & length(Y) < 10000) {

          if (abs(I[x[1] + M[1, S + 1], x[2] + M[2, S + 1]] - I[x[1], x[2]]) < 0.1) {
               a <- a + 1; X[a] <- x[1]; Y[a] <- x[2]; x <- x + M[, S + 1]
               SS[a] <- S + 1; S <- (S + 7) %% 8
          }
          else if (abs(I[x[1] + M[1,S + 2],x[2] + M[2,S + 2]]
                       - I[x[1],x[2]]) < 0.1) {
               a <- a + 1; X[a] <- x[1];Y[a] <- x[2]; x <- x + M[, S + 2]
               SS[a] <- S+2; S <- (S + 7) %% 8}
          else if (abs(I[x[1] + M[1, (S + 3)], x[2] + M[2, (S + 3)]]
                       - I[x[1],x[2]]) < 0.1)
          {a <- a + 1; X[a] <- x[1]; Y[a] <- x[2]; x <- x + M[,(S+3)]
          SS[a] <- S + 3; S <- (S + 7) %% 8}
          else S <- (S + 1) %% 8
     }

     if (length(Y) >= 10000) stop("Possible infinite loop")
     data.frame(x = (Y[-1]), y = ((dim(I)[1] - X))[-1])
}
# ________________________________________
which.matrix <- function(M.dim, index, is.dist.matrix = FALSE) {
     # Dice los indices de fila y columna de una matriz dado un vector index
     # de indices. M.dim puede ser una matriz o un vector de longitud 2
     # de las dimensiones de la matriz
     if (!is.matrix(M.dim) & length(M.dim) == 2) dims <- M.dim
     if (is.matrix(M.dim)) dims <- dim(M.dim)
     if (class(M.dim) == "dist") dims <- rep(attributes(M.dim)$Size, 2)
     if (!is.dist.matrix & any(index > prod(dims))) stop("Indice mayor que el maximo de la matriz")
     if (is.dist.matrix & any(index > ((dims[1]^2) - dims[1])/2)) stop("Indice mayor que el maximo de la matriz")
     if (length(index) == 0) return(integer(0))

     if (!is.dist.matrix) {
          (row <- index %% dims[1])
          (col <- index %/% dims[1])

          col[row != 0] <- col[row != 0] + 1; col
          row[row == 0] <- dims[1]; row

     } else {
          (lmt <- cumsum((dims[1] - 1):1))
          # columna
          (col <- sum((index - 1) %/% (lmt) != 0) + 1)
          (lmt <- cbind(c(0, lmt[-length(lmt)]) + 1, lmt))
          (row <- sum(index  %/% lmt[col, 1]:lmt[col, 2] != 0) + col)
     }
     return(cbind(row, col))
}
# ________________________________________
rota <- function(cnt, angle = 0, radians = TRUE) {
     # Funcion que acepta una lista cnt = list(X, Y) de coordenadas
     # de contorno y un angulo angle  y hace rotar los puntos del
     # contorno una cantidad de angulos igual al mismo, respecto
     # al origen de coordenadas {x=0,y=0}
     if (!radians) angle <- pi*angle/180

     # Calcular las distancias (d) entre los puntos y el origen {x=0,y=0}
     # por Pitagoras
     (d <- sqrt(cnt$x^2 + cnt$y^2))

     # Calcular los angulos correspondientes a cada punto, respecto al origen
     # {x=0,y=0}
     (angles <- apply(X = as.matrix(cnt), MARGIN = 1, FUN = angle2d))

     # Restar angle a todos los angles
     (angles <- angles - angle)

     # Recalcular cnt via, X e Y
     x <- d*cos(angles); y <- d*sin(angles); cnt <- data.frame(x = x, y = y)
     cnt

}
# ________________________________________
area.polygono <- function(X, Y = NULL) {
     Xm <- c(X[-1], X[1]); Ym <- c(Y[-1], Y[1])
     (Xdiff <- Xm - X)
     (Area <- abs(sum((Y + Ym)*Xdiff/2)))
}
#____________________________________________
perimetro.polygono <- function(X, Y = NULL) {
     if (is.null(Y)) Y <- 1:length(X)
     sum(sqrt((diff(Y))^2 + (diff(X))^2))
}
# ________________________________________
skewness <- function(x) # Asimetria
{
     (n <- length(x))
     (x <- x - mean(x))
     (r <- n * sum(x ^ 4)/(sum(x ^ 2) ^ 2))
     r * (1 - 1/n) ^ 2 - 3
}
# ________________________________________
angle2d <- function(v1, v2 = c(0, 0), radians = TRUE) {
     v1 <- complex(1,v1[1],v1[2])
     v2 <- complex(1,v2[1],v2[2])
     angulo <- (pi + Arg(v1) - Arg(v2)) %% (2*pi) - pi
     if (radians) return(angulo) else return(180*angulo/pi)
}
#____________________________________________
regularradius <- function(cnt, n) {
     Rx <- cnt$x;  Ry <- cnt$y
     # Estandariza el numero de puntos pero en funcion de q
     # la distancia interangulo sea la misma
     # tambien centra la figura
     le <- length(Rx)
     M <- matrix(c(Rx, Ry), le, 2)
     M1 <- matrix(c(Rx - mean(Rx), Ry - mean(Ry)), le, 2)
     V1 <- complex(real = M1[, 1], imaginary = M1[, 2])
     M2 <- matrix(c(Arg(V1), Mod(V1)), le, 2)
     V2 <- NA
     for (i in 0:(n - 1)) {
          V2[i + 1] <- which.max((cos(M2[, 1] - 2 * i * pi/n)))
     }
     V2 <- sort(V2)
     colnames(M1) <- c("x", "y"); M1 <- as.data.frame(M1)
     list("pixindices" = V2,"radii" = M2[V2, 2], "coord" = M1[V2, ])
}

write.tps <- function(puntos, diroutput, filename, fc = 1, EggId = 1) {
     # puntos debe ser una matriz o un data frame de nx2 puntos clave
     #puntos <- as.matrix(puntos);
     nr <- nrow(puntos)

     fileoutput <- file.path(diroutput, "Landmarks_FoliometriK.TPS")
     if (!file.exists(fileoutput)) {p <- ""} else {p <- "\n\n"}
     cat(paste0(p, "LM=", nr, "\n"), file = fileoutput, append = TRUE)
     write.table(x = puntos, file = fileoutput, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
     cat(paste0("IMAGE=", basename(path = filename)), file = fileoutput, append = TRUE)
     cat(paste0("\nID=", EggId), file = fileoutput, append = TRUE)
     cat(paste0("\nSCALE=", fc), file = fileoutput, append = TRUE)
}

#____________________________________________
write.datos_hojas <- function(datos_hojas, filename, landmarksformat = ".TPS",
                              img.dim, EggId, ref.length, diroutput,
                              dec = ",", addbasallobepoints = FALSE) {

     sep <- ";"; if (dec == ".") sep <- "," # definir separador en funcion de dec
     dimensiones <- paste0(img.dim[1], "X", img.dim[2])

     (Data <- data.frame(file = filename, Id = EggId, unit = datos_hojas$unit,
                         Length_1 = datos_hojas$L1, Length_2 = datos_hojas$L2,
                         Max_Width = datos_hojas$Am,
                         x_Max_Width  = datos_hojas$x_Am,
                         petiole_width = datos_hojas$petiole_width,
                         w1 = datos_hojas$WQ[1],
                         w2 = datos_hojas$WQ[2],
                         w3 = datos_hojas$WQ[3],
                         Perimeter = datos_hojas$Perim,
                         Area = datos_hojas$SA,
                         Area_Lat_Asym = datos_hojas$SA_Lat_Asym,
                         Area_Base_Lobes = datos_hojas$basallobearea,
                         Shape_Index = datos_hojas$Shape_Index,
                         ang_v1 = datos_hojas$angulos[1],
                         ang_m1 = datos_hojas$angulos[2],
                         ang_v2 = datos_hojas$angulos[3],
                         ang_m2 = datos_hojas$angulos[4],
                         ABL_angle = datos_hojas$bases_apice_angle,
                         img_dim = dimensiones,
                         fc = datos_hojas$fc, ref_length = ref.length))

     (file.output <- file.path(diroutput, "Measures_FoliometriK.csv"))

     (col.names <- !file.exists(file.output))

     write.table(x = Data, file = file.output, row.names = FALSE,
                 col.names = col.names, append = TRUE, quote = FALSE,
                 sep = sep, dec = dec)

     puntos <- datos_hojas$puntos

     if (addbasallobepoints) quitar <- "Am" else quitar <- c("Am", "b")
     if (datos_hojas$rejilla.de.puntos.clave != "vertical") quitar <- c("v", quitar)
     puntos <- quitar.nonlandmarks(puntos = puntos, quitar = quitar)


     if (landmarksformat == ".TPS") {
          write.tps(puntos = puntos, diroutput = diroutput, EggId = EggId,
                    filename = filename, fc = datos_hojas$fc)
     } else {
          cat(paste0(basename(filename), "_fc=", datos_hojas$fc, "_Id=", EggId), c(t(puntos)), "\n",
              file = file.path(diroutput, "Landmarks_FoliometriK.NTS"),
              append = TRUE)
     }
}

quitar.nonlandmarks <- function(puntos, quitar = c("Am", "b"))
     # donde puntos es una matriz de puntos clave de 2 columnas y nombres
     # de filas asociados al tipo de punto. Los puntos a eliminar seran
     # Am, b y en algunos casos, v (rejilla vertical)
{
     wquitar <- numeric(0)
     for (i in 1:length(quitar)) {
          wquitar <- c(wquitar, grep(pattern = quitar[i], x = rownames(puntos)))
     }
     if (length(wquitar) != 0) puntos <- puntos[-wquitar, ]
     return(puntos)

}

# ________________________________________
write.fourier.results <- function(Fourier, filename, diroutput, Id,
                                  dec = ",") {
     sep <- ";"; if (dec == ".") sep <- ","
     nombre <- paste0(basename(filename), " id=", Id)
     #nombre <- gsub(pattern = " ", replacement = "_", x = nombre)
     (Fourier <- data.frame(nombre, as.data.frame(rbind(c(t(as.data.frame(Fourier)[, 1:4]))))))
     write.table(x = Fourier, file = file.path(diroutput, "Fourier's_harmonics_FoliometriK.csv"),
                 append = TRUE, quote = FALSE, sep = sep, dec = dec,
                 row.names = FALSE, col.names = FALSE)

}
#____________________________________________
puntero <- function(ry, punto, texto, up = TRUE, cex  = 0.9, font = 1) {
     if (up) {
          segments(x0 = punto[1], y0 = punto[2], x1 = punto[1] + 0.1*diff(ry),
                   y1 = punto[2] + 0.05*diff(ry))
          text(punto[1] + 0.1*diff(ry), punto[2] + 0.05*diff(ry),
               texto, adj = 0, pos = 4, cex = cex, font = font)
     }
     if (!up) {
          segments(x0 = punto[1], y0 = punto[2], x1 = punto[1] + 0.1*diff(ry),
                   y1 = punto[2] - 0.05*diff(ry))
          text(punto[1] + 0.1*diff(ry), punto[2] - 0.05*diff(ry),
               texto, adj = 0, pos = 4, cex = cex, font = font)
     }
}
#____________________________________________
extension <- function(filename, value = NULL, maxchar = 10)
{
     if (!is.null(value)) {
          extension(filename) <- value
          return(filename)
     }
     lfn <- nchar(filename)
     ext <- list()
     for (f in 1:length(filename)) {
          extstart <- -1
          for (i in lfn[f]:2) {
               if (substr(filename[f], i, i) == ".") {
                    extstart <- i
                    break
               }
          }
          if (extstart > 0) {
               ext[f] <- substr(filename[f], extstart, lfn[f])
          }
          else {
               ext[f] <- ""
          }
     }
     ext <- unlist(ext)
     ext[nchar(ext) > maxchar] <- ""
     return(ext)
}

#_______________________________________________________
calcula.puntos.y.constantes.hoja <- function(cnts, id.pt.max.dist, filename,
                                             EggId, fc,
                                             unit = "cm",
                                             md_coef = 0.035,
                                             n.divisiones.rejilla = 3,
                                             nseqpoints = 15,
                                             desv.origen.coef = 0,
                                             rejilla.de.puntos.clave = "radial",
                                             addedlandmarks = NULL,
                                             addLength1 = TRUE, addLength2 = TRUE,
                                             addbasallobepoints = FALSE,
                                             addbasallobearea = FALSE,
                                             addbasalapexangle = TRUE,
                                             addpetiolewidth = FALSE) {
     # LLeva a cabo los calculos de todas las variables de la hoja. Devuelve una
     # lista con todas las longitudes, areas y puntos clave
     # Acepta una lista cnts = list(x, y) del contorno de la hoja, donde el eje
     # de la hoja coincide con el eje x
     # id.pt.max.dist es el vector de indices c(id1, id2) de los puntos de cnts
     # de maxima distancia entre ellos
     # fc: factor de conversion a unidades reales de la hoja
     # md_coef: si d es la distancia entre los puntos de maxima distancia del
     # contorno cnts, entonces la distancia minima entre dos puntos clave,
     # para que sean considerados diferentes entre si es md_coef*d. Para puntos
     # mas cercanos entre si, podran ser considerados un unico punto
     # n.divisiones de la rejilla: numero de divisiones de la rejilla vertical
     #  o radial


     # con esto se calcula la distancia maxima en el contorno
     (id <- id.pt.max.dist)
     (max_dist <- sqrt((cnts$x[id[2]] - cnts$x[id[1]])^2 + (cnts$y[id[2]] - cnts$y[id[1]])^2))

     # Puntos clave de tipo 1: vertice, ancho maximo, lobulos basales y apicales
     (puntos_del_vertice <- puntos.de.poligono.q.intercectan.una.recta(cnts = cnts))
     (puntos_del_vertice <- puntos_del_vertice[order(puntos_del_vertice[, 1]), ])
     rownames(puntos_del_vertice) <- c("v1", "v2"); puntos_del_vertice
     (puntos_de_ancho_maximo <- puntos.de.ancho.maximo(cnt = cnts))
     (puntos_lobulos_basales <- resume.puntos(puntos.lobulos.basales(cnts = cnts),
                                              md = md_coef * max_dist))


     petiole_width <- NA
     if (addpetiolewidth) {
     puntos_peciolo <- puntos.de.poligono.q.intercectan.una.recta(cnts = cnts, v = puntos_del_vertice[1, 1], np = 6)
     if (nrow(puntos_peciolo) > 1) petiole_width <- fc * max(dist(puntos_peciolo)) else ancho_peciolo <- 0
     }

     # Angulo entre el apice y los puntos de los lobulos basales
     if (!addbasalapexangle) bases_apice_angle <- NA else {
     bases_apice_angle <- ifelse(test = nrow(puntos_lobulos_basales) == 2, yes = abs(angle2d(v1 = c(puntos_lobulos_basales[1, 1] - puntos_del_vertice[2, 1], puntos_lobulos_basales[1, 2]), radians = FALSE,
     v2 = c(puntos_lobulos_basales[2, 1] - puntos_del_vertice[2, 1], puntos_lobulos_basales[2, 2]))), no = 0)
     }

     # Longitudes
     {# largo de la hoja desde x=0
          L1 <- ifelse(addLength1, fc * max(puntos_del_vertice[, 1]), NA)
          # largo desde el origen del vertice (v1)
          L2 <- ifelse(addLength2, fc * diff(sort(puntos_del_vertice[, 1])), NA)
          # Ancho maximo
          (Am <- fc * c(dist(puntos_de_ancho_maximo)))
          # perimetro de la hoja
          (Perim <- perimetro.polygono(X = fc*cnts$x, Y = fc*cnts$y))
     }

     # Anchos no maximos (en cuartiles)
     {
          sq <- seq(0, max(cnts$x), length.out = 5)[-c(1, 5)]
          WQ <- numeric(3); names(WQ) <- paste0("w", 1:3)
          for (i in 1:3) {
               WQ[i] <- fc*dist(puntos.de.poligono.q.intercectan.una.recta(cnts = cnts,
                                                                           v = sq[i], np = 2))[1]
          }
     }

     # Areas superficiales de la hoja y del rectangulo externo
     {# area de la hoja
          (SA <- area.polygono(X = fc*cnts$x, Y = fc*cnts$y))
          # area del rectangulo que rodea la hoja
          (SA_rect <- (fc^2) * diff(range(cnts$x)) * diff(range(cnts$y)))
          # indice de forma: area de la hoja/area del rectangulo
          Shape_Index <- SA/SA_rect
          # Area de Asimetria bilateral.
          (SA_Lat_Asym <- area.polygono(X =  fc * cnts$x, # hacer positivos todas
                                        Y = fc * abs(cnts$y)))#las coordenadas y
          # Area de lobulos basales
          basallobearea <- ifelse(addbasallobearea, basal.lobes.area(cnts = cnts, fc = fc), NA)
     }

     # Angulos
     {
          # Usamos los puntos del vertice y los puntos de la parte media del
          # contorno para hallar los angulos
          (ang <- rbind(puntos_del_vertice, puntos.verticales(cnts = cnts, n.lineas = 1)))
          rownames(ang) <- gsub(pattern = "q", replacement = "m", rownames(ang)); ang
          ang <- ang[c(1, 3, 2, 4), ]; ang
          ang <- ang[c(4, 1:4, 1), ]; ang
          (angulos <- numeric(4))
          for (i in 1:4) {
               (trespuntos <- ang[i:(i + 2), ])
               trespuntos[, 1] <- trespuntos[, 1] - trespuntos[2, 1]; trespuntos
               trespuntos[, 2] <- trespuntos[, 2] - trespuntos[2, 2]; trespuntos
               (angulos[i] <- angle2d(v1 = trespuntos[1, ],
                                      v2 = trespuntos[3, ], radians = FALSE))
          }
          attributes(angulos) <- list(ang = ang[2:5, ]); angulos
          names(angulos) <- paste0("ang_", rownames(ang)[2:5]); angulos
          (angulos <- abs(angulos))

     }

     # PUNTOS CLAVE de tipo 2: por rejillas
     {
          x <- y <- numeric(0); (puntos_clave <- cbind(x, y))

          (desv.origen <- desv.origen.coef * (round(mean(range(cnts$x))) -  puntos_del_vertice[1, 1]))

          if (rejilla.de.puntos.clave == "radial") {
               (puntos_clave <- puntos.radiales(cnts = cnts, desv.origen = desv.origen,
                                                n.radios = n.divisiones.rejilla))
          }

          if (rejilla.de.puntos.clave == "vertical") {
               (puntos_clave <- puntos.verticales(cnts = cnts,
                                                  n.lineas = n.divisiones.rejilla))
          }

          if (rejilla.de.puntos.clave == "sequential points") {
               (indx <- seq(1, nrow(cnts), length.out = nseqpoints + 1))
               (puntos_clave <- cnts[indx[-length(indx)], ])
               rownames(puntos_clave) <- paste0("sqp", 1:nrow(puntos_clave))
          }

          if (!is.null(addedlandmarks)) {
               if (nrow(addedlandmarks) == 0) {addedlandmarks <- NULL
               } else {
                    rownames(addedlandmarks) <- paste0("addlmk", 1:nrow(addedlandmarks))
               }
          }

          if (!is.null(addedlandmarks)) colnames(addedlandmarks) <- c("x", "y")
     }

     # if (addbasallobepoints) {

     puntos <- rbind(puntos_del_vertice, puntos_lobulos_basales,
                     puntos_de_ancho_maximo,
                     puntos_clave, addedlandmarks)


     {# Reordenar los puntos
          (med <- colMeans(puntos[1:2, ]))
          (puntos <- t(t(puntos) - med))
          (v <- puntos[1:2, ]); puntos <- puntos[-(1:2), ]
          a <- apply(X = puntos, MARGIN = 1, FUN = angle2d)*180/pi
          puntos <- puntos[order(a), ]
          cambio <- which(puntos[, 2] > 0)[1]
          puntos <- rbind(v[1, , drop = FALSE], puntos[1:(cambio - 1), , drop = FALSE], v[2, , drop = FALSE], puntos[cambio:nrow(puntos), ])
          puntos <- t(t(puntos) + med)

     }


     list(fc = fc, unit = unit, n.divisiones.rejilla  = n.divisiones.rejilla,
          desv.origen = desv.origen, filename = filename, EggId = EggId,
          rejilla.de.puntos.clave = rejilla.de.puntos.clave,
          id.pt.max.dist = id.pt.max.dist, max_dist = max_dist, nseqpoints = nseqpoints,
          L1 = L1, L2 = L2, Am = Am, x_Am = fc * puntos_de_ancho_maximo[1],
          WQ = WQ, Perim = Perim, SA = SA, SA_Lat_Asym = SA_Lat_Asym,
          Shape_Index = Shape_Index, angulos = angulos, puntos = puntos, cnts = cnts,
          addbasallobepoints = addbasallobepoints, basallobearea = basallobearea,
          bases_apice_angle = bases_apice_angle, petiole_width = petiole_width)

}



plot.landmarks <- function(datos_hojas, hidepoints = FALSE) {
     # acepta una lista que es el resultado
     # de la funcion 'calcula.puntos.y.constantes.hoja' y genera un grafico
     # con puntos clave y el contorno de la hoja
     # hidepoints determina si se dibujan o no los puntos y lineas

     par(col.axis = gray(0.7), col.lab = gray(0.2), cex.lab = 1.5, font.sub = 6, font = 6, font.axis = 6,
         font.lab = 6, font.main = 7, mar = c(6, 4, 1, 1))


     # contorno de la hoja
     plot.default(datos_hojas$cnts$x, datos_hojas$cnts$y, type = "l",
          asp = 1, las = 1, axes = FALSE, xlab = datos_hojas$unit,
          ylab = datos_hojas$unit, col = "#00000000")
     graphics::title(sub = datos_hojas$filename)

     (id <- round(seq(1, length(datos_hojas$cnts$x) - 30, length.out = 8)))
     arrows(x0 = datos_hojas$cnts$x[id], y0 = datos_hojas$cnts$y[id],
            x1 = datos_hojas$cnts$x[id + 20],
            y1 = datos_hojas$cnts$y[id + 20], length = 0.1, lwd = 2)

     s1 <- gray(0.9)
     axis(side = 1, col.ticks = s1, col = par()$bg, at = axTicks(1), labels = round(datos_hojas$fc * axTicks(1), 2))
     axis(side = 2, col.ticks = s1, col = par()$bg, at = axTicks(2), labels = round(datos_hojas$fc * axTicks(2), 2), las = 1)
     grid(col = s1)

     polygon(x = datos_hojas$cnts$x, y = datos_hojas$cnts$y, col = transp("#05a90a", 0.15) , border = gray(0.2))
     abline(h = 0, v = 0, lty = 1, col = gray(0.25))


     if (datos_hojas$addbasallobepoints) quitar <- "Am" else quitar <- c("Am", "b")
     if (datos_hojas$rejilla.de.puntos.clave != "vertical") quitar <- c(quitar, "v")
     (landmarks <- quitar.nonlandmarks(puntos = datos_hojas$puntos, quitar = quitar))


     rcol <- gray(0.3); if (hidepoints) rcol <- "#00000000" # color de las rejillas
     origen <- datos_hojas$puntos[1, ]; origen[1] <- origen[1] + datos_hojas$desv.origen
     if (datos_hojas$desv.origen < 0) origen <- sapply(datos_hojas$cnts, mean, na.rm = TRUE)

     (exclude <- agrep(pattern = "addlmk", x = rownames(landmarks), max.distance = 0)) # excluir radios de landmarks manuales
     exclude <- sort(c(exclude, agrep(pattern = "b", x = rownames(landmarks), max.distance = 0))) # y de puntos lobulo basales

     # Si los puntos clave de rejilla son de una rejilla radial, dibujar
     if (datos_hojas$rejilla.de.puntos.clave == "radial") {  # la rejilla

          if (length(exclude) == 0) {
               segments(x0 = origen[1], y0 = origen[2], x1 = landmarks[, 1],
                        y1 = landmarks[, 2], col = rcol)
          } else {
               segments(x0 = origen[1], y0 = origen[2], x1 = landmarks[-exclude, 1],
                        y1 = landmarks[-exclude, 2], col = rcol)}
     }

     # Si los puntos clave de rejilla son de una rejilla vertical, dibujar
     if (datos_hojas$rejilla.de.puntos.clave == "vertical") { # la rejilla
          r <- range(datos_hojas$cnts$x)
          s <- seq(r[1], r[2], length.out = datos_hojas$n.divisiones.rejilla + 2)
          s <- s[-c(1, length(s))]
          abline(v = s, col = rcol)
     }


     (nrl2 <- (nrow(landmarks) - length(exclude))/2)
     lcol <- "#78C679"; mlcol <- "blue"; blcol <- "orange"  # lcol: landmarks color,
                                                         # mlcol: my landmark color, blcol: basal lobulo color
     if (nrl2 != round(nrl2)) lcol <- "#FB6A4A"
     if (hidepoints) lcol <- mlcol <- "#00000000"



     points(landmarks, pch = 21, cex = 3, bg = lcol, col = gray(0.5))
     if (length(exclude) != 0) points(landmarks[exclude, , drop = FALSE], pch = 21, cex = 3, bg = mlcol, col = gray(0.5))
     text(landmarks, labels = 1:nrow(landmarks), cex = 1, font = 2, col = "white")
     points(datos_hojas$cnts$x[1], datos_hojas$cnts$y[1], pch = 16, col = "red", cex = 0.5)
     points(mean(datos_hojas$cnts$x), mean(datos_hojas$cnts$y), pch = 3)

}

plot.angulos.hoja <- function(datos_hojas) {
     if (is.null(datos_hojas)) return(NULL)
     par(mar = c(6, 2, 2, 2))
     plot(datos_hojas$cnts, asp = 1, type = "n", axes = FALSE, xlab = "",
          ylab = "", main = "Internal angles")
     polygon(attr(datos_hojas$angulos, "ang"), lwd = 2)

     M <- attr(datos_hojas$angulos, "ang"); nm <- names(datos_hojas$angulos)
     medios <- rbind((M[1, ] + M[2, ])/2, (M[2, ] + M[3, ])/2, (M[3, ] + M[4, ])/2, (M[4, ] + M[1, ])/2)
     points(medios, pch = 16, col = "white", cex = 10)
     polygon(datos_hojas$cnts, border = "#10600220", lwd = 2)


     xj <- c(0, 0.5, 1, 0.5); yj <- c(0.5, 0, 0.5, 1)
     for (i in 1:4) {
          legend(M[i, 1], M[i, 2], legend = nm[i], bty = "o",
                 xjust = xj[i], yjust = yj[i], box.col = "#00000000",
                 bg = "#ffffff00", text.font = 4, text.col = "blue")
     }
     title(sub = datos_hojas$filename)
}

plot.rectangulo.hoja <- function(datos_hojas) {
     lmt <- sapply(datos_hojas$cnts, range)*datos_hojas$fc
     plot(datos_hojas$cnts*datos_hojas$fc,
          asp = 1, type = "n", main = "Shape Index",
          xlab = datos_hojas$unit, ylab = datos_hojas$unit, las = 1)
     grid()
     rect(xleft = lmt[1, 1], ybottom = lmt[1, 2], xright = lmt[2, 1],
          ytop = lmt[2, 2], col = gray(0.95), border = "white")
     polygon(datos_hojas$cnts*datos_hojas$fc,
             col = "white", border = gray(0.4))
     title(sub = datos_hojas$filename)
}

sombras.laterales <- function(datos_hojas) {
     cnts <- datos_hojas$cnts
     if (is.null(cnts)) return(NUL)
     # Genera sombras de los lados de la hoja
     # par(mar = rep(3, 4))
     plot(cnts$x, abs(cnts$y), axes = FALSE, xlab = "", ylab = "", main = "Surface area of asymmety between sides", asp = 1,
          type = "n")
     rx <- range(cnts$x); ry <- range(abs(cnts$y))
     rect(xleft = rx[1]-10*diff(rx), ybottom = ry[1]-10*diff(ry),
          xright = rx[2]+10*diff(rx), ytop = ry[2]+10*diff(ry),
          col = gray(0.5))
     abline(h=0, col = gray(0.8))
     id <- cnts$y >= 0; polygon(cnts[id, ],
                                border = "#00000000", col = "#ff00ff")
     id <- cnts$y <= 0; polygon(x = cnts$x[id], y = abs(cnts$y[id]),
                                border = "#00000000", col = "#00ff0088")
     legend(x = mean(range(cnts$x)), y = 0.3 * mean(range(abs(cnts$y))),
            legend = c("superior", "inferior"), border = NULL,
            fill = c("#ff00ff", "#00ff0088"), box.col = gray(0.5),
            col = gray(0.5), bg = gray(0.5), text.col = "white",
            text.font = 2)
     title(sub = datos_hojas$filename)
}

{
     puntos.de.poligono.q.intercectan.una.recta <- function(cnts, a = 0, b = 0, v = NULL,
                                                            max.dist = 1.5, np = 2) {
          # Encuentra el id de los puntos de un poligono formado por coordenadas
          # discretas e.g. {x = 1,2,3,4; y = 3,4,2,1) que intersectan una recta.
          # b es la pendiente de la recta y a el intersecto, max.dist es la distancia
          # maxima aceptada para asumir que un punto del poligono intersecta la recta
          # np es el numero de puntos que intersecta la recta. De manera que si muchos
          # puntos estan muy cerca, se tratara de agrupar todos los puntos en np grupos
          # y se data solo el id de los puntos mas cercanos a la recta en cada grupo

          if (!is.null(v)) {a <- v; b <- 0; cnts <- data.frame(x = cnts$y, y = cnts$x)}
          # cuales puntos cumplenla condicion de estar a una distancia <= que max.dist
          (id <- which(distancia.puntos.recta(xp = cnts$x, yp = cnts$y, a = a, b = b) <= max.dist))

          # si ningun punto cumple la condicion, devolver un numeric(0)
          if (length(id) == 0) return(numeric(0))
          # si uno o dos puntos cumplen la condicion, devolver el punto o los puntos
          if (length(id) == 1 | length(id) == 2) puntos <- cbind(cnts$x[id], cnts$y[id])
          # Si el numero de puntos que cumplen la condicion es menor que el numero
          # de grupos np, es necesario cambiar el valor de np, o dara un error en pam
          if (length(id) <= np) np <- length(id) - 1
          # si mas de dos puntos cumplen, hacer un agrupamiento en np grupos (puntos)
          if (length(id) > 2) {
               (puntos <- cluster::pam(cbind(cnts$x[id], cnts$y[id]), k = np)$medoids)
          }
          if (!is.null(v)) puntos <- puntos[, 2:1]
          colnames(puntos) <- c("x", "y"); puntos

          # md es una medida burda o aproximacion de la maxima distancia en el
          # poligono. Es la maxima distancia en el eje x o y, en dependencia de cual
          # de las dos sea mayor
          (md <- max(c(diff(range(cnts$x)), diff(range(cnts$y))))*0.015)

          # si la distancia entre cualquier par de puntos es menor que md, se
          # considerara que esta repetido y se eliminara el punto

          resume.puntos(puntos = puntos, md = md)
     }

     distancia.puntos.recta <- function(xp, yp, a = 0, b = 0) {
          abs(b*xp - yp + a)/sqrt(b^2 + 1)
     }

     resume.puntos <- function(puntos, md) {
          # si puntos es una matriz de coordenadas x, y de N puntos, donde
          # dim(puntos) es N x 2, siendo las columnas, de nombres x e y
          # evalua las distancias entre puntos, de manera jerarquica (de arriba
          # a abajo) y para cada punto, elimina cualquiera de los otros siempre
          # que cumpla que la distancia entre estos sea <= md
          # donde  md es la minima distancia admitida para ser considerados
          # dos puntos diferentes
          (d <- as.matrix(dist(puntos))) # matriz de dist entre pares de pntos
          delete <- numeric(0) # id de los puntos que cumplen la condicion
          for (j in 1:(ncol(d) - 1)) { # para cada punto, cual de los siguientes tiene
               for (i in (j + 1):ncol(d)) { # una distancia menor que lo permitido
                    if (d[i, j] <= md) delete <- c(delete, i)
               }
          }
          (delete <- unique(delete)) # eliminar indices repetidos
          if (length(delete) != 0) puntos <- puntos[-delete, , drop = FALSE]
          puntos
     }

     puntos.lobulos.basales <- function(cnts) {
          # Extrae del contorno, los puntos de los lobulos basales
          (puntos <- cbind(x = numeric(0), y = numeric(0)))

          # puntos de la mitad superior
          (condicion <- cnts$y >= 0)
          # punto mas a la izquierda de la mitad superior
          (id <- which.min(cnts$x[condicion])) # id del punto
          (puntos <- rbind(puntos, c(cnts$x[condicion][id], cnts$y[condicion][id])))
          # puntos de la mitad inferior
          (condicion <- cnts$y <= 0)
          # punto mas a la izquierda de la mitad inferior
          (id <- which.min(cnts$x[condicion])) # id del punto
          (puntos <- rbind(puntos, c(cnts$x[condicion][id], cnts$y[condicion][id])))
          rownames(puntos) <- paste0("b", 1:nrow(puntos))
          puntos
     }

     puntos.radiales <- function(cnts, n.radios = 1, desv.origen = 0) {
          # genera una serie de radios y da los puntos que intersectan con el contorno
          # el origen de los radios es el punto basal del vertice de la hoja: {origen,0}
          # desv.origen: cantidad de unidades que se desviara el origen en el
          # eje x

          if (desv.origen > 0) {
               (origen_x <- min(puntos.de.poligono.q.intercectan.una.recta(cnts = cnts, a = 0,
                                                                           b = 0)[, 1]) + desv.origen)
               origen_y <- 0

          } else {# si desv.origen == 2, este valor no tiene validez, pues significa
               # que el origen es el centroide
               origen_x <- mean(cnts$x); origen_y <- mean(cnts$y)
          }

          # todas las rectas pasaran por el punto origen y tendran distintas pendientes
          # el primer grupo de puntos radiales es para la recta de 90 grados

          # las pendientes de las otras rectas se calculan
          b <- seq(0, pi, length.out = n.radios + 2)
          b <- b[-length(b)]; b
          b <- unique(tan(c(rbind(b, NULL)))) # convertir angulos a pendientes

          # Dado las pendientes b y el punto origen, los intersectos con el eje y son
          (a = -b*origen_x + origen_y)

          puntos <- NULL
          if (length(a) != 0) {
               for (i in 1:length(a)) {
                    puntos <- rbind(puntos,
                                    puntos.de.poligono.q.intercectan.una.recta(cnts = cnts,
                                                                               a = a[i],
                                                                               b = b[i],
                                                                               np = 4))
               }}
          rownames(puntos) <- paste0("r", 1:nrow(puntos))
          puntos
     }

     puntos.de.ancho.maximo <- function(cnt) {
          # funcion para calcular el ancho maximo de una
          # hoja especificamente. Acepta cnt, el contorno de la hoja, horizontal
          # (de manera que el eje
          # maximo coincida con el eje x, donde y = 0) y los puntos de distancia
          # maxima descansan sobre el eje x: (p1, 0), (p2, 0). Ademas los valores
          # de contorno deben estar redondeados (ser numeros enteros). La funcion
          # devuelve los dos puntos donde se alcanza el maximo ancho

          # convertir cnt a una matriz M
          (M <- sapply(cnt, cbind))
          # Ordenar las filas de menor a mayor en funcion de y
          M <- M[order(M[, 1]), ]; head(M, 15)
          # yv contiene los valores unicos no repetidos de y
          yv <- unique(M[, 1]); yv; nyv <- length(yv)
          # extremos contendra los valores minimo y maximo de x, para cada
          # valor de y
          extremos <- matrix(NA, nr = nyv, nc = 3)
          colnames(extremos) <- c("x", "ymin", "ymax"); head(extremos)

          # Llenado de la matriz extremos
          for (i in 1:nyv) {
               extremos[i, ] <- c(yv[i], range(M[M[, 1] == yv[i], 2]))
          }; head(extremos)
          extremos <- na.omit(extremos)
          (ancho.maximo <- max(extremos[, 3] - extremos[, 2]))
          (puntos <- extremos[extremos[, 3] - extremos[, 2] == ancho.maximo, , drop = FALSE])

          (puntos <- rbind(puntos[, 1:2], puntos[, c(1, 3)]))
          if (nrow(puntos) > 2) puntos <- cluster::pam(x = puntos, k = 2)$medoids
          (puntos <- puntos[order(puntos[, 2], decreasing = TRUE), ])
          rownames(puntos) <- paste0("Am_", c("sup", "inf"))
          colnames(puntos) <- c("x", "y")
          puntos
     }

     puntos.verticales <- function(cnts, n.lineas = 1) {
          (v <- seq(min(cnts$x), max(cnts$x),
                    length.out = n.lineas + 2)[-c(1, n.lineas + 2)])
          x <- y <- numeric(0); puntos <- cbind(x, y); puntos
          for (i in 1:length(v)) {
               puntos <- rbind(puntos, puntos.de.poligono.q.intercectan.una.recta(cnts = cnts, v = v[i], np = 6))
          }
          rownames(puntos) <- paste0("q", 1:nrow(puntos))
          puntos
     }

     transp <- function(col, alpha = 0.5)
     {
          # adegenet::transp modificado ligeramente
          res <- apply(col2rgb(col), 2, function(c) rgb(c[1]/255, c[2]/255,
                                                        c[3]/255, alpha))
          if (length(alpha) > 1) {colnames(res) <- col; rownames(res) <- alpha}
          return(res)
     }

}


write.coord.results <- function(cnts, diroutput, fileinputname, append, EggId,
                                fc, unit, dec = ",")
     # cnts son las coordenadas del contorno
     # que se van aguardar en el archivo. El valor de este argumento deberia
     # ser solo de unas 150 coordenadas. diroutput es el directorio de salida
     # fileinputname es el nombre del archivo de origen
     # append es la orden de si se debe sobreescribir un archivo existente
     # puede tomar los valores TRUE o FALSE
     # id es un numero que puede variar si hay varios huevos en una misma foto
     # fc es un factor de conversion
     # unit es el nombre de la unidad de medida usada
{
     sep <- ";"; if (dec == ".") sep <- "," # definir separador en funcion de dec
     # Guardar en archivo txt los contornos
     # Si deben ser guardados en archivos separados:
     if (!append) {
          (diroutput <- file.path(diroutput, "Contour_FoliometriK"))
          if (!dir.exists(diroutput)) dir.create(diroutput)

          (filename <- file.path(diroutput, fileinputname))
          (filename <- gsub(pattern = extension(filename = filename),
                            replacement = " ", x = filename))
          (filename <- paste0(filename, "contour fc=", fc, " ", unit, " id=", EggId,
                              ".csv"))
          # Reducir el contorno a tan solo 150 puntos
          M <- round(regularradius(cnts, 150)$coord); colnames(M) <- c("x", "y")
          write.table(x = round(as.data.frame(cnts)), file = filename,
                      append = FALSE, row.names = FALSE,
                      col.names = TRUE, quote = FALSE, sep = sep, dec = dec)
     }

     # O en un mismo archivo
     if (append) {

          (filename <- paste0("Eggs_Contour_fc=", fc, " ",
                              unit, ".csv"))
          (filename <- file.path(diroutput, filename))

          # Si el archivo existe, anexar sin nombres de columna
          (cn <- !file.exists(filename))


          write.table(x = data.frame(cnts, id = EggId, file = fileinputname),
                      file = filename,  quote = FALSE, sep = sep, dec = dec,
                      append = TRUE, row.names = FALSE,
                      col.names = cn)
     }


}




# Funciones de flip, flop, transpose, setmaxdist, etc
{
     setmaxdist_ <- function(cnts, id.pt.max.dist) {
          (escala1 <- list(x = cnts$x[id.pt.max.dist[1]], y = cnts$y[id.pt.max.dist[1]]))
          (escala2 <- list(x = cnts$x[id.pt.max.dist[2]], y = cnts$y[id.pt.max.dist[2]]))
          (angle <- atan(escala1$y - escala2$y)/(escala1$x - escala2$x))
          # Angulo entre los puntos de maxima distancia
          (angle <- atan((cnts$y[id.pt.max.dist[1]] - cnts$y[id.pt.max.dist[2]])/(cnts$x[id.pt.max.dist[1]] - cnts$x[id.pt.max.dist[2]])))

          # Punto medio entre los puntos de maxima distancia
          (medio <- c(mean(c(cnts$x[id.pt.max.dist[1]], cnts$x[id.pt.max.dist[2]])),
                      mean(c(cnts$y[id.pt.max.dist[1]], cnts$y[id.pt.max.dist[2]]))))

          # Centrar el contorno en el origen antes de rotar
          cnts$x <- cnts$x - medio[1]; cnts$y <- cnts$y - medio[2]
          # Rotar contorno
          cnts <- rota(cnt = cnts, angle = angle)
          # Llevar a enteros
          cnts <- round(cnts)
          # Trasladar
          cnts$x <- cnts$x - min(cnts$x)
          return(as.data.frame(cnts))
     }

     setflip_ <- function(cnts) {
          cnts$y <- -cnts$y
          cnts$x <- cnts$x - min(cnts$x)
          return(cnts)
     }

     setflop_ <- function(cnts) {
          cnts$x <- -cnts$x
          cnts$x <- cnts$x - min(cnts$x)
          return(cnts)
     }

     settranspose_ <- function(cnts) {
          X <- cnts$x; cnts$x <- cnts$y; cnts$y <- X; rm(X)
          cnts$y <- cnts$y - cnts$y[which.min(cnts$x)]
          cnts$x <- cnts$x - min(cnts$x)
          return(cnts)
     }

}



saveGraphics <- function(diroutput, datos_hojas) {
     gr.pth <- file.path(diroutput, "Graphics_FoliometriK")
     gr.pth <- c(gr.pth, file.path(gr.pth, "Angles"),
                 file.path(gr.pth, "Asymmetry"),
                 file.path(gr.pth, "Rectangle_Area"),
                 file.path(gr.pth, "Outline"),
                 file.path(gr.pth, "Landmarks"))

     for (i in 1:length(gr.pth)) {
          if (!dir.exists(gr.pth[i])) dir.create(gr.pth[i])
     }

     ext <- extension(datos_hojas$filename)
     datos_hojas$filename <- gsub(pattern = ext,
                                  replacement = paste0("(", datos_hojas$EggId, ")", ext),
                                  x =  datos_hojas$filename)

     grDevices::png(filename = file.path(gr.pth[2], datos_hojas$filename), width = 900, height = 900)
     plot.angulos.hoja(datos_hojas = datos_hojas)
     dev.off()

     grDevices::png(filename = file.path(gr.pth[3], datos_hojas$filename), width = 900, height = 900)
     sombras.laterales(datos_hojas = datos_hojas)
     dev.off()

     grDevices::png(filename = file.path(gr.pth[4], datos_hojas$filename), width = 900, height = 900)
     plot.rectangulo.hoja(datos_hojas = datos_hojas)
     dev.off()

     grDevices::png(filename = file.path(gr.pth[5], datos_hojas$filename), width = 900, height = 900)
     plot(datos_hojas$cnts, asp = 1, axes = FALSE, ann = FALSE,
          type = "l", lwd = 2)
     title(sub = datos_hojas$filename)
     dev.off()

     grDevices::png(filename = file.path(gr.pth[6], datos_hojas$filename), width = 900, height = 900)
     plot.landmarks(datos_hojas = datos_hojas)
     dev.off()
}

#_____________________________________________________
which.csv.type <- function(path) {
     # Lee un archivo csv y dice el tipo de separador (; o ,)
     t1 <- scan(file = path, what = "", nlines = 2, sep = ";")
     t2 <- scan(file = path, what = "", nlines = 2, sep = ".")
     if (length(t1) > length(t2)) { return("csv2")} else {
          return("csv")
     }
}

is.counterclockwise <- function(cnts) {
     # Dice true si el contorno esta en sentido antihorario
     (r <- regularradius(cnts, 10)$coord)
     a <- numeric(nrow(r) - 2)
     for (i in 2:9) a[i - 1] <- angle2d(r[i, ], radians = FALSE)
     if (sum(diff(a), na.rm = TRUE) > 0) return(TRUE) else return(FALSE)
}

reorganiza.cnts <- function(cnts, id.pt.max.dist) {
     # cnts debe ser un data.frame de  x, y
     (cnts <- as.data.frame(cnts))
     (nr <- nrow(cnts))
     (v <- puntos.de.poligono.q.intercectan.una.recta(cnts = cnts, a = 0, b = 0, np = 6))
     dim(v) <- c(length(v)/2, 2); v
     (v <- v[order(v[, 1]), ][1, ])
     (idmin <- which(cnts$x == v[1] & cnts$y == v[2])[1])
     if (idmin != 1) (cnts <- rbind(cnts[idmin:nr, ], cnts[1:(idmin - 1), ]))

     (id.pt.max.dist <-  id.pt.max.dist - idmin + 1)
     (w <- which(id.pt.max.dist <= 0))
     if (length(w) != 0) id.pt.max.dist[w] <- nr + id.pt.max.dist[w]


     if (!is.counterclockwise(cnts)) {
          cnts <- as.data.frame(sapply(cnts, rev))
          id.pt.max.dist <- nr - id.pt.max.dist
     }

     list(cnts = cnts, id.pt.max.dist = id.pt.max.dist)
}

plot_Fourier <- function(cnts, Fourier, seeoriginal = TRUE, seefourier = TRUE) {
     layout(rbind(1:2), widths = c(10, 1.5))
     col <- gray(0.95); border <- "black"
     if (!seeoriginal) {col <- "white"; border <- "white"}
     Momocs::coo_plot(coo = as.matrix(cnts), col = col, border = border)

     if (seefourier) {
          Momocs::coo_draw(coo = Momocs::efourier_i(Fourier, nb.pts = 250),
                           border = "#0055ffaa", lwd = 2)
     }

     points(cnts[1, ], pch = 16, col = ifelse(seeoriginal, "red", "white"),
            cex = 1.5)
     par(mar = rep(0, 4)); plot(1, axes = FALSE, type = "n")
     legend(x = 1, y = 0.85, legend = c("original", "Fourier", "origin", "centroid"),
            lwd = c(1, 2, NA, NA), col = c("black", "blue", "red", "black"), xjust = 0.5,
            fill = c(gray(0.9), "white", NA, NA), text.font = 9, cex = 1.2, pch = c(NA, NA, 16, 3),
            border = c("black", "black", NA, NA))
}

pca <- function(D) {
     (D <- t(t(D) - colMeans(D)))
     (e <- eigen(var(D)))
     pvar <- cumsum(e$values)/sum(e$values)
     PC <- as.matrix(D) %*% e$vectors
     colnames(PC) <- names(pvar) <- paste0("PC", 1:ncol(PC))
     list(pvar = pvar, PC = PC)
}



# ________________________________________
suaviza <- function(cnts, repetir = 100, roundit = FALSE) {

     print(cnts[1:10, ])

     # Suavizado de contorno
     (l <- nrow(cnts))
     for (j in 1:repetir) {
          (X1 <- append(cnts$x[-l], cnts$x[l], 0)); (X3 <- append(cnts$x[-1], cnts$x[1], l))
          (Y1 <- append(cnts$y[-l], cnts$y[l], 0));   (Y3 <- append(cnts$y[-1], cnts$y[1], l))
          cnts$x <- rowMeans(cbind(X1,cnts$x, X3)); cnts$y <- rowMeans(cbind(Y1, cnts$y, Y3))
     }

     print(cnts[1:10, ])
     if (roundit) cnts <- round(cnts)
     return(cnts)
}




basal.lobes.area <- function(cnts, fc = 1) {
     pt.v <- puntos.de.poligono.q.intercectan.una.recta(cnts = cnts, a = 0, b = 0)
     idsup <- cnts$y > 0 & cnts$x < min(pt.v[, 1]) # puntos del lobulo superior
     idinf <- cnts$y < 0 & cnts$x < min(pt.v[, 1]) # puntos del lobulo inferior

     Asup <- ifelse(sum(idsup) > 4, area.polygono(X = cnts$x[idsup], Y = cnts$y[idsup]), 0)
     Ainf <- ifelse(sum(idinf) > 4, area.polygono(X = cnts$x[idinf], Y = cnts$y[idinf]), 0)

     (fc^2) * (Asup + Ainf)
}




