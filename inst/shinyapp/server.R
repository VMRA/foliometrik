# FOLIOMETRIK


server <- function(input, output, session) {


     output$baner <- renderImage({
          list(src = "baner.jpg")
     }, deleteFile = FALSE)


     # rv sera la lista de valores reactivos creados durante la ejecucion
     # de la app
     rv <- reactiveValues()

     # rv$i es el contador. Indexa las posibles imagenes a procesar
     rv$i <- 0

     # rv$npixels es el numero de pixeles que hay en la distancia seleccionada
     # por input$medir (take the length size), asimismo el factor de conversion
     # sera 1
     rv$npixels <- 1;    rv$factor.conversion <- 1

     # rv$original es la imagen que se muestra al principio
     rv$original <- "temp/original.jpg"

     # se selecciona el directorio de salida en dependencia del ultimo usado
     rv$diroutput <- diroutput

     #____________________________________________________________________________
     #____________________________________________________________________________
     ##___________________________________________________________________________
     ## IMAGENES

     # Si se seleccionan nuevos archivos (fileinput)
     observeEvent(input$fileinput, {

          # Cambian el numero de pixeles y factor de conversion
          rv$npixels <- 1;    rv$factor.conversion <- 1


          # Se cambia el valor del contador rv$i a 1
          rv$i <- 1

          # Se calcula el numero de imagenes y se guarda en la variable rv$nfiles
          rv$nfiles <- length(input$fileinput$datapath)

          # La direccion de la imagen original cambia a ser la primera imagen
          # de la lista
          rv$original <- input$fileinput$datapath[rv$i]

          # Cambia el valor de input$invisible
          updateSelectInput(session, inputId = "invisible", selected = "b")

          # Las unidades cambian a pixeles
          rv$unit <- "pixels"

     })


     # Si se toca el boton Next o el otro Next:
     observeEvent(input$Next, {Next()})
     # Esto esta en landmarks y contorno
     observeEvent(input$next2, {Next()})
     # Esto esta en resultados
     observeEvent(input$next3, {Next()})

     Next <- reactive({

          # Ir al primer panel
          updateTabsetPanel(session, inputId = "navbarpage", selected = "FOLIOMETRIK")
          updateTabsetPanel(session, inputId = "medidas", selected = "binarized image")

          # Se cambia el valor del contador rv$i a rv$i + 1
          rv$i <- rv$i + 1

          # La direccion de la imagen original cambia a ser la proxima imagen
          # de la lista siempre que no exceda el numero de imagenes
          if (rv$i <= rv$nfiles)
               rv$original <- input$fileinput$datapath[rv$i]

          # rv$cnt debe volver a ser NULL y Los valores calculados de medidas
          # de hojas deben ser reseteados
          rv$cnt <- rv$cnts <- rv$Fourier <- NULL
     })

     # Acciones comunes para los 3 botones anteriores
     observe({
          input$fileinput; input$Next; input$next2

          # rv$ebi debera contener una lista: rv$ebi$img_rgb: la imagen
          # de la clase image y rv$ebi$img_gray: la imagen en escala
          #  de grises, que es en realidad una matriz bidimensional
          rv$ebi <- imgarrays(path = rv$original, channel = input$rgbmatrix)

          # rv$dim almacena las dimensiones de la matriz
          rv$dim <- dim(rv$ebi$img_gray)

          # Se inicializa la matriz de extremos del patron de medidas
          rv$patronmatrix <- rv$patronmatrixzoom <- rv$enderezamatrix <-
               rv$addedlandmarks <- matrix(data = 0, nrow = 0, ncol = 2)

          # la brocha de original se hace nula
          #rv$brush2 <- NULL

          # Cambia el id de huevo por foto (util cuando en la foto hay mas
          # de un huevo)
          rv$EggId <- 1

          # Las zonas borradas de la imagen dejan de serlo
          rv$eraser <- matrix(1, 1, 5)

     })


     # # Si se toca el boton para medir
     # observeEvent(input$medir, {
     #      updateTabsetPanel(session, inputId = "medidas", selected = "scaling")
     # })

     # eraser es la matriz que contiene los indices de pixeles de la imagen
     # a borrar
     rv$eraser <- matrix(1, 1, 5)

     # Cada vez que se selecciona un area a borrar, esta se guarda en la matriz
     # rv$eraser
     observeEvent(input$brush, {
          # Solo se agregan nuevas zonas si brush no pasa a ser NULL
          if (!is.null(input$brush)) {
               xmin <- input$brush$xmin; xmax <- input$brush$xmax
               ymin <- input$brush$ymin; ymax <- input$brush$ymax

               # Ajustar los indices para que no excedan los bordes de la figura
               if (xmin < 1) xmin <- 1; if (ymin < 1) ymin <- 1
               if (xmax > rv$dim[1]) xmax <- rv$dim[1]
               if (ymax > rv$dim[2]) ymax <- rv$dim[2]

               v <- 1; if (input$pinceles == "black") v <- 0

               rv$eraser <- rbind(rv$eraser, c(xmin, xmax, ymin, ymax, v))
          }

     })

     # Cada vez que cambia el sitio de corte, se pierde lo borrado
     observeEvent(input$undo, {
          if (nrow(rv$eraser) > 2) {
               rv$eraser <- rv$eraser[1:(nrow(rv$eraser) - 1), ]
          } else {
               rv$eraser <- matrix(1, 1, 5)}
     })

     observeEvent(input$removeallerased, {
          rv$eraser <- matrix(1, 1, 5)
     })

     output$originalplot <- renderUI({
          shiny::tagList(
               plotOutput(outputId = "original", height = 325,
                          click = "click2",
                          brush = "brush2"),
               shinyBS::bsTooltip("original", paste("Click on a pair of points with a distance of", input$length, input$units, "between them"),
                         "right", options = list(container = "body"))
          )
     })

     # binmatrix sera la matriz binaria de clase Image que se recalculara
     # cada vez que
     # cambie el valor del umbral en el sliderinput input$corte. La matriz
     # binmatrix sera usada para graficarla en plotOutpot("bin") y para
     # hallar el contorno y depositarlo en la variable rv$cnt. En esta
     # ultima, se extrae solo la matriz de 0 y 1 del objeto de clase Image
     binmatrix <- reactive({
          # v es el valor que tomara cualquier area rectangular de la imagen en
          # blanco y negro que sea borrada. Depende del radiobutton pinceles
          # v <- 1; if (input$pinceles == "black") v <- 0
          input$corte; input$Next; input$fileinput
          Mbin <- binarize(matrix = rv$ebi$img_gray, threshold = input$corte)

          # Las zonas se borran al tomar el valor 0 o 1
          for (i in 1:nrow(rv$eraser)) {
               try(Mbin[rv$eraser[i, 1]:rv$eraser[i, 2], rv$eraser[i, 3]:rv$eraser[i, 4]] <- rv$eraser[i, 5])
          }
          Mbin
     })

     observeEvent(input$rgbmatrix, {
          if (!is.null(input$fileinput)) {
               updateTabsetPanel(session, inputId = "medidas", selected = "binarized image")}
     })

     output$vistaprevia <- renderImage({
          list(src = rv$original, height = 100)

     }, deleteFile = FALSE)

     # Plot de la matriz en binario: El primero de los graficos
     output$bin <- renderPlot({

          if (is.null(input$fileinput)) {
               plot(1, axes = FALSE, type = "n", ann = FALSE)
               text(rep(1, 5), seq(1.3, 0.7, length.out = 6),
                    labels = c("No ", "images selected yet", "Please:", "select a group of",
                               "image files to", "analyze"), cex = 2.5, font = 8)
               return(NULL)}
          input$Next
          if (rv$i <= rv$nfiles) {
          EBImage::display(binmatrix(), method = "raster")
          if (!is.null(rv$clicksaved)) points(rv$clicksaved$x, rv$clicksaved$y, pch = 21, bg = "red", cex = 0.9)}
          if (rv$i > rv$nfiles) {
               plot(1, axes = FALSE, type = "n", ann = FALSE); box(lwd = 3)
               text(rep(1, 5), seq(1.3, 0.7, length.out = 5),
                    labels = c("No more", "images selected", "Please:", "select a new",
                              "group of images"), cex = 2.5, font = 8)
          }
     })


     # Si se da click sobre un punto de la imagen:
     observeEvent(input$click, {
          # solo debe correr si la imagen no es nula
          if (!is.null(rv$ebi) & is.null(input$brush) &
              input$click$x > 5 & input$click$x < rv$dim[2] - 5 &
              input$click$y > 5 & input$click$y < rv$dim[1] - 5 ) {
               # Salvar en otra variable
               rv$clicksaved <- input$click
               # Se halla el contorno de la figura
               binM <- as.numeric(!binmatrix()@.Data)
               dim(binM) <- rv$dim
               # contorno crudo
               rv$cnt <- try({
                    contorno(p = c(input$click$x, input$click$y),
                             imagematrix = binM)})
               if (class(rv$cnt) != "try-error") {
                    rv$xl <- range(rv$factor.conversion * (-rv$cnt$y))
                    rv$yl <- range(rv$factor.conversion * (-rv$cnt$x))
                    rv$cnts <- rv$cnt
               }

          }})

     # output$textopatron <- renderText({
     #      paste("* Click on a pair of points with a distance of",
     #            input$length, input$units, "between them and APPLY.  * Select an area to zoom")
     # })

     observeEvent(input$medido, {
          if (nrow(rv$patronmatrix) == 2) {
               updateTabsetPanel(session, inputId = "medidas", selected = "binarized image")}
     })

     output$original <- renderPlot({
          input$clickzoom
          if (is.null(input$fileinput)) {
               plot(1, axes = FALSE, type = "n", ann = FALSE)
               text(rep(1, 5), seq(1.3, 0.7, length.out = 6),
                    labels = c("No ", "images selected yet", "Please:", "select a group of",
                               "image files to", "analyze"), cex = 2.5, font = 8)
               return(NULL)}

          EBImage::display(rv$ebi$img_rgb, method = "raster")
          if (nrow(rv$patronmatrix) == 2) {
               segments(x0 = rv$patronmatrix[1, 1], y0 = rv$patronmatrix[1, 2],
                        x1 = rv$patronmatrix[2, 1], y1 = rv$patronmatrix[2, 2])
          }
          if (nrow(rv$patronmatrix) != 0) {
               points(x = rv$patronmatrix[, 1], y = rv$patronmatrix[, 2], pch = 16, col = "red")
          }

          if (rv$i > rv$nfiles) {
               plot(1, axes = FALSE, type = "n", ann = FALSE); box(lwd = 3)
               text(rep(1, 5), seq(1.3, 0.7, length.out = 5),
                    labels = c("No more", "images selected", "Please:", "select a new",
                               "group of images"), cex = 2.5, font = 8)
          }
     })


     reescale <- reactive({
          if (nrow(rv$patronmatrix) == 2) {
               scala <- input$length
               scala <- gsub(pattern = " ", replacement = "", scala)
               scala <- gsub(pattern = ",", replacement = ".", scala)
               scala <- as.numeric(scala)
               if (is.na(scala)) scala <- 1

               rv$unit <- input$units
               if (rv$unit == "other") rv$unit <- input$otherunit
               if (rv$unit == "") rv$unit <- "pixels"

               # Cambia el valor de npixels;
               rv$npixels <- sqrt((rv$patronmatrix[1, 1] - rv$patronmatrix[2, 1])^2 +
                                       (rv$patronmatrix[1, 2] - rv$patronmatrix[2, 2])^2)

               # Calcular el valor de rv$factor.conversion
               rv$factor.conversion <- scala/rv$npixels
          }

          if (!is.null(rv$cnts)) {
               if (class(rv$cnts) != "try-error") {
                    rv$xl <- range(rv$factor.conversion * (-rv$cnts$y))
                    rv$yl <- range(rv$factor.conversion * (-rv$cnts$x))
               }}
     })

     # Cambia la matriz patronmatrix, que contiene los extremos del patron de long
     observeEvent(input$click2, {
          nrptmt <- nrow(rv$patronmatrix)
          if (nrptmt < 2) {
               rv$patronmatrix <- rbind(rv$patronmatrix,
                                        c(input$click2$x, input$click2$y))
          }
          if (nrptmt == 2) {
               rv$patronmatrix <- cbind(input$click2$x, input$click2$y)
          }
          reescale()
     })

     observe({
          input$length; input$units
          reescale()
     })

     observeEvent(input$brush2, {
          rv$brush2 <- round(cbind(c(input$brush2$xmin, input$brush2$xmax),
                                   c(input$brush2$ymin, input$brush2$ymax)))
          rv$patronmatrix <- matrix(NA, ncol = 2, nrow = 0)
     })

     output$zooming <- renderPlot({
          if (is.null(rv$brush2)) {
               plot(1, type = "n", ann = FALSE, axes = FALSE); graphics::box(); title(main = "Zoom")
               text(1, 1, "Select an area in the image leaf to zoom")
          }

          if (!is.null(rv$brush2)) {
               imgzoom <- rv$ebi$img_rgb
               imgzoom@.Data <- imgzoom@.Data[rv$brush2[1, 1]:rv$brush2[2, 1], rv$brush2[1, 2]:rv$brush2[2, 2], ]
               par(mar = c(0, 0, 2, 0))
               EBImage::display(imgzoom, method = "raster")
               title(main = "Zoom"); graphics::box(col = "gray", lty = 3)

               if (nrow(rv$patronmatrixzoom) == 2) {
                    segments(x0 = rv$patronmatrixzoom[1, 1], y0 = rv$patronmatrixzoom[1, 2],
                             x1 = rv$patronmatrixzoom[2, 1], y1 = rv$patronmatrixzoom[2, 2])
               }

               if (nrow(rv$patronmatrixzoom) != 0) {
                    points(x = rv$patronmatrixzoom[, 1], y = rv$patronmatrixzoom[, 2], pch = 16, col = "red")
               }
          }
     })

     # Cambia la matriz patronmatrix, que contiene los extremos del patron de long
     observeEvent(input$clickzoom, {
          #rv$patronmatrix <- matrix(NA, ncol = 2, nrow = 0)
          nrptmt <- nrow(rv$patronmatrixzoom)
          if (nrptmt < 2) {
               rv$patronmatrixzoom <- rbind(rv$patronmatrixzoom,
                                            c(input$clickzoom$x, input$clickzoom$y))
          }
          if (nrptmt == 2) {
               rv$patronmatrixzoom <- cbind(input$clickzoom$x, input$clickzoom$y)
          }
          rv$patronmatrix <- cbind(rv$brush2[1, 1] + rv$patronmatrixzoom[, 1],
                                   rv$brush2[1, 2] + rv$patronmatrixzoom[, 2])


          if (nrow(rv$patronmatrix) == 2) {
               scala <- input$length
               scala <- gsub(pattern = " ", replacement = "", scala)
               scala <- gsub(pattern = ",", replacement = ".", scala)
               scala <- as.numeric(scala)
               if (is.na(scala)) scala <- 1

               rv$unit <- input$units
               if (rv$unit == "other") rv$unit <- input$otherunit
               if (rv$unit == "") rv$unit <- "pixels"

               # Cambia el valor de npixels;
               rv$npixels <- sqrt((rv$patronmatrix[1, 1] - rv$patronmatrix[2, 1])^2 +
                                       (rv$patronmatrix[1, 2] - rv$patronmatrix[2, 2])^2)

               # Calcular el valor de rv$factor.conversion
               rv$factor.conversion <- scala/rv$npixels
          }

     })


     #################################


     # Grafico de contornos crudo y suavizado
     output$contour <- renderPlot({
          if (is.null(rv$cnt)) return(NULL)
          s1 <- gray(0.8)
          par(col.axis = gray(0.3), col.lab = "black", bg = "white",
              mar = c(4, 4, 0.5, 0.5), font.axis = 6, font.lab = 6)

          plot(x = rv$factor.conversion * (-rv$cnts$y) - rv$xl[1],
               y = rv$factor.conversion * (-rv$cnts$x) - rv$yl[1], asp = 1,
               type = "l", col = "white", las = 1, xlab = rv$unit, ylab = rv$unit)
          axis(side = 1, col.ticks = s1, col = "black")
          axis(side = 2, col.ticks = s1, col = "black", las = 1)
          grid(col = s1)
          graphics::box(col = gray(0.4))

          polygon(x = rv$factor.conversion * (-rv$cnts$y) - rv$xl[1],
                  y = rv$factor.conversion * (-rv$cnts$x) - rv$yl[1],
                  col = "#187e2040", border = "#000000")

          lines(x = rv$factor.conversion * (-rv$cnts$y) - rv$xl[1],
                y = rv$factor.conversion * (-rv$cnts$x) - rv$yl[1],
                col = "black" )

     })





     # Si se toca el boton OK:
     observeEvent(input$ok, {

          # # solo si el contorno existe
          if (!is.null(rv$cnts)) {
               analyze()

               # ir a la seccion de resultados
               updateTabsetPanel(session, inputId = "navbarpage", selected = "Shape Descriptors")
               updateTabsetPanel(session, inputId = "contornos", selected = "Landmarks")

               if (input$rememberresize & nrow(rv$patronmatrix) < 2) {
                    updateTabsetPanel(session, inputId = "navbarpage", selected = "FOLIOMETRIK")
                    updateTabsetPanel(session, inputId = "medidas", selected = "scaling")
               }
          }


     })





     observeEvent(input$gotoresults, {

          # # solo si el contorno existe
          if (!is.null(rv$cnts)) {
               analyze()

               # ir a la seccion de resultados
               updateTabsetPanel(session, inputId = "navbarpage", selected = "Leaf Measures")

               if (input$rememberresize & nrow(rv$patronmatrix) < 2) {
                    updateTabsetPanel(session, inputId = "navbarpage", selected = "FOLIOMETRIK")
                    updateTabsetPanel(session, inputId = "medidas", selected = "scaling")
               }
          }
     })

     observeEvent(input$soften, {
          if (!is.null(rv$cnts)) rv$cnts <- suaviza(cnts = rv$cnts, roundit = TRUE)
     })

     analyze <- reactive({
          # solo si el contorno existe
          if (!is.null(rv$cnts) & (!input$rememberresize | (input$rememberresize & nrow(rv$patronmatrix) > 1)) ) {

               # Calcular la matriz de distancias del contorno, asi como los
               # puntos de maxima distancia
               ##################################################
               rv$d <- dist(as.data.frame(rv$cnts))

               # rv$id.pt.max.dist da una matriz de 1x2, con los indices de loa puntos
               # mas alejados entre si
               rv$id.pt.max.dist <- which.matrix(M.dim = rep(length(rv$cnts$x), 2),
                                                 index = which.max(rv$d), is.dist.matrix = TRUE)

               # Horizontalizar y reorganizar el contorno
               setmaxdist(); reorganizacnts()

               # calcular largo, ancho, etc y poner en objeto rv$datos_hojas
               calcula.datos.hojas(); calcula.RR.y.Fourier()
          }

     })



     # Estos dos check buton no pueden estar activos a la vez
     observeEvent(input$rememberresize, {
          if (input$rememberresize) {
               updateCheckboxInput(session = session, inputId = "useprevscaling", value = FALSE)
          }
     })

     observeEvent(input$useprevscaling, {
          if (input$useprevscaling) {
               updateCheckboxInput(session = session, inputId = "rememberresize", value = FALSE)
          }
     })


     #____________________________________________________________________________
     #____________________________________________________________________________
     #____________________________________________________________________________
     # CONTORNO

     output$contour2 <- renderPlot({

          if (is.null(rv$cnt)) return(NULL)

          plot.landmarks(datos_hojas = rv$datos_hojas, hidepoints = input$hidepoints)

          if (nrow(rv$enderezamatrix) == 2) {
               segments(x0 = rv$enderezamatrix[1, 1], y0 = rv$enderezamatrix[1, 2],
                        x1 = rv$enderezamatrix[2, 1], y1 = rv$enderezamatrix[2, 2])
          }

          if (nrow(rv$enderezamatrix) != 0) {
               points(x = rv$enderezamatrix[, 1], y = rv$enderezamatrix[, 2], pch = 16, col = "blue")
          }
     })


     observeEvent(input$clickendereza, {

          if (!input$addlandmark) {
               nrptmt <- nrow(rv$enderezamatrix)
               if (nrptmt < 2) {
                    rv$enderezamatrix <- rbind(rv$enderezamatrix,
                                               c(input$clickendereza$x, input$clickendereza$y))
               }
               if (nrptmt == 2) {
                    rv$enderezamatrix <- cbind(input$clickendereza$x, input$clickendereza$y)
               }
          }

          if (input$addlandmark) {

               if (!input$fixlandmarks) {
               rv$addedlandmarks <- rbind(rv$addedlandmarks,
                                          round(c(input$clickendereza$x, input$clickendereza$y)))
               } else {
                    idmin <- which.min(sqrt((input$clickendereza$x - rv$cnts$x)^2 + (input$clickendereza$y - rv$cnts$y)^2))
                    rv$addedlandmarks <- rbind(rv$addedlandmarks,
                                          round(c(rv$cnts$x[idmin], rv$cnts$y[idmin])))
               }
          }


     })

     observeEvent(input$removelandmark, {
          rv$addedlandmarks <- rv$addedlandmarks[-(nrow(rv$addedlandmarks)), , drop = FALSE]
     })

     observeEvent(input$removealllandmarks, {rv$addedlandmarks <- matrix(NA, nrow = 0, ncol = 2)})




     observeEvent(input$PCA, {
          fp <- file.path(rv$diroutput, "Fourier's_harmonics_FoliometriK.csv")
          if (!file.exists(fp)) {
               shinyalert::shinyalert(title = "Something was wrong!!!",
                          text = "Does not exist the file: 'Fourier's_harmonics_FoliometriK.csv' in the specified output address. So it is not possible to calculate PCA",
                          type = "error", timer = 0,
                          animation = "slide-from-bottom")
          }

          if (file.exists(fp)) {
               (fn <- paste0("read.", which.csv.type(fp)))
               (Fr <- eval(call(fn, file = fp, header = FALSE)))
               Fr[, 1] <- as.character(Fr[, 1]);  t <- 1

               if (any(duplicated(Fr[, 1]))) {
               Fr[duplicated.default(Fr[, 1]), 1] <- paste0(Fr[duplicated.default(Fr[, 1]), 1], "_", t)
               t <- t + 1}

               while (any(duplicated(Fr[, 1]))) {
                    Fr[duplicated.default(Fr[, 1]), 1] <- gsub(pattern = paste0(paste0("_", t - 1), "$"),
                                                               replacement = paste0("_", t), Fr[duplicated.default(Fr[, 1]), 1])
                    t <- t + 1
               }
               rownames(Fr) <- Fr[, 1]; Fr <- Fr[, -1]


               if (!is.null(Fr)) {
                    (Fr <- Fr[complete.cases(Fr), ])
                    (nc <- ncol(Fr))
                    (PCA <- pca(Fr))
                    if (input$dec == "comma  ( , )") {
                         dec <- ","; sep <- ";"} else {
                              dec <- "."; sep <- ","
                         }

                    (cumulative_var <- as.character(PCA$pvar * 100, 4))
                    if (dec == ",") cumulative_var <- gsub(pattern = "\\.", replacement = ",", cumulative_var)

                    (fileoutput <- file.path(rv$diroutput, "PCA_average_Fourier's_harmonics_FoliometriK.csv"))

                    w <- try({
                         cat("file; ", paste0("PC", 1:ncol(PCA$PC) , collapse = sep), sep, "\n", append = FALSE, file = fileoutput)

                         cat("cum_percent_var; ", paste0(cumulative_var, sep, " "), "\n", file = fileoutput, append = TRUE)
                         write.table(x = PCA$PC, file = fileoutput, append = TRUE,
                                     quote = FALSE, sep = sep, dec = dec, row.names = TRUE, col.names = FALSE)

                    })
                    if (class(w) == "try-error") {
                         shinyalert::shinyalert(title = "Something was wrong!!!",
                                    text = "It is not possible to write or modify the file: 'PCA_average_Fourier's_harmonics_FoliometriK.csv' at the specified address. It is possibly open and can not be overwritten",
                                    type = "error", timer = 0,
                                    animation = "slide-from-bottom")
                    } else {
                         shinyalert::shinyalert(title = "PCA calculated and saved successfully!", text = "", type = "success", timer = 0,
                                    animation = "slide-from-bottom")
                    }
               }
          }
     })


     output$plotfourier <- renderPlot({
          if (is.null(rv$Fourier)) return(NULL)
          plot_Fourier(cnts = rv$cnts/max(rv$cnts$x), Fourier = rv$Fourier,
                       seeoriginal = any(input$seelines == "original"),
                       seefourier = any(input$seelines == "Fourier's"))
     })

     output$modelo <- renderImage({list(src = "temp/modelo.jpg")}, deleteFile = FALSE)


     # Aqui van todas las acciones de los botones flip, plop, transpose, etc
     {
          # Establecer el eje manualmente
          observeEvent(input$setmanual, {

               # escala 1 y 2 son los valores x y de dos puntos que estamos buscando
               # Inicialmente deben establecerse como los valores xy del 1er y 2do pto
               # del contorno (por hacer)

               if (nrow(rv$enderezamatrix) == 2) {
                    escala1 <- list(x = rv$enderezamatrix[1, 1], y = rv$enderezamatrix[1, 2])
                    escala2 <- list(x = rv$enderezamatrix[2, 1], y = rv$enderezamatrix[2, 2])

                    rv$enderezamatrix <- matrix(data = 0, nrow = 0, ncol = 2)
                    # punto medio entre los otros dos
                    medio <- c(mean(c(escala1$x, escala2$x)), mean(c(escala1$y, escala2$y)))
                    #  Establecer escala 1 y 2 como los puntos que indican el angle a rotar
                    (angle <- atan((escala1$y - escala2$y)/(escala1$x - escala2$x)))*180/pi

                    # Traslacion d toddos los puntos d manera que medio sea el origen
                    # de coordenadas
                    rv$cnts$x <- rv$cnts$x - medio[1]; rv$cnts$y <- rv$cnts$y - medio[2]
                    # Rotar el contorno
                    (rv$cnts <- rota(cnt = rv$cnts, angle = angle))
                    # Llevar a enteros
                    rv$cnts <- round(rv$cnts)
                    # Trasladar
                    rv$cnts$x <- rv$cnts$x - min(rv$cnts$x)
                    reorganizacnts()
                    # calcular largo, ancho, etc y poner en objeto rv$datos_hojas
                    calcula.datos.hojas(); calcula.RR.y.Fourier()}
          })

          observeEvent(input$rotate90, {
               (rv$cnts <- rota(cnt = rv$cnts, angle = 180, radians = FALSE))
               rv$cnts$x <- rv$cnts$x - min(rv$cnts$x)
               rv$cnts <- round(rv$cnts)
               reorganizacnts()
               calcula.datos.hojas(); calcula.RR.y.Fourier()
          })



          # funcion reactiva que establece rv$cnts en el eje de la maxima distancia
          setmaxdist <- reactive({
               rv$cnts <- setmaxdist_(cnts = rv$cnts, id.pt.max.dist = rv$id.pt.max.dist)

          })

          reorganizacnts <- reactive({
               K <- reorganiza.cnts(cnts = rv$cnts, id.pt.max.dist = rv$id.pt.max.dist)
               rv$cnts <- K$cnts
               rv$id.pt.max.dist <- K$id.pt.max.dist
          })

          # Maxima distancia
          observeEvent(input$setmaxdist, {
               setmaxdist()
               reorganizacnts()
               # calcular largo, ancho, etc y poner en objeto rv$datos_hojas
               calcula.datos.hojas(); calcula.RR.y.Fourier()
          })

          # flip
          observeEvent(input$setflip, {
               rv$cnts <- setflip_(cnts = rv$cnts)
               reorganizacnts()

               # calcular largo, ancho, etc y poner en objeto rv$datos_hojas
               calcula.datos.hojas(); calcula.RR.y.Fourier()
          })

          # flop
          observeEvent(input$setflop, {
               rv$cnts <- setflop_(cnts = rv$cnts)
               reorganizacnts()
               # calcular largo, ancho, etc y poner en objeto rv$datos_hojas
               calcula.datos.hojas(); calcula.RR.y.Fourier()
          })

          # transpose
          observeEvent(input$settranspose, {
               rv$cnts <- settranspose_(cnts = rv$cnts)
               reorganizacnts()
               # calcular largo, ancho, etc y poner en objeto rv$datos_hojas
               calcula.datos.hojas(); calcula.RR.y.Fourier()
          })
     }


     observeEvent(input$nharmonics, {
          try(calcula.RR.y.Fourier())
     })

     calcula.RR.y.Fourier <- reactive({
          cnts <- rv$cnts/max(rv$cnts$x)
          rv$Fourier <- Momocs::efourier(x = as.matrix(cnts), nb.h = input$nharmonics)
     })

     observe({
          input$ndivisionesrejilla; input$tipoderejilla
          if (!is.null(rv$cnts)) try(calcula.datos.hojas())
     })

     # Funcion reactiva: calcular datos de las hojas
     calcula.datos.hojas <- reactive({
          try(rv$datos_hojas <-  calcula.puntos.y.constantes.hoja(cnts = rv$cnts, filename = input$fileinput$name[rv$i],
                                                                  EggId = rv$EggId,
                                                                  desv.origen.coef = switch(input$desvorigencoef, "base" = 0, "center" = 1, "centroid" = -1),
                                                                  rejilla.de.puntos.clave = input$tipoderejilla,
                                                                  id.pt.max.dist = rv$id.pt.max.dist, addedlandmarks = rv$addedlandmarks,
                                                                  unit = rv$unit,fc = rv$factor.conversion,
                                                                  n.divisiones.rejilla = input$ndivisionesrejilla,
                                                                  nseqpoints = input$nseqpoints,
                                                                  addLength1 = input$addLength1,
                                                                  addbasallobepoints = input$addbasallobepoints,
                                                                  addbasallobearea = input$addbasallobearea,
                                                                  addbasalapexangle = input$addbasalapexangle,
                                                                  addpetiolewidth = input$addpetiolewidth), silent = TRUE)
     })


     # Si se toca el boton View Results:
     observeEvent(input$viewresults, {
          updateTabsetPanel(session, inputId = "navbarpage", selected = "Leaf Measures")
     })



     #____________________________________________________________________________
     #____________________________________________________________________________
     #____________________________________________________________________________
     # RESULTADOS



     output$info1 <- output$info2 <- output$info3 <- renderText({
          if (is.null(rv$nfiles)) return(" ")
          if (rv$i <= rv$nfiles) {
          paste("<p>image file:", input$fileinput$name[rv$i], "</p>",
                "<p>", rv$i, "of", rv$nfiles, "images </p>", collapse = " ")
          } else {" "}
     })



     # Accion: Cambiar el directorio de salida
     observeEvent(input$selectdir, {
          prevdir <- rv$diroutput
          rv$diroutput <- choose.dir(default = prevdir,
                                     caption = "Select an output folder")
          if (is.na(rv$diroutput)) rv$diroutput <- prevdir
          cat(rv$diroutput, file = "diroutputleaves.txt")
          updateTextInput(session, inputId = "dirout", value = rv$diroutput)
     })

     # Al cambiar manualmente la direccion, rv$diroutput cambia solo si la
     # direccion escrita existe
     observeEvent(input$dirout, {
          if (dir.exists(input$dirout)) rv$diroutput <- input$dirout
     })


     # Accion: Salvar los datos. Incompleta
     observeEvent(input$save, {save()})
     observeEvent(input$save2, {save()})

     save <- reactive({

          if (input$dec == "comma  ( , )") {dec <- ","} else {dec <- "."}

          w <- try(write.fourier.results(Fourier = rv$Fourier,
                                         filename = input$fileinput$name[rv$i],
                                         dec = dec,
                                         diroutput = rv$diroutput, Id = rv$EggId))
          if (class(w) == "try-error") {
               shinyalert::shinyalert(title = "Something was wrong!!!",
                          text = "It is not possible to write or modify the Fourier harmonics file at the specified address. Try changing the address or close the file to overwrite it",
                          type = "error", timer = 0,
                          animation = "slide-from-bottom")
          }

          w1 <- try(write.datos_hojas(datos_hojas = rv$datos_hojas,
                                      EggId = rv$EggId,
                                      filename = input$fileinput$name[rv$i],
                                      landmarksformat = input$landmarksformat,
                                      img.dim = rv$dim, ref.length = input$length,
                                      diroutput = rv$diroutput, dec = dec,
                                      addbasallobepoints = input$addbasallobepoints))
          if (class(w1) == "try-error") {
               shinyalert::shinyalert(title = "Something was wrong!!!",
                          text = "It is not possible to write or modify the leaves measurements file and/or the landmarks file output at the specified address. Try changing the address or close the file before to overwrite it",
                          type = "error", timer = 0,
                          animation = "slide-from-bottom")
          }
          w2 <- w1
          if (input$savecontour) {
               # Guardar en archivo txt los contornos
               append <- FALSE

               w2 <- try(write.coord.results(cnts = rv$cnts,
                                             diroutput = rv$diroutput, dec = dec,
                                             fileinputname = input$fileinput$name[rv$i],
                                             append = append, EggId = rv$EggId,
                                             fc = rv$factor.conversion, unit = rv$unit))
               if (class(w2) == "try-error") {
                    shinyalert::shinyalert(title = "Something was wrong!!!",
                               text = "It is not possible to write or modify the contour coordinates file at the specified address. Try changing the address or close the file to overwrite it",
                               type = "error", timer = 0,
                               animation = "slide-from-bottom")
               }
          }

          w3 <- w1
          if (input$savegraphics) {

               w3 <- try(saveGraphics(diroutput = rv$diroutput, datos_hojas = rv$datos_hojas))
               if (class(w3) == "try-error") {
                    shinyalert::shinyalert(title = "Something was wrong!!!",
                               text = "It is not possible to write or modify the graphics files at the specified address. Try changing the address or close the image file/files to overwrite it",
                               type = "error", timer = 0,
                               animation = "slide-from-bottom")
               }
          }

          rv$EggId <- rv$EggId + 1

          if (class(w1) != "try-error" & class(w2) != "try-error" & class(w3) != "try-error") {
               shinyalert::shinyalert(title = "Files successfully saved!", type = "success", timer = 500,
                          animation = "slide-from-bottom")
          }
     })



     observeEvent(input$addLength1, {
          if (!input$addLength1) updateCheckboxInput(session, inputId = "addLength2", value = TRUE)
          if (!is.null(rv$cnts)) calcula.datos.hojas()
     })

     observeEvent(input$addLength2, {
          if (!input$addLength2) updateCheckboxInput(session, inputId = "addLength1", value = TRUE)
          if (!is.null(rv$cnts)) calcula.datos.hojas()
     })

     observeEvent(input$addbasallobearea, {
          if (!is.null(rv$cnts)) calcula.datos.hojas()
     })

     observeEvent(input$addbasallobepoints, {
          if (!is.null(rv$cnts)) calcula.datos.hojas()
     })

     # observe({input$addbasallobearea; input$addbasallobepoints
     #      if (!is.null(rv$cnts)) calcula.datos.hojas()})




     output$eggresultslist <- renderText({
          if (is.null(rv$datos_hojas)) {"<p>No data has been processed yet</p>"} else {
          paste("<p> Length =", round(rv$datos_hojas$L2, 4), rv$datos_hojas$unit, "</p>",
                "<p> Maximum Width (Max_Width) =", round(rv$datos_hojas$Am, 4), rv$datos_hojas$unit, "</p>",
                "<p> Lobe deepth =", round(rv$datos_hojas$L1 - rv$datos_hojas$L2, 4), rv$datos_hojas$unit, "</p>",
                "<p> x coord of Maximum Width (x_Max_Width) =", round(rv$datos_hojas$x_Am, 4), rv$datos_hojas$unit, "</p>",

                ifelse(test = !is.na(rv$datos_hojas$petiole_width),
                       yes = paste("<p> width at the junction with the petiole (width_at_petiole) =", round(rv$datos_hojas$petiole_width, 4), rv$datos_hojas$unit, "</p>", collapse = " "),
                       no = ""),
                "<p> Quantile Widths: </p> w1 = ", round(rv$datos_hojas$WQ[1], 3),  rv$datos_hojas$unit, #"</p>",
                ", w2 = ", round(rv$datos_hojas$WQ[2], 3),  rv$datos_hojas$unit, #"</p>",
                ", w3 = ", round(rv$datos_hojas$WQ[3], 3),  rv$datos_hojas$unit, "</p>",
                "<p> Perimeter (Perim) =", round(rv$datos_hojas$Perim, 4), rv$datos_hojas$unit, "</p>",
                "<p> Area =", round(rv$datos_hojas$SA, 4), rv$datos_hojas$unit, "<sup>2</sup></p>",
                "<p> Area of lateral asymmetry (Area_Lat_Asym) =", round(rv$datos_hojas$SA_Lat_Asym, 4), rv$datos_hojas$unit, "<sup>2</sup></p>",

                ifelse(test = !is.na(rv$datos_hojas$basallobearea),
                       yes = paste("<p> Basal lobes area =", round(rv$datos_hojas$basallobearea, 4), rv$datos_hojas$unit, "<sup>2</sup></p>", collapse = " "),
                       no = ""),
                "<p> Shape Index (Area/Area_rect) =", round(rv$datos_hojas$Shape_Index, 4), "</p>",
                "<p> Internal angles:</p>",
                "ang_v1 =",  round(rv$datos_hojas$angulos[1], 1), "<sup>o</sup>", "ang_m1 =",  round(rv$datos_hojas$angulos[2], 1), "<sup>o</sup>", "</p>",
                "ang_v2 =",  round(rv$datos_hojas$angulos[3], 1), "<sup>o</sup>", "    ang_m2 =",  round(rv$datos_hojas$angulos[4], 1), "<sup>o</sup>", "</p>",

                ifelse(test = !is.na(rv$datos_hojas$bases_apice_angle),
                       yes = paste("apex - basal lobes angle (ABL_angle) =",  round(rv$datos_hojas$bases_apice_angle, 1), "<sup>o</sup>", collapse = " "),
                       no = ""),

                collapse = " ")}
     })

     output$imgdata <- renderText({
          paste("<p> file name:", rv$datos_hojas$filename, "</p>",
                "<p> Reference object length (ref_length) =", input$length, rv$datos_hojas$unit, "</p>",
                "<p> Reference object length in pixels =", round(rv$npixels, 2), " pixels</p>",
                "<p> Image dimmensions (img_dim) =", rv$dim[1], "X", rv$dim[2], "pixels </p>",
                collapse = " ")})


     output$plotrectangulohoja <- renderPlot({
          plot.rectangulo.hoja(datos_hojas = rv$datos_hojas)
     })

     output$plotangulos <- renderPlot({
          plot.angulos.hoja(datos_hojas = rv$datos_hojas)
     })

     output$sombraslaterales <- renderPlot({
          sombras.laterales(datos_hojas = rv$datos_hojas)
     })

     output$outlineplot <- renderPlot({
          plot(rv$datos_hojas$cnts, asp = 1, axes = FALSE, ann = FALSE,
               type = "l", lwd = 2)
          title(sub = rv$datos_hojas$filename)
     })

     output$fouriertable <- renderTable(digits = 8, {
          as.data.frame(rv$Fourier)[, 1:4]
     })


     #__________________________________________________________________________
     observeEvent(input$saveandnext, {
          saveandnext()
          if (class(rv$cnt) != "try-error"| !is.null(rv$cnts)) {
               updateTabsetPanel(session, inputId = "navbarpage", selected = "Shape Descriptors")}

          if (class(rv$cnt) == "try-error" || length(rv$cnt$x) <= 30 || is.null(rv$cnt) || rv$i > rv$nfiles) {
               # Ir al primer panel
               updateTabsetPanel(session, inputId = "navbarpage", selected = "FOLIOMETRIK")

          }
     })

     observeEvent(input$saveandnext2, {
          saveandnext()
          if (class(rv$cnt) != "try-error" | !is.null(rv$cnts)) {
               updateTabsetPanel(session, inputId = "navbarpage", selected = "Leaf Measures")}

          if (class(rv$cnt) == "try-error" || length(rv$cnt$x) <= 30 || is.null(rv$cnt) || rv$i > rv$nfiles) {
               # Ir al primer panel
               updateTabsetPanel(session, inputId = "navbarpage", selected = "FOLIOMETRIK")

          }
     })

     saveandnext <- reactive({
          save(); Next()

          if (input$useprevscaling) {
          if (rv$i > rv$nfiles) {
               updateTabsetPanel(session, inputId = "navbarpage", selected = "FOLIOMETRIK")
               return(NULL)
          }

          # rv$ebi debera contener una lista: rv$ebi$img_rgb: la imagen
          # de la clase image y rv$ebi$img_gray: la imagen en escala
          #  de grises, que es en realidad una matriz bidimensional
          rv$ebi <- imgarrays(path = rv$original, channel = input$rgbmatrix)

          # rv$dim almacena las dimensiones de la matriz
          rv$dim <- dim(rv$ebi$img_gray)

          # la brocha de original se hace nula
          rv$brush2 <- NULL

          # Cambia el id de huevo por foto (util cuando en la foto hay mas
          # de un huevo)
          rv$EggId <- 1

          # Las zonas borradas de la imagen dejan de serlo
          rv$eraser <- matrix(1, 1, 5)

          # solo debe correr si la imagen no es nula
          if (!is.null(rv$ebi) & is.null(input$brush)) {
               # Se halla el contorno de la figura
               binM <- as.numeric(!binmatrix()@.Data)
               #dim(binM) <- rv$dim
               dim(binM) <- rv$dim
               # contorno crudo
               rv$cnt <- try({
                    contorno(p = c(rv$clicksaved$x, rv$clicksaved$y),
                             imagematrix = binM)})


               if (class(rv$cnt) != "try-error") {
                    #updateTabsetPanel(session, inputId = "navbarpage", selected = "Shape Descriptors")
                    rv$xl <- range(rv$factor.conversion * (-rv$cnt$y))
                    rv$yl <- range(rv$factor.conversion * (-rv$cnt$x))
                    # contorno suavizado
                    rv$cnts <- rv$cnt
               }

          }
          analyze()
     }})
}
