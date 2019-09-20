

 library(shiny); library(shinyalert)
library(shinyWidgets)
 library(shinyBS)
 library(EBImage); library(Momocs)
source("functions.R"); source("run.R")


fluidPage( theme = "journal.min.css",
           imageOutput(outputId = "baner", height = "1%"),
           navbarPage(title = " ", id = "navbarpage", windowTitle = "FOLIOMETRIK",


           tabPanel(title = "FoliometriK", icon = icon("leaf"),

                    helpText(h5(" ", strong("Steps:"),
                             strong("A)"), "Load the images", strong("B)"),
                             "Select the scale length (optional)",
                             strong("C)"), "Click over a leaf")),

                    sidebarPanel(width = 3,

                                 # Boton de seleccion de archivos
                                 fileInput(inputId = "fileinput", accept = c(".jpg", ".jpeg", ".png", ".tiff"),
                                           label = "Select leaf images", placeholder = "",
                                           multiple = TRUE),

                                 # Boton para pasar a la siguiente imagen
                                 conditionalPanel(condition = "input.invisible == 'b'",
                                                  shiny::htmlOutput(outputId = "info1"),

                                                  fillCol(
                                                       shiny::column(width = 3, actionButton(inputId = "Next", size = "sm", status =  "danger", icon = icon("arrow-right"),
                                                                                             label = "Next Image"),
                                                                     shinyBS::bsTooltip("Next", "Go to the next image file",
                                                                               "right", options = list(container = "body")))
                                                  )
                                 ), br(), br(),

                                 conditionalPanel(condition = "input.invisible == 'a'",
                                                  br(), br(), br()),

                                 imageOutput(outputId = "vistaprevia", width = 75, height = 150),
                                 shinyBS::bsTooltip("vistaprevia", "currently processing this image",
                                           "top", options = list(container = "body")),




                                 # Seleccion de la matriz de color
                                 radioButtons(inputId = "rgbmatrix", label = "Select the color channel",
                                              choices = c("Red", "Green", "Blue"), selected = "Red", inline = FALSE),

                                 br(),



                                 conditionalPanel(condition = "input.otherunit == 'caca'",
                                                  selectInput(inputId = "invisible",
                                                              label = "Invisible",
                                                              choices = c("a", "b")))

                    ),
                    mainPanel(

                         shiny::tabsetPanel(id = "medidas",
                              tabPanel(title = "binarized image",  icon = icon("adjust"),

                                       fillCol(flex = c(1, 1),

                                               column(width = 6,
                                                      br(), br(),

                                                      conditionalPanel(condition = "input.invisible == 'b'",
                                                      shinyWidgets::noUiSliderInput(inputId = "corte", label = "",
                                                                      min = 0, max = 1, value = 0.5, step = 0.01, color = "black",
                                                                      tooltips = TRUE, orientation = "vertical", inline = TRUE, height = "280px", width = "20px"),
                                                      shinyBS::bsTooltip("corte", "Select a pixel value as a binarizing threshold until you get a defined leaf silhouette",
                                                                "left", options = list(container = "body")))),

                                               column(width = 6, offset = 1,

                                                      fillCol(
                                                           shiny::column(width = 12, br(), helpText(h5(icon("eraser"),"eraser:"))),
                                                           shiny::column(width = 12, offset = 3,
                                                                         radioButtons(inputId = "pinceles", label = "",
                                                                                      choices = c("black", "white"),
                                                                                      selected = "white", inline = TRUE),
                                                                         shinyBS::bsTooltip("pinceles", "delete selected areas with a white or black eraser",
                                                                                   "left", options = list(container = "body"))),

                                                           shiny::column(width = 12, offset = 8, br(),
                                                                         shinyWidgets::actionBttn(inputId = "undo", label = "   ", size = "xs", style = "gradient", color = "success", icon = icon("undo")),
                                                                         shinyBS::bsTooltip("undo", "Undo (the last erased area)",
                                                                                   "top")),
                                                           shiny::column(width = 12, offset = 10, br(),
                                                                         shinyWidgets::actionBttn(inputId = "removeallerased", label = "   ", size = "xs", style = "gradient", color = "success", icon = icon("trash")),
                                                                         shinyBS::bsTooltip("removeallerased", "no erased areas",
                                                                                            "top"))

                                                      ),
                                                      br(), br(), br(),

                                                      wellPanel(shinyWidgets::addSpinner(
                                                           plotOutput(outputId = "bin", click = "click", brush = "brush", height = 365),  spin = "circle", color = "green")),
                                                      shinyBS::bsTooltip("bin", "click on a well defined leaf silhouette you wish to analyze",
                                                                "right", options = list(container = "body"))

                                               ),

                                               column(width = 6, offset = 8, br(),
                                                      fillCol(
                                                           shiny::column(width = 6,
                                                                         shiny::helpText(h5(icon("calculator"), "Calculate:"))),
                                                           # Boton calcular
                                                           column(7, offset = 3,
                                                                  shinyWidgets::actionBttn(inputId = "ok",
                                                                             label = icon("arrow-right"), style = "gradient", color = "success", size = "xs", icon = icon("connectdevelop")),
                                                                  shinyBS::bsTooltip("ok", "Analyze the above contour and go to Shape Descriptors page",
                                                                            "top", options = list(container = "body"))),

                                                           column(7, offset = 5,
                                                                  shinyWidgets::actionBttn(inputId = "gotoresults",
                                                                             label = icon("arrow-right"), style = "gradient", color = "success", size = "xs", icon = icon("pagelines")),
                                                                  shinyBS::bsTooltip("gotoresults", "Analyze the above contour and go directly to Leaf Measures page",
                                                                            "top", options = list(container = "body"))),
                                                           column(7, offset = 9,
                                                                  shinyWidgets::actionBttn(inputId = "soften", label = "S", style = "gradient", color = "success", size = "xs"),
                                                                  shinyBS::bsTooltip("soften", "Soften the outline",
                                                                            "top", options = list(container = "body")))
                                                      ),
                                                      br(), br(),
                                                      wellPanel(plotOutput(outputId = "contour", height = 365))
                                               )


                                       )),

                              tabPanel(title = "scaling",  icon = icon("ruler-vertical"),

                                       fillCol(
                                            shiny::column(width = 2,
                                                          textInput(inputId = "length", label = "Length", value = 1, placeholder = "e.g. 20.50"),
                                                          shinyBS::bsTooltip("length", "set the length of a reference object within the image and then mark the ends of that object",
                                                                    "top", options = list(container = "body"))),
                                            shiny::column(width = 2, offset = 2,
                                                          selectInput(inputId = "units", label = "unit", choices = unitnames),
                                                          shinyBS::bsTooltip("units", "select the length unit of the reference object",
                                                                    "top", options = list(container = "body"))),
                                            shiny::column(width = 2, offset = 4, br(), br(),
                                                          shinyWidgets::actionBttn(inputId = "medido", label = "APPLY", style = "gradient", color = "success", size = "xs")),
                                            shiny::column(width = 4, offset = 4,
                                                          shiny::checkboxInput(inputId = "rememberresize", label = "always remind me to rescale", value = FALSE),
                                                          shinyBS::bsTooltip("rememberresize", "if selected, for each image, the program will not take measurements until the image has been rescaled",
                                                                    "top", options = list(container = "body"))
                                                          )
                                       ),

                                       br(), br(), br(), br(),




                                       mainPanel(shiny::fillCol(
                                            shiny::column(width = 9,
                                                          wellPanel(shiny::uiOutput(outputId = "originalplot"))),
                                            shiny::column(width = 9, offset = 10,
                                                          wellPanel(plotOutput(outputId = "zooming", height = 325,
                                                                               click = "clickzoom")))


                                       ))
                              ),


                              shiny::tabPanel(title = "output options", icon = icon("cogs"),
                                                 helpText(h4("Output options")),

                                                                    h1(shiny::actionButton(inputId = "selectdir", label = "Change output directory", icon = icon("sitemap"))),

                                                 shiny::textInput(inputId = "dirout", label = "output directory", value = diroutput, width = "200%"),

                                                 checkboxInput(inputId = "savecontour",
                                                               label = "Save also contour coordinates"),

                                                 checkboxInput(inputId = "savegraphics",
                                                               label = "Save also the graphic results"),

                                                 radioButtons("landmarksformat", inline = TRUE,
                                                              "save landmarks in file format:", choices = c(".TPS", ".NTS")),

                                                 radioButtons("dec", inline = TRUE,
                                                              "Decimal separator",
                                                              choices = c("comma  ( , )", "point  ( . )")))
                         )

)
           ),


tabPanel(title = "Shape Descriptors", icon = icon("connectdevelop"),

         fillCol(


              column(width = 9,
                     mainPanel(
                          shiny::tabsetPanel(id = "contornos", selected = "Landmarks",
                                             tabPanel(title = "Landmarks", icon = icon("spinner"), br(),
                                                      sidebarPanel(width = 4, # segunda

                                                                   shiny::htmlOutput(outputId = "info3"),
                                                                   radioButtons(inputId = "tipoderejilla", label = "landmarks fan", choices = c("vertical", "radial"), selected = "vertical", inline = FALSE),

                                                                   shiny::conditionalPanel(condition = "input.tipoderejilla != 'sequential points'",
                                                                   numericInput(inputId = "ndivisionesrejilla", label = "fan divisions", value = 2, min = 1, max = 32, step = 1)),

                                                                   shiny::conditionalPanel(condition = "input.tipoderejilla == 'sequential points'",
                                                                   shiny::sliderInput(inputId = "nseqpoints", label = "sequential points number", min = 4, max = 100, value = 15, step = 1)),

                                                                   shiny::conditionalPanel(condition = "input.tipoderejilla == 'radial'",
                                                                   radioButtons(inputId = "desvorigencoef", label = "origin (only for radial fan)", choices = c("base", "center", "centroid"), selected = "center", inline = FALSE)),
                                                                   checkboxInput(inputId = "hidepoints", label = "hide landmarks", value = FALSE)

                                                      ),
                                                      mainPanel(
                                                           shiny::fillCol(
                                                                shiny::column(width = 12,
                                                                              shinyWidgets::addSpinner(
                                                                                   plotOutput(outputId = "contour2", width = 700, height = 550, click = "clickendereza"),
                                                                                   spin = "circle", color = "green")
                                                                ),
                                                                shiny::column(width = 12, offset = 12,
                                                                              shinyWidgets::actionBttn(inputId = "viewresults", label = "View Leaf Measures",
                                                                                         style = "gradient", size = "sm", color = "success", icon = icon("pagelines")))
                                                                              ))),


                                             tabPanel(title = "Contour", icon = icon("circle"),
                                                      shiny::fillCol(
                                                           shiny::column(width = 12,
                                                                         sliderInput(inputId = "nharmonics",
                                                                                     label = "number of Fourier's harmonics",
                                                                                     value = 3, min = 3, max = 100, step = 1)),

                                                           shiny::column(width = 4, offset = 9, br(),
                                                                         shinyWidgets::actionBttn(inputId = "PCA", label = "PCA", style = "gradient", color = "success", size = "md", icon = icon("calculator")),
                                                                         shinyBS::bsTooltip("PCA", "Calculate and save PCA form Fourier`s harmonics.",
                                                                                   "right", options = list(container = "body"))),
                                                           shiny::column(width = 4, offset = 11, helpText("Calculate and save PCA form Fourier's harmonics"))
                                                      ),
                                                      br(), br(), br(), br(), br(), br(),

                                                      fillCol(
                                                           shiny::column(width = 10,
                                                      plotOutput(outputId = "plotfourier", width = 800, height = 400)),

                                                      shiny::column(width = 2, offset = 12,
                                                                    shiny::checkboxGroupInput(inputId = "seelines",
                                                                                              label = "show lines:",
                                                                                              choices = c("original", "Fourier's"),
                                                                                              selected = c("original", "Fourier's"),
                                                                                              inline = FALSE))
                                                      )
                                             ),

                                             # tabPanel(title = "EFD",
                                             #          helpText(h4("Elliptic Fourier's Descriptors")),
                                             #          shiny::tableOutput(outputId = "fouriertable"),
                                             #          tags$style(HTML("
                                             #                          #fouriertable {
                                             #                          height:300px;
                                             #                          overflow-y:scroll
                                             #                          }
                                             #                          ")))

                                             shiny::tabPanel(title = "Measure Options", icon = icon("cog"),

                                                             br(), br(),
                                                             "For heart shaped or lanceolate leaves it is possible to calculate some measures concerning the basal lobes. These measures will not make sense unless they are really leaves with basal lobes.",


                                                             hr(),
                                                             fillCol(
                                                                  shiny::column(width = 12, offset = 0,
                                                                                tags$img(src = "L3.png", width = "30%"),
                                                                                shiny::checkboxInput(inputId = "addLength1", label = "calculate lobe deepth", value = FALSE)),

                                                                  shiny::column(width = 12, offset = 7,  tags$img(src = "basalarea.png", width = "30%"),  br(), br(),
                                                                                shiny::checkboxInput(inputId = "addbasallobearea", label = "calculate basal lobes area", value = FALSE))

                                                             ), br(), br(), br(), br(), br(), br(), br(), br(), br(),

                                                             fillCol(
                                                                  shiny::column(width = 12, offset = 0, tags$img(src = "basalpoints.png", width = "30%"),
                                                                                shiny::checkboxInput(inputId = "addbasallobepoints", label = "add basal lobe points as landmarks", value = FALSE)),
                                                                  shiny::column(width = 12, offset = 7, tags$img(src = "angle.png", width = "30%"),
                                                                                shiny::checkboxInput(inputId = "addbasalapexangle", label = "include apex - basal lobes angle", value = FALSE))
                                                             ), br(), br(), br(), br(), br(), br(),

                                                             fillCol(
                                                                  shiny::column(width = 12, offset = 0, tags$img(src = "pw.png", width = "30%"),
                                                                                shiny::checkboxInput(inputId = "addpetiolewidth", label = "calculate width at the junction with the petiole", value = FALSE))
                                                             ), br(), br(), br(), br(), br(), br(), hr())
                          )

                     )),
              shiny::column(width = 2, offset = 9,

                            shiny::fillCol(
                                 shiny::column(width = 12, offset = 1,
                                               helpText(h4("Save Data"))
                            )),

                            br(), br(),


                                 shinyalert::useShinyalert(),

                            shiny::fillCol(
                                 shiny::column(width = 12, offset = 0,
                                 shinyWidgets::actionBttn(inputId = "save", label = "", style = "gradient", size = "md", color = "success", icon = icon("save")),
                                 shinyBS::bsTooltip("save", "Save Results",
                                           "left", options = list(container = "body"))),


                                 shiny::column(width = 12, offset = 4,
                                 shinyWidgets::actionBttn(inputId = "next2", label = "", style = "gradient", size = "md", color = "success", icon = icon("arrow-right")),
                                 shinyBS::bsTooltip("next2", "Next image file",
                                           "bottom", options = list(container = "body"))
                                 ),
                                 shiny::column(width = 12, offset = 8,
                                               shinyWidgets::actionBttn(inputId = "saveandnext", size = "sm", label = tagList("+", icon("arrow-right")), style = "gradient", color = "success", icon = icon("save")), #, icon = icon("forward")),
                                               shinyBS::bsTooltip("saveandnext", "Save and Next: Save results and go to the next image file",
                                                         "bottom", options = list(container = "body")))

                                 ),


                            br(), br(),
                            fillCol(shiny::column(width = 12, offset = 1,
                            shiny::checkboxInput(inputId = "useprevscaling", label = "keep previous scaling", value = TRUE),
                            shinyBS::bsTooltip("useprevscaling", "If selected, for Save and Next button, it reuse the current scale length in the next photo. Also try to get the leaf outline automatically. Useful when all the images have been taken at the same distance and the leaves are in the same position in all the images.",
                                      "left", options = list(container = "body")))),


                                 br(), br(), br(),



                                 hr(),
                                 shiny::fillCol(
                                      shiny::column(width = 12, offset = 1, helpText(h4(strong("Set the leaf axis:"))))),
                            br(), br(),
                            shiny::fillCol(

                                 shiny::column(width = 12, offset = 0,
                                 shinyWidgets::actionBttn(inputId = "setmanual", size = "md", label = "", style = "gradient", color = "success", icon = icon("compass")),
                                 shinyBS::bsTooltip("setmanual", "By Hand: you must first select two points in the figure (option `Add landmarks by hand` must be unselected). The line between the points defines the angle of rotation.",
                                           "left", options = list(container = "body"))),

                                 shiny::column(width = 12, offset = 4,
                                               shinyWidgets::actionBttn(inputId = "rotate90", label = ("180º"), style = "gradient", size = "sm", color = "success", icon = icon("")),
                                               shinyBS::bsTooltip("rotate90", "Rotate contour 180º",
                                                         "right", options = list(container = "body"))),

                                 shiny::column(width = 12, offset = 8,
                                               shinyWidgets::actionBttn(inputId = "setmaxdist", label = strong(""), style = "gradient", size = "md", color = "success", icon = icon("ruler")),
                                               shinyBS::bsTooltip("setmaxdist", "set the maximum distance as the main leaf axis",
                                                         "right", options = list(container = "body")))
                                 ),
                                 br(), br(), br(),



                                 fillCol(
                                      column(width = 6, offset = 0,
                                             shinyWidgets::actionBttn(inputId = "setflip", label = "", style = "gradient", size = "md", color = "success", icon = icon("arrows-v")),
                                             shinyBS::bsTooltip("setflip", "flip vertical",
                                                       "left", options = list(container = "body"))),

                                      column(width = 6, offset = 4,
                                             shinyWidgets::actionBttn(inputId = "setflop", label = "", style = "gradient", size = "md", color = "success", icon = icon("arrows-h")),
                                             shinyBS::bsTooltip("setflop", "flip horizontal",
                                                       "bottom", options = list(container = "body"))),

                                      column(width = 12, offset = 8,
                                             shinyWidgets::actionBttn(inputId = "settranspose", label = "", style = "gradient", size = "md", color = "success", icon = icon("random")),
                                             shinyBS::bsTooltip("settranspose", "exchange x and y axes",
                                                       "bottom", options = list(container = "body"))
                                      )),

                            br(), br(),  hr(),


                            fillCol(shiny::column(width = 12, offset = 1,
                            shiny::checkboxInput(inputId = "addlandmark", label = "Add landmarks by hand", value = FALSE),
                            shinyBS::bsTooltip("addlandmark", "If selected, it is possible to set new landmarks by click on the graphic",
                                      "left", options = list(container = "body")))), br(), br(),

                            shiny::conditionalPanel(condition = "input.addlandmark", br(),

                                                    shiny::fillCol(
                                                         shiny::column(width = 6, offset = 0,
                                                                       shinyWidgets::actionBttn(inputId = "removelandmark", label = "", style = "gradient",
                                                                                  size = "sm", color = "success", icon = icon("undo")),
                                                                       shinyBS::bsTooltip("removelandmark", "Remove the last added landmark",
                                                                                 "left", options = list(container = "body"))),
                                                         shiny::column(width = 6, offset = 4,
                                                                       shinyWidgets::actionBttn(inputId = "removealllandmarks", label = "",
                                                                                  style = "gradient", size = "sm", color = "success", icon = icon("trash")),
                                                                       shinyBS::bsTooltip("removealllandmarks", "Remove all the added landmarks",
                                                                                 "right", options = list(container = "body")))
                                                    ),

                                                    br(),br(),


                                                    fillCol(shiny::column(width = 12, offset = 1,
                                                    shiny::checkboxInput(inputId = "fixlandmarks", label = "set added landmarks on contour", value = FALSE),
                                                    shinyBS::bsTooltip("fixlandmarks", "If selected, all the new landmarks will be set on the contour line",
                                                              "left", options = list(container = "body")))), br(), br(), br()
                            ), hr()
                            )
         )),



tabPanel(title = "Leaf Measures", icon = icon("pagelines"),
         headerPanel(title = "Leaf Measures"),

         # shiny::fillCol( # Este fillcol vacio por alguna razon es necesario para
         #      shiny::column(width = 12, offset = 0, # que los botones funcionen
         #                    helpText(h4("."))
         #      )),

              fillRow(width = "90%",

                      shiny::column(width = 12,

                                    shiny::fillCol(width = "50%",
                                                   shiny::column(width = 12, offset = 6,
                                                                 shinyWidgets::actionBttn(inputId = "save2", label = "", style = "gradient", size = "md", color = "success", icon = icon("save")),
                                                                 shinyBS::bsTooltip("save2", "Save Results",
                                                                           "bottom", options = list(container = "body"))),


                                                   shiny::column(width = 12, offset = 9,
                                                                 shinyWidgets::actionBttn(inputId = "next3", label = "", style = "gradient", size = "md", color = "success", icon = icon("arrow-right")),
                                                                 shinyBS::bsTooltip("next3", "Next image file",
                                                                           "bottom", options = list(container = "body"))
                                                   ),
                                                   shiny::column(width = 12, offset = 12,
                                                                 shinyWidgets::actionBttn(inputId = "saveandnext2", size = "sm", label = tagList("+", icon("arrow-right")), style = "gradient", color = "success", icon = icon("save")), #icon = icon("forward")),
                                                                 shinyBS::bsTooltip("saveandnext2", "Save and Next: Save results and go to the next image file",
                                                                           "right", options = list(container = "body")))
                                    ), br(), br(),

                                    shiny::htmlOutput(outputId = "info2"), br(), br(),
                                    helpText(h4("Leaf Measures:")),

                                    tags$style(HTML("
                  #eggresultslist {
                    height:300px;
                    overflow-y:scroll
                  }
                  ")),

                                    htmlOutput(outputId = "eggresultslist")),

                                    shiny::tabsetPanel(

                                         tabPanel(title = "angles",
                                                  plotOutput("plotangulos", width = 600)),
                                         tabPanel(title = "asymmetry",
                                                  plotOutput("sombraslaterales", width = 500)),
                                         tabPanel(title = "area",
                                                  plotOutput("plotrectangulohoja", width = 500)),
                                         tabPanel(title = "outline",
                                                  plotOutput("outlineplot", width = 500)),
                                         tabPanel(title = "EFD",
                                                  helpText(h4("Elliptic Fourier's Descriptors")),
                                                  shiny::tableOutput(outputId = "fouriertable"),
                                                  tags$style(HTML("
                  #fouriertable {
                    height:300px;
                    overflow-y:scroll
                  }
                  "))),
                                         tabPanel(title = "Image Data",
                                                  helpText(h4("Image Data")),
                                                  shiny::htmlOutput(outputId = "imgdata"))

                                    ))







),

           tabPanel(title = "Help", icon = icon("info"), includeMarkdown("www/help.rmd"))
))










