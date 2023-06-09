library(dplyr) ### Gestion a base de datos
library(ggplot2) ### Libreria de graficos
library(shiny)
library(DT) ####Datatable libreria
library(ggrepel) ### separador de etiquetas de datos para ggplot
library(tidyr) #####Tidyverse
library(shinycssloaders) #### Libreria visual
library(shinythemes) #### Libreria visual
library(SwimmeR) ####

#Import Data
BigTop100 <- read.csv("BigTop100.csv")
fiftystatesCAN <- read.csv("fiftystatesCAN.csv")
uniquecities <- read.csv("uniquecities.csv")

Events <- ordered(BigTop100$Event, levels = c("hora","50 Free", "100 Free", "200 Free", "500 Free", "1000 Free", "1650 Free", "100 Fly", "200 Fly", "100 Back", "200 Back", "100 Breast", "200 Breast", "100 IM", "200 IM", "400 IM", "200 Free Relay", "400 Free Relay", "800 Free Relay", "200 Medlay Relay", "400 Medlay Relay"))

# mmss_format <- function(x, ...) {
#   sec <- x%%60
#   min <- x%/%60
#   sec <- base::sprintf("%05.2f", sec)
#   ifelse(min == 0, paste(sec),
#          paste(min, sec, sep = ":"))
# }

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

# Define UI
ui <- fluidPage(

#Navbar structure for UI
  navbarPage("Dashboard Colecciones especiales USBI Xalapa", theme = shinytheme("superhero"),
             tabPanel("Monitor temperatura y humedad", fluid = TRUE, icon = icon("globe-americas"),
                      tags$style(button_color_css),
                      # Sidebar layout with a input and output definitions
                      titlePanel("Monitor de temperatura y humedad"),
                      hr(),
                      sidebarLayout(

                        sidebarPanel(

                          titlePanel("Temperatura y humedad promedio"),
                          hr(),
                          #shinythemes::themeSelector(),
                          fluidRow(column(12,

                                          # Select which Gender(s) to plot
                                          textInput(inputId = "Temperatura",
                                                    label = "Humedad relativa",
                                                    value = "74%",
                                                    #text-size: "15px",
                                                    width = "220px"),

                                          # Select which Division(s) to plot
                                          textInput(inputId = "Temperatura",
                                                    label = "Temperatura",
                                                    value = "26.3°C",
                                                    width = "220px")
                          ),

                          ),

                          # Set Time Range

                          helpText("Aqui se muestran los valores promedio de temperatura y humedad dentro de laa sala"),

                          hr(),

                        ),
                        mainPanel(
                          fluidRow(
                            column(3, offset = 9,

                                   #radioButtons(inputId = "show_NamesFinder",
                                   #             label = "Zona de croquis",
                                  #              choices = c("croquis"),
                                  #               selected = "Croquis")
                            )),
                          # hr(),
                          HTML('<img src="Croquis.png", height="520px"'),
                          hr(),
#                          br(),
                          #fluidRow(
                          #withSpinner(dataTableOutput(outputId = "schoolstableFinder")))
)
                      )
             ),

             tabPanel("Exploración Espacial", fluid = TRUE, icon = icon("globe-americas"),
                      titlePanel("Exploración Espacial"),
                      hr(),
                      sidebarLayout(
                        sidebarPanel(

                          titlePanel("Seleccione alguna de las siguientes opciones para iniciar"),
                          #shinythemes::themeSelector(),
                          fluidRow(column(12,

                                          # Select which Gender(s) to plot
                                          radioButtons(inputId = "show_NamesFinder",
                                                                  label = "",
                                                                     choices = c("Selección simple","Selección multiple","Excluir nodo de la selección", "Añadir nodo a la selección"),
                                                                     selected = "Selección multiple"),

                                          # Select which Division(s) to plot
                          ),

                          ),

                          #helpText("Forma de exploración temporal(mover a la otra pestaña) "),
                          actionButton(inputId = "EnterTimes", label = "Reiniciar selección"),
                          hr(),
                          #helpText("Ingresar fechas en el formato dd/mm/aaaa"),
                          hr(),

                        ),
                        mainPanel(
                          fluidRow(
                            column(3, offset = 9,

                                   #radioButtons(inputId = "show_NamesFinder",
                                   #             label = "Zona de croquis",
                                   #              choices = c("croquis"),
                                   #               selected = "Croquis")
                            )),
                          # hr(),
                          HTML('<img src="Croquis.png", height="520px"'),
                          hr(),
                          fluidRow(column(7,
                                          #helpText("Prueba")
                                          #actionButton(inputId = "draw", label = "Input Event and Times")

                          ),
                          column(width = 2, offset = 2, conditionalPanel(
                            condition = "output.schoolstableFinder",
                            actionButton(inputId = "FinderClear", label = "Clear Table")))),
                          br(),
                          fluidRow(
                            withSpinner(dataTableOutput(outputId = "schoolstableFinder"))))
                      )


             ),

             tabPanel("Exploración temporal", fluid = TRUE, icon = icon("globe-americas"),
                      titlePanel("Exploración temporal"),
                      hr(),
                      sidebarLayout(
                        sidebarPanel(

                          titlePanel("Temperatura y humedad promedio"),
                          #shinythemes::themeSelector(),
                          fluidRow(column(6,
                                          textInput(inputId = "TimeFinderMin",
                                                    label = "Desde:",
                                                    value = "02/03/2021",
                                                    width = "150px")
                          ),
                          column(6, ofset = 3,
                                 textInput(inputId = "TimeFinderMax",
                                           label = "Hasta:",
                                           value = "03/03/2021",
                                           width = "150px")
                          )),
                          helpText("Ingresar fechas en el formato dd/mm/aaaa"),
                          fluidRow(column(8,

                                          # Select which Gender(s) to plot
                                          selectInput(inputId = "DivCompRaceA",
                                                      label = "Seleccione una resolución:",
                                                      choices = levels(Events),
                                                      selected = "Hora"),

                                          helpText("Esto indica cada cuanto tiempo es de interes ver un cambio"),
                          ),

                          ),

                          fluidRow(column(8,

                                          # Select which Gender(s) to plot
                                          radioButtons(inputId = "show_NamesFinder",
                                                       label = "",
                                                       choices = c("Mapa de calor"),
                                                       selected = ""),

                          ),

                          ),
                          fluidRow(column(8, offset = 2,

                                          # Select which Gender(s) to plot

                                          sliderInput("slider1", label = h3("Navegación temporal"), min = 0,
                                                      max = 24, width = "750px", value = c(6,6)

                          ),
                          helpText("Modifique esta barra para cambiar *Hora en el mapa" ),
                          )),


                          # Set Time Range


                          actionButton(inputId = "EnterTimes", label = "Ingresar"),
                          hr(),
                          actionButton(inputId = "EnterTimes", label = "Reiniciar selección"),

                        ),
                        mainPanel(
                          fluidRow(
                            column(3, offset = 9,

                            )),
                          # hr(),
                          HTML('<img src="Croquis.png", height="520px"'),
                          hr(),
                          fluidRow(
                            column(3, offset = 5,


                            )),
                          )
                      )

             ),
             tabPanel("Filtrar", fluid = TRUE, icon = icon("globe-americas"),
                      titlePanel("Filtrar"),
                      hr(),

                      sidebarPanel(

                        titlePanel("Filtrar"),
                        #shinythemes::themeSelector(),
                        fluidRow(column(6,
                                        textInput(inputId = "TimeFinderMin",
                                                  label = "Desde:",
                                                  value = "02/03/2021",
                                                  width = "150px")
                        ),
                        column(6, ofset = 3,
                               textInput(inputId = "TimeFinderMax",
                                         label = "Hasta:",
                                         value = "03/03/2021",
                                         width = "150px")
                        )),
                        helpText("Ingresar fechas en el formato dd/mm/aaaa"),
                        fluidRow(column(8,

                                        # Select which Gender(s) to plot
                                        selectInput(inputId = "DivCompRaceA",
                                                    label = "Seleccione una resolución:",
                                                    choices = levels(Events),
                                                    selected = "Hora"),

                                        helpText("Esto indica cada cuanto tiempo es de interes ver un cambio"),
                        ),

                        ),

                        fluidRow(column(8,

                                        # Select which Gender(s) to plot
                                        radioButtons(inputId = "show_NamesFinder",
                                                     label = "",
                                                     choices = c("Mapa de calor"),
                                                     selected = ""),

                        ),

                        ),
                        fluidRow(column(8, offset = 2,

                                        # Select which Gender(s) to plot

                                        sliderInput("slider1", label = h3("Navegación temporal"), min = 0,
                                                    max = 24, width = "750px", value = c(6,6)

                                        ),
                                        helpText("Modifique esta barra para cambiar *Hora en el mapa" ),
                        )),


                        # Set Time Range


                        actionButton(inputId = "EnterTimes", label = "Ingresar"),
                        hr(),
                        actionButton(inputId = "EnterTimes", label = "Reiniciar selección"),

                      ),
                      mainPanel(
                        fluidRow(
                          column(3, offset = 9,

                          )),
                        # hr(),
                        HTML('<img src="Croquis.png", height="520px"'),
                        hr(),
                        fluidRow(
                          column(3, offset = 5,


                          )),
                      )


             ),
tabPanel("Comparar", fluid = TRUE, icon = icon("globe-americas"),
         titlePanel("Filtrar"),
         hr(),

         sidebarPanel(

           titlePanel("Filtrar"),
           #shinythemes::themeSelector(),
           fluidRow(column(6,
                           textInput(inputId = "TimeFinderMin",
                                     label = "Desde:",
                                     value = "02/03/2021",
                                     width = "150px")
           ),
           column(6, ofset = 3,
                  textInput(inputId = "TimeFinderMax",
                            label = "Hasta:",
                            value = "03/03/2021",
                            width = "150px")
           )),
           helpText("Ingresar fechas en el formato dd/mm/aaaa"),
           fluidRow(column(8,

                           # Select which Gender(s) to plot
                           selectInput(inputId = "DivCompRaceA",
                                       label = "Seleccione una resolución:",
                                       choices = levels(Events),
                                       selected = "Hora"),

                           helpText("Esto indica cada cuanto tiempo es de interes ver un cambio"),
           ),

           ),

           fluidRow(column(8,

                           # Select which Gender(s) to plot
                           radioButtons(inputId = "show_NamesFinder",
                                        label = "",
                                        choices = c("Mapa de calor"),
                                        selected = ""),

           ),

           ),
           fluidRow(column(8, offset = 2,

                           # Select which Gender(s) to plot

                           sliderInput("slider1", label = h3("Navegación temporal"), min = 0,
                                       max = 24, width = "750px", value = c(6,6)

                           ),
                           helpText("Modifique esta barra para cambiar *Hora en el mapa" ),
           )),


           # Set Time Range


           actionButton(inputId = "EnterTimes", label = "Ingresar"),
           hr(),
           actionButton(inputId = "EnterTimes", label = "Reiniciar selección"),

         ),
         mainPanel(
           fluidRow(
             column(3, offset = 9,

             )),
           # hr(),
           HTML('<img src="Croquis.png", height="520px"'),
           hr(),
           fluidRow(
             column(3, offset = 5,


             )),
         )


),

  navbarMenu("Ayuda", icon = icon("info-circle"),
             tabPanel("Ayuda", fluid = TRUE,
                      fluidRow(
                        column(6,
                               h4(p("Aqui va la ayuda a usuarios")),
                               h5(p("Aqui va la ayuda a usarios en letra mas pequeña")

                               )
                        ),
                        column(6,
                               h4(p("Columna 2")),
                               h5(p("Prueba enlace",
                                    a("web.",
                                      href = "https://shiny.rstudio.com/gallery/widget-gallery.html"))
                               )
                      ))

                        ),

               tabPanel("Ayuda", fluid = TRUE,
               fluidRow(
               column(6,
                      #br(),
                      h4(p("Aqui va la ayuda a usuarios")),

                          )
          ),
          br(),
          hr(),
          h5("Built with",
             img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
             "by",
             img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
             ".")
                        )
  )
)
)

# Define server
server <- function(input, output, session) {

  #Program Finder

  TimeFinderDF <- reactive({
    req(input$TimeFinderMin)
    TimeFinderDF <- as.data.frame(c(input$TimeFinderMin, input$TimeFinderMax))
    names(TimeFinderDF)[1] <- "UserTimes"
    TimeFinderDF$UserTimes <- as.character(TimeFinderDF$UserTimes)
    TimeFinderDF <- tidyr::separate(TimeFinderDF, col = UserTimes, c("min", "sec"), sep = ":", remove = FALSE, extra = "drop", fill = "left")
  TimeFinderDF[is.na(TimeFinderDF)] <- 0
  TimeFinderDF$sec <- as.numeric(TimeFinderDF$sec)
  TimeFinderDF$min <- as.numeric(TimeFinderDF$min)
  TimeFinderDF <- TimeFinderDF %>%
    mutate(Time = (TimeFinderDF$min*60) + TimeFinderDF$sec)
  })

  BigTop100_finder <- reactive({
    req(input$DivisionFinder)
    req(input$RegionFinder)
    req(input$School_TypeFinder)
    req(input$GenderFinder)
    req(input$EventFinder)
    #req(Input$School_Rank)
    filter(BigTop100, Division %in% input$DivisionFinder) %>%
      filter(Region %in% input$RegionFinder) %>%
      filter(Event %in% input$EventFinder) %>%
      filter(Time >= TimeFinderDF()$Time[1], Time <= TimeFinderDF()$Time[2]) %>%
      filter(Sex %in% input$GenderFinder) %>%
      filter(Type %in% input$School_TypeFinder) %>%
      filter(Y2019 >= input$School_RankFinder[1], Y2019 <= input$School_RankFinder[2]) %>%
      filter(RankInEvent_Team >= input$RankOnTeam[1], RankInEvent_Team <= input$RankOnTeam[2]) %>%
      group_by(Team, Event) %>%
      dplyr::mutate(Entries = n()) %>%
      dplyr::mutate(MinTime = mmss_format(min(Time))) %>%
      dplyr::mutate(MaxTime = mmss_format(max(Time)))

  })

  fiftystatesCAN_Finder <- reactive({
    req(input$RegionFinder)
    filter(fiftystatesCAN, GeoRegion %in% input$RegionFinder)
  })

  uniquecities_Finder <- reactive({
    req(input$RegionFinder)
    filter(uniquecities, Region %in% input$RegionFinder) %>%
      filter(Team %in% BigTop100_finder()$Team)
  })

  output$scatterplotFinder <- renderPlot({
    input$EnterTimes
    input$show_NamesFinder
    input$GenderFinder
    input$DivisionFinder
    input$RegionFinder
    input$RankOnTeam
    input$School_TypeFinder
    input$School_RankFinder
    isolate({
      if (length(BigTop100_finder()$Address) == 0) {
        ggplot() +
          geom_polygon(data = fiftystatesCAN_Finder(), aes(x = long, y = lat, group = group), color = "white", fill = "grey") +
          coord_quickmap() +
          theme_void() +
          ggtitle("No programs fit selected characteristics. \nPlease modify selections.") +
          theme(plot.title = element_text(face = "bold", color = "#FF8D1E", size = 20))
      } else {
        ggplot() +
          geom_polygon(data = fiftystatesCAN_Finder(), aes(x = long, y = lat, group = group), color = "white", fill = "grey") +
          geom_point(data = uniquecities_Finder(), aes(x = lon, y = lat, alpha = 0.8)) +
          {if(input$show_NamesFinder == "School Names") geom_text_repel(data = uniquecities_Finder(), aes(x = lon, y = lat, label = as.character(Team)))} +
          {if(input$show_NamesFinder == "City Names") geom_text_repel(data = uniquecities_Finder(), aes(x = lon, y = lat, label = as.character(City)))} +
          coord_quickmap() +
          guides(fill = FALSE) +
          geom_point(data = BigTop100_finder(), aes(x = lon, y = lat, color = Division, shape = Sex), alpha = 0.5) +
          theme_void() +
          labs(color = "Division", shape = "Gender"
               #, title = pretty_plot_title()
          ) +
          {if(length(input$DivisionFinder) <= 1) scale_color_manual(guide = "none", values = c("DI" = "#1E90FF", "DII" = "#FF8D1E", "DIII" = "#20FF1E"))} +
          {if(length(input$DivisionFinder) > 1)
            scale_color_manual(values = c("DI" = "blue", "DII" = "red", "DIII" = "green"))} +
            {if(length(input$GenderFinder) <= 1) scale_shape_manual(guide = "none", values = c("M" = "circle", "F" = "triangle"))} +
            {if(length(input$GenderFinder) > 1)
              scale_shape_manual(values = c("M" = "circle", "F" = "triangle"))} +
          theme(axis.text = element_blank(), axis.ticks = element_blank()) +
          theme(plot.title = element_text(hjust=0.5, face = "bold")) +
          theme(plot.background = element_rect(fill = "white"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
          guides(alpha = FALSE) +
          theme(legend.text = element_text(size = 12),
                legend.title = element_text(size = 15)) +
          theme(plot.background = element_rect(
            color = "white"
          ))

      }
    })
  })

  user_clickFinder <- reactiveValues()
  reactive({
    user_clickFinder$DT <- data.frame(matrix(0, ncol = ncol(BigTop100), nrow = 1))
    names(user_clickFinder$DT) <- colnames(BigTop100)
  })

  observeEvent(input$click_plotFinder, {
    add_row <-     nearPoints(BigTop100_finder(), input$click_plotFinder, xvar = "lon", yvar = "lat", threshold = 5)
    user_clickFinder$DT <- rbind(add_row, user_clickFinder$DT)
  })

  brushFinder <- reactive({
    req(length(user_clickFinder$DT) > 1)
    user_clickFinder$DT
  })

  observeEvent({
    input$FinderClear
    #input$EnterTimes
  },{
    user_clickFinder$DT <- NULL
  })

  output$schoolstableFinder<-DT::renderDataTable({

    DT::datatable(unique(brushFinder()[,c("Name", "Class", "X.swim_time", "Team", "Relative_RankInEvent_Team", "Division", "Address", "Y2019", "Type", "Time")]),
                  colnames = c("Sort" = "Time", "Time" = "X.swim_time", "US News School Ranking" = "Y2019", "School Type" = "Type", "Swimmer Rank In Event On Team" = "Relative_RankInEvent_Team"),
                  rownames = FALSE,
                  options = list(order = list(9, 'asc'),
                                 columnDefs = list(list(visible=FALSE, targets=c(9)),
                                                   list(className = "dt-center", targets = 1:7),
                                                   list(classname = "dt-right", targets = 8))
                  ))

  })

  #Program Comparisons

  BigTop100_SchoolComp <- reactive({
    req(input$SchoolCompGender)
    req(input$SchoolSelectA)
    req(input$SchoolCompRace)
    filter(BigTop100, Sex %in% input$SchoolCompGender) %>%
      filter(Event %in% input$SchoolCompRace) %>%
      filter(Team %in% input$SchoolSelectA | Team %in% input$SchoolSelectB)

  })
  reactive({
    BigTop100_SchoolComp$Time <- as.numeric(format(BigTop100_SchoolComp()$Time, nsmall = 2))
  })

  output$SchoolCompPlotEvent <- renderPlot({
    ggplot(data = BigTop100_SchoolComp(), aes(y = Time, x = Team, color = Team)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(position = position_jitter(width = 0.05), alpha = 0.8) +
      scale_color_manual(values=c("#1E90FF", "#20FF1E", "#FF8D1E", "#FD1EFF")) +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      theme(legend.title=element_blank(), panel.grid.major = element_line(color = "white"), panel.grid.minor = element_line(color = "white")) +
      theme(plot.title = element_text(hjust=0.5, face = "bold")) +
      theme(legend.position="none") +
      scale_y_continuous(labels = scales::trans_format("identity", mmss_format)) +
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 15),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15))


  })

  output$SchoolCompDT<-DT::renderDataTable({
    DT::datatable(BigTop100_SchoolComp()[,c("Name", "Team", "X.swim_time", "Class", "Rank", "Division", "Time")],
                  colnames = c("Sort" = "Time", "Time" = "X.swim_time"),
                  rownames = FALSE,
                  options = list(order = list(6, 'asc'),
                                 columnDefs = list(list(visible=FALSE, targets=6),
                                                   list(className = "dt-center", targets = 1:5)
                                                   #list(className = "dt-right", targets = 5)
                                 ))

    )
  })


  output$SchoolCompStats<-DT::renderDataTable({
    if(input$TuitionType == "Yes"){
      DT::datatable(unique(BigTop100_SchoolComp()[,c("Team", "Type", "Y2019", "Tuition_In", "Enrollment", "Public")]),
                    colnames = c("US News Ranking" = "Y2019", "Tuition" = "Tuition_In"),
                    rownames = FALSE,
                    options = list(order = list(0, 'asc'),
                                   columnDefs = list(list(className = "dt-center", targets = 1:5)),
                                   dom = 't'

                    ))
    }
    else if(input$TuitionType == "No"){
      DT::datatable(unique(BigTop100_SchoolComp()[,c("Team", "Type", "Y2019", "Tuition_Out", "Enrollment", "Public")]),
                    colnames = c("US News Ranking" = "Y2019", "Tuition" = "Tuition_Out"),
                    rownames = FALSE,
                    options = list(order = list(0, 'asc'),
                                   dom = 't',
                                   list(columnDefs = list(list(className = "dt-center", targets = 1:5)))
                    ))
    }
  })

  #Division Comparisons

  BigTop100_subsetACA_DI <- reactive({
    req(input$GenderDI)
    req(input$RegionDI)
    req(input$RankDI)
    filter(BigTop100, Division == "DI") %>%
      filter(Sex %in% input$GenderDI) %>%
      filter(Region %in% input$RegionDI) %>%
      filter(Rank >= input$RankDI[1], Rank <= input$RankDI[2]) %>%
      filter(Y2019 >= input$School_RankDI[1], Y2019 <= input$School_RankDI[2]) %>%
      group_by(Team) %>%
      dplyr::mutate('No. of Top Times' = n())
  })

  BigTop100_subsetACA_DII <- reactive({
    req(input$GenderDII)
    req(input$RegionDII)
    req(input$RankDII)
    filter(BigTop100, Division == "DII") %>%
      filter(Sex %in% input$GenderDII) %>%
      filter(Region %in% input$RegionDII) %>%
      filter(Rank >= input$RankDII[1], Rank <= input$RankDII[2]) %>%
      filter(Y2019 >= input$School_RankDII[1], Y2019 <= input$School_RankDII[2]) %>%
      group_by(Team) %>%
      dplyr::mutate('No. of Top Times' = n())
  })

  BigTop100_subsetACA_DIII <- reactive({
    req(input$GenderDIII)
    req(input$RegionDIII)
    req(input$RankDIII)
    filter(BigTop100, Division == "DIII") %>%
      filter(Sex %in% input$GenderDIII) %>%
      filter(Region %in% input$RegionDIII) %>%
      filter(Rank >= input$RankDIII[1], Rank <= input$RankDIII[2]) %>%
      filter(Y2019 >= input$School_RankDIII[1], Y2019 <= input$School_RankDIII[2]) %>%
      group_by(Team) %>%
      dplyr::mutate('No. of Top Times' = n())
  })

  BigTop100_DivCompA <- reactive({
    req(input$DivCompGenderA)
    req(input$DivCompRankA)
    req(input$DivCompRaceA)
    filter(BigTop100, Sex %in% input$DivCompGenderA) %>%
    filter(Rank >= input$DivCompRankA[1], Rank <= input$DivCompRankA[2]) %>%
    filter(Event %in% input$DivCompRaceA)
  })
  reactive({
  BigTop100_DivCompA$Time <- as.numeric(format(BigTop100_DivCompA()$Time, nsmall = 2))
  })


  output$barplotDI <- renderPlot({
    ggplot() +
    geom_bar(data = BigTop100_subsetACA_DI(), aes(x = Division, y = (..count../sum(..count..)*100), fill = Type)) +
    labs(y = "Percent", x = "Divison") +
    coord_polar("y", start=0) +
    scale_fill_manual(values = c("National University" = "#1E90FF", "National Liberal Arts College" = "#FD1EFF", "Regional College" = "#FF8D1E", "Regional University" = "#20FF1E"), aesthetics = "fill") +
    theme_void()
  })

  output$description_DI <- renderText({
    paste0("Division I is primarily made of national universities, with a sizable subset of regional universities.
           There are relatively few colleges.")
  })

  output$barplotDII <- renderPlot({
    ggplot() +
      geom_bar(data = BigTop100_subsetACA_DII(), aes(x = Division, y = (..count../sum(..count..)*100), fill = Type)) +
      labs(y = "Percent", x = "Divison") +
      scale_fill_manual(values = c("National University" = "#1E90FF", "National Liberal Arts College" = "#FD1EFF", "Regional College" = "#FF8D1E", "Regional University" = "#20FF1E"), aesthetics = "fill") +
      coord_polar("y", start=0) +
      theme_void()
  })

  output$description_DII <- renderText({
    paste0("Division II is primarily made of regional universities, with a national universities as the second largest component.
           There are relatively few national or regional colleges.")
  })

  output$barplotDIII <- renderPlot({
    ggplot() +
      geom_bar(data = BigTop100_subsetACA_DIII(), aes(x = Division, y = (..count../sum(..count..)*100), fill = Type)) +
      labs(y = "Percent", x = "Divison") +
      scale_fill_manual(values = c("National University" = "#1E90FF", "National Liberal Arts College" = "#FD1EFF", "Regional College" = "#FF8D1E", "Regional University" = "#20FF1E"), aesthetics = "fill") +
      coord_polar("y", start=0) +
      theme_void()
  })

  output$description_DIII <- renderText({
    paste0("Division III is primarily made of national universities and national liberal arts colleges.
          Regional universities and colleges are a smaller component")
  })

  output$DivCompPlotA <- renderPlot({
      ggplot(data = BigTop100_DivCompA(), aes(y = Time, x = Division, color = Division)) +
      geom_violin() +
      geom_jitter(position = position_jitter(width = 0.08), alpha = 0.5, size = 3) +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      scale_color_manual(values = c("DI" = "#1E90FF", "DII" = "#FF8D1E", "DIII" = "#20FF1E")) +
      theme(legend.title=element_blank(), panel.grid.major = element_line(color = "white"), panel.grid.minor = element_line(color = "white")) +
      theme(plot.title = element_text(hjust=0.5, face = "bold")) +
      theme(legend.position="none") +
      scale_y_continuous(labels = scales::trans_format("identity", mmss_format)) +
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 15),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15))
  })

  #using brush plot

  brushDiv <- reactive({
    user_brushDiv <- input$brush_plotDiv
    brushedPoints(BigTop100_DivCompA(), user_brushDiv, xvar = "Division", yvar =
                    "Time")
  })

  observeEvent(input$DivCompClear, {
    brushDiv <- NULL
  })

  #using click plot

  # user_clickDiv <- reactiveValues()
  # reactive({
  #   user_clickDiv$DT <- data.frame(matrix(0, ncol = ncol(BigTop100_DivCompA()), nrow = 1))
  #   names(user_clickDiv$DT) <- colnames(BigTop100_DivCompA())
  # })
  #
  # observeEvent(input$click_plotDiv, {
  #   add_row <-     nearPoints(BigTop100_DivCompA(), input$click_plotDiv, xvar = "Division", yvar = "Time", threshold = 8)
  #   user_clickDiv$DT <- rbind(add_row, user_clickDiv$DT)
  # })
  #
  # brushDiv <- reactive({
  #   req(length(user_clickDiv$DT) > 1)
  #   user_clickDiv$DT
  # })
  #
  # observeEvent(input$DivCompClear, {
  #   user_clickDiv$DT <- NULL
  # })

  output$DivCompTable<-DT::renderDataTable({
    DT::datatable(unique(brushDiv()[,c("Name", "Team", "X.swim_time", "Rank", "Division", "Time")]),
                  colnames = c("Sort" = "Time", "Time" = "X.swim_time", "Rank In Division" = "Rank"),
                  rownames = FALSE,
                  options = list(order = list(5, 'asc'),
                                 columnDefs = list(list(visible=FALSE, targets=c(5)),
                                                   list(className = "dt-center", targets = 1:5)
                                 ))
    )
    output$text <- renderText({ input$txt })
    output$verb <- renderText({ input$txt })
  })

  #session$onSessionEnded(stopApp)
}
# Run the application
shinyApp(ui = ui, server = server)

