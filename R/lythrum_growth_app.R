lythrum_growth_app <- function(){

  #' Imports
  #' @importFrom dplyr filter
  #' @importFrom dplyr pull
  #' @importFrom dplyr select
  #' @importFrom dplyr mutate
  #' @import ggplot2

  pop_names <- unique(parameters$POP)

  # user interface ----

  ui <- fluidPage(
    withMathJax(),
    p(),
    navbarPage("Lythrum growth curves",
               tabPanel("Instructions",
                        p(),
                        p("This app displays growth curves for ", em("Lythrum salicaria"), " individuals measured in a common garden in 2017
    (2278 individuals total). The growth curves are generated using a sigmoidal function of the form:",
                          "$$Height(t) = \\frac{A_{sym}}{1 + e^{-(t-T_{i}) * r}}$$"),
                        p("Under the growth curves tab, you can select a population, family, then individual to initiate the app. The main panel shows
                        the predicted growth curve and height measurements collected for a selected individual through the season, along with the numeric
                        values for the parameters ", em("Asym"), ", ", em("Ti"), ", and ", em("r"), ", and the number of measurements taken from the
                        individual. The three smaller panels (histograms panel) show how the individual estimate for each of the three parameters compares
                        to the entire common garden population. The four panels are updated only when a new individual is selected.")),
               tabPanel("Growth curves",

    # Select family, settings, and main graph ----

    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          id = "mainset",
          tabPanel(
            "Select Individual",
            p(),
            fluidRow(selectInput("population", "Population", choices = c("Population" = "", pop_names))),
            fluidRow(selectInput("family", "Family", choices = c("Family" = ""))),
            fluidRow(selectInput("id", "Individual", choices = c("Individual" = "")))
            ),
          tabPanel(
            "Options",
            fluidRow(
              p(),
              p("By default, the y-axis changes to fit the data when a new panel is loaded. You may choose instead to set a fixed height:"),
            ),
            fluidRow(
              radioButtons("ht_opt", label = NULL, choices = c("Variable", "Fixed"), selected = "Variable")
              ),
            fluidRow(
              sliderInput("height", "Max height", min = 30, max = 120, value = 120, step = 5))
            )
        )
          ),
      mainPanel(
        tabsetPanel(id = "switcher", type = "hidden",
                    tabPanelBody("no_select",
                                 p("To initiate this app, select a population, family, then individual from the dropdown menu. The growth curve and histograms
                          will then be displayed.")),
                    tabPanelBody("growth_plot", plotOutput("growth_curve")))
      )
      ),

    # Instructions and histograms ----

    fluidRow(style = "padding:20px",
      column(4, plotOutput("Asym_id")),
      column(4, plotOutput("Ti_id")),
      column(4, plotOutput("r_id")))
    )
  )
  )

  # backend ----
  server <- function(input, output, session) {

    # show instructions if no id selected ----

    observe({
      req(input$id, cancelOutput = TRUE)
      updateTabsetPanel(session, "switcher", selected = "growth_plot")
    })

    # update dropdown lists as required ----
    # will leave placeholder names

    observe({
      updateSelectInput(session, "family",
                        choices = c("Family" = "",
                                    unique(pull(filter(parameters, POP == input$population), FAMCODE))))
    }, priority = 1)

    observe({
      updateSelectInput(session, "id",
                        choices = c("Individual" = "",
                                    unique(pull(filter(parameters, FAMCODE == input$family), CODE))))
    }, priority = 0)

    # specify parameters for histogram ----
    # cancelOutput to leave figures in place when no new ID selected

    growth_id <- reactive({
      req(input$id, cancelOutput = TRUE)
      input$id
    })


    bins <- reactive({
      req(input$id, cancelOutput = TRUE)
      filter(parameters, CODE == input$id)
    })

    parameters_out <- reactive({
      req(input$id, cancelOutput = TRUE)
      mutate(parameters, Asym_col = ifelse(Asym_bin == pull(bins(), Asym_bin), T, F),
             Ti_col = ifelse(Ti_bin == pull(bins(), Ti_bin), T, F),
             r_col = ifelse(r_bin == pull(bins(), r_bin), T, F))
    })

    # draw growth curve using observations and predicted ----

    y_max <- reactive(input$height)

    output$growth_curve <- renderPlot({
      p <- ggplot(data = filter(observations, CODE == growth_id()),
                  aes(x = DAYS, y = S1)) +
        geom_point(size = 4) +
        geom_line(data = filter(predicted, CODE == growth_id()),
                  aes(x = DAYS, y = S1), size = 2) +
        annotate("text", x = 0, y = Inf, label = paste0("Individual = ", growth_id(),
                                                        "\nAsym = ", round(bins()$Asym, digits = 2),
                                                        "\nTi = ", round(bins()$Ti, digits = 2),
                                                        "\nr = ", round(bins()$r, digits = 3),
                                                        "\nn = ", nrow(filter(observations, CODE == growth_id()))),
                 hjust = 0, vjust = 1) +
        theme_classic()
      if(input$ht_opt == "Fixed"){
        p + ylim(0, y_max())
      } else{
        p + ylim(0, NA)
      }

      # show location in distribution of parameters among block, population, family, individual
      # show total parameter value
      # count number of observations
    }, res = 96)

    # draw histograms using parameters_cut ----

    output$Asym_id <- renderPlot({

      ggplot(data = parameters_out(), aes(Asym, fill = Asym_col)) +
        stat_bin(binwidth = 12, boundary = 0, show.legend = FALSE) +
        theme_classic() +
        theme(aspect.ratio = 0.8)

    }, res = 96)

    output$Ti_id <- renderPlot({

      ggplot(data = parameters_out(), aes(Ti, fill = Ti_col)) +
        stat_bin(binwidth = 4, boundary = 0, show.legend = FALSE) +
        theme_classic() +
        theme(aspect.ratio = 0.8)

    }, res = 96)

    output$r_id <- renderPlot({

      ggplot(data = parameters_out(), aes(r, fill = r_col)) +
        stat_bin(binwidth = 0.01, boundary = 0, show.legend = FALSE) +
        theme_classic() +
        theme(aspect.ratio = 0.8)

    }, res = 96)

  }

  # Run the application
  shinyApp(ui, server)

}
