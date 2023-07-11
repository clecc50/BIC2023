# Load R packages
library("shiny")
library("shinythemes")
library("flowCore")
library("flowAI")
library("ggcyto")
library("flowWorkspace")
library("flowWorkspaceData")
library("flowCore")
library("flowViz")
library("ggcyto")
library("openCyto")
library("CytoML")

# Set the maximum upload size (in bytes)
max_upload_size <- 100 * 1024^2

# Increase the maximum upload size
options(shiny.maxRequestSize = max_upload_size)

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  "My first app",
                  tabPanel("Navbar 1",
                           sidebarPanel(
                             tags$h3("Input:"),
                             fileInput("file", "Upload .fcs file")
                           ),
                           mainPanel(
                             h1("Header 1"),
                             h4("Output 1"),
                             verbatimTextOutput("txtout"),
                             uiOutput("param_selection"),
                             plotOutput("scatter_plot")
                           )
                  ),
                  tabPanel("Navbar 2", "This panel is intentionally left blank"),
                  tabPanel("Navbar 3", "This panel is intentionally left blank")
                  
                )
)

# Define server function
server <- function(input, output, session) {
  
  output$txtout <- renderText({
    paste(input$file$name, "uploaded.")
  })
  
  observeEvent(input$file, {
    file <- input$file$datapath
    fcs_data <- flowCore::read.FCS(file)
    param_names <- colnames(fcs_data)
    
    output$param_selection <- renderUI({
      fluidRow(
        column(
          width = 6,
          selectInput(
            "x_var",
            "X-Axis Parameter",
            choices = param_names
          )
        ),
        column(
          width = 6,
          selectInput(
            "y_var",
            "Y-Axis Parameter",
            choices = param_names
          )
        )
      )
    })
    
    # Compensation
    spillover_data <- spillover(fcs_data)
    fcs_data_flowSet <- flowSet(fcs_data)
    fcs_data_comp <- compensate(fcs_data_flowSet, spillover_data$`$SPILLOVER`)
    
    # Cleaning
    fcs_data_clean <- flow_auto_qc(fcs_data_comp)
    
    # Transformation
    logicleGml2_trans2 <- function (x, selected_cols, T = 16777215, M = 4.5, W = 0.1, A = 0, n = 6, equal.space = FALSE, DR = 1024) {
      T = eval(T)
      M = eval(M)
      W = eval(W)
      A = eval(A)
      
      trans <- function(x) {
        flowCore:::logicle_transform(as.double(x), as.double(T), as.double(W), as.double(M), as.double(A), FALSE)/M * DR
      }
      
      inv <- function(x) {
        flowCore:::logicle_transform(as.double(x) * M / DR, as.double(T), as.double(W), as.double(M), as.double(A), TRUE)
      }
      
      flow_trans(name = "logicleGml2", trans.fun = trans, inverse.fun = inv, n = n, equal.space = equal.space)
    }
    
    selected_cols <- parameters(fcs_data_clean)
    trans2 <- logicleGml2_trans2(fcs_data_clean, selected_cols)
    fcs_data_trans <- transform.data.frame(fcs_data_clean, trans2)
    
    # Store transformed data in a reactive value for later use
    reactive_data <- reactiveValues(data = fcs_data_trans)
    
    # Render plot with transformed data
    output$scatter_plot <- renderPlot({
      req(input$x_var)
      req(input$y_var)
      
      x_var <- input$x_var
      y_var <- input$y_var
      
      ggcyto::autoplot(reactive_data$data, x = x_var, y = y_var, bins = 456) +
        scale_x_logicle() +
        scale_y_logicle()
    })
    
  })
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)
