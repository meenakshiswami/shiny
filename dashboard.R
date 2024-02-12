library(gtsummary)
library(shinydashboard)
library(survminer)
library(DT)
library(survival)
library(haven)
library(shiny)
library(gt)
library(tidyverse)
library(shinydashboardPlus)
library(dplyr)
library(plotly)
library(reshape2)
set.seed(35)
dat <-  data.frame(Subject = 1:15, 
                   Months = sample(5:20, 15, replace=TRUE),
                   Treated = sample(0:1, 15, replace=TRUE),
                   Stage = sample(1:4, 15, replace=TRUE),
                   Continued = sample(0:15, 15, replace=TRUE))
dat <-  dat %>%
  group_by(Subject) %>%
  mutate(
    Complete = sample(c(4:(max(Months)-1),NA), 1,
                  prob = c(rep(1, length(4:(max(Months)-1))),5), replace=TRUE),
    Partial = sample(c(4:(max(Months)-1),NA), 1,
                  prob = c(rep(1, length(4:(max(Months)-1))),5), replace=TRUE),
    Durable = sample(c(-0.5,NA), 1, replace=TRUE)
  )
dat$Subject <- factor(dat$Subject, levels=dat$Subject[order(dat$Months)])
dat.m <- melt(dat %>% select(Subject, Months, Complete, Partial, Durable),
              id.var=c("Subject","Months"), na.rm = TRUE)
adrs <- data.frame(
  id = c(1:56),
  type = sample(rep(c("laMCC", "metMCC"), times = 28)),
  response = c(30,sort(runif(n=53, min = -10,max =19),decreasing=TRUE),-25,-31),
  dose = sample(rep(c(80, 150), 28))
)
#  assign Best Overall Response (BOR)
adrs$BOR = c("PD", rep(c("SD"), times = 54), "PR")
# Specify the path to  XPT file# Read the XPT file into R
xpt_file_path <- "C:/Users/Devendra/Downloads/adamdata/xpt/adtte.xpt"
adtte1 <- read_xpt(xpt_file_path)
adae1 <- read_xpt("C:/Users/Devendra/Downloads/adamdata/xpt/adae.xpt")
adlbsi1 <- read_xpt("C:/Users/Devendra/Downloads/adamdata/xpt/adlbsi.xpt")
lbshift <- read_xpt("C:/Users/Devendra/Downloads/labshift.xpt")

# UI
# UI
ui <- dashboardPage(
  # Step 1: Header
  dashboardHeader(
    title = "My Shiny Dashboard",
    tags$li(
      class = "dropdown",
      tags$div(
        class = "dropdown-menu",
        id = "custom-header",
        "This is a custom header"
      )
    )
  ),
  # Step 2: Sidebar
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Safety", tabName = "page1", icon = icon("chart-bar")),
      menuItem("Efficacy", tabName = "page2", icon = icon("line-chart")),
      menuItem("Table", tabName = "page3", icon = icon("table"))
    )
  ),
  
  # Step 3: Body
  dashboardBody(
    # Step 3.1: Tab Items
    tabItems(
      # Page 1
      tabItem(tabName = "page1", width = NULL, column(3,
                                                      sliderInput(
                                                        "slider",
                                                        "Select Five Point AE (1=MILD, 5= SERIOUS)",
                                                        min = 1,
                                                        max = 5,
                                                        value = c(3, 4)
                                                      ),
                                                      selectInput(
                                                        "soc",
                                                        "Select soc",
                                                        choices = adae1$AEBODSYS,
                                                        multiple = TRUE,
                                                        selectize = TRUE,
                                                        selected = "ALL"
                                                      ),
                                                      selectInput(
                                                        "pt",
                                                        "Select pt",
                                                        choices = adae1$AEDECOD,
                                                        multiple = TRUE,
                                                        selectize = TRUE
                                                      ),
                                                      selectInput(
                                                        "teae",
                                                        "Select flag",
                                                        choices = adae1$TRTEM,
                                                        multiple = TRUE,
                                                        selectize = TRUE
                                                      ),
                                                      checkboxInput(
                                                        inputId = "unq",
                                                        label = "unique",
                                                        value = TRUE
                                                      ),
                                                      selectInput(
                                                        "trt",
                                                        "Select trt",
                                                        choices = adae1$TRT01A,
                                                        multiple = TRUE,
                                                        selectize = TRUE
                                                      )
      ),
      column(9,
             tabBox(
               width = NULL, title = "AE",
               tabPanel(title = "Tab 1",
                        box(
                          plotlyOutput("BARPLOT"),
                          width = NULL, height = "auto"
                        ),
                        box(
                          title = "tt",
                          tableOutput("tremtable"),
                          width = NULL
                        )
               ),
               tabPanel(
                 title = "Tab 2",
                 selectInput(
                   "selectparamcd",
                   "Select parameter",
                   choices = unique(adlbsi1$PARAMCD), multiple = TRUE
                 ),
                 plotlyOutput("barplot2")
               )
             )
      )
      ),
      
      # Page 2
      tabItem(tabName = "page2",
              fluidRow(
                tabBox(width = NULL,
                       tabPanel(title = "km ",
                                radioButtons("radio", "Select REMISSION", choices = c(2, 1)),
                                selectInput(
                                  "selectRace",
                                  "Select RACE",
                                  choices = unique(adtte1$RACE), multiple = TRUE
                                ),
                                plotOutput("kmplot")
                       ),
                       tabPanel(width = NULL,
                                title = "waterfall ",
                                plotlyOutput("waterfallPlot")
                       ),
                       tabPanel(width = NULL,
                                title = "spider ",
                                mainPanel(plotlyOutput("spiderplot"))
                       ),
                       tabPanel(width = NULL,
                                title = "swimmer ",
                                plotlyOutput("swimmerPlot")
                       )
                )
              )
      ),
      
      # Page 3
      tabItem(tabName = "page3",
              fluidRow(
                tabBox(width = NULL,
                       tabPanel(title = "ADTTE",
                                selectInput(
                                  "selectInput",
                                  "Select PARAMETER",
                                  choices = c("TTOS", "TTPFS", "TTPFS125"),
                                  multiple = TRUE,
                                  selectize = TRUE
                                ),
                                DTOutput("dataTable")
                       ),
                       tabPanel(
                         title = "SUBJECT LEVEL DATASTRUCTURE",
                         column(4,
                                fileInput("file", label = h3("File input")),
                                DTOutput("uploadedTable")
                         )
                       ),
                       tabPanel(
                         title = "BASIC DATA STRUCTURE",
                         column(4,
                                selectInput("dataset", "Choose Dataset", choices = c("adrs", "advs", "adlbsi1"))
                         ),
                         mainPanel(
                           tableOutput("summary_table")
                         )
                       )
                )
              )
      )
    ),
    
    # Additional CSS
    tags$style(HTML("
    #sidebar {
      background-color: #f0f0f0;
    }
  ")),
    
    # Additional JavaScript
    tags$script(HTML("
    $(document).ready(function() {
      $('#sidebar').click(function() {
        alert('Sidebar clicked!');
      });
    });
  "))
  )
)

# Server
server <- function(input, output) {
#page 1# Render the bar plot for tab 1
  ae_filter <- reactive({
    adae1_filtered <- adae1 %>%
      filter(AETOXGRN %in% seq(input$slider[1], input$slider[2])) %>%
      filter(if (!is.null(input$SOC)) AEBODSYS %in% input$SOC else TRUE)  %>% 
      filter(if (!is.null(input$pt)) AEDECOD %in% input$pt else TRUE)
  })
  
  ae <- reactive({
    if (input$unq) {
      ae_filter() %>% select(USUBJID, TRT01A, TRTEM, AEBODSYS) %>% group_by(TRT01A, USUBJID, TRTEM) %>% slice(1)
    } else {
      ae_filter() %>% select(USUBJID, TRT01A, TRTEM, AEBODSYS) %>% group_by(TRT01A, USUBJID, TRTEM)
    }
  })
  
  output$BARPLOT <- renderPlotly({
    ggplot(ae(), aes(y = AEBODSYS, fill = TRTEM)) +
      geom_bar() +
      labs(y = "system organ class", x = "count")+coord_cartesian(xlim=c(0,65))
  })
  output$tremtable <- renderDataTable({ adae1 %>% filter(TRT01A=input$trt %>% distinct(TRTEM))})
    # Render another plot for tab 2
  lb_filter <- reactive({
    adlbsi1 %>% filter(PARAMCD == input$selectparamcd)
  })
  output$barplot2 <- renderPlotly({
    ggplot(lb_filter(), aes(x = ATOXGR, Y = BTOXGR)) +
      geom_bar() +
      labs(x = "ANALYSIS TOXICITY TRTP", y = "BASELINE TRTP ") +
      coord_cartesian(ylim = c(1, 4), xlim = c(0, 4))
  })
#page 2 km
  trt_data <- reactive({
    adtte1 %>% filter(REMISSN == input$radio) %>% 
      filter(if (!is.null(input$selectRace)) RACE == input$selectRace else TRUE)
  })
  output$kmplot <- renderPlot({
    data_subset <- trt_data()
    
    if (nrow(data_subset) > 0) {
      ggsurvplot(
        survfit(Surv(AVAL, CNSR) ~ TRTP, data = data_subset),
        data = data_subset,
        risk.table = TRUE,
        risk.table.col = "strata",
        title = "KM plot for PFS by Treatment Arm in 1nd Remission",
        xlab = "Time to Progression (month)",
        ylab = "Progression-Free Rate",
        risk.table.title = "Number at Risk",
        pval = TRUE
      )
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), main = "No data available")
    }
  })
  # bar plot for waterfall
  adrs1 <- reactive({
    adrs %>%
      filter(response >-100)
  })
  output$waterfallPlot <- renderPlot({
    dd <- adrs1()
    ggplot(dd, aes(x = id, y = response, fill = factor(ifelse(response >= 0, "red", "green"), levels = c("red", "green")))) +
      geom_bar(stat = "identity") +
      labs(title = "Bar Plot",
           y = "Response",
           fill = "Color") +
      theme_minimal() +
      coord_cartesian(ylim = c(-40, 40))
  })
  
  # bar plot for swimmerplot
  swimmer_plot <- ggplot(dat, aes(Subject, Months)) +
    geom_bar(stat="identity", aes(fill=factor(Stage))) +
    geom_point(data=dat.m, 
               aes(Subject, value, colour=variable, shape=variable), size=5) +
    geom_segment(data=dat %>% filter(Continued==1), 
                 aes(x=Subject, xend=Subject, y=Months + 0.1, yend=Months + 1), 
                 arrow=arrow(type="closed", length=unit(0.1,"in"))) +
    coord_flip() +
    scale_y_continuous(limits=c(-1,20), breaks=0:20) +
    labs(fill="Disease Stage", 
         x="Subjects on Treatment Drug A") +
    theme_bw() 
  
  output$swimmerPlot <- renderPlotly({
    print(swimmer_plot)
  })
  #page3
  # data table code for ADTTE
  output$dataTable <- renderDT({
    datatable(filter(adtte1, PARAMCD %in% input$selectInput))
  }) # Data table code for Uploaded Data SUBJECT LEVEL STRUCTURE
  output$uploadedTable <- renderDT({
    uploaded_data <- read.csv(input$file$datapath)
    datatable(uploaded_data)  })
#summary table  BASIC DATA STRUCTURE
  PARAMCD_stats <- reactive({
    input$dataset %>%
      group_by(PARAMCD, TRTP) %>%
      summarize(
        Average = mean(Marks),
        Median = median(Marks),
        Min = min(Marks),
        Max = max(Marks)
      ) %>%
      pivot_longer(cols = -c(TRTP, PARAMCD)) %>%
      pivot_wider(names_from = TRTP, values_from = value)
  })
  
  # Render the summary table
  output$summary_table <- renderTable({
    PARAMCD_stats()
  })
  
}

shinyApp(ui, server)