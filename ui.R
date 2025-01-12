

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Compute rhythm metrics from Praat TextGrids"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 3,
      
      # Input: Select a file ----
      fileInput("file1", "Upload or drag your TextGrids.",
                multiple = TRUE, accept = ".TextGrid"),
      p("I have tried to make the app quite robust but if the uploading fails, you can try to convert your TextGrids to UTF-16. And check that all tiers have names"),
      
      # Horizontal line ----
      tags$hr(),
      
      fileInput("groups", "(Optional) Upload a xlsx with the file-group equivalence ",
                multiple = FALSE, accept = ".xlsx"),
      p("This will be a Excel file, with two columns, the first named file and the second with te group for example sp-maria.TextGrid and Spanish in the second column."),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: select tier where segment annotation is
      numericInput("tier", h4("N tier by-sound annotation"),1, 1, 100, 1),
      
      p("How have you annotated your TextGrids? Phonetic transcription? Maybe orthographic? Or you have just labelled whether sounds were consonants or vowels (CV)"),
      
      # Input: select annotation type
      selectInput("annotation", h4("Annotation type"), 
                  choices = list("Phonetic-IPA" = 1,"Orthographic" = 2, "C and V" = 3,
                                 "PTK" = 4,"Phonetic-SAMPA" = 5), selected = 1),
      tags$hr(),
      h4("Download data in .csv"),
      # Add a download button
      downloadButton(outputId = "downloadData", label = "Download Table"),
      tags$hr(),
      
      downloadButton(outputId = "downloadDurations", label = "Download all Durations"),
      
      
      
      tags$hr(),
      
      
      checkboxInput("outliers", "Remove outliers", value = TRUE),
      
      tags$hr(),
      
      
      p("To download the graphs you can drag them to your Desktop.")
      # Input: Select number of rows to display ----
    #  checkboxGroupInput("disp", "Metrics",
     #              choices = c(SpeechRate = "SpeechRate", PercentageConsonants = "PercentageConsonants",
      #                         PercentageVowel = "PercentageVowel", VarcoV="Varco", VarcoC = "VarcoC"),
       #            selected = c("SpeechRate","PercentageVowel","PercentageConsonants")),
    # Input: Select number of rows to display ----

      # Input: Select number of rows to display ----
     # checkboxGroupInput("disp", "Graphs",
      #                   choices = c(PerV = "PerV", Varcos = "Varcos",
       #                              Deltas = "Deltas"),
        #                 selected = c("PerV","Varcos","PercentageConsonants")),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      h4("Numeric Results"),
      tableOutput("contents"),
      h4("Graphs"),
      plotOutput("plot1"),
      plotOutput("plot8_mds"),
      plotOutput("plot7_pca"),
      
      plotOutput("plot6_dengrogram"),
      plotOutput("plot_varcos"),
      
      plotOutput("plot2"),
      plotOutput("plot3"),
      plotOutput("plot4"),
      plotOutput("plot5")
      
      
      
      
    )
    
  )
)
