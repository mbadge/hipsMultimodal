fluidPage(
  sidebarLayout(
    sidebarPanel(
        uiOutput("imgIdUi")
    ),

    # Show a plot of the generated distribution
    mainPanel(
        verbatimTextOutput("noteTxt"),

        selectInput("isFx", "Fracture?", choices = c("Y", "N", "No mention")),
        selectInput("priorImaging", "Comparison Image?", choices = c("Y", "N", "No mention")),
        selectInput("cc", "Clinical Indication?", choices = c("No mention", "Pain", "Fall", "Other")),
        textInput("otherInd", label = "Other Indication"),
        actionButton(inputId = "submit_btn", label = "Submit Annotation")
    )
  )
)
