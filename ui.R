library(shiny)

# Define UI for application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("IPPT Score Calculator"),
  
  # Sidebar with input
  sidebarPanel(
    checkboxInput("elite", "Commando, Guardsman or Diver?", value = FALSE),
    sliderInput("age", "Age:", min = 16, max = 60, value = 29, step = 1),
    textInput("pushups", "Pushups:", value = "60"),
    textInput("situps", "Situps:", value = "60"),
    textInput("run", "2.4km (mm:ss):", value = "8:30")
  ),
  
  # Show the calculated result
  mainPanel(
    wellPanel(
      h4('Your Details:'),
      textOutput("elite"),
      textOutput("age"),
      textOutput("pushups"),
      textOutput("situps"),
      textOutput("run")
    ),
    wellPanel(
      h4('Results:'),
      textOutput("ageGroup"),
      textOutput("pushUpsScore"),
      textOutput("sitUpsScore"),
      textOutput("runningScore"),
      textOutput("totalScore"),
      textOutput("overall")
    ),
    helpText("This is based on the new IPPT Format and Scoring System for all National Servicemen in the Singapore Armed Forces (SAF). Enter your age, the number of pushups you can currently do, the number of situps you can currently do, and the amount of time it takes for you to complete a 2.4km run to see what you'll achieve using the new IPPT system. Reference: http://www.mindef.gov.sg/imindef/press_room/official_releases/nr/2015/feb/27feb15_nr.html")
  )
))