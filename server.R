library(shiny)
library(reshape2)

#Import pushups scoring table
pushUpsRaw <- read.csv("./pushupsResultsTable.csv", header = TRUE, row.names = 1)
pushUpsRaw$reps <- rownames(pushUpsRaw)
pushUps <- melt(pushUpsRaw, na.rm= FALSE, id.vars="reps", value.name="score", variable.name="agegroup" )
pushUps$score[is.na(pushUps$score)] <- 25
pushUps$agegroup <- gsub("X", "", pushUps$agegroup)
pushUps$reps <- sapply(pushUps$reps, as.numeric)
pushUps$agegroup <- sapply(pushUps$agegroup, as.numeric)

#Import situps scoring table
sitUpsRaw <- read.csv("./situpsResultsTable.csv", header = TRUE, row.names = 1)
sitUpsRaw$reps <- rownames(sitUpsRaw)
sitUps <- melt(sitUpsRaw, na.rm= FALSE, id.vars="reps", value.name="score", variable.name="agegroup" )
sitUps$score[is.na(sitUps$score)] <- 25
sitUps$agegroup <- gsub("X", "", sitUps$agegroup)
sitUps$reps <- sapply(sitUps$reps, as.numeric)
sitUps$agegroup <- sapply(sitUps$agegroup, as.numeric)

#Function to convert time to seconds, for 2.4km run
timetoseconds <- function(time) {
  t <- strsplit(as.character(time), ":")[[1]]
  seconds <- as.numeric(t[1]) * 60 + as.numeric(t[2])
  return(seconds)
}

#Import 2.4km run scoring table
runningRaw <- read.csv("./runningResultsTable.csv", header = TRUE, row.names = 1)
runningRaw$time <- lapply(rownames(runningRaw), timetoseconds)
running <- melt(runningRaw, na.rm= FALSE, id.vars="time", value.name="score", variable.name="agegroup" )
running$score[is.na(running$score)] <- 50
running$agegroup <- gsub("X", "", running$agegroup)
running$time <- sapply(running$time, as.numeric)
running$agegroup <- sapply(running$agegroup, as.numeric)

goldScoreElite <- 90
goldScore <- 85
silverScore <- 75
passIncentiveScore <- 61
passScore <- 51

shinyServer(function(input, output) {
  output$elite <- renderText({paste("Commando, Guardsman or Divers?", input$elite)})
  output$age <- renderText({paste("Age: ", input$age)})
  output$pushups <- renderText({paste("Pushups: ", input$pushups)})
  output$situps <- renderText({paste("Situps: ", input$situps)})
  output$run <- renderText({paste("2.4km: ", input$run)})
  
  #Classify age into age groups
  ageGroupResult <- reactive({
    if (input$age < 22) {
      ageGroup <- 1
    }
    if (input$age < 25) {
      ageGroup <- 2
    }
    else if (input$age < 28) {
      ageGroup <- 3
    }
    else if (input$age < 31) {
      ageGroup <- 4
    }
    else if (input$age < 34) {
      ageGroup <- 5
    }
    else if (input$age < 37) {
      ageGroup <- 6
    }
    else if (input$age < 40) {
      ageGroup <- 7
    }
    else if (input$age < 43) {
      ageGroup <- 8
    }
    else if (input$age < 46) {
      ageGroup <- 9
    }
    else if (input$age < 49) {
      ageGroup <- 10
    }
    else if (input$age < 52) {
      ageGroup <- 11
    }
    else if (input$age < 55) {
      ageGroup <- 12
    }
    else if (input$age < 58) {
      ageGroup <- 13
    }
    else {
      ageGroup <- 14
    }
    return(ageGroup)
  })
  
  #Find the pushups score based on the score table
  pushUpsScore <- reactive({
    reps <- as.numeric(input$pushups)
    if (reps > 60) {
      reps <- 60
    }
    
    ageGroup <- ageGroupResult()
    
    return (pushUps[which(pushUps$agegroup == ageGroup & pushUps$reps == reps), ]$score)
  })
  
  #Find the situps score based on the score table
  sitUpsScore <- reactive({
    reps <- as.numeric(input$situps)
    if (reps > 60) {
      reps <- 60
    }
    
    ageGroup <- ageGroupResult()
    
    return (sitUps[which(sitUps$agegroup == ageGroup & sitUps$reps == reps), ]$score)
  })
  
  #Find the 2.4km score based on the score table
  runningScore <- reactive({
    runTime <- timetoseconds(input$run)
    ageGroup <- ageGroupResult()    
    running <- running[which(running$agegroup == ageGroup & running$time >= runTime), ]
    
    return (running[order(running$time),]$score[1])
  })
  
  #Add all the score
  totalScore <- reactive({    
    total <- pushUpsScore() + sitUpsScore() + runningScore()
    return (total)
  })
  
  #Provide some comments to inspire the user :)
  overall <- reactive( {
    if (pushUpsScore() == 0 | sitUpsScore() == 0 | runningScore() == 0) {
      return ("You fail the IPPT overall. A score of at least 1 is needed for each station.")
    }
    
    if (totalScore() >= goldScoreElite) {
      return("Congratulations! You achieved Gold for ($500) your IPPT.")
    }
    else if (totalScore() >= goldScore) {
      if (isTRUE(input$elite)) {
        diff <- 85-totalScore()
        point <- "points"
        if (diff == 1) {
          point <- "point"
        }
        return(paste("Congratulations! You achieved Silver ($300) for your IPPT.", "You are", diff, point, "from achieving Gold."))
      }
      else {
        return("Congratulations! You achieved Gold ($500) for your IPPT.")
      }
    }
    else if (totalScore() >= silverScore) {
      if (isTRUE(input$elite)) {
        diff <- goldScore-totalScore()
      }
      else {
        diff <- goldScoreElite-totalScore()
      }
      point <- "points"
      if (diff == 1) {
        point <- "point"
      }
      return(paste("Congratulations! You achieved Silver ($300) for your IPPT.", "You are", diff, point, "from achieving Gold."))
    }
    else if (totalScore() >= passIncentiveScore) {
      if (isTRUE(input$elite)) {
        diffGold <- goldScore-totalScore()
      }
      else {
        diffGold <- goldScoreElite-totalScore()
      }
      pointGold <- "points"
      if (diffGold == 1) {
        pointGold <- "point"
      }
      diffSilver <- silverScore-totalScore()
      pointSilver <- "points"
      if (diffSilver == 1) {
        pointSilver <- "point"
      }
      return(paste("Congratulations! You achieved Pass (with Incentive) ($200) for your IPPT.", "You are", diffGold, pointGold, "from achieving Gold and", diffSilver, pointSilver, "from achieving Silver."))
    }
    else if (totalScore() >= passScore) {
      if (isTRUE(input$elite)) {
        diffGold <- goldScore-totalScore()
      }
      else {
        diffGold <- goldScoreElite-totalScore()
      }
      pointGold <- "points"
      if (diffGold == 1) {
        pointGold <- "point"
      }
      diffSilver <- silverScore-totalScore()
      pointSilver <- "points"
      if (diffSilver == 1) {
        pointSilver <- "point"
      }
      diffPassIncentive <- passIncentiveScore-totalScore()
      pointPassIncentive <- "points"
      if (diffPassIncentive == 1) {
        pointPassIncentive <- "point"
      }
      return(paste("Congratulations! You achieved Pass for your IPPT.", "You are", diffGold, pointGold, "from achieving Gold,", diffSilver, pointSilver, "from achieving Silver, and", diffPassIncentive, pointPassIncentive, "from achieving Pass (with Incentive)."))
    }
  })
  
  output$ageGroup <- renderText({paste("Age Group:", ageGroupResult())})
  output$pushUpsScore <- renderText({paste("Pushups Score:", pushUpsScore())})
  output$sitUpsScore <- renderText({paste("Situps Score:", sitUpsScore())})
  output$runningScore <- renderText({paste("Running Score:", runningScore())})
  output$totalScore <- renderText({paste("Total Score:", totalScore())})
  output$overall <- renderText(overall())
})