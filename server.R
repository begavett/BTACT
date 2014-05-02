library(shiny)
library(lavaan)
library(car)

# Define server logic required to generate factor scores
shinyServer(function(input, output) {
  FScores <- reactive({
    model.data <- readRDS("BTACT_bifactor.rds")
    new.data <- data.frame(b_digit = input$b_digit, ns_1 = input$ns_1+1, 
                           ns_2 = input$ns_2+1, ns_3 = input$ns_3+1, ns_4 = input$ns_4+1, 
                           ns_5 = input$ns_5+1, wli_early = input$wli_early, 
                           wli_mid = input$wli_mid, wli_late = input$wli_late, 
                           wld_early = input$wld_early, wld_mid = input$wld_mid, 
                           wld_late = input$wld_late, rg_norm_switch = input$rg_norm_switch, 
                           rg_rev_switch = input$rg_rev_switch, 
                           prg_rev_other = recode(input$prg_rev_other, 
                                                  "0 = 0; 
                                                  1:2 = 1; 
                                                  3 = 2; 
                                                  4:5 = 3; 
                                                  6 = 4; 
                                                  7 = 5; 
                                                  8 = 6; 
                                                  9 = 7; 
                                                  10 = 8;
                                                  11 = 9"), 
                           prg_norm_other = recode(input$prg_norm_other, 
                                                   "0:7 = 0; 
                                                   8 = 1; 
                                                   9 = 2;
                                                   10 = 3;
                                                   11 = 4;
                                                   12 = 5;
                                                   13 = 6;
                                                   14 = 7;
                                                   15 = 8"),
                           pcat = recode(input$pcat, 
                                         "0:11 = 1;
                                         12:14 = 2;
                                         15:16 = 3;
                                         17 = 4;
                                         18:19 = 5;
                                         20 = 6;
                                         21:22 = 7;
                                         23:24 = 8;
                                         25:27 = 9;
                                         28:100 = 10"), 
                           pbc_score = recode(input$pbc_score, 
                                              "0:24 = 1;
                                              25:28 = 2;
                                              29:31 = 3;
                                              32:34 = 4;
                                              35:37 = 5;
                                              38:40 = 6;
                                              41:43 = 7;
                                              44:46 = 8;
                                              47:52 = 9;
                                              53:100 = 10"))    
    fake.data <- data.frame(b_digit = sample(0:7,19,replace=T))
    fake.data$ns_1 <- sample(1:2,19,replace=T)
    fake.data$ns_2 <- sample(1:2,19,replace=T)
    fake.data$ns_3 <- sample(1:2,19,replace=T)
    fake.data$ns_4 <- sample(1:2,19,replace=T)
    fake.data$ns_5 <- sample(1:2,19,replace=T)
    fake.data$wli_early <- sample(0:5,19,replace=T)
    fake.data$wli_mid <- sample(0:5,19,replace=T)
    fake.data$wli_late <- sample(0:5,19,replace=T)
    fake.data$wld_early <- sample(0:5,19,replace=T)
    fake.data$wld_mid <- sample(0:5,19,replace=T)
    fake.data$wld_late <- sample(0:5,19,replace=T)
    fake.data$rg_norm_switch <- sample(0:3,19,replace=T)
    fake.data$rg_rev_switch <- sample(0:3,19,replace=T)
    fake.data$prg_rev_other <- sample(0:9,19,replace=T)
    fake.data$prg_norm_other <- sample(0:8,19,replace=T)
    fake.data$pcat <- sample(1:10,19,replace=T)
    fake.data$pbc_score <- sample(1:10,19,replace=T)
    f.scores <- data.frame(predict(model.data, newdata = rbind(new.data,fake.data)))
    names(f.scores) <- c("Unadjusted","NS","RG")
    f.scores <- f.scores[1,]
    f.scores <- f.scores[-2]
    f.scores <- f.scores[-2]
    norm.data <- readRDS("BTACT_norms.rds")
    demog.dataA <- data.frame(bifactor.z = f.scores$Unadjusted, 
                              B1PAGE_M2 = input$age)
    demog.dataAE <- data.frame(bifactor.z = f.scores$Unadjusted, 
                               B1PAGE_M2 = input$age, 
                               Edu = input$edu)
    demog.dataAG <- data.frame(bifactor.z = f.scores$Unadjusted, 
                               B1PAGE_M2 = input$age, 
                               B1PRSEX = input$gender)
    demog.dataAO <- data.frame(bifactor.z = f.scores$Unadjusted, 
                               B1PAGE_M2 = input$age, 
                               CurrentPastOcc = input$occ)
    demog.dataAEG <- data.frame(bifactor.z = f.scores$Unadjusted, 
                                B1PAGE_M2 = input$age, 
                                Edu = input$edu, 
                                B1PRSEX = input$gender)
    demog.dataAEO <- data.frame(bifactor.z = f.scores$Unadjusted, 
                                B1PAGE_M2 = input$age, 
                                Edu = input$edu,
                                CurrentPastOcc = input$occ)
    demog.dataAGO <- data.frame(bifactor.z = f.scores$Unadjusted, 
                                B1PAGE_M2 = input$age, 
                                B1PRSEX = input$gender,
                                CurrentPastOcc = input$occ)
    demog.dataAEGO <- data.frame(bifactor.z = f.scores$Unadjusted, 
                                 B1PAGE_M2 = input$age, 
                                 Edu = input$edu, 
                                 B1PRSEX = input$gender,
                                 CurrentPastOcc = input$occ)
    f.scores$Normed <- NA_integer_
    normed.scores <- data.frame(none=NA_integer_)
    normed.scores$A <- (demog.dataA$bifactor.z - predict(norm.data$A, newdata = demog.dataA))/summary(norm.data$A)$sigma
    normed.scores$AE <- (demog.dataAE$bifactor.z - predict(norm.data$AE, newdata = demog.dataAE))/summary(norm.data$AE)$sigma
    normed.scores$AG <- (demog.dataAG$bifactor.z - predict(norm.data$AG, newdata = demog.dataAG))/summary(norm.data$AG)$sigma
    normed.scores$AO <- (demog.dataAO$bifactor.z - predict(norm.data$AO, newdata = demog.dataAO))/summary(norm.data$AO)$sigma
    normed.scores$AEG <- (demog.dataAEG$bifactor.z - predict(norm.data$AEG, newdata = demog.dataAEG))/summary(norm.data$AEG)$sigma
    normed.scores$AEO <- (demog.dataAEO$bifactor.z - predict(norm.data$AEO, newdata = demog.dataAEO))/summary(norm.data$AEO)$sigma
    normed.scores$AGO <- (demog.dataAGO$bifactor.z - predict(norm.data$AGO, newdata = demog.dataAGO))/summary(norm.data$AGO)$sigma
    normed.scores$AEGO <- (demog.dataAEGO$bifactor.z - predict(norm.data$AEGO, newdata = demog.dataAEGO))/summary(norm.data$AEGO)$sigma
    f.scores$none <- demog.dataA$bifactor.z
    f.scores$A <- normed.scores$A
    f.scores$AE <- normed.scores$AE
    f.scores$AG <- normed.scores$AG
    f.scores$AO <- normed.scores$AO
    f.scores$AEG <- normed.scores$AEG
    f.scores$AEO <- normed.scores$AEO
    f.scores$AGO <- normed.scores$AGO
    f.scores$AEGO <- normed.scores$AEGO
    rownames(f.scores) <- c("z-score")
    return(data.frame(round(f.scores,3)))
  })
  output$FScoresN <- renderTable({FScores()[1]})
  output$FScoresA <- renderTable({FScores()[4]})
  output$FScoresAE <- renderTable({FScores()[5]})
  output$FScoresAG <- renderTable({FScores()[6]})
  output$FScoresAO <- renderTable({FScores()[7]})
  output$FScoresAEG <- renderTable({FScores()[8]})
  output$FScoresAEO <- renderTable({FScores()[9]})
  output$FScoresAGO <- renderTable({FScores()[10]})
  output$FScoresAEGO <- renderTable({FScores()[11]})
})