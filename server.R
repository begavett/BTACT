# The MIT License (MIT)
#
# Copyright (c) 2014 Brandon Gavett

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

library(car)
library(lavaan)

# Define server logic required to generate factor scores
shinyServer(function(input, output, session) {
  FScores <- reactive({
    if (input$calc == 0)
      return()
    isolate({
      model.data <- readRDS("BTACT_bifactor.rds")
      midus.data <- readRDS("MIDUS.rds")
      new.data <- data.frame(m2id = 1,
                             b_digit = recode(input$b_digit,
                                              "0 = 0;
                                              1 = 0;
                                              2 = 1;
                                              3 = 2;
                                              4 = 3;
                                              5 = 4;
                                              6 = 5;
                                              7 = 6;
                                              8 = 7"),
                             ns_1 = input$ns_1,
                             ns_2 = input$ns_2, ns_3 = input$ns_3, ns_4 = input$ns_4,
                             ns_5 = input$ns_5, wli_early = input$wli_early,
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
                                           "0:11 = 0;
                                           12:14 = 1;
                                           15:16 = 2;
                                           17 = 3;
                                           18:19 = 4;
                                           20 = 5;
                                           21:22 = 6;
                                           23:24 = 7;
                                           25:27 = 8;
                                           28:100 = 9"),
                             pbc_score = recode(input$pbc_score,
                                                "0:24 = 0;
                                                25:28 = 1;
                                                29:31 = 2;
                                                32:34 = 3;
                                                35:37 = 4;
                                                38:40 = 5;
                                                41:43 = 6;
                                                44:46 = 7;
                                                47:52 = 8;
                                                53:100 = 9"))
      all.data <- midus.data
      all.data[1,] <- new.data
      f.scores <- data.frame(predict(model.data, newdata = all.data))
      names(f.scores) <- c("Unadjusted","NS","RG")
      f.scores <- f.scores[1,]
      f.scores <- f.scores[-2]
      f.scores <- f.scores[-2]
      norm.data <- readRDS("BTACT_norms.rds")
      demog.dataA <- data.frame(bifactor.z = f.scores$Unadjusted,
                                B1PAGE_M2 = input$age)
      demog.dataE <- data.frame(bifactor.z = f.scores$Unadjusted,
                                Edu = input$edu)
      demog.dataG <- data.frame(bifactor.z = f.scores$Unadjusted,
                                B1PRSEX = input$gender)
      demog.dataO <- data.frame(bifactor.z = f.scores$Unadjusted,
                                CurrentPastOcc = input$occ)
      demog.dataAE <- data.frame(bifactor.z = f.scores$Unadjusted,
                                 B1PAGE_M2 = input$age,
                                 Edu = input$edu)
      demog.dataAG <- data.frame(bifactor.z = f.scores$Unadjusted,
                                 B1PAGE_M2 = input$age,
                                 B1PRSEX = input$gender)
      demog.dataAO <- data.frame(bifactor.z = f.scores$Unadjusted,
                                 B1PAGE_M2 = input$age,
                                 CurrentPastOcc = input$occ)
      demog.dataEO <- data.frame(bifactor.z = f.scores$Unadjusted,
                                 Edu = input$edu,
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
      normed.scores$E <- (demog.dataE$bifactor.z - predict(norm.data$E, newdata = demog.dataE))/summary(norm.data$E)$sigma
      normed.scores$G <- (demog.dataG$bifactor.z - predict(norm.data$G, newdata = demog.dataG))/summary(norm.data$G)$sigma
      normed.scores$O <- (demog.dataO$bifactor.z - predict(norm.data$O, newdata = demog.dataO))/summary(norm.data$O)$sigma
      normed.scores$AE <- (demog.dataAE$bifactor.z - predict(norm.data$AE, newdata = demog.dataAE))/summary(norm.data$AE)$sigma
      normed.scores$AG <- (demog.dataAG$bifactor.z - predict(norm.data$AG, newdata = demog.dataAG))/summary(norm.data$AG)$sigma
      normed.scores$AO <- (demog.dataAO$bifactor.z - predict(norm.data$AO, newdata = demog.dataAO))/summary(norm.data$AO)$sigma
      normed.scores$EO <- (demog.dataEO$bifactor.z - predict(norm.data$EO, newdata = demog.dataEO))/summary(norm.data$EO)$sigma
      normed.scores$AEG <- (demog.dataAEG$bifactor.z - predict(norm.data$AEG, newdata = demog.dataAEG))/summary(norm.data$AEG)$sigma
      normed.scores$AEO <- (demog.dataAEO$bifactor.z - predict(norm.data$AEO, newdata = demog.dataAEO))/summary(norm.data$AEO)$sigma
      normed.scores$AGO <- (demog.dataAGO$bifactor.z - predict(norm.data$AGO, newdata = demog.dataAGO))/summary(norm.data$AGO)$sigma
      normed.scores$AEGO <- (demog.dataAEGO$bifactor.z - predict(norm.data$AEGO, newdata = demog.dataAEGO))/summary(norm.data$AEGO)$sigma
      f.scores$none <- demog.dataA$bifactor.z
      f.scores$A <- normed.scores$A
      f.scores$E <- normed.scores$E
      f.scores$G <- normed.scores$G
      f.scores$O <- normed.scores$O
      f.scores$AE <- normed.scores$AE
      f.scores$AG <- normed.scores$AG
      f.scores$AO <- normed.scores$AO
      f.scores$EO <- normed.scores$EO
      f.scores$AEG <- normed.scores$AEG
      f.scores$AEO <- normed.scores$AEO
      f.scores$AGO <- normed.scores$AGO
      f.scores$AEGO <- normed.scores$AEGO
      rownames(f.scores) <- c("z-score")
      return(data.frame(round(f.scores,3)))
    })
  })
  output$FScoresN <- renderTable({
    if (input$calc == 0)
      return()
    isolate({FScores()[1]})
  })
  output$FScoresA <- renderTable({
    if (input$calc == 0)
      return()
    isolate({FScores()[4]})
  })
  output$FScoresE <- renderTable({
    if (input$calc == 0)
      return()
    isolate({FScores()[5]})
  })
  output$FScoresG <- renderTable({
    if (input$calc == 0)
      return()
    isolate({FScores()[6]})
  })
  output$FScoresO <- renderTable({
    if (input$calc == 0)
      return()
    isolate({FScores()[7]})
  })
  output$FScoresAE <- renderTable({
    if (input$calc == 0)
      return()
    isolate({FScores()[8]})
  })
  output$FScoresAG <- renderTable({
    if (input$calc == 0)
      return()
    isolate({FScores()[9]})
  })
  output$FScoresAO <- renderTable({
    if (input$calc == 0)
      return()
    isolate({FScores()[10]})
  })
  output$FScoresEO <- renderTable({
    if (input$calc == 0)
      return()
    isolate({FScores()[11]})
  })
  output$FScoresAEG <- renderTable({
    if (input$calc == 0)
      return()
    isolate({FScores()[12]})
  })
  output$FScoresAEO <- renderTable({
    if (input$calc == 0)
      return()
    isolate({FScores()[13]})
  })
  output$FScoresAGO <- renderTable({
    if (input$calc == 0)
      return()
    isolate({FScores()[14]})
  })
  output$FScoresAEGO <- renderTable({
    if (input$calc == 0)
      return()
    isolate({FScores()[15]})
  })
})
