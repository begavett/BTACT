# The MIT License (MIT)
#
# Copyright (c) 2014-2015 Brandon Gavett
#
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

shinyUI(fluidPage(
  title = "BTACT Global Factor Score and Norms Calculator",
  fluidRow(
    column(8,
           h2("BTACT Global Factor Score and Norms Calculator"),
           HTML('If this app is valuable to you, please consider making a small donation to help support hosting fees.
                <a href = "https://www.paypal.me/begavett/"><img src = "https://www.paypalobjects.com/webstatic/en_US/i/btn/png/btn_donate_74x21.png" /></a>')
           ),
    column(4,
           img(src = "http://www.uccs.edu/Images/brand/uccs-logo.png", width=400, height=58))),
  fluidRow(HTML("Based on Gavett, B. E., Crane, P. K., & Dams-O'Connor, K. (2013). Bi-factor analyses of the Brief Test of Adult Cognition by Telephone. <i>NeuroRehabilitation</i>, <i>32</i>, 253-365. <a href = 'http://www.uccs.edu/Documents/bgavett/BTACT_2013.pdf'><img src = 'http://www.adobe.com/images/pdficon_small.png'></a>")),
  fluidRow(HTML("and on Gurnani, A. S., John, S. E., & Gavett, B. E. (in press). Regression-based norms for a bi-factor model for scoring the Brief Test of Adult Cognition by Telephone (BTACT). <i>Archives of Clinical Neuropsychology</i>. <a href = 'http://doi.org/10.1093/arclin/acv005'><img src = 'http://www.adobe.com/images/pdficon_small.png'></a>")),
  fluidRow(HTML("Occupation coding based on the <a href = 'http://www.bls.gov/nls/quex/r1/y97r1cbka1.pdf'>1990 Census Industrial & Occupational Classification Codes</a>")),
  tags$hr(),
  fluidRow(
    column(3,
           h4("BTACT Raw Scores"),
           numericInput("b_digit", "Backward Digit Span (0-8):",0,min=0,max=8),
           numericInput("ns_1", "Number Series Trial 1 (0-1):",0,min=0,max=1),
           numericInput("ns_2", "Number Series Trial 2 (0-1):",0,min=0,max=1),
           numericInput("ns_3", "Number Series Trial 3 (0-1):",0,min=0,max=1),
           numericInput("ns_4", "Number Series Trial 4 (0-1):",0,min=0,max=1),
           numericInput("ns_5", "Number Series Trial 5 (0-1):",0,min=0,max=1),
           numericInput("wli_early", "Immediate RAVLT Recall Words 1-5 (0-5):",0,min=0,max=5),
           numericInput("wli_mid", "Immediate RAVLT Recall Words 6-10 (0-5):",0,min=0,max=5),
           numericInput("wli_late", "Immediate RAVLT Recall Words 11-15 (0-5):",0,min=0,max=5),
           numericInput("wld_early", "Delayed RAVLT Recall Words 1-5 (0-5):",0,min=0,max=5),
           numericInput("wld_mid", "Delayed RAVLT Recall Words 6-10 (0-5):",0,min=0,max=5),
           numericInput("wld_late", "Delayed RAVLT Recall Words 11-15 (0-5):",0,min=0,max=5),
           numericInput("rg_norm_switch", "Red-Green Test Normal Switching Condition (0-3):",0,min=0,max=3),
           numericInput("rg_rev_switch", "Red-Green Test Reverse Switching Condition (0-3):",0,min=0,max=3),
           numericInput("prg_rev_other", "Red-Green Test Reverse Other Condition (0-11):",0,min=0,max=11),
           numericInput("prg_norm_other", "Red-Green Test Normal Other Condition (0-15):",0,min=0,max=15),
           numericInput("pcat", "Category Fluency (0-50):",0,min=0,max=50),
           numericInput("pbc_score", "Backwards Counting (0-100):",0,min=0,max=100)),
    column(3,
           h4("Demographic Variables"),
           numericInput("age", "Age (32-84):",32,min=32,max=84),
           radioButtons("edu", "Education:",
                        c("0-8 Years" = "Junior",
                          "9-12 Years" = "SomeHigh",
                          "GED" = "GED",
                          "High School Graduate" = "High",
                          "1-2 Years of College, no Degreee" = "SomeCol1-2",
                          "3+ Years of College, no Degree" = "SomeCol3+",
                          "Graduated from 2 Year College, Vocational School, or Associate's Degree" = "Assoc",
                          "Graduated from 4 or 5 Year College, or Bachelor's Degree" = "BA",
                          "Some Graduate School" = "SomeGrad",
                          "Master's Degree" = "MA",
                          "Ph.D., Ed.D, MD, DDS, LLB, LLD, JD, or other Professional Degree" = "PhD")),
           radioButtons("gender", "Gender:",
                        c("Female" = "Female",
                          "Male" = "Male")),
           radioButtons("occ", "Occupation:",
                        c("Operator, Laborer, and Military" = "Labor",
                          "Executive, Administrative, and Managerial" = "Exec",
                          "Professional Specialty" = "Prof",
                          "Technician and Related Support" = "Tech",
                          "Sales Occupation" = "Sales",
                          "Administrative Support, Including Clerical" = "Admin",
                          "Service Occupation" = "Service",
                          "Farming, Forestry, and Fishing" = "FFF",
                          "Precision Production, Crafts, and Repair" = "Repair"
                        ))
    ),
    column(1,
           br(),br(),br(),br(),br(),br(),br(),actionButton("calc", "Calculate")),
    column(5,
           tabsetPanel(
             tabPanel("Unadjusted",
                      h4("BTACT Global Factor z-score (M = 0, SD = 1)"),
                      tableOutput("FScoresN")),
             tabPanel("Age",
                      h4("BTACT Global Factor z-score (M = 0, SD = 1)"),
                      tableOutput("FScoresA")),
             tabPanel("Edu",
                      h4("BTACT Global Factor z-score (M = 0, SD = 1)"),
                      tableOutput("FScoresE")),
             tabPanel("Gender",
                      h4("BTACT Global Factor z-score (M = 0, SD = 1)"),
                      tableOutput("FScoresG")),
             tabPanel("Occupation",
                      h4("BTACT Global Factor z-score (M = 0, SD = 1)"),
                      tableOutput("FScoresO")),
             tabPanel("A + E",
                      h4("BTACT Global Factor z-score (M = 0, SD = 1)"),
                      tableOutput("FScoresAE")),
             tabPanel("A + G",
                      h4("BTACT Global Factor z-score (M = 0, SD = 1)"),
                      tableOutput("FScoresAG")),
             tabPanel("A + O",
                      h4("BTACT Global Factor z-score (M = 0, SD = 1)"),
                      tableOutput("FScoresAO")),
             tabPanel("E + O",
                      h4("BTACT Global Factor z-score (M = 0, SD = 1)"),
                      tableOutput("FScoresEO")),
             tabPanel("A + E + G",
                      h4("BTACT Global Factor z-score (M = 0, SD = 1)"),
                      tableOutput("FScoresAEG")),
             tabPanel("A + E + O",
                      h4("BTACT Global Factor z-score (M = 0, SD = 1)"),
                      tableOutput("FScoresAEO")),
             tabPanel("A + G + O",
                      h4("BTACT Global Factor z-score (M = 0, SD = 1)"),
                      tableOutput("FScoresAGO")),
             tabPanel("A + E + G + O",
                      h4("BTACT Global Factor z-score (M = 0, SD = 1)"),
                      tableOutput("FScoresAEGO"))
           )
    )
  )))
