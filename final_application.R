# -----------------------------------------------
# Author: Gustav Söderström (kgdx754)
# Creation Date: 16AUG2023
#
# File Name: final_application
#
# Purpose: Create a shiny application for PHUSE
#          2023 Conference
#
# -----------------------------------------------

# Load packages ---
library(shiny) #Shiny framework
library(shinydashboard)
library(shinyWidgets)
library(tidyverse) 
library(plotly) #Interactive plots
library(haven) #Read SAS datasets
library(tools)
library(bslib) # Customize themes
library(shinythemes) #Differen themes for Shiny 
library(DT) #DataTable to show tables in better way
library(RColorBrewer) #Colors in R

# Read in CDISC pilot data
# Load SDTM datasets ---
sdtm_dm <- read_xpt("data/sdtm/dm.xpt") %>% 
  mutate(RFXSTDT = as.Date(RFXSTDTC),
         RFXENDT = as.Date(RFXENDTC))

sdtm_ae <- read_xpt("data/sdtm/ae.xpt") %>% 
  mutate(AESTDT = as.Date(AESTDTC),
         AEENDT = as.Date(AEENDTC), 
         USUBJID = as.factor(USUBJID))

sdtm_ae_dm <-sdtm_ae %>% 
  left_join(sdtm_dm %>% select(STUDYID,USUBJID,AGE,RACE,SEX,ACTARM,ARM,ETHNIC), 
            by = c("STUDYID", "USUBJID"))

sdtm_ae1 <- sdtm_ae %>% select(USUBJID, AESTDT, AEENDT, AEOUT, AESEQ, AESER) %>% 
  pivot_longer(cols = c(AESTDT, AEENDT), values_to = "AEDAT")

sdtm_ex <- read_xpt("data/sdtm/ex.xpt") %>% 
  mutate(EXSTDTC = as.Date(EXSTDTC),
         EXENDTC = as.Date(EXENDTC),
         USUBJID = as.factor(USUBJID))

sdtm_ds <- read_xpt("data/sdtm/ds.xpt")
sdtm_lb <- read_xpt("data/sdtm/lb.xpt")
sdtm_cm <- read_xpt("data/sdtm/cm.xpt") %>% 
  mutate(CMDT = as.Date(CMDTC))
sdtm_mh <- read_xpt("data/sdtm/mh.xpt")

sdtm_vs <- read_xpt("data/sdtm/vs.xpt")

sdtm_dm_vs <- sdtm_dm %>% select(STUDYID, USUBJID, SEX, AGE, RACE, ARM, ACTARM, ETHNIC) %>% 
  right_join(y = sdtm_vs, by = c("STUDYID","USUBJID")) %>% 
  pivot_wider(names_from = VSTESTCD, values_from = VSSTRESN) %>% 
  mutate(VISIT = as.factor(VISIT),
         RACE  = as.factor(RACE),
         ACTARM = as.factor(ACTARM),
         ARM = as.factor(ARM)) %>%
  filter(VISIT %in% c("SCREENING 1", "BASELINE","WEEK 2","WEEK 4","WEEK 6", "WEEK 8", "WEEK 12", "WEEK 16", "WEEK 20", 
                      "WEEK 24", "WEEK 26"))
sdtm_dm_vs[sdtm_dm_vs == ''] <- NA

sdtm_dm_vs <- sdtm_dm_vs %>% filter(case_when(VSTEST == "Diastolic Blood Pressure" ~ VSTPT == "AFTER STANDING FOR 1 MINUTE",
                                              VSTEST == "Pulse Rate" ~ VSTPT == "AFTER STANDING FOR 1 MINUTE",
                                              VSTEST == "Systolic Blood Pressure" ~ VSTPT == "AFTER STANDING FOR 1 MINUTE"))

sdtm_dm_vs$VISIT <- factor(sdtm_dm_vs$VISIT, levels=c("SCREENING 1", "BASELINE","WEEK 2","WEEK 4","WEEK 6", "WEEK 8", "WEEK 12", "WEEK 16", "WEEK 20", 
                                                      "WEEK 24", "WEEK 26"))

# Define UI for application that draws a histogram
ui <- fluidPage(useShinydashboard(),
  theme = bslib::bs_theme(bootswatch = "superhero"),
  navbarPage(
             title= "PHUSE EU 2023 DEMO",
              tabPanel("Individual timelines",
                       titlePanel("Individual timelines"),
                       sidebarLayout(
                         sidebarPanel(width = 2, selectInput("subjectID", h4("Unique Subject ID"), 
                                                             sdtm_dm$USUBJID, multiple = TRUE),
                                      h4("Layers"),
                                      checkboxInput("chk_ae", "Adverse Events", value =TRUE),
                                      checkboxInput("chk_dosing", "Dosing"),
                                      checkboxInput("chk_conmeds", "Concomitant Meds"),
                                      selectInput("prohib_cm", "CM filter", choices = c("ALL CMs", "Only prohibited CMs"))),
                         mainPanel(plotlyOutput("indiv_plot"))),
                       fluidRow(h4("SDTM Datasets"),
                       tabsetPanel(useShinydashboard(),
                                   tabPanel("DM",
                                              column(12,dataTableOutput("dm_table"))),
                                   tabPanel("AE",
                                              column(12,dataTableOutput("ae_table"))),
                                   tabPanel("CM",
                                                column(12,dataTableOutput("cm_table"))),
                                   tabPanel("EX",
                                                column(12,dataTableOutput("ex_table")))))),
              tabPanel("Grouped Demography",
                       titlePanel("Grouped Demography"),
                       sidebarLayout(
                         sidebarPanel(width = 2,
                                      selectInput("grp1", "Group By:", choices = c("ACTARM","ARM","ETHNIC","RACE","SEX"), selected = "SEX"),
                                      selectInput("var1", "Variable: ", choices = c("AGE", "DIABP", "PULSE","SYSBP"),selected = "AGE"),
                                      checkboxInput("chk_facet", "Separate groups")),
                         mainPanel(plotlyOutput("grpPlot1"))),
                       fluidRow(h4("Descriptive Statistics"),
                                    column(12, dataTableOutput("summary1")))),
              tabPanel("Adverse Events",
                       titlePanel("Adverse Events"),
                       sidebarLayout(
                         sidebarPanel(width = 2,
                                      selectInput("grp2", "Group By:", choices = c("ALL","ACTARM","ARM","ETHNIC","RACE","SEX")),
                                      selectInput("filt4", "Filter:", choices = c("ALL AEs", "SAE"))),
                         mainPanel(
                           fluidRow(column(6,
                                           plotlyOutput("ae_p1")),
                                    column(6, 
                                        DT::dataTableOutput("adae_t"))))),
                       fluidRow(h4("Adverse Events by Term/SOC"),
                                column(4,
                                       selectInput("grp3", "X-axis:", choices = c("AETERM","AESOC"))),
                                column(3,
                                       numericInput("term_soc_n", "Number of terms/soc:", 5, 1, 20, 1)),
                                column(4, 
                                       selectInput("filt5", "Filter AE", choices = c("All AEs", "SAE", "Non-SAE")))),
                       fluidRow(column(12, 
                                       plotlyOutput("ae_tops")))),
              tabPanel("Concomitant Medication",
                       fluidRow(column(12,
                                       titlePanel("Concomitant Medication")),
                                column(4,
                                       selectInput("grp4", "Group By:", choices = c("CMTRT","CMDECOD"))),
                                column(4,
                                       selectInput("filt6", "Filter:", choices = c("ALL CMs", "Prohibited Meds")))),
                       fluidRow(column(3, numericInput("meds_n", "Number of medications:", 10,
                                                       1, 30, 1))),
                       fluidRow(
                             column(12,
                                    plotlyOutput("cm_tops"))),
                       fluidRow(
                             column(12,
                                    DT::dataTableOutput("cm_t", width = 1000)))
              )
  )
)
# ----------
# SERVER --- 
# ----------

server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  # filter datasets on the input ---  
  adae_f <- 
    reactive({
      req(input$subjectID)
      if(input$chk_ae == TRUE){
        adae %>% 
          filter(USUBJID %in% c(input$subjectID)) %>% 
          group_by(USUBJID) %>% 
          mutate(ID = cur_group_id()) %>% ungroup()
      } 
    })
  
  sdtm_ae_f <- reactive({
    req(input$subjectID)
    if(input$chk_ae == TRUE){
      sdtm_ae %>% 
        filter(USUBJID %in% c(input$subjectID)) %>% 
        group_by(USUBJID) %>% 
        mutate(ID = cur_group_id()) %>% ungroup()
    } 
  })
  
  sdtm_ae_f1 <- 
    reactive({
      req(input$subjectID)
      if(input$chk_ae == TRUE){
        sdtm_ae1 %>% 
          filter(USUBJID %in% c(input$subjectID))%>% 
          group_by(USUBJID) %>% 
          mutate(ID = cur_group_id()) %>% ungroup() %>% 
          group_by(USUBJID,AESEQ) %>% 
          mutate(SEQ = cur_group_id()) %>% 
          ungroup()
      } 
    })
  
  
  adae_f1 <- 
    reactive({
      req(input$subjectID)
      if(input$chk_ae == TRUE){
        adae1 %>% 
          filter(USUBJID %in% c(input$subjectID))%>% 
          group_by(USUBJID) %>% 
          mutate(ID = cur_group_id()) %>% ungroup() %>% 
          group_by(USUBJID,AESEQ) %>% 
          mutate(SEQ = cur_group_id()) %>% 
          ungroup()
      } 
    })
  
  adsl_f <- reactive({
    req(input$subjectID)
    adsl %>% filter(USUBJID %in% c(input$subjectID))
  })
  
  sdtm_dm_f <- reactive({
    req(input$subjectID)
    sdtm_dm %>% filter(USUBJID %in% c(input$subjectID))
  })
  
  sdtm_ex_f <- reactive({
    req(input$subjectID)
    sdtm_ex %>% filter(USUBJID %in% c(input$subjectID))
  })
  sdtm_cm_f <- reactive({
    req(input$subjectID)
    sdtm_cm %>% filter(USUBJID %in% c(input$subjectID)) %>% 
      group_by(USUBJID) %>% 
      mutate(ID = cur_group_id())
  })
  sdtm_cm_prohib_f <- reactive({
    req(input$subjectID)
    sdtm_cm %>% filter(USUBJID %in% c(input$subjectID),
                       PROHIB_TRT == "Y") %>% 
      group_by(USUBJID) %>% 
      mutate(ID = cur_group_id())
  })
  
  sdtm_ae_terms <- 
    reactive({
      sdtm_ae %>% count(AETERM,AESER) %>% 
        arrange(desc(n))  %>% 
        mutate(AETERM = as.factor(AETERM))
    })
  
  sdtm_ae_soc <- reactive({
    sdtm_ae %>% count(AESOC,AESER) %>% 
      arrange(desc(n))})
  
  cm_terms <- 
    reactive({
      sdtm_cm %>% count(CMTRT) %>% 
        arrange(desc(n)) %>% 
        mutate(CMTRT = as.factor(CMTRT))
    })
  
  sdtm_ae_counts <- 
    reactive({
      sdtm_ae %>% count(AESEV,AESER) %>% 
        arrange(desc(n)) %>% 
        mutate(AESEV = as.factor(AESEV))
    })
  sdtm_ae_dm_ser_counts <- 
    reactive({
      sdtm_ae_dm %>% filter(AESER == "Y") %>% count(AESEV,get(input$grp2)) %>%
        arrange(desc(n)) %>% 
        mutate(AESEV = as.factor(AESEV),
               grp_var = as.factor(input$grp2))
    })
  
  cm_decod <-
    reactive({
      sdtm_cm %>% count(CMDECOD) %>% 
        arrange(desc(n)) %>% 
        mutate(CMDECOD = as.factor(CMDECOD))
    })
  
  sdtm_dm_vs_f <- reactive({
    sdtm_dm_vs %>% filter(!is.na(input$grp1))
  })
  
  ### ------------------------------
  ### Define prohibited medication(s)
  #### -----------------------------
  
  prohib_meds_trt <-    c("ATROVENT","CARDURA")
  prohib_meds_decod <-  c("IPRATROPIUM BROMIDE","DOXAZOSIN MESILATE")
  
  sdtm_cm <- sdtm_cm %>% 
    mutate(PROHIB_TRT = case_when(CMTRT %in% prohib_meds_trt ~ "Y"),
           PROHIB_DECOD = case_when(CMDECOD %in% prohib_meds_decod ~ "Y")) 
  
  prohib_meds_f <- reactive({
    sdtm_cm %>% filter(PROHIB_TRT == "Y") %>% 
      count(CMTRT,CMDECOD) %>% 
      arrange(desc(n)) %>% 
      mutate(CMTRT = as.factor(CMTRT),
             CMDECOD = as.factor(CMDECOD))})
  
  ### Color palettes ---
  AZcol <-c("#830051", "#C4D600", "#F0AB00", "#003865", "#3F4444", "#68D2DF", "#C0006F", "#3C1053", "#9DB0AC",
            "#CD99B9", "#E7EF99", "#F9DD99", "#99AFC1", "#B2B4B4", "#C3EDF2", "#EC99C5", "#B19FBA", "#D8DFDE",
            "#4F0031", "#768000", "#906700", "#00223D", "#262929", "#3E7E86", "#7D0043", "#240A32", "#5E6A67")
  AZcol_meds <- c( "#CD99B9", "#E7EF99", "#F9DD99", "#99AFC1", "#B2B4B4", "#C3EDF2", "#EC99C5", "#B19FBA", "#D8DFDE",
                   "#830051", "#C4D600", "#F0AB00", "#003865", "#3F4444", "#68D2DF", "#C0006F", "#3C1053", "#9DB0AC",
                   "#4F0031", "#768000", "#906700", "#00223D", "#262929", "#3E7E86", "#7D0043", "#240A32", "#5E6A67")
  
  ### -------------------------------  
  ### Individual Timeline page
  ### -------------------------------  
  
  # Plot ae data ---
  p <- reactive({
    pl <-
      ggplot() +
      suppressWarnings({
        geom_jitter(data = sdtm_dm_f(), aes(y = USUBJID,x = RFXSTDT,
                                            text = paste("USUBJID:", USUBJID)), 
                    color = "forestgreen", width = 0, height = 0, shape = 23, size = 5)}) +
      geom_hline(yintercept = c(1:50), color = "lightgray", linetype = 'dotted', alpha = 0.4) +
      suppressWarnings({
        geom_jitter(data = sdtm_dm_f(),aes(x = RFXENDT, y = USUBJID,
                                           text = paste0("USUBJID:", USUBJID,
                                                         "\nTRTDUR: "," DAY(S)")) , 
                    color = "red", width = 0, height = 0, shape = 23, size = 5)}) +
      
      suppressWarnings({
        if(input$chk_ae == TRUE){
          # AE start date points ---
          list(geom_point(data = sdtm_ae_f(), 
                          aes(x = AESTDT, y = ID + AESEQ/(AESEQ + max(AESEQ)) , group = AESEQ,
                              color = AESEV, 
                              text = paste("USUBJID:", USUBJID,
                                           "\nAETERM:", AETERM,
                                           "\nAESEQ:", AESEQ,
                                           #"\nAE Duration:", ADURN, ADURU,
                                           "\nAESEV:", AESEV,
                                           "\nAESER:", AESER,
                                           "\nAEOUT:", AEOUT)),
                          size = 2, shape = 4),
               # AE end date points ---
               geom_point(data = sdtm_ae_f(), 
                          aes(x = AEENDT, y = ID + AESEQ/(AESEQ + max(AESEQ)), group = AESEQ,
                              color = AESEV, 
                              text = paste("USUBJID:", USUBJID,
                                           "\nAETERM:", AETERM,
                                           "\nAESEQ:", AESEQ,
                                           #"\nAE Duration:", ADURN, ADURU, 
                                           "\nAESEV:", AESEV,
                                           "\nAESER:", AESER,
                                           "\nAEOUT:", AEOUT)), 
                          size = 2, shape = 4), 
               #AE duration lines ---
               geom_line(data = sdtm_ae_f1(),
                         aes(x = AEDAT,  y = ID + AESEQ/(AESEQ + max(AESEQ)) , group = SEQ, color = AESER)))
        }}) +
      suppressWarnings({
        if(input$chk_dosing == TRUE){
          # Dosing points ---
          list(geom_point(data = sdtm_ex_f(),
                          aes(x=EXSTDTC, y = USUBJID, group = USUBJID,
                              text = paste("USUBJID:", USUBJID,
                                           "\nVISTNUM:", VISITNUM,
                                           "\nEXDOSE:", EXDOSE,
                                           "\nEXDOSU:", EXDOSU,
                                           "\nEXTRT:", EXTRT)), shape = 13, size = 3, color = "#E69F00"))}}) + 
      suppressWarnings({
        #Conmeds
        if(input$chk_conmeds == TRUE && input$prohib_cm == "ALL CMs"){
          list(geom_jitter(data = sdtm_cm_f(),
                           aes(x = CMDT, y = ID - 0.05, group = USUBJID,
                               text = paste("USUBJID:", USUBJID,
                                            "\nVISITNUM:", VISITNUM,
                                            "\nVISIT:", VISIT,
                                            "\nCMSEQ:", CMSEQ,
                                            "\nCMTRT:", CMTRT,
                                            "\nCMDECOD:",CMDECOD,
                                            "\nCMDOSE:", CMDOSE,
                                            "\nCMDOSU:", CMDOSU),
                               color = CMTRT), 
                           shape = 21, size = 2,
                           height = 0.1)
               )}
        else if(input$chk_conmeds == TRUE && input$prohib_cm == "Only prohibited CMs"){
          list(geom_jitter(data = sdtm_cm_prohib_f(),
                           aes(x = CMDT, y = ID - 0.05, group = USUBJID,
                               text = paste("USUBJID:", USUBJID,
                                            "\nVISITNUM:", VISITNUM,
                                            "\nVISIT:", VISIT,
                                            "\nCMSEQ:", CMSEQ,
                                            "\nCMTRT:", CMTRT,
                                            "\nCMDECOD:",CMDECOD,
                                            "\nCMDOSE:", CMDOSE,
                                            "\nCMDOSU:", CMDOSU),
                               color = CMTRT), 
                           shape = 21, size = 2,
                           height = 0.1)
               )
        }}) +
      labs(y= "Unique Subject ID", x = "Date") + 
      scale_colour_manual(values=AZcol_meds) 
       #theme_minimal()  
    
    
     ggplotly(pl) %>% layout(legend=list(x = 0.5,
                         y = 1.2,
                         xanchor='center',
                         yanchor='top',
                         orientation='h')) 
  })
  
  output$indiv_plot <- renderPlotly({p()})
  
  # Produce sdtm dm table ---
  output$dm_table <- renderDataTable({
    DT::datatable(data = sdtm_dm_f(),
                  options = list(scrollX = TRUE,
                                 pageLength = 5),
                  style = "bootstrap", rownames = FALSE)
  })
  
  # produce sdtm ae table ---
  output$ae_table <- 
    renderDataTable({
      DT::datatable(data = sdtm_ae_f(),
                    options = list(scrollX = TRUE,
                                   pageLength = 5),
                    style = "bootstrap", rownames = FALSE)
    })
  
  # produce sdtm cm table ---
  output$cm_table <- 
    renderDataTable({
      DT::datatable(data = sdtm_cm_f(),
                    options = list(scrollX = TRUE,
                                   pageLength = 5),
                    style = "bootstrap", rownames = FALSE)
    })
  
  # produce sdtm ex table ---
  output$ex_table <- 
    renderDataTable({
      DT::datatable(data = sdtm_ex_f(),
                    options = list(scrollX = TRUE,
                                   pageLength = 5),
                    style = "bootstrap", rownames = FALSE)
    })
  ### -----------------------------
  ### Grouped Demographic page ---
  ### -----------------------------
  
  # Grouped Demographic plot --- 
  
  gp_1 <- 
    reactive({
      
      if(input$var1 %in% c("AGE", "WEIGHT", "HEIGHT")){    
        gp <- 
          ggplot(data = sdtm_dm_vs_f()) + 
          geom_boxplot(aes_string(x = input$grp1, y = input$var1, fill = input$grp1), 
                       show.legend = FALSE) + 
          geom_violin(aes_string(x = input$grp1, y = input$var1), 
                      alpha = 0.15, show.legend = FALSE, color = "#999999") +
          scale_fill_manual(values = AZcol)
        

      }
      
      else if( input$var1 != "AGE"){
        gp <- 
          ggplot(data = sdtm_dm_vs_f()) + 
          geom_boxplot(aes_string(x = "VISIT", y = input$var1, fill = input$grp1, group = input$grp1), 
                       show.legend = FALSE) + 
          scale_fill_manual(values = AZcol) +
          facet_wrap(input$grp1) +
          theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
        
        suppressWarnings({
          ggplotly(gp) %>% 
            layout(legend=list(x = 0.5,
                               y = 1.2,
                               xanchor='center',
                               yanchor='top',
                               orientation='h'))})
      }
    })
  output$grpPlot1 <- renderPlotly({ggplotly(gp_1())})
  
  # Grouped Demographic tables --- 
  sum1 <- 
    reactive({
      if(input$var1 == "AGE"){
        
        mydf <- sdtm_dm_vs %>%  distinct(USUBJID, .keep_all = TRUE)  %>% 
          group_by_at(input$grp1) %>%                                             
          summarise(                                                         
            n      = round(n(), digits = 0),                                               
            Max    = max(get(input$var1), na.rm = T),                    
            Min    = min(get(input$var1), na.rm = T), 
            Mean   = round(mean(get(input$var1), na.rm=T), digits = 1),  
            Median = round(median(get(input$var1), na.rm = T), digits = 1),  
            SD     = round(sd(get(input$var1), na.rm = T), digits = 2)) %>% 
          as.data.frame()
        
        mydf2 <- mydf %>% pivot_longer(cols = c(n,Max,Min,Mean,Median,SD), 
                                       names_to = "STATISTIC", values_to = "TEMP1")
        mydf3 <- mydf2 %>% pivot_wider(names_from = input$grp1, values_from = TEMP1)
        
      }
      else if(input$var1 != "AGE"){
        mydf <- sdtm_dm_vs %>%
          group_by_at(c(input$grp1, "VISIT")) %>%                                           
          summarise(                                                         
            n      = round(n(), digits = 0),                                               
            Max    = max(get(input$var1), na.rm = T),                    
            Min    = min(get(input$var1), na.rm = T), 
            Mean   = round(mean(get(input$var1), na.rm=T), digits = 1),  
            Median = round(median(get(input$var1), na.rm = T), digits = 1),  
            SD     = round(sd(get(input$var1), na.rm = T), digits = 2)) %>% 
          as.data.frame()
        
        mydf2 <- mydf %>% pivot_longer(cols = c(n,Max,Min,Mean,Median,SD), 
                                       names_to = "STATISTIC", values_to = "TEMP1")
        mydf3 <- mydf2 %>% pivot_wider(names_from = VISIT, values_from = TEMP1)
      }
    })
  
  output$summary1 <- renderDataTable(sum1(), options = list(lengthMenu = c(15, 30, 45)),
                                     style = "bootstrap", rownames = FALSE)
  
  ### ----------------------
  ### Adverse Events Page ---
  ### ----------------------
  sdtm_ae_p <- 
    reactive({
      ae_p <-
        suppressWarnings({
          if(input$grp2 == "ALL" && input$filt4 == "ALL AEs"){
            sdtm_ae_counts() %>% ggplot() + 
              geom_col(aes(x = AESEV, fill = AESEV, y = n,
                           text = paste("AESEV:", AESEV,
                                        "\nCOUNT:", n),
                           show.legend = FALSE))+
              scale_fill_manual(values = AZcol) + 
              labs(x = "All Adverse Events", y = "Number of occurences") 
          }
          else if(input$grp2 == "ALL" && input$filt4 == "SAE"){
            sdtm_ae_counts() %>% filter(AESER == "Y") %>%  ggplot() + 
              geom_col(aes(x = AESEV, fill = AESEV,y = n,
                           text = paste("AESEV:", AESEV,
                                        "\nCOUNT:", n)),
                       show.legend = FALSE, 
                       position = "dodge", stat="count")+
              scale_fill_manual(values = AZcol) + 
              labs(x = "Serious Adverse Events", y = "Number of occurences") 
          }
          else if(input$grp2 == "ACTARM"){
            if(input$filt4 == "SAE"){
              sdtm_ae_dm %>% filter(AESER == "Y") %>% group_by(ACTARM, AESEV) %>% 
                mutate(NOAE = n()) %>% ungroup() %>% distinct(ACTARM,AESEV,NOAE) %>% 
                arrange(-NOAE, AESEV) %>% 
                as.data.frame()%>% 
                ggplot() + 
                geom_col(aes(x = ACTARM,y = NOAE, fill = AESEV,
                             text = paste("AESEV:", AESEV,
                                          "\nCOUNT:", NOAE)), 
                         position = "dodge2", stat="count", show.legend = FALSE) +
                labs(x = "Actual Arm", y = "Number of occurences") +
                scale_fill_manual(values = AZcol)}
            else if(input$filt4 == "ALL AEs"){
              sdtm_ae_dm %>% group_by(ACTARM, AESEV) %>% 
                mutate(NOAE = n()) %>% ungroup() %>% distinct(ACTARM,AESEV,NOAE) %>% 
                arrange(-NOAE, AESEV) %>% 
                as.data.frame()%>% 
                ggplot() + 
                geom_col(aes(x = ACTARM,y = NOAE, fill = AESEV,
                             text = paste("AESEV:", AESEV,
                                          "\nCOUNT:", NOAE)), 
                         position = "dodge2", stat="count", show.legend = FALSE) +
                labs(x = "Actual Arm", y = "Number of occurences") +
                scale_fill_manual(values = AZcol)}
          }
          else if(input$grp2 == "ARM"){
            if(input$filt4 == "SAE"){
              sdtm_ae_dm %>% filter(AESER == "Y") %>% group_by(ARM, AESEV) %>% 
                mutate(NOAE = n()) %>% ungroup() %>% distinct(ARM,AESEV,NOAE) %>% 
                arrange(-NOAE, AESEV) %>% 
                as.data.frame()%>% 
                ggplot() + 
                geom_col(aes(x = ARM,y = NOAE, fill = AESEV,
                             text = paste("AESEV:", AESEV,
                                          "\nCOUNT:", NOAE)), 
                         position = "dodge2", stat="count", show.legend = FALSE) +
                labs(x = "Actual Arm", y = "Number of occurences") +
                scale_fill_manual(values = AZcol)}
            else if(input$filt4 == "ALL AEs"){
              sdtm_ae_dm %>% group_by(ARM, AESEV) %>% 
                mutate(NOAE = n()) %>% ungroup() %>% distinct(ARM,AESEV,NOAE) %>% 
                arrange(-NOAE, AESEV) %>% 
                as.data.frame()%>% 
                ggplot() + 
                geom_col(aes(x = ARM,y = NOAE, fill = AESEV,
                             text = paste("AESEV:", AESEV,
                                          "\nCOUNT:", NOAE)), 
                         position = "dodge2", stat="count", show.legend = FALSE) +
                labs(x = "Actual Arm", y = "Number of occurences") +
                scale_fill_manual(values = AZcol)}
          }
          else if(input$grp2 == "SEX"){
            if(input$filt4 == "SAE"){
              sdtm_ae_dm %>% filter(AESER == "Y") %>% group_by(SEX, AESEV) %>% 
                mutate(NOAE = n()) %>% ungroup() %>% distinct(SEX,AESEV,NOAE) %>% 
                arrange(-NOAE, AESEV) %>% 
                as.data.frame()%>% 
                ggplot() + 
                geom_col(aes(x = SEX,y = NOAE, fill = AESEV,
                             text = paste("AESEV:", AESEV,
                                          "\nCOUNT:", NOAE)), 
                         position = "dodge2", stat="count", show.legend = FALSE) +
                labs(x = "Sex", y = "Number of occurences") +
                scale_fill_manual(values = AZcol)}
            else if(input$filt4 == "ALL AEs"){
              sdtm_ae_dm %>% group_by(SEX, AESEV) %>% 
                mutate(NOAE = n()) %>% ungroup() %>% distinct(SEX,AESEV,NOAE) %>% 
                arrange(-NOAE, AESEV) %>% 
                as.data.frame()%>% 
                ggplot() + 
                geom_col(aes(x = SEX,y = NOAE, fill = AESEV,
                             text = paste("AESEV:", AESEV,
                                          "\nCOUNT:", NOAE)), 
                         position = "dodge2", stat="count", show.legend = FALSE) +
                labs(x = "Sex", y = "Number of occurences") +
                scale_fill_manual(values = AZcol)}
          }
          
          else if(input$grp2 == "RACE"){
            if(input$filt4 == "SAE"){
              sdtm_ae_dm %>% filter(AESER == "Y") %>% group_by(RACE, AESEV) %>% 
                mutate(NOAE = n()) %>% ungroup() %>% distinct(RACE,AESEV,NOAE) %>% 
                arrange(-NOAE, AESEV) %>% 
                as.data.frame()%>% 
                ggplot() + 
                geom_col(aes(x = RACE,y = NOAE, fill = AESEV,
                             text = paste("AESEV:", AESEV,
                                          "\nCOUNT:", NOAE)), 
                         position = "dodge2", stat="count", show.legend = FALSE) +
                labs(x = "Race", y = "Number of occurences") +
                scale_fill_manual(values = AZcol)}
            else if(input$filt4 == "ALL AEs"){
              sdtm_ae_dm %>% group_by(RACE, AESEV) %>% 
                mutate(NOAE = n()) %>% ungroup() %>% distinct(RACE,AESEV,NOAE) %>% 
                arrange(-NOAE, AESEV) %>% 
                as.data.frame()%>% 
                ggplot() + 
                geom_col(aes(x = RACE,y = NOAE, fill = AESEV,
                             text = paste("AESEV:", AESEV,
                                          "\nCOUNT:", NOAE)), 
                         position = "dodge2", stat="count", show.legend = FALSE) +
                labs(x = "Race", y = "Number of occurences") +
                scale_fill_manual(values = AZcol)}
          }
          
          else if(input$grp2 == "ETHNIC"){
            if(input$filt4 == "SAE"){
              sdtm_ae_dm %>% filter(AESER == "Y") %>% group_by(ETHNIC, AESEV) %>% 
                mutate(NOAE = n()) %>% ungroup() %>% distinct(ETHNIC,AESEV,NOAE) %>% 
                arrange(-NOAE, AESEV) %>% 
                as.data.frame()%>% 
                ggplot() + 
                geom_col(aes(x = ETHNIC,y = NOAE, fill = AESEV,
                             text = paste("AESEV:", AESEV,
                                          "\nCOUNT:", NOAE)), 
                         position = "dodge2", stat="count", show.legend = FALSE) +
                labs(x = "Ethnic", y = "Number of occurences") +
                scale_fill_manual(values = AZcol)}
            else if(input$filt4 == "ALL AEs"){
              sdtm_ae_dm %>% group_by(ETHNIC, AESEV) %>% 
                mutate(NOAE = n()) %>% ungroup() %>% distinct(ETHNIC,AESEV,NOAE) %>% 
                arrange(-NOAE, AESEV) %>% 
                as.data.frame()%>% 
                ggplot() + 
                geom_col(aes(x = ETHNIC,y = NOAE, fill = AESEV,
                             text = paste("AESEV:", AESEV,
                                          "\nCOUNT:", NOAE)), 
                         position = "dodge2", stat="count", show.legend = FALSE) +
                labs(x = "Ethnic", y = "Number of occurences") +
                scale_fill_manual(values = AZcol)}
          }})
      
      
      ggplotly(ae_p, tooltip = c("text")) %>% layout(legend=list(x = 0.5,
                                                                 y = 1.2,
                                                                 xanchor='center',
                                                                 yanchor='top',
                                                                 orientation='h'))
    })
  output$ae_p1 <- renderPlotly(sdtm_ae_p())
  
  #AE table1
  sdtm_ae_t <- reactive({
    
    if(input$grp2 == "ALL" && input$filt4 == "ALL AEs"){
      sdtm_ae_dm %>% group_by(USUBJID) %>% 
        mutate(NOAE = n()) %>% ungroup() %>% distinct(USUBJID,AESEV,NOAE) %>% 
        arrange(-NOAE, AESEV) %>% 
        as.data.frame()}
    else if(input$grp2 != "ALL" && input$filt4 == "ALL AEs"){
      sdtm_ae_dm %>% group_by(USUBJID) %>% 
        mutate(NOAE = n()) %>% ungroup() %>% distinct(USUBJID,get(input$grp2),AESEV,NOAE) %>% 
        arrange(-NOAE, AESEV) %>% as.data.frame() %>% 
        rename(GROUP = "get(input$grp2)")}
    else{
      sdtm_ae_dm %>% filter(AESER == "Y") %>% group_by(USUBJID) %>% 
        mutate(NOAE = n()) %>% ungroup() %>% distinct(USUBJID, AESEV,NOAE) %>% 
        arrange(-NOAE, AESEV) %>% 
        as.data.frame()}
  })
  
  output$adae_t <- renderDataTable(sdtm_ae_t(),options = list(lengthMenu = c(5, 10, 25,50)), 
                                   style = "bootstrap", rownames = FALSE)
  
  # AE decod/soc barplot
  sdtm_ae_top_term <-reactive({
    p <- 
      suppressWarnings({
        if(input$grp3 == "AETERM" && input$filt5 == "All AEs"){
          sdtm_ae_terms() %>% slice(1:input$term_soc_n) %>% 
            ggplot() + 
            geom_col(aes(x = fct_rev(fct_reorder(get(input$grp3), n)), y = n,
                         fill = fct_rev(fct_reorder(get(input$grp3),n)),
                         text = paste("AETERM:",AETERM,
                                      "\nCOUNT:", n))) +
            labs(x = "AETERM", y = "Number of occurences",
                 fill = "AETERM") + 
            theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))+
            scale_fill_manual(values = AZcol)}
        else if(input$grp3 == "AETERM" && input$filt5 == "SAE"){
          sdtm_ae_terms() %>% filter(AESER =="Y") %>% slice(1:input$term_soc_n) %>% 
            ggplot() + 
            geom_col(aes(x = fct_rev(fct_reorder(get(input$grp3), n)), y = n,
                         fill = fct_rev(fct_reorder(get(input$grp3),n)),
                         text = paste("AETERM:",AETERM,
                                      "\nCOUNT:", n))) +
            labs(x = "AETERM", y = "Number of occurences",
                 fill = "AETERM") + 
            theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))+
            scale_fill_manual(values = AZcol)}
        else if(input$grp3 == "AETERM" && input$filt5 == "Non-SAE"){
          sdtm_ae_terms() %>% filter(AESER == "N") %>% slice(1:input$term_soc_n) %>% 
            ggplot() + 
            geom_col(aes(x = fct_rev(fct_reorder(get(input$grp3), n)), y = n,
                         fill = fct_rev(fct_reorder(get(input$grp3),n)),
                         text = paste("AETERM:",AETERM,
                                      "\nCOUNT:", n))) +
            labs(x = "AETERM", y = "Number of occurences",
                 fill = "AETERM") + 
            theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))+
            scale_fill_manual(values = AZcol)}
        
        else if(input$grp3 == "AESOC" && input$filt5 == "All AEs"){
          sdtm_ae_soc() %>% slice(1:input$term_soc_n) %>% 
            ggplot() + 
            geom_col(aes(x = fct_rev(fct_reorder(get(input$grp3), n)), y = n,
                         fill = fct_rev(fct_reorder(get(input$grp3),n)),
                         text = paste("AESOC:",AESOC,
                                      "\nCOUNT:", n))) +
            labs(x = "AESOC", y = "Number of occurences",
                 fill = "AESOC") + 
            theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=1, size = 4))+
            scale_fill_manual(values = AZcol)}
        
        else if(input$grp3 == "AESOC" && input$filt5 == "SAE"){
          sdtm_ae_soc() %>% filter(AESER == "Y") %>% slice(1:input$term_soc_n) %>% 
            ggplot() + 
            geom_col(aes(x = fct_rev(fct_reorder(get(input$grp3), n)), y = n,
                         fill = fct_rev(fct_reorder(get(input$grp3),n)),
                         text = paste("AESOC:",AESOC,
                                      "\nCOUNT:", n))) +
            labs(x = "AESOC", y = "Number of occurences",
                 fill = "AESOC") + 
            theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=1, size = 4))+
            scale_fill_manual(values = AZcol)}
        
        else if(input$grp3 == "AESOC" && input$filt5 == "Non-SAE"){
          sdtm_ae_soc() %>% filter(AESER == "N") %>% slice(1:input$term_soc_n) %>% 
            ggplot() + 
            geom_col(aes(x = fct_rev(fct_reorder(get(input$grp3), n)), y = n,
                         fill = fct_rev(fct_reorder(get(input$grp3),n)),
                         text = paste("AESOC:",AESOC,
                                      "\nCOUNT:", n))) +
            labs(x = "AESOC", y = "Number of occurences",
                 fill = "AESOC") + 
            theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=1, size = 4))+
            scale_fill_manual(values = AZcol)}
      })
  })
  
  output$ae_tops <- renderPlotly({ggplotly(sdtm_ae_top_term(),
                                           tooltip = c("text"))})
  
  ###-------------------------------
  ### Concomitant Medication Tab ---
  ###-------------------------------
  # CM table ---
  sdtm_cm_t <- reactive({
    if(input$grp4 == "CMTRT" && input$filt6== "ALL CMs"){
      sdtm_cm %>% group_by(USUBJID) %>% 
        mutate(NOCM = n()) %>% ungroup() %>% distinct(USUBJID,CMTRT, CMDECOD, NOCM) %>% 
        arrange(-NOCM) %>% 
        as.data.frame()}
    
    else if(input$grp4 == "CMTRT" && input$filt6 == "Prohibited Meds"){
      sdtm_cm %>% filter(PROHIB_TRT == "Y") %>% group_by(USUBJID) %>% 
        mutate(NOCM = n()) %>% 
        ungroup() %>% distinct(USUBJID,CMTRT,CMDECOD,NOCM) %>% 
        arrange(-NOCM) %>% 
        as.data.frame()
    }
    else if(input$grp4 == "CMDECOD" && input$filt6 == "ALL CMs"){
      sdtm_cm %>% group_by(USUBJID) %>% 
        mutate(NOCM = n()) %>% 
        ungroup() %>% distinct(USUBJID,CMDECOD,CMTRT,NOCM) %>% 
        arrange(-NOCM) %>% 
        as.data.frame()
    }
    else if(input$grp4 == "CMDECOD" && input$filt6 == "Prohibited Meds"){
      sdtm_cm %>% filter(PROHIB_DECOD == "Y") %>% group_by(USUBJID) %>% 
        mutate(NOCM = n()) %>% 
        ungroup() %>% distinct(USUBJID,CMDECOD,CMTRT,NOCM) %>% 
        arrange(-NOCM) %>% 
        as.data.frame()
    }
  })
  
  # CM top bar chart ---
  conmeds_top_term <- reactive({
    p <- 
      
      suppressWarnings({
        if(input$grp4 == "CMTRT" && input$filt6 == "ALL CMs"){
          cm_terms() %>% slice(1:input$meds_n) %>% 
            ggplot() + 
            geom_col(aes(x = fct_rev(fct_reorder(get(input$grp4), n)), y = n,
                         fill = fct_rev(fct_reorder(get(input$grp4),n)),
                         text = paste("CMTRT:",CMTRT,
                                      "\nCOUNT:", n))) +
            labs(x = "CMTRT", y = "Number of occurences",
                 fill = "CMTRT") + 
            theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1)) + 
            scale_fill_manual(values = AZcol)
        }
        
        else if(input$grp4 == "CMTRT" && input$filt6 == "Prohibited Meds"){
          prohib_meds_f() %>% slice(1:input$meds_n) %>% 
            ggplot() + 
            geom_col(aes(x = fct_rev(fct_reorder(get(input$grp4), n)), y = n,
                         fill = fct_rev(fct_reorder(get(input$grp4),n)),
                         text = paste("CMTRT:",CMTRT,
                                      "\nCOUNT:", n))) + 
            labs(x = "CMTRT", y = "Number of occurences",
                 fill = "CMTRT") + 
            theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))+
            scale_fill_manual(values = AZcol)}
        
        else if(input$grp4 == "CMDECOD" && input$filt6 == "ALL CMs"){
          cm_decod() %>% slice(1:input$meds_n) %>% 
            ggplot() + 
            geom_col(aes(x = fct_rev(fct_reorder(get(input$grp4), n)), y = n,
                         fill = fct_rev(fct_reorder(get(input$grp4), n)),
                         text = paste("CMDECOD:",CMDECOD,
                                      "\nCOUNT:", n))) +
            labs(x = "CMDECOD", y = "Number of occurences",
                 fill = "CMDECOD") + 
            theme(axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=1))+
            scale_fill_manual(values = AZcol)}
        
        else if(input$grp4 == "CMDECOD" && input$filt6 == "Prohibited Meds"){
          prohib_meds_f() %>% slice(1:input$meds_n) %>% 
            ggplot() + 
            geom_col(aes(x = fct_rev(fct_reorder(get(input$grp4), n)), y = n,
                         fill = fct_rev(fct_reorder(get(input$grp4),n)),
                         text = paste("CMDECOD:",CMDECOD,
                                      "\nCOUNT:", n))) + 
            labs(x = "CMDECOD", y = "Number of occurences",
                 fill = "CMDECOD") + 
            theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))+
            scale_fill_manual(values = AZcol)}
      })
  })
  
  
  output$cm_tops <- renderPlotly({ggplotly(conmeds_top_term(),
                                           tooltip = c("text"))})
  output$cm_t <- renderDataTable(sdtm_cm_t(),options = list(lengthMenu = c(7, 10, 25, 50)),
                                 style = "bootstrap", rownames = FALSE)
  
  session$onSessionEnded(function() {
    stopApp()})
}

# Run the application 
shinyApp(ui = ui, server = server)
