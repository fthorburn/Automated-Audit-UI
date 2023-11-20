library(readxl)
library(tidyverse)
library(fastmap)
library(shiny)
library(shinyFiles)
library(shinyjs)


ui <- fluidPage(
  titlePanel("Auto-'Demographic Data Audit' User-Interface"),
  p("Note: Upload data while in 'Processed Data' tab"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Import Justice Server Report (.xlsx file) (Details Only)', accept = c(".xlsx")),
      conditionalPanel(
        condition = "output.conditionalText_js == 'Your Justice Server file has been Processed!'",
        fileInput('file2', 'Import UCLS Report (.xlsx file)', accept = c(".xlsx"))
      ),
      conditionalPanel(
        condition = "output.conditionalText_ucls == ''",
        fileInput('file3', 'Import UC Portal Report (.xlsx file)', accept = c(".xlsx"))
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "mytabs",
        tabPanel("Processed Data",
                 tags$hr(),
                 p(""),
                 conditionalPanel(
                   condition = "output.conditionalText_js == 'Please upload a Justice Server file.'",
                   p("Steps:")
                 ),
                 textOutput("conditionalText_js"),
                 textOutput("conditionalText_ucls"),
                 textOutput("conditionalText_ucportal"),
                 tags$hr(),
                 tableOutput('js_file'),
                 tags$hr(),
                 textOutput('js_duplicate_check'),
                 textOutput('numrows_js'),
                 tags$hr(),
                 conditionalPanel(
                   condition = "output.conditionalText_js != 'Please upload a Justice Server file.'",
                   strong("Instructions:"),
                   p(""),
                   p("1. Paste these A#s into the UCLS and UC Portal A# search feature:"),
                   textOutput('grab'),
                   p(""),
                   p("2. Copy resulting tables (do not alter column names or any other aspect of the file) and place in .xlsx files"),
                   p("3. Import .xlsx files into Interface"),
                   tags$hr(),
                 ),
                 conditionalPanel(
                   condition = "output.conditionalText_ucls == ''",
                   p("Your UCLS file has been Processed!")
                 ),
                 tableOutput('ucls_file'),
                 tags$hr(),
                 textOutput('ucls_duplicate_check'),
                 textOutput('numrows_ucls'),
                 textOutput('numrows_js_ucls'),
                 tags$hr(),
                 conditionalPanel(
                    condition = "output.conditionalText_ucportal == ''",
                    p("Your UC Portal file has been Processed!")
                 ),
                 tableOutput('ucportal_data'),
                 tags$hr(),
                 textOutput('ucportal_duplicate_check'),
                 textOutput('numrows_ucportal'),
                 textOutput('numrows_ucls_ucportal'),
                 tags$hr(),
        ),
        tabPanel("Discrepancies",
                tags$hr(),
                p("UCLS-JS Discrepancies:"),
                tags$hr(),
                tableOutput('disc_table_js_ucls'),
                
                tags$hr(),
                p("UCLS-UCPortal Discrepancies:"),
                tags$hr(),
                tableOutput('disc_table_ucls_ucportal'),
                ),
        tabPanel("Original JS Data",
                 tableOutput('og_file'),
        ),
        tabPanel("Original UCLS Data",
                 tableOutput('og_file2'),
        ),
        tabPanel("Original UC Portal Data",
                 tableOutput('og_file3'),
        ),
      )
    )
  )
)







server <- function(input, output) {
  
  #making data frames able to be refrenced in all functions
  shared_data <- reactiveValues(js_data = NULL,
                                ucls_data = NULL,
                                ucportal_data = NULL)
  
  #start with most tabs hidden
  hideTab(inputId = "mytabs", target = "Discrepancies")
  hideTab(inputId = "mytabs", target = "Original JS Data")
  hideTab(inputId = "mytabs", target = "Original UCLS Data")
  hideTab(inputId = "mytabs", target = "Original UC Portal Data")
  
  #show tabs once an input happens with the ucls or JS file or ucportal file
  observeEvent(input$file3, {
    showTab(inputId = "mytabs", target = "Original UC Portal Data")
  })
  observeEvent(input$file2, {
   showTab(inputId = "mytabs", target = "Discrepancies")
  })
  observeEvent(input$file2, {
    showTab(inputId = "mytabs", target = "Original UCLS Data")
  })
  observeEvent(input$file1, {
    showTab(inputId = "mytabs", target = "Original JS Data")
  })

  #Checking to see if a JS file has been uploaded
  hasFile <- reactive({
    return(!is.null(input$file1))
  })
  output$conditionalText_js <- renderText({
    if (hasFile()) {
      # Do something when a file is uploaded
      #return("Your Justice Server File has been Processed!")
      return("Your Justice Server file has been Processed!")
    } else {
      # Do something else when no file is uploaded
      return("Please upload a Justice Server file.")
    }
  })
  
  #Checking to see if a UCLS file has been uploaded
  hasFile2 <- reactive({
    return(!is.null(input$file2))
  })
  output$conditionalText_ucls <- renderText({
    if (hasFile2()) {
      # Do something when a file is uploaded
      #return("Your UCLS File has been Processed!")
      return("")
    } else {
      # Do something else when no file is uploaded
      return("Please upload a UCLS file.")
    }
  })
  
  #Checking to see if a UCLS file has been uploaded
  hasFile3 <- reactive({
    return(!is.null(input$file3))
  })
  output$conditionalText_ucportal <- renderText({
    if (hasFile3()) {
      # Do something when a file is uploaded
      #return("Your UCLS File has been Processed!")
      return("")
    } else {
      # Do something else when no file is uploaded
      return("Please upload a UC Portal file.")
    }
  })
  
  ## Printing original JS file
  output$og_file <- renderTable({
    #checking file
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    readxl::read_excel(inFile$datapath)
  })
  
  ## Formatting JS file and printing it 
  output$js_file <- renderTable({
    #checking file
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    # Read the original data
    js_data_og <- readxl::read_excel(inFile$datapath)
    
    js_data_og <- js_data_og %>% rename(
      A_Number = `Contact's A#`,
      First_Name = `Contact: First Name`,
      Last_Name = `Contact: Last Name`,
      DOB = `Contact: Birthdate`,
      Gender = `Contact: Gender`,
      Nationality = `Contact: Nationality`
    ) %>%
      mutate(Languages = paste(js_data_og$`Contact: Primary Language`, 
                               js_data_og$`Contact: Secondary Language`, 
                               sep = ","))
    
    # Adjusting nationality variable
    js_data_og <- js_data_og %>% mutate(Nationality = str_trim(str_extract(Nationality, "\\|(.*)")))
    js_data_og$Nationality <- substring(js_data_og$Nationality, first = 2)
    js_data_og$Nationality <- str_trim(js_data_og$Nationality)
    
    # Only selecting relevant data
    js_data_og <- js_data_og %>% select(A_Number, First_Name, Last_Name, DOB, Gender, Nationality, Languages)
    
    # Making DOB type data for comparison, format: Y-M-D
    js_data_og[] <- lapply(js_data_og, as.character)
    js_data_og$DOB <- as.character(as.Date(js_data_og$DOB, format = "%m/%d/%Y"))
    
    #entering NA characters for NA values, so that code can run with missing cells
    js_data_og[is.na(js_data_og)] <- as.character("NA")
    
    #updating shared js data
    shared_data$js_data <- js_data_og
    
    #print re-formatted file
    shared_data$js_data
  })
  
  #prting out A# list for UCLS instructions
  output$grab <- renderText({
    
    #create a string of A#s seperated by commas and a space
    grab <- paste(shared_data$js_data$A_Number, collapse = ", ")
    
    #return resulting string
    grab
  })

  ## Checking JS Duplicates and Printing
  output$js_duplicate_check <- renderText({
    inFile <- input$file1
    
    #if there is a file inputed, check 
    if (is.null(inFile) == FALSE) {
    
      if(!any(duplicated(shared_data$js_data$A_Number))) {
        print("No A# duplicates in JS")
      } else {
        print("There are A# duplicates in JS... Please resolve")
      }
    }
  })
  
  ## Checking UC Portal Duplicates and Printing
  output$ucportal_duplicate_check <- renderText({
    inFile3 <- input$file3
    
    #if there is a file inputed, check 
    if (is.null(inFile3) == FALSE) {
      
      if(!any(duplicated(shared_data$ucportal_data$A_Number))) {
        print("No A# duplicates in UC Portal")
      } else {
        print("There are A# duplicates in UC Portal... Please resolve")
      }
    }
  })
  
  ##  Printing Original UCLS file
  output$og_file2 <- renderTable({
    #checking file
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    #reading file
    ucls_data <- readxl::read_excel(inFile2$datapath)
    #making all variables characters so dates don't bug out
    ucls_data[] <- lapply(ucls_data, as.character)
    #printing table
    ucls_data
  })
  
  ## creating re-formatted UCLS data and printing it 
  output$ucls_file <- renderTable({
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    
    # Read the original data
    ucls_data <- readxl::read_excel(inFile2$datapath)
    
    #renaming variables so they can be referenced
    new_names_ucls <- sub(" ", "_", names(ucls_data))
    names(ucls_data) <- new_names_ucls
    rm(new_names_ucls)
    
    #renaming relevant columns
    ucls_data <- ucls_data %>% rename(A_Number = `A-Number`)
    
    #making A# an integer rather than double 
    ucls_data$A_Number <- as.integer(ucls_data$A_Number)
    
    #making UCLS DOB type character so it can be compared
    ucls_data[] <- lapply(ucls_data, as.character)
    
    #Adding a ",NA" to Languages that do not have a listed secondary language
    p<-0
    for (p in 1:length(ucls_data$Languages)) {
      if(FALSE == grepl(",",ucls_data$Languages[p])) {
        ucls_data$Languages[p] <- paste(ucls_data$Languages[p],",NA")
      }
    }
    
    #Removing whitespace in Languages
    ucls_data$Languages<- gsub(" ", "", ucls_data$Languages, fixed = TRUE)
    
    #entering NA characters for NA values, so that code can run with missing cells
    ucls_data[is.na(ucls_data)] <- as.character("NA")
    
    #updating shared data
    shared_data$ucls_data <- ucls_data
    
    #print
    shared_data$ucls_data
  })
  
  
  ## Formatting UC Portal Data
  output$ucportal_data <- renderTable({
    
    inFile3 <- input$file3
    if (is.null(inFile3))
      return(NULL)
    
    # Read the original data
    ucportal_data <- readxl::read_excel(inFile3$datapath)
    
    #renaming variables so they can be referenced
    new_names_uc <- sub(" ", "_", names(ucportal_data))
    names(ucportal_data) <- new_names_uc
    rm(new_names_uc)
    
    ucportal_data[] <- lapply(ucportal_data, as.character)
    
    #making UCportal DOB type date so it can be compared
    #ucportal_data$DOB <- as.Date(ucportal_data$DOB, format = "%y-%m-%d")
    
    #renaming relevant columns
    ucportal_data <- ucportal_data %>% rename(A_Number = `A_#`,
                                              Nationality = COB)
    
    #only selecting relevant data
    ucportal_data <- ucportal_data %>% select(A_Number, First_Name, Last_Name, DOB, Nationality)
    
    #entering NA characters for NA values, so that code can run with missing cells
    ucportal_data[is.na(ucportal_data)] <- as.character("NA")
    
    #puting data into shared dataframe
    shared_data$ucportal_data <- ucportal_data
    
    #printing
    shared_data$ucportal_data
  })
  
  ## Checking UCLS Duplicates and Printing Message
  output$ucls_duplicate_check <- renderText({
    inFile2 <- input$file2
    
    #if there is a file inputed, check duplicates
    if (is.null(inFile2) == FALSE) {
      if(!any(duplicated(shared_data$ucls_data$A_Number))) {
        print("No A# duplicates in UCLS")
      } else {
        print("There are A# duplicates in UCLS... Please Resolve")
    }
    }
  })
  
  ##  Printing Original UC Portal file
  output$og_file3 <- renderTable({
    #checking file
    inFile3 <- input$file3
    if (is.null(inFile3))
      return(NULL)
    #printing
    #reading file
    uc_portal_data <- readxl::read_excel(inFile3$datapath)
    #making all variables characters so dates don't bug out
    uc_portal_data[] <- lapply(uc_portal_data, as.character)
    #printing table
    uc_portal_data
  })
  
  #Discrepancy table between justice server and ucls
  output$disc_table_js_ucls <- renderTable({
    
    #reset counters
    count1 <- 0 #counts js rows
    count2 <- 0 #counts ucls rows
    count3 <- 0 #counts number of discrepancies b/w js and ucls
    
    #creating df for discrepancies_js_ucls
    discrepancies_js_ucls <- data.frame(A_Number =  "0",
                                        variable = "0",
                                        js_entry = "0",
                                        ucls_entry = "0")
    
    ##double for loop iterates through both JS and UCLS, and compares the rows when the A#s match
    for (count1 in 1:length(shared_data$js_data$A_Number)) {
      for (count2 in 1:length(shared_data$ucls_data$A_Number)) {
        if (shared_data$js_data$A_Number[count1] == shared_data$ucls_data$A_Number[count2]) {
          
          ##Comparing First Name
          if (shared_data$js_data$First_Name[count1] == shared_data$ucls_data$First_Name[count2]) {
            #print("First Name Matches")
          } else {
            #print(paste(shared_data$js_data$A_Number[i],"Disparity:","JS First Name:",shared_data$js_data$First_Name[i],"UCLS First Name:",shared_data$ucls_data$First_Name[j]))
            discrepancies_js_ucls <- rbind(discrepancies_js_ucls, list(shared_data$js_data$A_Number[count1],
                                                                       paste("First_Name"),
                                                                       shared_data$js_data$First_Name[count1],
                                                                       shared_data$ucls_data$First_Name[count2]))
            count3 <- count3+1
          }
          
          
          ##Comparing Last Name
          if (shared_data$js_data$Last_Name[count1] == shared_data$ucls_data$Last_Name[count2]) {
            #print("Last Name Matches")
          } else {
            #print(paste(shared_data$js_data$A_Number[i],"Disparity:","JS Last Name:",shared_data$js_data$Last_Name[i],"UCLS Last Name:",shared_data$ucls_data$Last_Name[j]))
            discrepancies_js_ucls <- rbind(discrepancies_js_ucls, list(shared_data$js_data$A_Number[count1],
                                                                       paste("Last_Name"),
                                                                       shared_data$js_data$Last_Name[count1],
                                                                       shared_data$ucls_data$Last_Name[count2]))
            count3 <- count3+1
          }
          
          ##Comparing Data of Birth (DOB)
          if (shared_data$js_data$DOB[count1] == shared_data$ucls_data$DOB[count2]) {
            #print("DOB Matches")
          } else {
            #print(paste(shared_data$js_data$A_Number[i],"Disparity:","JS DOB:",shared_data$js_data$DOB[i],", UCLS DOB:",shared_data$ucls_data$DOB[j]))
            discrepancies_js_ucls <- rbind(discrepancies_js_ucls, list(shared_data$js_data$A_Number[count1],
                                                                       paste("DOB"),
                                                                       shared_data$js_data$DOB[count1],
                                                                       shared_data$ucls_data$DOB[count2]))
            count3 <- count3+1
          }
          
          ##Comparing Gender
          if (shared_data$js_data$Gender[count1] == shared_data$ucls_data$Gender[count2]) {
            #print("Gender Matches")
          } else {
            #print(paste(shared_data$js_data$A_Number[i],"Disparity:","JS Gender:",shared_data$js_data$Gender[i],"UCLS Gender:",shared_data$ucls_data$Gender[j]))
            discrepancies_js_ucls <- rbind(discrepancies_js_ucls, list(shared_data$js_data$A_Number[count1],
                                                                       paste("Gender"),
                                                                       shared_data$js_data$Gender[count1],
                                                                       shared_data$ucls_data$Gender[count2]))
            count3 <- count3+1
          }
          
          ##Comparing Nationality
          if (shared_data$js_data$Nationality[count1] == shared_data$ucls_data$Nationality[count2]) {
            #print("Nationality Matches")
          } else {
            #print(paste(shared_data$js_data$A_Number[i],"Disparity:","JS Nationality:",shared_data$js_data$Nationality[i],"UCLS Nationality:",shared_data$ucls_data$Nationality[j]))
            discrepancies_js_ucls <- rbind(discrepancies_js_ucls, list(shared_data$js_data$A_Number[count1],
                                                                       paste("Nationality"),
                                                                       shared_data$js_data$Nationality[count1],
                                                                       shared_data$ucls_data$Nationality[count2]))
            count3 <- count3+1
          }
          
          ##Comparing Languages
          if (shared_data$js_data$Languages[count1] == shared_data$ucls_data$Languages[count2]) {
            #print("Languages Match")
          } else {
            #print(paste(shared_data$js_data$A_Number[i],"Disparity:","JS Languages:",shared_data$js_data$Languages[i],"UCLS Languages:",shared_data$ucls_data$Languages[j]))
            discrepancies_js_ucls <- rbind(discrepancies_js_ucls, list(shared_data$js_data$A_Number[count1],
                                                                       paste("Languages"),
                                                                       shared_data$js_data$Languages[count1],
                                                                       shared_data$ucls_data$Languages[count2]))
            count3 <- count3+1
          }
        }
      }
    }
    discrepancies_js_ucls <- discrepancies_js_ucls[-c(1), ]
    discrepancies_js_ucls
  })
  
 #discrepancy table b/w ucls and uc portal
 output$disc_table_ucls_ucportal <- renderTable({
   
   #reset counters
   i <- 0 #counts js rows
   j <- 0 #counts ucls rows
   p <- 0 #counts descrepancies_js_ucls
   
   #creating df for descrepancies_js_ucls
   descrepancies_ucportal_ucls <- data.frame(A_Number =  "0",
                                             variable = "0",
                                             ucportal_entry = "0",
                                             ucls_entry = "0")
   
   ##double for loop iterates through both JS and UCLS, and compares the rows when the A#s match
   for (i in 1:length(shared_data$ucportal_data$A_Number)) {
     for (j in 1:length(shared_data$ucls_data$A_Number)) {
       if (shared_data$ucportal_data$A_Number[i] == shared_data$ucls_data$A_Number[j]) {
         
         ##Comparing First Name
         if (shared_data$ucportal_data$First_Name[i] == shared_data$ucls_data$First_Name[j]) {
           #print("First Name Matches")
         } else {
           #print(paste(shared_data$ucportal_data$A_Number[i],"Disparity:","ucportal First Name:",shared_data$ucportal_data$First_Name[i],"UCLS First Name:",shared_data$ucls_data$First_Name[j]))
           descrepancies_ucportal_ucls <- rbind(descrepancies_ucportal_ucls, list(shared_data$ucportal_data$A_Number[i],
                                                                                  paste("First_Name"),
                                                                                  shared_data$ucportal_data$First_Name[i],
                                                                                  shared_data$ucls_data$First_Name[j]))
           p <- p+1
         }
         
         ##Comparing Last Name
         if (shared_data$ucportal_data$Last_Name[i] == shared_data$ucls_data$Last_Name[j]) {
           #print("Last Name Matches")
         } else {
           #print(paste(shared_data$ucportal_data$A_Number[i],"Disparity:","ucportal Last Name:",shared_data$ucportal_data$Last_Name[i],"UCLS Last Name:",shared_data$ucls_data$Last_Name[j]))
           descrepancies_ucportal_ucls <- rbind(descrepancies_ucportal_ucls, list(shared_data$ucportal_data$A_Number[i],
                                                                                  paste("Last_Name"),
                                                                                  shared_data$ucportal_data$Last_Name[i],
                                                                                  shared_data$ucls_data$Last_Name[j]))
           p <- p+1
         }
         
         ##Comparing Data of Birth (DOB)
         if (shared_data$ucportal_data$DOB[i] == shared_data$ucls_data$DOB[j]) {
           #print("DOB Matches")
         } else {
           #print(paste(shared_data$ucportal_data$A_Number[i],"Disparity:","ucportal DOB:",shared_data$ucportal_data$DOB[i],", UCLS DOB:",shared_data$ucls_data$DOB[j]))
           descrepancies_ucportal_ucls <- rbind(descrepancies_ucportal_ucls, list(shared_data$ucportal_data$A_Number[i],
                                                                                  paste("DOB"),
                                                                                  shared_data$ucportal_data$DOB[i],
                                                                                  shared_data$ucls_data$DOB[j]))
           p <- p+1
         }
         
         ##Comparing Gender
         #if (shared_data$ucportal_data$Gender[i] == shared_data$ucls_data$Gender[j]) {
         #print("Gender Matches")
         #} else {
         #print(paste(shared_data$ucportal_data$A_Number[i],"Disparity:","ucportal Gender:",shared_data$ucportal_data$Gender[i],"UCLS Gender:",shared_data$ucls_data$Gender[j]))
         #  descrepancies_ucportal_ucls <- rbind(descrepancies_ucportal_ucls, list(shared_data$ucportal_data$A_Number[i],shared_data$ucportal_data$Gender[i],shared_data$ucls_data$Gender[j]))
         #  p <- p+1
         #}
         
         ##Comparing Nationality
         if (shared_data$ucportal_data$Nationality[i] == shared_data$ucls_data$Nationality[j]) {
           #print("Nationality Matches")
         } else {
           #print(paste(shared_data$ucportal_data$A_Number[i],"Disparity:","ucportal Nationality:",shared_data$ucportal_data$Nationality[i],"UCLS Nationality:",shared_data$ucls_data$Nationality[j]))
           descrepancies_ucportal_ucls <- rbind(descrepancies_ucportal_ucls, list(shared_data$ucportal_data$A_Number[i],
                                                                                  paste("Nationality"),
                                                                                  shared_data$ucportal_data$Nationality[i],
                                                                                  shared_data$ucls_data$Nationality[j]))
           p <- p+1
         }
         
         
       }
     }
   }
   
   #remove row that initialized df
   descrepancies_ucportal_ucls <- descrepancies_ucportal_ucls[-c(1), ]
   
   #printing table
   descrepancies_ucportal_ucls
 })
 
 
 #count the number of rows in js
 output$numrows_js <- renderText({
   inFile <- input$file1
   
   if (is.null(inFile) == FALSE) 
     paste("Number of rows: ",nrow(shared_data$js_data))
 })
 
 #count number of rows in ucls
 output$numrows_ucls <- renderText({
   inFile2 <- input$file2
   
   if (is.null(inFile2) == FALSE) 
     paste("Number of rows: ",nrow(shared_data$ucls_data))
 })
 
 #if rownum is different, print message
 output$numrows_js_ucls <- renderText({
   inFile2 <- input$file2
   
   if (is.null(inFile2) == FALSE) {
      if (nrow(shared_data$js_data) == nrow(shared_data$ucls_data)) 
          print("Same row number as JS")
      else
          print("Different row number then JS")
   }
 })

 #count number of rows in ucportal
 output$numrows_ucportal <- renderText({
   inFile3 <- input$file3
   
   if (is.null(inFile3) == FALSE) 
     paste("Number of rows: ",nrow(shared_data$ucportal_data))
 })
  

 #if rownum is different, print message
 output$numrows_ucls_ucportal <- renderText({
   inFile3 <- input$file3
   
   if (is.null(inFile3) == FALSE) {
     if (nrow(shared_data$ucls_data) == nrow(shared_data$ucportal_data)) 
       print("Same row number as UCLS")
     else
       print("Different row number then UCLS")
   }
 })
 
 
 
 
 
 
  
  
  
}

shinyApp(ui, server)




