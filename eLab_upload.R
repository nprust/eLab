
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Webform"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      #Input: add access token
      textInput("token", label = h3("Enter your access token"), value = "Enter text..."),
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))
      
      ,
      # Input: Select a file ----
      fileInput("file1", "Choose txt File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
      ,
      actionButton("update", "Update")
    ), 
    
    
    # Main panel for displaying outputs ----
    mainPanel(h1("eLab webform import"),
              p(strong("Upload the webform as .txt file. The app will then check if a respecitive project and study already exists
                       withing eLab, otherwise it will creat the respective study and or project. 
                       The added experiment will atomatically numbered based on the last existing experiment.
                       For this import the standard template will be used")),
              
              # Output: Data file ----
              tabsetPanel(type = "tabs",
                          tabPanel("Projects",  tableOutput("Project.overview")),
                          tabPanel("Studies", tableOutput("Study.overview")),
                          tabPanel("Experiments",tableOutput("Experiment.overview")),
                          tabPanel("Update", tableOutput("update.overview" ))
                          
              )
              
              
              
    )
    
  )
)



# Define server logic to read selected file ----
server <- function(input, output) {
  if(!require(shiny)){
    install.packages("shiny")
    library(shiny)
  }
  if(!require(httr)){
    install.packages("httr")
    library(httr)
  }
  if(!require(jsonlite)){
    install.packages("jsonlite")
    library(jsonlite)
  }
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  if(!require(tidyr)){
    install.packages("tidyr")
    library(tidyr)
  }
  if(!require(magrittr)){
    install.packages("magrittr")
    library(magrittr)
  }
  
  
  
  output$value <- renderPrint({ as.character(paste("Bearer", input$token, sep = " ")) })
  
  
  
  
  ##functions
  #add experiment
  add.experiment <- function(study_identifier, new_exp_name, api_key){
    
    experiment.info <-'{  
       "studyID": "study.name",  
       "name": "new.exp",  
       "status": "PENDING",  
       "templateID": "615658",  
       "autoCollaborate": true  
     }'
    
    experiment.info <-gsub("study.name", study_identifier, experiment.info)
    experiment.info <- gsub("new.exp", new_exp_name, experiment.info)
    
    a<- POST("https://emc.elabjournal.com/api/v1/experiments", 
             add_headers(.headers = c(Host = "www.elabjournal.com", 
                                      `User-Agent` = "libcurl/7.64.1 r-curl/4.3.2 httr/1.4.3",
                                      Authorization = api_key,
                                      `Content-Type` = "application/json; charset=utf-8")),
             body = experiment.info, encode = "raw")
    
    
  }
  
  
  #add study 
  
  add.study <- function(project_ID, study_name, study, api_key){
    
    study.info <- '{
  "studyID": 0,
  "projectID": "pID",
  "groupID": 0,
  "subgroupID": 0,
  "userID": 44482,
  "name": "Study.name",
  "statusChanged": "2022-07-05T06:45:50.535Z",
  "description": "All projects from Study.name",
  "notes": "",
  "approve": "NOTREQUIRED",
  "created": "2022-07-05T06:45:50.535Z",
  "deleted": true
}'
    
    study.info <- gsub("pID", project_ID, study.info)
    study.info <- gsub("Study.name", study_name, study.info)
    
    
    a<-  POST("https://emc.elabjournal.com/api/v1/studies",
              add_headers(.headers = c(Host = "www.elabjournal.com", 
                                       `User-Agent` = "libcurl/7.64.1 r-curl/4.3.2 httr/1.4.3",
                                       Authorization = api_key,
                                       `Content-Type` = "application/json; charset=utf-8")),
              body = study.info, encode = "raw") %>% content(.) %>% as.data.frame(.) %>% 
      select(., "studyID", "name") %>% set_names(c("StudyID", "Name"))
    
    if(!is.null(study)) {
      IDs <-  rbind(study,a)
      return(IDs)
    } else {
      
      return(a)
      
    }
    
  }
  
  #add project
  
  add.projects <-function(pi, project_name,institute, department, projects, api_key){
    
    project.info <- '{  
  "name": "Name",   
  "longname": "project.name",  
  "description": "All projects from PI.name group",  
  "notes": "",  
  "label": [  
    "lable1",
    "lable2", 
  ],  
  "projectMeta": [  
    {  
      "name": "meta",  
      "value": "",  
      "metatype": "TEXT",  
    }  
  ]  
}'
    
    
    project.info <- gsub("Name", pi, project.info)
    project.info <- gsub("project.name", project_name, project.info)
    project.info <- gsub("PI.name", pi, project.info)
    project.info <- gsub("label1", institute, project.info)
    project.info <- gsub("label2", department, project.info)
    
    
    a<-  POST("https://emc.elabjournal.com/api/v1/projects",
              add_headers(.headers = c(Host = "www.elabjournal.com", 
                                       `User-Agent` = "libcurl/7.64.1 r-curl/4.3.2 httr/1.4.3",
                                       Authorization = api_key,
                                       `Content-Type` = "application/json; charset=utf-8")),
              body = project.info, encode = "raw") %>% content(.) %>% as.data.frame(.) %>% 
      select(., "projectID",  "name", "longName") %>% set_names(c("projectID", "Name", "ProjectName"))
    
    
    if(!is.null(projects)) {
      IDs <-  rbind(projects, a)
      return(IDs)
    } else {
      
      return(a)
    }
  }
  
  inputdata <- function(IN, out){
    a <- read.csv(IN,
                  header = TRUE,
                  sep = "\t") %>% set_names(c("From.", "Input")) %>%  slice(15:n()) %>%
      separate(From., c("Query", "Input"), sep = ":") %>% mutate(Query = as.character(Query)) %>%
      filter(., Query == "ACADEMY OR INDUSTRY" | Query == "PI LAST NAME" | Query == "PI PREFIX" |
               Query == "PI FIRST NAME" |Query == "FIRST NAME" | Query == "LAST NAME" | 
               Query =="INSTITUTE OR COMPANY NAME" |Query == "DEPARTMENT" ) %>% select(Query, Input) %>%
      mutate(Input = gsub(" ", "", Input)) %>%
      rbind(cbind(Query = "PI", Input = paste(.[.$Query == "PI FIRST NAME",2],
                                              .[.$Query == "PI PREFIX",2], 
                                              .[.$Query == "PI LAST NAME",2], sep = " "))) %>%
      rbind(cbind(Query = "NAME", Input = paste(.[.$Query == "FIRST NAME",2],
                                                .[.$Query == "PREFIX",2], 
                                                .[.$Query == "LAST NAME",2], sep = " "))) %>%
      rbind(cbind(Query = "ProjectName", Input = paste(.[.$Query == "PI LAST NAME",2],
                                                       .[.$Query == "INSTITUTE OR COMPANY NAME",2], sep = "-"))) %>%  
      mutate(Input =gsub("  ", " ", Input)) %>% filter(., Query == out) %>% select(Input)
    
    
    return(as.character(a))
    
  }
  
  current_projects<- function(api_key){
    current.projects.list<- GET("https://emc.elabjournal.com/api/v1/projects",
                                
                                
                                add_headers(.headers =c(Host = "www.elabjournal.com", 
                                                        `User-Agent` = "libcurl/7.64.1 r-curl/4.3.2 httr/1.4.3",
                                                        Authorization = api_key,
                                                        `Content-Type` = "application/json; charset=utf-8"
                                                        
                                ))) %>% content(.) %>% .$data
    
    current.projects <- data.frame() 
    for (i in 1:length(current.projects.list)) {
      current.projects <- rbind.data.frame(current.projects,
                                           cbind( current.projects.list[[i]][["projectID" ]], 
                                                  current.projects.list[[i]][["name" ]],
                                                  current.projects.list[[i]][["longName" ]]))
    }
    
    current.projects %<>% set_names(c("projectID", "Name", "ProjectName"))
    
    return(current.projects)
  }
  
  current_studies<- function(api_key){
    
    current.studies.list<- GET("https://emc.elabjournal.com/api/v1/studies",
                               
                               
                               add_headers(.headers = c(Host = "www.elabjournal.com", 
                                                        `User-Agent` = "libcurl/7.64.1 r-curl/4.3.2 httr/1.4.3",
                                                        Authorization = api_key,
                                                        `Content-Type` = "application/json; charset=utf-8"
                                                        
                               ))) %>% content(.) %>% .$data
    
    current.studies <- data.frame() 
    for (i in 1:length(current.studies.list)) {
      current.studies <- rbind.data.frame(current.studies,
                                          cbind( current.studies.list[[i]][["studyID" ]], 
                                                 current.studies.list[[i]][["name" ]]))
    }
    
    current.studies %<>% set_names(c("StudyID", "Name"))
    
    return(current.studies)
  }
  
  current.experiment <- function(api_key){
    
    
    
    current.experiment.list <- GET("https://emc.elabjournal.com/api/v1/experiments",
                                   
                                   add_headers(.headers =c(Host = "www.elabjournal.com", 
                                                           `User-Agent` = "libcurl/7.64.1 r-curl/4.3.2 httr/1.4.3",
                                                           Authorization = api_key,
                                                           `Content-Type` = "application/json; charset=utf-8"
                                                           
                                   ))) %>% content(.) %>% .$data
    
    Exp.IDs <- data.frame() 
    for (i in 1:length(current.experiment.list)) {
      Exp.IDs <- rbind.data.frame(Exp.IDs,
                                  cbind( current.experiment.list[[i]][["experimentID" ]],
                                         current.experiment.list[[i]][["name" ]]))
      
    }
    colnames(Exp.IDs) <- c("Exp.IDs", "Exp.name")
    
    Exp.IDs %<>% arrange(desc(Exp.name))
    return(Exp.IDs)
    
  }
  
  get.new.exp.id <- function(new_exp_name, api_key){
    
    
    Exp.search.URL <- "https://emc.elabjournal.com/api/v1/experiments?searchName="
    get.exp <- paste(Exp.search.URL, new_exp_name, sep="")
    
    new.exp <- GET(get.exp,
                   
                   add_headers(.headers =c(Host = "www.elabjournal.com", 
                                           `User-Agent` = "libcurl/7.64.1 r-curl/4.3.2 httr/1.4.3",
                                           Authorization = api_key,
                                           `Content-Type` = "application/json; charset=utf-8"
                                           
                   ))) %>% content(.) %>% .$data
    a <- cbind("Type" = 'Experiment', "ID" = new.exp[[1]][["experimentID" ]],
               "Name"= new.exp[[1]][["name" ]])
    return(a)
    
  }
  
  
  Project_search <- function(Project_Name,api_key){
    Project.search.URL <- "https://emc.elabjournal.com/api/v1/projects?searchName="
    get.project <- paste(Project.search.URL, Project_Name, sep="") #fill in the search term for Project.name
    
    project.status<- GET(get.project,
                         
                         add_headers(.headers = c(Host = "www.elabjournal.com", 
                                                  `User-Agent` = "libcurl/7.64.1 r-curl/4.3.2 httr/1.4.3",
                                                  Authorization = api_key,
                                                  `Content-Type` = "application/json; charset=utf-8"
                                                  
                         ))) %>% content(.) %>% .$data
    project.status[[1]] %>% .[["projectID"]] %>% as.character(.)
    
    return(project.status)
    
    
  } 
  
  
  Study_search <- function(Study.name, api_key){
    
    
    Study.search.URL <- "https://emc.elabjournal.com/api/v1/studies?searchName="
    get.study <- paste(Study.search.URL, Study.name, sep="") #fill in the search term for Study.name
    
    
    
    info<-  GET(get.study,
                
                add_headers(.headers = c(Host = "www.elabjournal.com", 
                                         `User-Agent` = "libcurl/7.64.1 r-curl/4.3.2 httr/1.4.3",
                                         Authorization = api_key,
                                         `Content-Type` = "application/json; charset=utf-8"
                                         
                ))) %>% content(.) %>% .$data 
    if(length(info) != 0) { info[[1]] %>% .[["studyID"]] %>% as.character(.)}
    
    return(info)
    
  }
  
  
  
  
  ##current inventory
  
  observeEvent(input[["file1"]], {
    
    api.key <- paste("Bearer", input$token, sep = " ")
    
    
    
    ProjectName <-inputdata(input$file1$datapath, "ProjectName")
    PI <- inputdata( input$file1$datapath, "PI")
    NAME <- inputdata( input$file1$datapath, "NAME")
    NAME.search <- inputdata(input$file1$datapath, "NAME") %>% gsub(" ", "%20", .)
    Institute <- inputdata(input$file1$datapath, "INSTITUTE OR COMPANY NAME")
    Department <- inputdata(input$file1$datapath, "DEPARTMENT")
    client.type <- inputdata(input$file1$datapath, "ACADEMY OR INDUSTRY")
    
    
    current.eLab.projects <- current_projects(api.key)
    
    
    current.elab.studies <- current_studies(api.key)
    
    
    Experiment.IDs <-current.experiment(api.key)
    
    Acedemy <- "Academy" %in% client.type
    Industry <- "Industry" %in% client.type
    CRO <- "CRO" %in% client.type
    Other <-  "Other" %in% client.type
    test.project <- ProjectName %in% current.eLab.projects$ProjectName
    test.study <- NAME %in% current.elab.studies$Name
    
    new.exp.name <- as.character(Experiment.IDs %>%  filter(., grepl("^[[:digit:]]+$",Exp.name) ) %>%
                                   mutate(Exp.name = as.numeric(Exp.name)) %>% 
                                   summarise(max(Exp.name)+1))           
    
    
    
    if(isTRUE(Acedemy)){
      
      #check if project exists
      if(isTRUE(test.project)) { 
        project.ID <- current.eLab.projects[current.eLab.projects$ProjectName %in% ProjectName, "projectID"] 
        
        #check if study already exists
        if(isTRUE(test.study)){
          study.identifier <- current.elab.studies[current.elab.studies$Name %in% NAME, "StudyID"]
          #if Study already exists, add experiment
          
          Experiment.IDs <-add.experiment(study.identifier,new.exp.name, api.key)
          
        }else{#create study
          
          project.identifier <-as.character(current.eLab.projects[current.eLab.projects$ProjectName == ProjectName, "projectID"])
          
          
          current.elab.studies <- add.study(project_ID = project.identifier, study_name = NAME, 
                                            study = current.elab.studies, api.key)
          
          
          study.identifier <- as.character(current.elab.studies[current.elab.studies$Name == NAME, "StudyID"])
          
          #add experiment to created study
          Experiment.IDs <- add.experiment( study.identifier, new.exp.name,
                                            api.key)
          
        }
        
      }else{#if project does not exist, create project
        
        current.eLab.projects <- add.projects(pi =PI,project_name =  ProjectName, institute =  Institute, 
                                              department =  Department, projects = current.eLab.projects,api_key =  api.key)
        
        #create study for new project
        project.identifier <-as.character(current.eLab.projects[current.eLab.projects$ProjectName == ProjectName, "projectID"])
        
        
        
        current.elab.studies <-add.study(project_ID = project.identifier, study_name = NAME, 
                                         study = current.elab.studies, api.key)
        
        study.identifier <-as.character(current.elab.studies[current.elab.studies$Name == NAME, "StudyID"])
        
        #add experiment to created study
        
        
        Experiment.IDs <- add.experiment( study.identifier, new.exp.name,
                                          api.key) 
        
        
      }
      
      
      
    }else if (isTRUE(Industry)){
      #get CRO project ID
      Industry.ID <- current.eLab.projects[current.eLab.projects$Name == "Industry", "projectID"]
      #check if study already exists under other projects
      if(isTRUE(test.study)) {
        NAME.Industry <- as.character(paste(NAME, Institute, sep = "-"))
        study.identifier <-current.elab.studies[current.elab.studies$Name %in% NAME.Industry, "StudyID"]
        #if Study already exists, add experiment
        
        Experiment.IDs <- add.experiment( study.identifier, new.exp.name,
                                          api.key)
      }else{
        #create study
        NAME.Industry <- as.character(paste(NAME, Institute, sep = "-"))
        
        current.elab.studies <-add.study(project_ID = Other.ID, study_name = NAME.Industry, 
                                         study = current.elab.studies, api.key)
        
        
        study.identifier <- as.character(current.elab.studies[current.elab.studies$Name %in% NAME.Industry, "StudyID"])
        
        #add experiment to created study
        
        
        Experiment.IDs <- add.experiment(study.identifier, new.exp.name,
                                         api.key) 
      }
    } else if (isTRUE(CRO)){
      #get CRO project ID
      CRO.ID <- current.eLab.projects[current.eLab.projects$Name == "CRO", "projectID"]
      #check if study already exists under other projects
      if(isTRUE(test.study)) {
        NAME.CRO <- as.character(paste(NAME, Institute, sep = "-"))
        study.identifier <-current.elab.studies[current.elab.studies$Name %in% NAME.CRO, "StudyID"]
        #if Study already exists, add experiment
        
        Experiment.IDs <- add.experiment( study.identifier, new.exp.name,
                                          api.key)
      }else{
        #create study
        NAME.CRO <- as.character(paste(NAME, Institute, sep = "-"))
        
        current.elab.studies <-add.study(project_ID = Other.ID, study_name = NAME.CRO, 
                                         study = current.elab.studies, api.key)
        
        
        study.identifier <- as.character(current.elab.studies[current.elab.studies$Name %in% NAME.CRO, "StudyID"])
        
        #add experiment to created study
        
        
        Experiment.IDs <- add.experiment(study.identifier, new.exp.name,
                                         api.key) 
        
      }
      
    } else{
      #get other project ID
      Other.ID <- current.eLab.projects[current.eLab.projects$Name == "Other", "projectID"]
      #check if study already exists under other projects
      if(isTRUE(test.study)) {
        NAME.Other <- as.character(paste(NAME, Institute, sep = "-"))
        study.identifier <-current.elab.studies[current.elab.studies$Name %in% NAME.Other, "StudyID"]
        #if Study already exists, add experiment
        
        Experiment.IDs <- add.experiment( study.identifier, new.exp.name,
                                          api.key)
      }else{
        #create study
        NAME.Other <- as.character(paste(NAME, Institute, sep = "-"))
        
        current.elab.studies <-add.study(project_ID = Other.ID, study_name = NAME.Other, 
                                         study = current.elab.studies, api.key)
        
        
        study.identifier <- as.character(current.elab.studies[current.elab.studies$Name %in% NAME.Other, "StudyID"])
        
        #add experiment to created study
        
        
        Experiment.IDs <- add.experiment(study.identifier, new.exp.name,
                                         api.key) 
        
        
        
      }
    }
    
  })
  
  ##output
  
  output$Project.overview <- renderTable({
    api.key <- paste("Bearer", input$token, sep = " ")
    Projects.update <- current_projects(api.key)
    
    
  })
  
  output$Study.overview <- renderTable({
    api.key <- paste("Bearer", input$token, sep = " ")
    study.update <- current_studies(api.key)
  })
  
  output$Experiment.overview <- renderTable({
    api.key <- paste("Bearer", input$token, sep = " ")
    Exp.update <- current.experiment(api.key)
    
  })
  
  update_overview <- eventReactive(input$update, {
    api.key <- paste("Bearer", input$token, sep = " ")
    
    
    Experiment.IDs <-current.experiment(api.key)
    
    new.exp.name <- as.character(Experiment.IDs %>%  filter(., grepl("^[[:digit:]]+$",Exp.name) ) %>%
                                   mutate(Exp.name = as.numeric(Exp.name)) %>% 
                                   summarise(max(Exp.name)))    
    
    ProjectName <-inputdata(input$file1$datapath, "ProjectName")
    NAME <- inputdata(input$file1$datapath, "NAME")
    NAME.search <- inputdata(input$file1$datapath, "NAME") %>% gsub(" ", "%20", .)
    New.exp <-as.data.frame(get.new.exp.id(new.exp.name, api.key))
    
    Study.id <- Study_search(NAME.search, api.key)
    project.ID <-Project_search(ProjectName, api.key)  
    
    updated <- rbind.data.frame(cbind("Type"= "Project", "ID" = project.ID[[1]][["projectID"]], "Name" = ProjectName),
                                cbind("Type" = "Study","ID" = Study.id[[1]][["studyID"]], 
                                      "Name" = NAME), New.exp)
    
    
  })
  
  output$update.overview <- renderTable({
    update_overview()$updated
  })
  
  
}

# Run the app ----
shinyApp(ui, server)

