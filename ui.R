################################################################################################
##
##  This is the Shiny ui.R file that is used for a gymnastics scoring app.
##
##  This script does the following:
##      - Reads a website (www.meetscoresonline.com) and parses the html code.
##      - Loads the parsed data and creates a data frame that contains the following:
##          -   meet:   Name of the gymnastics meet
##          -   gender: Gender for the participants in the gymnastics meet
##          -   state:  State for where the gymnastics meet was completed.
##          -   V1:     Code that specifies the link to the scoring website.
##          -   site:   Website that contains the scoring information for the gynmastics meet.
##      - Allows the user to use a drop down box to select the state for thy gymnastics meet.
##      - Outputs the following:
##          -   Report showing all of the gymnastics meets for men and women by state.
##          -   Plots showing the distribution of meets for men and women by state.
##
################################################################################################

##  Load the following packages to manipulate data as needed.
    
    library(shiny)
    library(shinydashboard)
    library(leaflet)
    library(rvest)
    library(dplyr)
    library(ggplot2)
    library(XML)
    library(stringr)
    library(tidyr)
    
##  Load and store the url of the meetscoresonline.com website

    my_url <- "http://www.meetscoresonline.com/Results"

##  Parse the html code and use xml xpath functions to extract the desired fields.   

    webpage <- htmlParse(my_url)
    webroot <- xmlRoot(webpage)
    
    meet_list <- xpathSApply(webroot, "//table/..//tr", xmlGetAttr,"data-filter-by")
    meet_df <- do.call(rbind, meet_list) %>% as.data.frame()
    meet_df <- data.frame(lapply(meet_df, trimws), stringsAsFactors = FALSE)
    names(meet_df) <- c("meet")

##  Create new dataframe that extracts the gender and state for the meet name
    
    meet_df <- mutate(meet_df, 
                    gender = substr(meet,str_length(meet)-2,str_length(meet)),
                    state=substr(meet,str_length(meet)-6,str_length(meet)-5))
    
##  Extract the states for the gymnastics meets which will be used as input to the drop down list 
##  for the user to select.
    
    meet_states <- unique(meet_df$state) %>%
                      sort(meet_df$state, decreasing = FALSE) 
    
## Parse the html code to find the website links containing the scores for each individual meet.  

    test_list <- as.list(xpathSApply(webroot, "//table/..//a",xmlGetAttr,"href"))
    test_df <- do.call(rbind, test_list) %>% as.data.frame()
    lst <- lapply(test_df, grep, pattern="/R", value=TRUE)
    combined_df <- cbind(meet_df, lst)
    combined_df <- mutate(combined_df,
                          site=paste("www.meetscoresonline.com",V1,sep=""))

##  ShinyUI Code
##  Create a shiny dashboard with header, sidebar, and body
##  The format will consist of two tables stacked veritcally and two plots stacked vertically.
    
    shinyUI(dashboardPage(
        dashboardHeader(title = "USAG Gymnastics Meets", 
                        dropdownMenu(
                            type = "notifications", 
                            icon = icon("question-circle"),
                            badgeStatus = NULL,
                            headerText = "See also:",
                            
                            notificationItem("help", icon = icon("file"),
                                             href = "https://htmlpreview.github.io/?https://github.com/jjoyce1000/Shiny_App/blob/master/Gymnastics_Scoring.html")
                        )
                        ),
            dashboardSidebar(
                selectInput("state", "State:",meet_states)
            ),
        dashboardBody(fluidRow
                     (
                         box(width = 6,
                             h3("USAG Gymnastics Meets Report - Women"),
                             tableOutput("report_women")
                         ),
                         
                         box(width = 6,
                             h3("Distribution of Meets by State"),
                             plotOutput("plot_women")
                         )
                     ),
                     fluidRow
                     (    
                         box(width = 6,
                             h3("USAG Gymnastics Meets Report - Men"),
                             tableOutput("report_men")
                         ),
                         box(width = 6,
                             h3("Distribution of Meets by State"),
                             plotOutput("plot_men")
                         )
                         
                     )  
        )
        )
        
    )
