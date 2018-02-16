################################################################################################
##
##  This is the Shiny server.R file that is used for a gymnastics scoring app.
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

## Parse the html code to find the website links containing the scores for each individual meet.
    
    test_list <- as.list(xpathSApply(webroot, "//table/..//a",xmlGetAttr,"href"))
    test_df <- do.call(rbind, test_list) %>% as.data.frame()
    lst <- lapply(test_df, grep, pattern="/R", value=TRUE)
    combined_df <- cbind(meet_df, lst)
    combined_df <- mutate(combined_df,
                          site=paste("www.meetscoresonline.com",V1,sep=""))

##  ShinyServer Code

shinyServer(function(input, output){
   
##  Create a reative function to generate a report for women gymnasts that is filtered
##  by the input "state" of the user in the ui.R code.
    
    report_women_react <- reactive({
        gender_code <- "wom"
        filtered_meet_df <- combined_df[which(combined_df$state==input$state & combined_df$gender==gender_code),]
        filtered_meet_df
    })
    
##  Create a reative function to generate a report for men gymnasts that is filtered
##  by the input "state" of the user in the ui.R code.

    report_men_react <- reactive({
        gender_code <- "men"
        filtered_meet_df <- combined_df[which(combined_df$state==input$state & combined_df$gender==gender_code),]
        filtered_meet_df
    })
    
##  Create a reative function to generate a plot for women gymnasts that is filtered
##  by the input "state" of the user in the ui.R code.
    
        plot_women_react <- reactive({
        by_state_gender <- data.frame(with(combined_df, table(state,gender)))
        by_gender <- spread(by_state_gender,gender,Freq)
        women <- by_gender[,c("state","wom")]
        g_women <- ggplot(data.frame(women), aes(state,wom)) + geom_bar(stat="identity", color="Red",fill="Red") +
            labs(title="Distribution of Meets by State - Women") + labs(x="State",y="# of Meets") + theme(plot.title = element_text(hjust = 0.5)) 
        g_women
    })
    
##  Create a reative function to generate a plot for men gymnasts that is filtered
##  by the input "state" of the user in the ui.R code.

    plot_men_react <- reactive({
        by_state_gender <- data.frame(with(combined_df, table(state,gender)))
        by_gender <- spread(by_state_gender,gender,Freq)
        men <- by_gender[,c("state","men")]
        g_men <- ggplot(data.frame(men), aes(state,men)) + geom_bar(stat="identity", color="Blue",fill="Blue") +
            labs(title="Distribution of Meets by State - Men") + labs(x="State",y="# of Meets") + theme(plot.title = element_text(hjust = 0.5)) 
        g_men
    })
    
## Create a reative function to render the report table for women's gymnastics results.    
    output$report_women <- renderTable({
        report_women_react()
    }) 

## Create a reative function to render the report table for men's gymnastics results. 
    output$report_men <- renderTable({
        report_men_react()
    }) 
    
## Create a reative function to render the plot showing the distribution of women's meets by state. 
    output$plot_women <- renderPlot({
        plot_women_react()
    })

## Create a reative function to render the plot showing the distribution of men's meets by state. 
    output$plot_men <- renderPlot({
        plot_men_react()
    })
    
    output$msgOutput <- renderMenu({
        dropdownMenu(type="messages", "test")
    })
})    
