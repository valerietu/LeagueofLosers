library(shiny)
library(jsonlite)
library(stringr)
library(dplyr)
library(httr)
library(xtable)
library(DT)

#pull data
data <- fromJSON("http://ddragon.leagueoflegends.com/cdn/11.17.1/data/en_US/champion.json")

#test to see if the data actually got read
#data%>%glimpse()

champion_data <- data$data

champion_names <- data.frame(names(champion_data))



library(data.table)
library(DT)

#turn the list of lists of lists into one list and then as a data frame
pivotval <- function(x){
    as.data.frame(t(unlist(x)))
}
champion.df.list = lapply(champion_data, pivotval)
champion.df = rbindlist(champion.df.list, fill=TRUE)

###renaming columns for better descriptions
champion.df = champion.df %>% rename(attack.power = info.attack)
champion.df = champion.df %>% rename(defense.power = info.defense)
champion.df = champion.df %>% rename(magic.power = info.magic)
champion.df = champion.df %>% rename(player.difficulty = info.difficulty)
champion.df = champion.df %>% rename(base.health = stats.hp)
champion.df = champion.df %>% rename(base.mana = stats.mp)

champion.info = champion.df[,c(4, 5, 7, 8, 9, 10, 21, 23, 25, 26, 28, 37, 40)]





#next tab for statistics
library(plotly)
library(ggplot2)
library(stringr)
str_which(colnames(champion.df), "stats")
# it's 21 through 40 that are numeric vars, just remember that and move along, I got bogged down
champion.stats = champion.df[,21:40]
champion.levels = champion.df[,7:10]
champion.stats = apply(champion.stats, 2, as.numeric)
champion.levels = apply(champion.levels, 2, as.numeric)
champion.df2 = cbind(champion.df[,1:6], champion.levels, champion.df[,11:20], champion.stats)

#test that we got the numerics sorted out:
mean(as.numeric(champion.df$stats.attackspeed)) #whatever, it works, just roll


####testing to see if plot_lys work

# Attack = champion.df2 %>%
#   plot_ly(x = ~attack.power,
#           y = ~player.difficulty,
#           color = ~tags1, text = ~id, type = 'scatter', mode = "markers")%>% layout(legend=list(title=list(text='<b> Champion Type </b>')))
# 
# 
# Attack
# 
# ######
# Defense = champion.df2 %>%
#   plot_ly(x = ~defense.power,
#           y = ~player.difficulty,
#           color = ~tags1, text = ~id, type = 'scatter', mode = "markers")%>% layout(legend=list(title=list(text='<b> Champion Type </b>')))
# 
# Defense
# 
# ########
# Magic = champion.df2 %>%
#   plot_ly(x = ~magic.power,
#           y = ~player.difficulty,
#           color = ~tags1, text = ~id, type = 'scatter', mode = "markers")%>% layout(legend=list(title=list(text='<b> Champion Type </b>')))
# 
# Magic




ui <- fluidPage( title = "League of Legends Champions",
                 # controls the layout and content of the application
                 
                 tabsetPanel(
                     tabPanel(title = "Champion", 
                              fluidRow(
                                  column(8, 
                                         h1("League of Legend Champions Selector"),
                                         h2("Find out information and stats on your favorite champion"),
                                         h3("Search champion by name"),
                                         selectInput("nom", "Name",c("", champion.info$name)), 
                                         
                                         actionButton("search", "Search")),
                                  
                                  
                                  column(12, 
                                         tableOutput("one.champ")),
                                  br(),
                                  
                                  
                                  
                                  
                              ),
                              fluidRow(
                                  column(8,
                                         h3("Or browse manually through all champions here!")),
                                  h5("please note that Akshan, Lilia, and Rell are new champions, so their statistics have not been evaluated yet.")
                              ),
                              DT::dataTableOutput("champion.info")
                     ),
                     
                     #####stats tab
                     tabPanel(title = "Power Levels Versus Difficulty",
                              fluidRow(
                                  column(8,
                                         selectInput("choice", "Select power type to compare champions' power levels and their difficulty", choices = names(champion.df2[,c (7, 8, 9)])), 
                                         actionButton("search", "Search"),    
                                         h3("You can use the plot to see what type of champion you would like to play based off of their stats, and how hard they are to play! (this is super useful for beginner players)"),
                                         h4("We can see super cool patterns in the data, such as tanks having relatively low attack power, but high defense."),
                                         plotlyOutput("test_plot"))
                              )
                     ),
                     
                     
                     #####stats tab
                     tabPanel(title = "Compare Different Power Levels",
                              fluidRow(
                                  column(8,
                                         selectInput("choice2", "Select power type to compare champions' different power levels", choices = names(champion.df2[,c (7, 8, 9)])), 
                                         actionButton("search", "Search"),    
                                         
                                         selectInput("choice3", "Select second power type", choices = names(champion.df2[,c (7, 8, 9)])), 
                                         actionButton("search", "Search"), 
                                         h4("This is particularly helpful if you want to choose a champion based off of their stats regardless of difficulty. For example, do you want someone who is the best of both worlds and is strong in both magic and attack power? Diana is the champ for you"),
                                         plotlyOutput("test_plot2"))
                              )
                     )
                     
                 )
                 
                 
                 
)


server <- function(input, output, session) {
    # controls the interaction, modify output based on user input
    rv = reactiveValues(data=NULL)
    
    #results from search
    observeEvent(input$search, {
        req(input$nom !="", cancelOutput = TRUE)
        rv$data = champion.info[which(champion.info$name == input$nom),]
    })
    
    
    #output basic info for the champion  
    output$one.champ = renderTable({
        rv$data[1]
    }, caption = "Basic stats:",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    
    
    output$champion.info = DT::renderDataTable({champion.info})
    
    
    
    ###stats tab
    
    output$test_plot <- renderPlotly(plot_ly(champion.df2, x = ~get(input$choice),
                                             y = ~player.difficulty,
                                             color = ~tags1, text = ~id, type = 'scatter', mode = "markers")%>% layout(legend=list(title=list(text='<b> Champion Type </b>')), xaxis = list(title = input$choice)))
    
    #### Compare Tab
    output$test_plot2 <- renderPlotly(plot_ly(champion.df2, x = ~get(input$choice2),
                                              y = ~get(input$choice3),
                                              color = ~tags1, text = ~id, type = 'scatter', mode = "markers")%>% layout(legend=list(title=list(text='<b> Champion Type </b>')), xaxis = list(title = input$choice2), yaxis = list(title = input$choice3)))
    
}

# run the application 
shinyApp(ui = ui, server = server)
