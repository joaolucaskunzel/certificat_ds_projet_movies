#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages('ggridges')
library(corrplot)
library(scales)
library(FactoMineR)
library(tidyverse)
#library(ggridges)
library(ggplot2)
library(shiny)
library(tidyselect)
library(tidyverse)
library(shinyWidgets)
#
# Get Caracteristics and Method
#
final_df <- readRDS('user_summary_v3_no_ts')
Var1<-tibble(VAR = colnames(final_df))
misc<- Var1[1:5,1]
print(misc)
Var1<-Var1 %>% filter(!(VAR %in% misc$VAR))%>% 
  mutate(Type = str_split(VAR,"\\_",simplify = TRUE)[,1]) %>% 
  mutate(Car = str_split(VAR,"\\_",simplify = TRUE)[,2]) %>% 
  mutate(Name = str_split(VAR,"\\_",simplify = TRUE)[,3]) 
Genres<-Var1 %>% select(Type,Car,Name) %>% filter(Car=="genre") %>% 
  pivot_wider(names_from = Type, values_from = Car) %>%  select(Name)
Directors <- Var1 %>% select(Type,Car,Name) %>% filter(Car=="dir") %>% 
  pivot_wider(names_from = Type, values_from = Car) %>%  select(Name)
Actors<-Var1 %>% select(Type,Car,Name) %>% filter(Car=="act") %>% 
  pivot_wider(names_from = Type, values_from = Car) %>%  select(Name)
Periods <-Var1 %>% select(Type,Car,Name) %>% filter(Car=="dec") %>% 
  pivot_wider(names_from = Type, values_from = Car) %>%  select(Name)
Cal<-Var1 %>% select(Type,Name,Car) %>% 
  pivot_wider(names_from = Name, values_from = Car) %>%  select(Type)
Fmin<-min(final_df$nbr_films_watched)
Fmax<-max(final_df$nbr_films_watched)

#
# Get Film data
#
Y_ts <- readRDS('Y_ts')
Donmovies<-read.csv("C:/Users/r083485/OneDrive - Volvo Group/R/Projet/Data/ml-latest/movies.csv")
Y<-Y_ts %>% mutate("1" = rating) %>% mutate(rating=NULL)
lfilm<-colnames(Y %>% select(-userId))
namefilm<-Donmovies %>% filter(movieId %in% all_of(lfilm))
#
Doncast<-readRDS("C:/Users/r083485/OneDrive - Volvo Group/R/Projet/Doncast_large_focus")
#
#
#####################################################################################################
#
#
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Data Analyse"),
    actionButton('act', 'GO !'),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          tags$h2("Select data agregation"),
#          pickerInput("Type", "Method :", Cal ,
#                      options = list(`actions-box` = TRUE),multiple = TRUE),
#          radioGroupButtons(inputId = "Type",label = "Calculs",choices = all_of(Cal$Type)),
          checkboxGroupButtons(inputId = "Type",label = "Calculs",choices = all_of(Cal$Type), 
                               selected = all_of(Cal$Type %>% head(1)),status = "primary"),
          tags$h2("Select variables"),
          pickerInput("Gen", "Genres :", Genres,
                      options = list(`actions-box` = TRUE),multiple = TRUE),
          pickerInput("Dir", "Directors :", Directors,
                      options = list(`actions-box` = TRUE),multiple = TRUE),
          pickerInput("Act", "Actors :", Actors,
                      options = list(`actions-box` = TRUE),multiple = TRUE),
          pickerInput("Dec", "Decades :", Periods,
                      options = list(`actions-box` = TRUE),multiple = TRUE),
          materialSwitch(inputId = "carf", label = "Add selected film genres and cast", value = FALSE,  status = "primary"),
          tags$h2("Select users"),
          tags$h5("Number of watched films :"),
          numericInput("Min", "Min :", value = 1),
          numericInput("Max", "Max :", value = 30000),
          sliderInput("Rate", "Average rate :",
            min = 0, max = 5, value = c(0,5), step = seq(from = 1, to = 5, by = 0.25)),
          tags$h2("Select film"),
          pickerInput("film","",lfilm,
                      options = list(`live-search` = TRUE))
          ),
        # Show a plot of the generated distribution
        mainPanel(         
          tabsetPanel(
            tabPanel("General",
                     fluidRow(
                              column(6,plotOutput("nbf")),
                              column(6,plotOutput("old")),
                              ),
                     fluidRow(
                       column(6,plotOutput("Rate")),
                       column(6,plotOutput("bin")),
                     ),
                     fluidRow(
                       column(6,plotOutput("Sca")),
                       column(6,plotOutput("Ratenb")),
                     )
                     ),
            tabPanel("Variables",
                     fluidRow(
                       column(12,plotOutput("box")),
                       ),
                     fluidRow(style = "height:1200px",
                       column(12,plotOutput("cor")),
                     )
            ),
            tabPanel("Film", 
                     fluidRow(
                       column(12,plotOutput("F")),
                     ),
                     fluidRow(
                       column(12,plotOutput("Fcor")),
                     ),
                     fluidRow(
                       column(12,textOutput("finfo")),
                              ),
                     ), 
            tabPanel("Database", dataTableOutput("tableView"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
#  
     var<-eventReactive(input$act, { 
       if (input$carf == TRUE){
         gen<-namefilm  %>% filter(movieId==input$film) %>% 
           select(movieId,genres) %>% 
           separate(genres, sep = "\\|", c('genre1','genre2','genre3','genre4','genre5','genre6')) %>%
           pivot_longer(c('genre1','genre2','genre3','genre4','genre5','genre6'), names_to = 'genres_all') %>%
           drop_na()
         gen<-all_of(gen$value)
         pers<- Doncast %>% filter(movieId==input$film) %>% select(person)
         pers<-all_of(pers$person)
       }
        Var1 %>% 
          filter(Name %in% c(input$Gen,input$Dir,input$Act,input$Dec,gen,pers)) %>%
          filter(Type %in% input$Type) %>% select(VAR)
     })
     df0<- eventReactive(input$act, {
     final_df %>% filter(nbr_films_watched>=input$Min & nbr_films_watched<=input$Max) %>% 
         filter(average_rating>=input$Rate[1] & average_rating<=input$Rate[2]) %>% 
         select(all_of(misc$VAR),all_of(var()$VAR))
   })
    output$tableView <- renderDataTable({ df0() })
    # distribution of users avg rating
    output$Rate <- renderPlot({
      ggplot(data = df0()%>% select(average_rating))+
        aes(x=average_rating)+
        geom_histogram(bins=250)
    })
   # distribution of users avg rating and nbr_films
    output$Ratenb <- renderPlot({
      ggplot(data = df0()%>% select(average_rating, nbr_films_watched))+
        aes(y=average_rating, x=nbr_films_watched)+
        geom_bin2d(bins=125)
    })
    # scatter plot of users avg rating and nbr_films
    
    output$Sca <- renderPlot({
      ggplot(data = df0() %>% 
             select(average_rating, nbr_films_watched) %>%
               sample_frac(size = 0.3))+
        aes(y=average_rating, x=nbr_films_watched)+
        geom_point()
    })
    # distribution plot of users avg rating by binned nbr_films
    output$bin <- renderPlot({
      ggplot(data = df0()%>% 
               select(average_rating, nbr_films_watched) %>%
               mutate(nb_films_watch = cut(nbr_films_watched, breaks=c(0,20, 200, 750, 30000))))+
        aes(x=average_rating, fill=nb_films_watch)+
        geom_histogram(aes(y = ..density..),position='identity',bins=125, alpha=0.5)+
        facet_wrap(~nb_films_watch, nrow=4)
    })
    # distribution Nbr films watched by user
    output$nbf <- renderPlot({
      ggplot(data = df0()%>%  select(nbr_films_watched))+
        aes(x=nbr_films_watched)+
        geom_histogram(bins=250)
    })
    # dist. oldest film watched
    output$old <- renderPlot({
      ggplot(data = df0()%>% 
               filter(oldest_film_watched_ts>1910 & oldest_film_watched_ts<2000) %>% 
               select(oldest_film_watched_ts) )+
        aes(x=oldest_film_watched_ts)+
        stat_ecdf()+
        scale_x_continuous(breaks=c(seq(1910,2000,10)))+
        scale_y_continuous(breaks=c(seq(0,1,0.125)))
    })
# Box plot per variable
    df2 <- eventReactive(input$act, { 
      df0() %>% 
        select(all_of(misc$VAR),all_of(var()$VAR)) %>% 
       na_if(0) %>% 
        pivot_longer(cols = all_of(var()$VAR))  %>%  mutate(name = gsub("_", " ", name))
      })
    output$box <- renderPlot({
      ggplot(data = df2())+
        aes(x=name, y=value, label=userId)+
        geom_boxplot()+
        scale_x_discrete(labels = wrap_format(10))
    })
# Corelation between variables
    df3 <- eventReactive(input$act, { 
      df0() %>%select(nbr_films_watched,average_rating,all_of(var()$VAR))
    })
   output$cor <- renderPlot({
      cor_mat_films_gen <- cor(df3(), use = 'pairwise.complete.obs')                      
      corrplot(cor_mat_films_gen)
    })

#  ###################################
#   #==Film rating vs caracteristics rating
#   
   dF1 <- eventReactive(input$act, { 
       df0() %>% inner_join(Y,by="userId")%>% na_if(0) %>%  select(c(starts_with("av"),input$film)) %>% 
       pivot_longer(cols = everything())  %>%  mutate(name = gsub("_", " ", name))
   })
   output$F <- renderPlot({
     ggplot(data = dF1()) +
       aes(x=name, y=value)+
     geom_boxplot()+
       scale_x_discrete(labels = wrap_format(10))
   })
   dF2 <- eventReactive(input$act, { 
     df0() %>%inner_join(Y %>% select(c("userId",input$film)),by="userId") %>% select(-userId)
   })
   output$Fcor<- renderPlot({
     cor_ts <- cor(dF2())
     n1<-which(rownames(cor_ts)=="average_rating")
     n2<-which(rownames(cor_ts)==input$film)
     corrplot(cor_ts[c(n1,n2),])
   })
   output$finfo = renderText({
     paste0(" Film : " , input$film , 
            " -  Tittle : ", all_of(namefilm %>% filter(movieId == all_of(input$film)) %>% select(title)),
            " -  Genre(s) : ", all_of(namefilm  %>% filter(movieId==input$film) %>% select(genres)),
            " -  Actors / Directors : ", all_of(Doncast %>% filter(movieId==input$film) %>% select(person)))
   })
}
# Run the application 
shinyApp(ui = ui, server = server)
