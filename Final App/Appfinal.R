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
#library(mclust)
library(shinythemes)
library(shinydashboard)
library(rvest)
library("jpeg")
library(cowplot)
library(magick)
#
# Load data
Donmovies<-read.csv("C:/Users/r083485/OneDrive - Volvo Group/R/Projet/Data/ml-latest/movies.csv")
Donlinks<-read.csv("C:/Users/r083485/OneDrive - Volvo Group/R/Projet/Data/ml-latest/links.csv")
#
Doncast<-readRDS("Doncast_large_focus")
#
reco<-read.csv('Reco_films.csv')
final_df <- readRDS('user_summary_v4_no_target_films')
Y<- readRDS('Y_films')
#
#
# Get Caracteristics and Method
#
final_df<-final_df %>% mutate(across(.cols = starts_with('avg_'), .fns = ~ na_if(.x,0)))

final_df0<-final_df %>% mutate(across(.cols=starts_with("avg_"),.fns=~(.-average_rating))) %>% 
  mutate(across(.cols=starts_with("count_"),.fns=~((.)/nbr_films_watched))) %>% select(c(userId,starts_with("avg_"),starts_with("count_")))
colnames(final_df0)<-c("userId",paste0("0",colnames(final_df0 %>% select(-1))))
final_df<-final_df %>% left_join(final_df0, by= "userId")
#
Var1<-tibble(VAR = colnames(final_df))
misc<- Var1[1:5,1]
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
#
# Get Film data
#
#lfilm<-colnames(Y %>% select(-userId))
#namefilm<-Donmovies %>% filter(movieId %in% all_of(lfilm))
lfilm<-Y %>% group_by(movieId) %>% summarise(N=mean(rating)) %>% select(movieId)
namefilm<-lfilm %>% left_join(Donmovies, by = "movieId")
#
kmod<-c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen")
#
#
# Get results
dd<-namefilm %>%  left_join(unique(Doncast), by="movieId") %>% 
  select("ID" = movieId,"Title" = title, "Genres" = genres, "Cast" = person) %>% 
  mutate(Cast= replace_na(Cast, "No")) %>% group_by(ID,Title,Genres) %>% 
  summarise(Cast=paste0(across(Cast))) %>% 
  mutate(Cast=str_replace_all(Cast,c("c\\(" = "","\\\"" = "",", "="|","\\)"="")))
print("1")
#
dd<- dd %>% left_join(Donlinks, by=c("ID"="movieId"))%>%
  mutate(linkTM=paste0(c("https://www.themoviedb.org/movie/"),tmdbId)) %>% 
  mutate(Affiche=paste0("https://www.themoviedb.org/",(read_html(linkTM) %>% 
                                                         html_nodes("#main > section > div:nth-of-type(2)") %>% 
                                                         html_elements("img") %>% html_attr(c("data-src")))[[1]]))
print("1")
best<-reco %>% pivot_longer(cols=starts_with("movie")) %>% filter(!is.na(value)) %>% arrange(userId,desc(value)) %>% 
  group_by(userId) %>% slice(1) %>% select(userId,best=name)
reco<-reco %>% left_join(best,by="userId")
print("1")
reco2<-reco%>% pivot_longer(cols=starts_with("movie")) %>% arrange(userId,desc(value)) %>% 
  separate(name,sep= "\\_",c("bidon","movieId")) %>% select(-bidon) %>% mutate(movieId=as.integer(movieId)) %>% 
  left_join(dd,by=c("movieId"="ID")) %>% 
  separate(best,sep= "\\_",c("bidon","best")) %>% select(-bidon) %>% mutate(best=as.integer(best)) %>%
  select(userId,Title,Genres,Cast,"Rate"=value,Affiche,best)
print("2")
#
#
#####################################################################################################
#
#
# Define UI for application that draws a histogram
  #theme = shinytheme("slate"),
#        title = span(
#                img(src = "Going-To-The-Cinema-Alone-Getty-hero.png", style = "float:right;"),
#                h1("Ready for YOUR film ? ...", align = "center", style="color:#b16a7a;vertical-align:middle;")),
                 
# Côté
slidebar1<-dashboardSidebar(width = 350,
          sidebarMenu(id="slidebar1",
            menuItem(text=h2("Avatar",style="color:orange"), tabName = "Result"),
            menuItem(text=h2("Lost in Data",style="color:orange"),  tabName = "Explore")
            ),
          hr(),
          actionButton('act', 'GO !',status = "warning"),
          #
#
#     Users
          tags$h4("Select users",style="color:orange"),
          tags$h5("Number of watched films :"),
          numericInput("Min", "Min :", value = 1),
          numericInput("Max", "Max :", value = 30000),
          setSliderColor(c("orange"),c(1)),
          sliderInput("Rate", "Average rate :",
                      min = 0, max = 5, value = c(0,5), step = 0.25),
#
#     Films
          tags$h4("Select films",style="color:orange"),
          pickerInput("film","",namefilm$title,
                      options = list(`live-search` = TRUE,`actions-box` = TRUE),multiple=TRUE),
#

#     Variables
          tags$h4("Select data agregation",style="color:orange"),
          checkboxGroupButtons(inputId = "Type",label = "Calculs",choices = all_of(Cal$Type), 
                               selected = all_of(Cal$Type %>% head(1)),status = "warning"),
          tags$h4("Select variables",style="color:orange"),
          pickerInput("Gen", "Genres :", Genres,
                      options = list(`actions-box` = TRUE),multiple = TRUE),
          pickerInput("Dir", "Directors :", Directors,
                      options = list(`actions-box` = TRUE),multiple = TRUE),
          pickerInput("Act", "Actors :", Actors,
                      options = list(`actions-box` = TRUE),multiple = TRUE),
          materialSwitch(inputId = "carf", label = "Add selected film genres and cast", value = FALSE,  status = "warning"),
          pickerInput("Dec", "Decades :", Periods,
                      options = list(`actions-box` = TRUE),multiple = TRUE)
#
)
js <- '.nav-tabs-custom .nav-tabs li.active {border-top-color: orange;}
       .content-wrapper {background-color: #7f7f7f;} 
#      .irs-bar .irs-bar-edge .irs-single .irs-grid-pol {  background: red;  border-color: red};
'

body1 <- dashboardBody(
    tags$style(js),
    tabItems(
      tabItem(tabName = "Result",
              fluidRow(
                h2("Hi, please select a user and Avatar will recommand 3 films in the selection :  ",style="color:orange"),
                column(12,selectInput("User", "", c("Please select your ID",reco$userId)))
                ),
              fluidRow(column(8,imageOutput("urlimage")),
                      tableOutput("finfo3"),
                      ),
              h2("You can compare with other users :",style="color:orange"),
              box(
                tableOutput("finfo1"),
              ),
              box(
                tableOutput("finfo2"),
              ),
),
              
    tabItem(tabName = "Explore",
        # Show a plot of the generated distribution
            tabBox(width = 12,
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
                     fluidRow(column(12,plotOutput("cor",height = "800px")),
                     )
                    ),
            
            tabPanel("Users",
                     #     Kmeans
                     fluidRow(column(12,tags$h5("Select kmeans parameters"),)),
                     fluidRow(column(3,actionButton('act2', 'GO for kmeans!')),
                              column(3,numericInput("Ncen", "Number of centers :", value = 4)),
                              column(3,numericInput("Nsta", "Number of nstart :", value = 50)),
                              column(3,pickerInput("Met", "Methode :", kmod)),
                     ),
                     fluidRow(column(12,plotOutput("gp1")),
                     ),
                   fluidRow(column(12,plotOutput("Rategen")),
                   ),
                  ),
            tabPanel("Films", 
                     fluidRow(
                       column(12,plotOutput("F")),
                     ),
                     fluidRow(
                       column(12,plotOutput("Fcor")),
                     ),
                     fluidRow(
                       column(12,tableOutput("finfo")),
                     )
                     ), 
            tabPanel("Database", dataTableOutput("tableView"))
            
            ),)
        
        
    )
)

ui <- dashboardPage(dashboardHeader(),slidebar1,body1,skin="yellow")

# Define server logic required to draw a histogram
server <- function(input, output) {
#  
     var<-eventReactive(input$act, { 
       if (input$carf == TRUE){
         gen<-namefilm  %>% filter(title %in% all_of(input$film)) %>% 
           select(movieId,genres) %>% 
           separate(genres, sep = "\\|", c('genre1','genre2','genre3','genre4','genre5','genre6')) %>%
           pivot_longer(c('genre1','genre2','genre3','genre4','genre5','genre6'), names_to = 'genres_all') %>%
           drop_na()
         gen<-all_of(gen$value)
         pers<- Doncast %>% 
           filter(movieId %in% all_of(namefilm  %>% filter(title %in% all_of(input$film)) %>% select(movieId))) %>% 
           select(person)
         pers<-all_of(pers$person)
       }
       else
       {gen<-c()
       pers<-c()}
        Var1 %>% 
          filter(Name %in% c(input$Gen,input$Dir,input$Act,input$Dec,gen,pers)) %>%
          filter(Type %in% input$Type) %>% select(VAR)
     })
     df0<- eventReactive(input$act, {
       print("df0()")
     final_df %>% filter(nbr_films_watched>=input$Min & nbr_films_watched<=input$Max) %>% 
         filter(average_rating>=input$Rate[1] & average_rating<=input$Rate[2]) %>% 
         select(all_of(misc$VAR),all_of(var()$VAR)) 
   })
    dY<-eventReactive(input$act, { 
      print("dY()")
       Y %>% select(c(userId,as.character((namefilm %>% filter(title %in% all_of(input$film)))$movieId)))
     })
     dF2 <- eventReactive(input$act, { 
       print("dF2()")
       df0() %>%
         inner_join(dY(), by="userId") %>% 
         select(-userId)
     })
     dF1 <- eventReactive(input$act, { 
       print("dF1()")
       dF2() %>%  na_if(0) %>%  
                  select(starts_with("av"),as.character((namefilm %>% filter(title %in% all_of(input$film)))$movieId)) %>% 
#         select(-starts_with(input$type[-c("av")])) %>% 
       pivot_longer(cols = everything(),names_to="Dimension",values_to = "Value")  %>%  mutate(Dimension = gsub("_", " ", Dimension))
     })

############ General ###################################
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
     # distribution of users avg rating
     output$Rate <- renderPlot({
       ggplot(data = df0()%>% select(average_rating))+
         aes(x=average_rating)+
         geom_histogram(bins=250)
     })
     # distribution plot of users avg rating by binned nbr_films
     output$bin <- renderPlot({
       ggplot(data = df0()%>% 
                select(average_rating, nbr_films_watched) %>%
                mutate(nb_films_watch = cut(nbr_films_watched, breaks=c(0,20, 200, 750, 30000))))+
         aes(x=average_rating, fill=nb_films_watch)+
         #        aes(x=average_rating)+
         geom_histogram(aes(y = ..density..),position='identity',bins=125, alpha=0.5)+
         facet_wrap(~nb_films_watch, nrow=4)
     })
     # scatter plot of users avg rating and nbr_films
     output$Sca <- renderPlot({
       ggplot(data = df0() %>% 
                select(average_rating, nbr_films_watched) %>%
                sample_frac(size = 0.3))+
         aes(y=average_rating, x=nbr_films_watched)+
         geom_point()
     })
      # distribution of users avg rating and nbr_films
     output$Ratenb <- renderPlot({
       ggplot(data = df0()%>% select(average_rating, nbr_films_watched))+
         aes(y=average_rating, x=nbr_films_watched)+
         geom_bin2d(bins=125)
     })
############ Variables ###################################
     output$box <- renderPlot({
       ggplot(data = df0()%>% 
                pivot_longer(cols = all_of(var()$VAR)))+
         aes(x=name, y=value, label=userId,color=name)+
         geom_boxplot()+
         geom_point(position = "jitter", alpha = 0.005) +
         scale_x_discrete(labels = wrap_format(10))
     })
     # Corelation between variables
     output$cor <- renderPlot({
       cor_mat_films_gen <- cor(df0()%>%select(nbr_films_watched,average_rating,all_of(var()$VAR)), use = 'pairwise.complete.obs')                      
       corrplot(cor_mat_films_gen,)},
       height = 1000 )
############ Users ###################################
     #   kmeans
     dgp10 <- eventReactive(input$act2, { 
       kmeans(df0()%>%replace(is.na(.), 0) %>% select(all_of(var()$VAR)),centers=input$Ncen,nstart=input$Nsta,algorithm=input$Met)
     })
     output$gp1 <- renderPlot({
       ggplot(df0()%>%replace(is.na(.), 0),aes(x=nbr_films_watched,y=average_rating))+geom_point(col=dgp10()$cluster)+xlim(0,500)
     })
     # distribution of users avg rating et PP per genre
     output$Rategen <- renderPlot({
       ggplot(data = df0()%>% 
                pivot_longer(cols=(starts_with("av")| starts_with("0av") | starts_with("PP")), names_to = "type", values_to = "rating") %>%
                filter(type != "average_rating") %>% 
                filter(rating != 0) %>%
                mutate(nb_films_watch = cut(nbr_films_watched, breaks=c(0,20, 200, 750, 30000))) %>% 
                select(type,rating,nb_films_watch))+
         aes(x=rating)+
         geom_histogram(bins=250)+
         facet_grid(cols=vars(type), rows=vars(nb_films_watch))
     })
     
############ Films ###################################     
     output$F <- renderPlot({
       ggplot(data = dF1()) +
         aes(x=Dimension, y=Value,color=Dimension)+
         geom_boxplot()+
         geom_point(position = "jitter", alpha = 0.005) +
         scale_x_discrete(labels = wrap_format(10))
     })
     output$Fcor<- renderPlot({
       cor_ts <- cor(scale(dF2()),use = 'pairwise.complete.obs')
       n1<-which(rownames(cor_ts)=="average_rating")
       n2<-which(rownames(cor_ts) %in% as.character((namefilm %>% filter(title %in% all_of(input$film)))$movieId))
       corrplot(cor_ts[c(n1,n2),])
     })
     output$finfo <- renderTable({
       dd %>% filter(Title %in% all_of(input$film)) %>% select(ID,Title,Genres,Cast)
     })
############ Database ################################### 
     output$tableView <- renderDataTable({ datatable(df0(),formatStyle(backgroundColor = "red",color="green")) })
############ Result ###################################    
#
#
     observeEvent((input$Min|input$Max|input$Rate),{
       output$lus<-renderDataTable({
           lusers<-(final_df %>% filter(nbr_films_watched>=input$Min & nbr_films_watched<=input$Max) %>% 
                      filter(average_rating>=input$Rate[1] & average_rating<=input$Rate[2]))
         })
     })
     
     observeEvent(input$User,{
       if(input$User!="Please select your ID") {
          output$urlimage<- renderPlot({
            #            g<-  ggdraw() +   draw_image("https://image.tmdb.org//t/p/w300_and_h450_bestv2/6FfCtAuVAW8XJjZ7eWeLibRLWTw.jpg")
            g<-  ggdraw(xlim=c(0,3)) +   draw_image((reco2 %>%  filter(userId %in% all_of(input$User)) %>% 
                                           select(Affiche) %>% head(1))[[1]],(0.))+   
                            draw_image((reco2 %>%  filter(userId %in% all_of(input$User)) %>% 
                            select(Affiche) %>% slice(2))[[1]],1)+   
                            draw_image((reco2 %>%  filter(userId %in% all_of(input$User)) %>% 
                                          select(Affiche) %>% slice(3))[[1]],2)+
                            theme_dark()
            g
          })
          output$finfo3 <- renderTable({
  #        reco2 %>%  filter(userId %in% all_of(input$User)) %>% filter(is.na(Rate)) %>% select("Already watched film(s) in the selection"=Title)
           Y %>%  filter(userId %in% all_of(input$User))  %>% 
              mutate(Rate=replace_na(as.character(rating),"Not watched yet")) %>% 
              arrange(desc(Rate))%>% left_join(dd %>% mutate(movieId=ID),by="movieId") %>%
              select("Already watched films in the selection"=Title,"Rate"=Rate)
            })
       }
       output$finfo1 <- renderTable({
         dd %>% left_join((reco2 %>% select(c(userId,best)) %>% group_by(userId,best) %>% summarise(n(),.groups=NULL) %>% group_by(best) %>% 
                             summarise(Nbest=n(),.groups=NULL)),by=c("ID"="best")) %>% arrange(desc(Nbest)) %>% 
           select(Title,"Number of recommandations :" = Nbest) %>% replace(is.na(.), 0) 
       })
       output$finfo2 <- renderTable({
         Y %>% mutate(rating=as.numeric(rating),F1=1) %>% filter(!is.na(rating))%>%group_by(movieId) %>%summarise(Nview=sum(F1),Rate=mean(rating)) %>% 
           left_join(dd %>% mutate(movieId=ID),by="movieId") %>% 
           arrange(desc(Rate)) %>% select(Title,"Number of view"=Nview,"Average rate" = Rate)
#         Y %>% pivot_longer(cols = c(colnames(Y),-userId),names_to="Film", values_to="Rate") %>% mutate(Rate=as.numeric(Rate),F1=1,Film=as.integer(Film)) %>% 
#           filter(!is.na(Rate))%>% group_by(Film) %>% summarise(Nview=sum(F1),Rate=mean(Rate)) %>% left_join(dd %>% mutate(Film=ID),by="Film") %>% 
#           arrange(desc(Rate)) %>% select(Title,"Number of view"=Nview,"Average rate" = Rate)
       })
     })
}
# Run the application 
shinyApp(ui = ui, server = server)
