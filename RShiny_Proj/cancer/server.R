

shinyServer(function(input, output) {
  
  output$map <- renderGvis({
    geo = gvisGeoChart(cancer %>%
                         filter(Year==2015, States!='District of Columbia') %>%
                         group_by(States) %>% summarise(Diagnosed=sum(Count)),
                       locationvar = "States", colorvar = 'Diagnosed',
                       options=list(region="US",
                                    displayMode="regions",
                                    resolution="provinces",
                                    colorAxis="{colors:['white','purple']}",
                                    background='blue',
                                    backgroundColor = 'skyblue',
                                    backgroundColor.stroke = 'black',
                                    backgroundColor.strokeWidth = 20,
                                    width=1100, height=500))
  })
  
    output$CountByAge <- renderPlot(
    cancer %>%
      filter(Race == input$Race1 & States == input$States1 &
               Sex == input$Sex1 & Year == input$Year1) %>%
      ggplot(cancer, mapping = aes(x = factor(Age.Groups.Code), y = Count)) +
      geom_col(fill = 'blue') + ggtitle("Cancer Diagnoses by Age Group") +
      xlab("Age Group") + ylab("Count") +
      theme(plot.title = element_text(size = 24, face = "bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
  )
    
  output$CountByRace <- renderPlot(
    cancer %>%
      filter(States == input$States2 & Sex == input$Sex2 &
               Age.Groups.Code == input$Age.Groups.Code2 & Year == input$Year2) %>%
      ggplot(cancer, mapping = aes(x = reorder(Race, Count), y = Count)) +
      geom_col(aes(fill = Race)) + ggtitle("Cancer Diagnoses by Race") +
      xlab("Race") + ylab("Count") +
      scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n")) +
      theme(plot.title = element_text(size = 24, face = "bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
  )
  
  output$DistByRace <- renderPlot(
    cancer %>%
      filter(States == input$States3 & Sex == input$Sex3 &
               Year == input$Year3) %>%
      ggplot(cancer, mapping = aes(x = Race, y = Age.Groups.Code)) +
      geom_boxplot(aes(fill = Race)) + ggtitle("Age Distribution by Race") +
      xlab("Race") + ylab("Age") +
      scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n")) +
      theme(plot.title = element_text(size = 24, face = "bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
  )
  
  output$CountByState <- renderPlot(
    cancer %>%
      filter(Race == input$Race4 & Age.Groups.Code == input$Age.Groups.Code4 &
               Sex == input$Sex4 & Year == input$Year4) %>%
      ggplot(cancer, mapping = aes(x = reorder(States, Count), y = Count)) +
      geom_col(fill = 'purple') + coord_flip() +
      ggtitle("Cancer Diagnoses by State") + xlab("State") + ylab("Count") +
      theme(plot.title = element_text(size = 24, face = "bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
    )
  
  output$CountByGender <- renderPlot(
    cancer %>%
      filter(Race == input$Race5 & Age.Groups.Code == input$Age.Groups.Code5 &
               Year == input$Year5, States == input$States5) %>%
      ggplot(data = cancer, mapping = aes(x = Sex, y = Count)) +
      geom_col(aes(color = Sex)) + ggtitle("Cancer Diagnoses by Gender") +
      xlab("Sex") + ylab("Count") +
      theme(plot.title = element_text(size = 24, face = "bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
  )
  
  output$raw_table = DT::renderDataTable(
    cancer_stat %>%
      filter(Sex == input$Sex6, Year == input$Year6, States == input$States6,
             Race == input$Race6, Age.Groups.Code == input$Age.Groups.Code6),
    options = list(scrollX=TRUE)
  )
  
  output$mosaic1 = renderPlot(
    mosaic(Race ~ Age.Groups.Code + Sex, data=cancer, 
           main = "Age & Race by Gender",shade=T,
           labeling= labeling_border(set_varnames = c(Age.Groups.Code="Age Group"),
                                     rot_labels = c(0,90,-7.5,0), 
                                     just_labels=c("center","left","left","center")),
           highlighting_fill=c('violetred',"pink", "green", "blue", "yellow"),
           highlighting_direction="left")
  )
  
})