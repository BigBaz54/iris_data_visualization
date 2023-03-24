library(shiny)
library(ggplot2)
library(plyr)
data(iris)


# Question 5
# On pourrait ajouter la possibilité de modifier le titre du graphique et des axes, et éventuellement de l'exporter
# On pourrait permettre à l'utilisateur de choisir quels attributs de "iris" il veut représenter sur le graphique
# On pourrait choisir la teinte des points en  fonction d'un autre attribut que ceux déjà représentés par les coordonnées

ui <- fluidPage(
  titlePanel("Modèle de régression de la largeur du sépale en fonction de la longueur et de l'espèce"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("species", "Species:",
                                    choices = c("setosa", "versicolor", "virginica"),
                                    selected = "setosa"),
      radioButtons("model", "Regression model:", 
                   choices = c("lm", "glm", "gam", "loess", "MASS:rlm"), 
                   selected = "lm"),
      sliderInput("x", "Sepal.Length Range:", min = 4, max = 8, value = c(4,8)),
      sliderInput("y", "Petal.Width Range:", min = 2.0, max = 4.5, value = c(2.0,4.5))
      ),
    mainPanel(
      navbarPage("Iris",
                 tabPanel("Plots",
                          plotOutput("plot")),
                 tabPanel("Histograms",
                          tabsetPanel(
                            tabPanel("Chevauchant", plotOutput("hist1")),
                            tabPanel("Séparés", plotOutput("hist2")),
                            tabPanel("Densité", plotOutput("hist3"))
                          ))
      )
    )
  )
)


server <- function(input, output) {
  output$plot <- renderPlot({
    n = length(input$species)
    filtered_iris = iris[iris$Species %in% input$species, ]
    q= ggplot(filtered_iris, aes(Sepal.Length,Sepal.Width,  colour = Species))
    q=q+geom_point(aes(shape = Species))+facet_grid(.~Species)
    q=q+ labs(title = "Linear modelling of Sepal.Length against Sepal.Width (IRIS) ", subtitle = "By Species")
    q=q+scale_x_continuous(limits=input$x)+scale_y_continuous(limits=input$y)
    q=q+theme_bw()
    if (input$model == "lm") {
      q=q+stat_smooth(method = "lm")
    } else if (input$model == "glm") {
      q=q+stat_smooth(method = "glm")
    } else if (input$model == "gam") {
      q=q+stat_smooth(method = "gam")
    } else if (input$model == "loess") {
      q=q+stat_smooth(method = "loess")
    } else if (input$model == "MASS:rlm") {
      q=q+stat_smooth(method = "MASS:rlm")
    }
    coef = lm(Sepal.Width ~ Sepal.Length:Species + Species - 1, data=iris)$coefficients
    ann_text = data.frame(Sepal.Length = 7, Sepal.Width = 4,
                          intercept = coef[1:3],
                          a = coef[4:6], 
                          Species = c("setosa", "versicolor", "virginica"))
    q = q + geom_text(data = ann_text[ann_text$Species %in% input$species, ], aes(label = paste(round(a, 2), "x+",round(intercept,2))))
    q
  })
  
  output$hist3 <- renderPlot({
    mu <- ddply(iris, "Species", summarise, grp.mean=mean(Sepal.Length))
    head(mu)
    ggplot(iris, aes(x=Sepal.Length, fill=Species, color=Species)) +
      theme(legend.position="top")+
      geom_density(alpha=.1) +geom_vline(data=mu, aes(xintercept=grp.mean, color=Species),
                                         linetype="dashed")
  })
  
  output$hist2 <- renderPlot({
    ggplot(iris, aes(x=Sepal.Length, color=Species)) +
      geom_histogram(fill="white", position="dodge")+
      theme(legend.position="top")
  })
  
  output$hist1 <- renderPlot({
    ggplot(iris, aes(x=Sepal.Length, color=Species)) +
      geom_histogram(bins = 30,fill="white", alpha=0.5, position="identity")
  })
}


shinyApp(ui = ui, server = server)
