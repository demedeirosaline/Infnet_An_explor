```{r}
install.packages("shiny")
library(shiny)
ui <- fluidPage(
  titlePanel("Dashboard Shiny"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variavel", "Selecione a variável:",
                  choices = colnames(dados_imputados)),
      selectInput("cor", "Selecione a cor da linha:",
                  choices = c("blue", "red", "green")),
      sliderInput("limite_x", "Limite do eixo X:",
                  min = min(dados_imputados$Age, na.rm = TRUE),
                  max = max(dados_imputados$Age, na.rm = TRUE),
                  value = c(min(dados_imputados$Age, na.rm = TRUE), max(dados_imputados$Age, na.rm = TRUE))),
      sliderInput("limite_y", "Limite do eixo Y:",
                  min = min(dados_imputados$Age, na.rm = TRUE),
                  max = max(dados_imputados$Age, na.rm = TRUE),
                  value = c(min(dados_imputados$Age, na.rm = TRUE), max(dados_imputados$Age, na.rm = TRUE)))
    ),
    mainPanel(
      plotOutput("grafico")
    )
  )
)
server <- function(input, output) {
  output$grafico <- renderPlot({
    ggplot(dados_imputados, aes_string(x="Age", y=input$variavel)) +
      geom_line(color=input$cor) +
      xlim(input$limite_x) +
      ylim(input$limite_y)
  })
}
shinyApp(ui = ui, server = server)