---
title: "Análise exploratória de dados - Projeto da disciplina"
author: "Aline Medeiros"
date: "2024-04-08"
output: PDF_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
#Questão 1: O relatório final deve ser apresentado utilizando RMarkdown. Nesse relatório deve haver:imagens estáticas ("prints" de tela, imagens da internet - com a devida fonte mencionada - ou figuras criadas pelo aluno fora do ambiente do R); imagens geradas através do ambiente R, particularmente com a biblioteca ggplot; links clicáveis (como fontes e referências).


# Questão 2: a base de dados escolhida foi a "Heart Disease Data Set" disponível no repositório Kaggle. O conjunto de dados contém informações sobre pacientes e a presença de doença cardíaca. O motivo da escolha foi o conhecimento especialista da autora do trabalho, permitindo análise crítica sobre os resultados encontrados. O resultado esperado é a análise da importância das variáveis clínicas para o desfecho avaliado (presença ou ausência de doença cardíaca)
## Carregando a base de dados e realizando a análise exploratória
```{r}
dados <- read.csv("heart_IC3.csv", header = TRUE, sep = ";")
head(dados) # Visualizar as primeiras linhas
![Imagem 01](01.jpg)
str(dados) # Estrutura da base de dados
![Imagem 02](02.jpg)
summary(dados) # Resumo estatístico das variáveis numéricas
![Imagem 03](03.jpg)
#Análises descritivas:
table(dados$Age) # Contagem de pacientes por idade
table(dados$RestingBP) #Contagem de pacientes por pressão arterial em repouso
table(dados$Cholesterol) # Contagem de pacientes por colesterol
table(dados$MaxHR) # Contagem de pacientes de acordo com a frequência cardíaca máxima
table(dados$HeartDisease) # Contagem de pacientes com e sem doença cardíaca
![Imagem 04](04.jpg)
```	
#Questão 3: Utilizando o pacote summarytools (função descr), descreva estatisticamente a sua base de dados.
```{r}
install.packages("summarytools")
library(summarytools)
descr(dados)
![Imagem 05](05.jpg)
```
#Questão 4: Crie um gráfico com a matriz de espalhamento (scatter matrix plot) para sua base de dados. Através de investigação visual, quais são as variáveis mais correlacionadas. Apresente o gráfico e justifique.
```{r}
install.packages("car")
library(car)
pairs(dados)
![Imagem 06](06.jpg)
#Calculando a matriz de correlação
cor(dados, use = "complete.obs")
![Imagem 07](07.jpg)
#Visualizando a matriz de correlação com heatmap
heatmap(cor(dados, use = "complete.obs"), col = colorRampPalette(c("blue", "white", "red"))(100), symm = TRUE, margins = c(5, 5))
```
!imagem 08](08.jpg)
###As variáveis mais correlacionadas são a idade e a frequência cardíaca máxima, com uma correlação negativa de -0,39. Isso significa que, em geral, quanto maior a idade, menor a frequência cardíaca máxima. Outra correlação significativa é entre a pressão arterial em repouso e a frequência cardíaca máxima, com uma correlação negativa de -0,04. Isso indica que, em geral, quanto maior a pressão arterial em repouso, menor a frequência cardíaca máxima.


#Questão 5: Sobre a normalidade das variáveis: 
##Descreva o que é uma distribuição normal - distribuição normal é uma distribuição de probabilidade contínua simétrica ao redor da média, onde a maioria dos valores se concentra perto da média e os valores extremos são raros. A distribuição normal é caracterizada por sua forma de sino e é amplamente utilizada em estatística devido a suas propriedades matemáticas.

##Crie um histograma para cada variável da sua base de dados. 
library(ggplot2)
optimal_bins <- function(x) {
  n <- length(x)
  iqr <- IQR(x)
  bin_width <- 2 * iqr * (n^(-1/3))
  range_x <- range(x)
  bins <- (range_x[2] - range_x[1]) / bin_width
  return(round(bins))
}
bins <- optimal_bins(dados$Age)
ggplot(dados, aes(x=Age)) +
  geom_histogram(bins=bins, fill="blue", color="black") +
  theme_minimal() +
  labs(title="Histograma de idades", x="Idade", y="Count")
  ![Imagem 09](09.jpg)
    
optimal_bins <- function(x) {
  n <- length(x)
  iqr <- IQR(x)
  bin_width <- 2 * iqr * (n^(-1/3))
  range_x <- range(x)
  bins <- (range_x[2] - range_x[1]) / bin_width
  return(round(bins))
}
bins <- optimal_bins(dados$RestingBP)
ggplot(dados, aes(x=RestingBP)) +
  geom_histogram(bins=bins, fill="blue", color="black") +
  theme_minimal() +
  labs(title="Histograma de pressão arterial em repouso", x="PA repouso", y="Count")
  ![Imagem 10](10.jpg)

optimal_bins <- function(x) {
  n <- length(x)
  iqr <- IQR(x)
  bin_width <- 2 * iqr * (n^(-1/3))
  range_x <- range(x)
  bins <- (range_x[2] - range_x[1]) / bin_width
  return(round(bins))
}
bins <- optimal_bins(dados$Cholesterol)
ggplot(dados, aes(x=Cholesterol)) +
  geom_histogram(bins=bins, fill="blue", color="black") +
  theme_minimal() +
  labs(title="Histograma de colesterol", x="Colesterol", y="Count")

![Imagem 11](11.jpg)

optimal_bins <- function(x) {
  n <- length(x)
  iqr <- IQR(x)
  bin_width <- 2 * iqr * (n^(-1/3))
  range_x <- range(x)
  bins <- (range_x[2] - range_x[1]) / bin_width
  return(round(bins))
}
bins <- optimal_bins(dados$MaxHR)
ggplot(dados, aes(x=MaxHR)) +
  geom_histogram(bins=bins, fill="blue", color="black") +
  theme_minimal() +
  labs(title="Histograma de FC máxima", x="FC máxima", y="Count")
  ![Imagem 12](12.jpg)
##Justifique a escolha do número de bins para seu trabalho. (usando o pacote ggplot);
###Para determinar o número ideal de bins em um histograma usando o pacote ggplot2 em R, usei a regra Freedman-Diaconis. Esta regra sugere que o número de bins deve ser proporcional à raiz cúbica do número de observações nos dados e inversamente proporcional ao intervalo interquartil (IQR).

##Crie um gráfico Q-Q para cada variável de sua base de dados. (use as funções presentes no pacote ggpubr);
```{r}
install.packages("ggpubr")
library(ggpubr)
dados$Age <- replace(dados$Age, is.na(dados$Age), mean(dados$Age, na.rm = TRUE))
plots_qq <- list()
for (i in 1:ncol(dados)) {
  if (colnames(dados)[i] != "") {
    plots_qq[[i]] <- ggqqplot(dados[,i], main = colnames(dados)[i])
  }
}
plots_qq <- list()
for (i in 1:ncol(dados)) {
  plots_qq[[i]] <- ggqqplot(dados[,i], main = colnames(dados)[i])
}
plots_qq
```
![Imagem 13](13.jpg)
##Execute um teste de normalidade Shapiro-Wilk;
```{r}
shapiro.test(dados$Age)
shapiro.test(dados$RestingBP)
shapiro.test(dados$Cholesterol)
shapiro.test(dados$MaxHR)
```
![Imagem 14](14.jpg)
##Baseado nos itens anteriores, é possível afirmar que algumas das variáveis se aproximam de uma distribuição normal? Justifique.
###Com base nos histogramas e gráficos Q-Q, as variáveis Age, RestingBP, Cholesterol e MaxHR parecem se aproximar de uma distribuição normal. Além disso, os testes de normalidade de Shapiro-Wilk para essas variáveis indicam que elas não são significativamente diferentes de uma distribuição normal (p > 0,05). Portanto, é possível afirmar que essas variáveis se aproximam de uma distribuição normal.

#Questão 6: Qualidade de dados tem sido um dos temas mais abordados nos projetos de estruturação em data analytics, sendo um dos principais indicadores do nível de maturidade das organizações. Um dos problemas mais comuns de qualidade é relacionado à completude de dados. Em suas palavras, como é definido completude? Qual o impacto em uma análise exploratória de dados?
###Completude refere-se à presença de valores em todas as observações de uma variável. Em outras palavras, uma variável é considerada completa se não houver valores ausentes ou faltantes em suas observações. A completude dos dados é essencial para uma análise exploratória de dados, pois valores ausentes podem distorcer as estatísticas descritivas, a matriz de correlação e outros resultados da análise. Além disso, a presença de valores ausentes pode levar a conclusões errôneas e prejudicar a interpretação dos resultados.

# Questão 7: Qual a completude para cada uma das variáveis do seu banco de dados?
```{r}
#dados$Age <- replace(dados$Age, is.na(dados$Age), mean(dados$Age, na.rm = TRUE))
#dados$RestingBP <- replace(dados$RestingBP, is.na(dados$RestingBP), mean(dados$RestingBP, na.rm = TRUE))
#dados$Cholesterol <- replace(dados$Cholesterol, is.na(dados$Cholesterol), mean(dados$Cholesterol, na.rm = TRUE))
#dados$MaxHR <- replace(dados$MaxHR, is.na(dados$MaxHR), mean(dados$MaxHR, na.rm = TRUE))

completude <- function(x) {
  sum(!is.na(x)) / length(x) * 100
}
# Replace 'full_file_path' with the full path to your file

completude(dados$Age)
completude(dados$RestingBP)
completude(dados$Cholesterol)
completude(dados$MaxHR)
```
![Imagem 15](15.jpg)
# Questão 8: Realize uma operação de imputação de dados usando o pacote MICE.
```{r}
install.packages("mice")
library(mice)
dados_imputados <- mice(dados, m=5, maxit=50, method="pmm", seed=500)
dados_imputados <- complete(dados_imputados)
head(dados_imputados)
```
![Imagem 16](16.jpg)
#Questão 9: Crie um dashboard Shiny onde seja possível selecionar (tire um print-screen da tela final do sistema): uma variável da sua base de dados e um gráfico em linha seja mostrado na tela;
escolher a cor da linha do gráfico; selecionar o limite inferior e superior do eixo X do gráfico; selecionar o limite inferior e superior do eixo Y do gráfico. 
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
```
![Imagem 17](17.jpg)

#Questão 10: Disponibilize os códigos (RMarkdown e Shiny) em uma plataforma de compartilhamento de códigos (sugestão GitHub)
###Os códigos RMarkdown e Shiny foram disponibilizados no GitHub no seguinte repositório:


