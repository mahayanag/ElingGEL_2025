##########################################
### CODIGO PARA ACOMPANHAR O MINICURSO ###
##########################################

## instalar pacote

install.packages("tidyverse")

## carregando o pacote para o curso

library(tidyverse)

#### Importar conjuntos de dados no R

## carregando conjunto de dados

esplex <- read.csv("dados/esplex_2025.csv")

## inspecionando o conjunto de dados
str(esplex)


#### Inspecionando níveis de uma variável categórica


## conferir níveis da variável categoria

unique(esplex$categoria)

## conferir níveis da variável concretude

unique(esplex$concretude)

#### Medidas descritivas

## média de concretude

mean(esplex$concretude)

esplex %>%                ## Linha 1
  group_by(categoria) %>% ## Linha 2
  summarise()             ## Linha 3


esplex %>%                      ## Linha 1    
  group_by(categoria) %>%       ## Linha 2    
  summarise(mean(concretude))   ## Linha 3  


esplex %>%                      ## Linha 1    
  group_by(categoria) %>%       ## Linha 2    
  summarise(mean(concretude),   ## Linha 3  
            n())  


esplex %>%                              ## Linha 1    
  group_by(categoria) %>%               ## Linha 2    
  summarise(media = mean(concretude),   ## Linha 3  
            n.obs = n())  


########################
## PAUSA PARA PRATICA ##
########################

## FAÇA AQUI SUA PRATICA

####################
## FIM DA PRATICA ##
####################

#### Visualizando distribuição de dados: histograma

# ordenando notas em ordem crescente e apresentando as 10 primeiras

sort(esplex$concretude)[1:10]


#### Amplitude e desvio-padrão

# valores minimo e maximo de imageabilidade

range(esplex$imageabilidade)

# amplitude da imageabilidade

diff(range(esplex$imageabilidade))

### calculando o desvio padrao
# calculando diferenca entre media e cada observacao

diferenca <- mean(esplex$imageabilidade)-esplex$imageabilidade

# soma dos quadrados das diferencas

soma.quadrados <-sum(diferenca^2)

# divisao da soma dos quadrados por n-1

divisao<-soma.quadrados/(length(esplex$imageabilidade)-1)

# raiz quadrada da divisao = desvio - padrao

desvio.padrao <- sqrt(divisao)

# desvio padrao de imageabilidade

desvio.padrao

### fim do calculo

# desvio-padrao de imageabilidade 

sd(esplex$imageabilidade)

########################
## PAUSA PARA PRATICA ##
########################

## FAÇA AQUI SUA PRATICA

####################
## FIM DA PRATICA ##
####################

#### Mediana

## criar um vetor
x <- c(3.1, 4.2, 7.2, 7.4, 5.5)

## colocar em ordem crescente
sort(x)

## extrair mediana
median(x)

## extrair media
mean(x)


## criar um vetor
x <- c(3.1, 4.2, 70.2, 7.4, 5.5)

## colocar em ordem crescente
sort(x)

## extrair mediana
median(x)

## extrair media
mean(x)

########################
## PAUSA PARA PRATICA ##
########################

## FAÇA AQUI SUA PRATICA

####################
## FIM DA PRATICA ##
####################

#### Comparando distribuições graficamente: boxplot

#### Desvio interquartílico

# medidas de posicao e dispersao para frq

esplex %>% 
  group_by(categoria) %>% 
  summarise(media = mean(frq), sd(frq), mediana = median(frq), desvio.iqr = IQR(frq))


#### Inspeção gráfica de dados

# ## sintaxe básica
# 
# ggplot(dados, aes(variavel_x, variavel_y))
# 

## vamos lembrar o nome das colunas do nosso dataframe

names(esplex)

# criando a estrutura básica do nosso gráfico

ggplot(esplex, aes(x = categoria, y = concretude))

# adicionando a camada de geometria, indicando geometria requerida

ggplot(esplex, aes(x = categoria, y = concretude))+
  geom_boxplot()

# ajustes na aparencia: fundo branco (tema) e alpha

ggplot(esplex, aes(x = categoria, y = concretude))+
  geom_boxplot(alpha = 0.5)+
  theme_bw()

# ajustes na aparencia: mudando cor

ggplot(esplex, aes(x = categoria, y = concretude))+
  geom_boxplot(alpha = 0.5, color = "red")+
  theme_bw()

# ajustes na aparencia: ligando cor a variavel

ggplot(esplex, aes(x = categoria, y = concretude, color = categoria))+
  geom_boxplot(alpha = 0.5)+
  theme_bw()

# ajustes na aparencia: ligando cor a variavel (fill)

ggplot(esplex, aes(x = categoria, y = concretude, fill = categoria))+
  geom_boxplot(alpha = 0.5)+
  theme_bw()

### histograma de concretude

ggplot(esplex, aes(concretude))+
geom_histogram()+
  theme_bw()

### histograma de concretude por categoria gramatical

ggplot(esplex, aes(concretude))+
geom_histogram()+
  theme_bw()+
  facet_wrap(~ categoria)

########################
## PAUSA PARA PRATICA ##
########################

## FAÇA AQUI SUA PRATICA

####################
## FIM DA PRATICA ##
####################

############################################
################## DIA 2 ###################
############################################

#### Por que fazemos análise estatística?

## calculando a probabilidade (x, size, prob)

dbinom(x=6, size=10, prob=0.5)

## calculando a probabilidade (x, size, prob)

dbinom(x=7, size=10, prob=0.5)

## calculando a probabilidade (x, size, prob)

dbinom(x=9, size=10, prob=0.5)

########################
## PAUSA PARA PRATICA ##
########################

## FAÇA AQUI SUA PRATICA

####################
## FIM DA PRATICA ##
####################



########################
## PAUSA PARA PRATICA ##
########################

## FAÇA AQUI SUA PRATICA

####################
## FIM DA PRATICA ##
####################




########################
## PAUSA PARA PRATICA ##
########################

## FAÇA AQUI SUA PRATICA

####################
## FIM DA PRATICA ##
####################




########################
## PAUSA PARA PRATICA ##
########################

## FAÇA AQUI SUA PRATICA

####################
## FIM DA PRATICA ##
####################

#### Fazendo um teste-t no R

## importando conjuntos de dados

cenario1 <- read.csv("dados/cenario1.csv")
cenario2 <- read.csv("dados/cenario1.csv")
cenario3 <- read.csv("dados/cenario1.csv")


## cenário 1

head(cenario1, 10)


## cenário 1

t.test(notas ~ metodo, amostra1)


## cenário 2

t.test(notas ~ metodo, cenario2)


## cenário 3

t.test(notas ~ metodo, cenario3)

############################################
################## DIA 3 ###################
############################################

#### Contrastando duas variáveis numéricas

#### Gráficos

## importar dados (se ainda não tiver importado)
esplex <- read.csv("dados/esplex_2025.csv")

str(esplex)

# adicionando a camada de geometria, indicando geometria de pontos

ggplot(esplex, aes(x = concretude, y = imageabilidade))

# adicionando a camada de geometria, indicando geometria de pontos
## alterando tema e o alpha (transparencia) dos pontos

ggplot(esplex, aes(x = concretude, y = imageabilidade))+
  geom_point(alpha = 0.5)+
  theme_bw()

########################
## PAUSA PARA PRATICA ##
########################

## FAÇA AQUI SUA PRATICA

####################
## FIM DA PRATICA ##
####################

#### Correlação e valor r


## coeficiente r entre concretude e imageabilidade

cor(esplex$concretude, esplex$imageabilidade)


########################
## PAUSA PARA PRATICA ##
########################

## FAÇA AQUI SUA PRATICA

####################
## FIM DA PRATICA ##
####################

#### Teste de hipóteses

## teste de correlacao

cor.test(esplex$imageabilidade, esplex$concretude)

########################
## PAUSA PARA PRATICA ##
########################

## FAÇA AQUI SUA PRATICA

####################
## FIM DA PRATICA ##
####################

#### Algumas questões com testes de hipóteses


## simulando amostra de uma população de média 0 e desvio-padrao 1 
rnorm(20)



t.test(rnorm(20), rnorm(20))

