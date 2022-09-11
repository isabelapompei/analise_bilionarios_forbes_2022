

#-------------------------------------------------#
#                                                 #
#              MARIA ISABELA POMPEI               #
#           LABORATORIO DE ESTATISTICA            #   
#                                                 #
#-------------------------------------------------#

#-------------------------------------------------#
#                                                 #
#                  BIBLIOTECAS                    #
#                                                 #
#-------------------------------------------------#

#----------Instalando pacotes
#install.packages("readr")
library(readr)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("scales")
library(scales)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("forcats")
library(forcats)
#install.packages("corrplot")
library(corrplot)
library(readr)
library(tidyverse)
library(tidyverse)
install.packages("cluster")
library(cluster)
install.packages("dendextend")
library(dendextend)
install.packages("factoextra")
library(factoextra) 
install.packages("gridExtra")
library(gridExtra)
install.packages("purrr")
library(purrr)
library(readxl)

#-------------------------------------------------#
#                                                 #
#              LEITURA DE DADOS                   #
#                                                 #
#-------------------------------------------------#

#----------Lendo os dados 2022
data_2022 = read_csv("E:/DOCUMENTOS/ESTATISTICA GRADUACAO/2022/1 SEMESTRE 2022/LABORATORIO DE ESTATISTICA/trabalho_individual/archive (2)/forbes_2022_billionaires.csv")
View(data_2022)
colnames(data_2022)
str(data_2022)

#----------Lendo os dados 2021
data_2021 <- read_csv("E:/DOCUMENTOS/ESTATISTICA GRADUACAO/2022/1 SEMESTRE 2022/LABORATORIO DE ESTATISTICA/TRABALHO1/dados_bilionarios_forbes_2021/archive (2)/forbes_billionaires.csv")
View(data_2021)
colnames(data_2021)
str(data_2021)

#----------Rename colunas
data2022 = data_2022 %>% rename (ranking = 1,
                            nome = 2,
                            idade = 3,
                            fortuna = 4,
                            ano = 5,
                            mes = 6,
                            categoria = 7,
                            fonte = 8,
                            pais = 9,
                            estado = 10,
                            cidade = 11,
                            pais_cidadania = 12,
                            organizacao = 13,
                            self_made = 14,
                            genero = 15,
                            aniversario = 16,
                            titulo = 17,
                            score_filant = 18,
                            residencia =19,
                            numero_irmaos = 20,
                            bio = 21,
                            sobre = 22)
View (data2022)

       
#-------------------------------------------------#
#                                                 #
#            BILIONARIOS FORBES 2022              #
#             ANÁLISE EXPLORATORIA                #   
#                                                 #
#-------------------------------------------------#

#----------Idade 2022
#Hisograma
hist(data2022$idade,
     main="Histograma Idade Bilionários 2022",
     xlab="Idade",ylab="Frequência",
     col=c("#1FA662"))
#summary
summary (data2022$idade)
#boxplot
boxplot(data2022$idade,
        main = "Idade Bilionários 2022",
        col = "green")
#CONCLUSÃO: Temos a presença de alguns outliers abaixo do limite inferior, e a 
#média de bilionários é de aproximadamente 64 anos

#----------Idade 2021
#Hisograma
hist(data_2021$Age,
     main="Histograma Idade Bilionários 2021",
     xlab="Idade",ylab="Frequência",
     col=c("#1FA662"))
#summary
summary (data_2021$Age)
#boxplot
boxplot(data_2021$Age,
        main = "Idade Bilionários 2021",
        col = "green")
#CONCLUSÃO: Temos a presença de alguns outliers abaixo do limite inferior, e a 
#média de bilionários é de aproximadamente 64 anos


#----------Categoria dos Bilionários 2022
ggplot(data = data2022) +
  geom_bar(aes(x = fct_infreq(categoria), y = (..count..)/sum(..count..),
               fill = categoria),show.legend = F)+
  ggtitle("Categoria dos Bilionários de 2022")+
  labs(y = "Percentual de Bilionários", x = "Categoria") +
  scale_y_continuous(expand = expansion(add = c(0,0.02)),
                     labels = scales::percent_format()) +
  coord_flip()+
  scale_fill_manual(values=c("#3ABD7B","#36F595","#05B25B","#2CB973","#206643","#021B0E","#057C40","#CCFFE5","#2F6B4D","#66FFB2","#0B5731","#0DA95B","#0ED873","#079A50","#1BE17E","#99FFCC","#0D3C24","#59FAAA"))


#----------Organizando os países para plotar o gráfico
#Visualizando coluna de países
table(data2022$pais) #tabela para visualizar a quantidade de bilionários por países
sort(table(data2022$pais)) #colocando em ordem crescente
#Criando nova coluna para países, com apenas os 10 países com mais bilionários
data2022 = data2022 %>% 
  mutate(data2022 = case_when(TRUE ~ 'Outros'))
View(data2022)


#----------Gráfico para empreendedores (sim ou nao)
plot_empr = ggplot(data = data2022) +
geom_bar(aes(x = self_made, y = (..count..)/sum(..count..),
                          fill = self_made),show.legend = F)+
ggtitle("Percentual de Bilionários SelfMade")+
labs(y = "Percentual de Bilionários", x = "SelfMade") +
scale_y_continuous(expand = expansion(add = c(0,0.02)),
                                labels = scales::percent_format()) +
                                coord_flip()+
scale_fill_manual(values=c("#9FF9CC","#167646"))
plot_empr
plot_empr + scale_x_discrete(labels = c("Não", "Sim")) #alterando nome das colunas

           
#----------Quantidade de irmaos
plot_irmao = ggplot(data = data2022) +
             geom_bar(aes(x = numero_irmaos, y = (..count..)/sum(..count..)))+
             ggtitle( "Quantidade de Irmãos dos Bilionários")+
             labs(y = "Percentual de Bilionários", x = "Quantidade de irmãos") +
             scale_y_continuous(expand = expansion(add = c(0,0.05)),
                                labels = scales::percent_format())+
             coord_cartesian(xlim = c(0, 14)) +
             scale_y_continuous(expand = expansion(add = c(0,0.02)),
                             labels = scales::percent_format()) +
             coord_flip()
           
plot_irmao + scale_fill_manual(values = hcl.colors(n = 18,
                                                              palette = "Greens 3"))
plot_irmao + scale_x_discrete(labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "12", "14"))
           
table(dados_bilion$qntd_irmaos)
table(dados_bilion$qntd_irmaos == "12")
           

#---------- SelfMade 2021
plot_self2021 = ggplot(data = data_2021) +
  geom_bar(aes(x = Self_made, y = (..count..)/sum(..count..),
               fill = Self_made),show.legend = F)+
  ggtitle("Percentual de Bilionários SelfMade 2021")+
  labs(y = "Percentual de Bilionários", x = "SelfMade") +
  scale_y_continuous(expand = expansion(add = c(0,0.02)),
                     labels = scales::percent_format()) +
  coord_flip()+
  scale_fill_manual(values=c("#9FF9CC","#167646"))
plot_self2021
plot_self2021 + scale_x_discrete(labels = c("Não", "Sim", "Sem resposta")) #alterando nome das colunas
table(data_2021$Self_made)

#---------- Sexo 2022
plot_sexo2022 = ggplot(data = data_2022) +
  geom_bar(aes(x = gender, y = (..count..)/sum(..count..),
               fill = gender),show.legend = F)+
  ggtitle("Gênero Bilionários 2022")+
  labs(y = "Percentual de Bilionários", x = "Gênero") +
  scale_y_continuous(expand = expansion(add = c(0,0.02)),
                     labels = scales::percent_format()) +
  coord_flip()+
  scale_fill_manual(values=c("#9FF9CC","#167646"))
plot_sexo2022
plot_sexo2022 + scale_x_discrete(labels = c("Feminino", "Masculino", "Sem Resposta")) #alterando nome das colunas
table(data_2022$gender)
gen = (2341)/(311+2341)
gen



#-------------------------------------------------#
#                                                 #
#              LEITURA DE DADOS                   #
#                                                 #
#-------------------------------------------------#

#----------Lendo os dados
dados_forbes = read_csv("E:/DOCUMENTOS/ESTATISTICA GRADUACAO/2022/1 SEMESTRE 2022/LABORATORIO DE ESTATISTICA/TRABALHO1/dados_bilionarios_forbes/forbes_2022_billionaires.csv/forbes_2022_billionaires.csv")

#----------Vizualizar dados
View(dados_forbes)
glimpse(dados_forbes)
#2668 linhas 
colnames(dados_forbes)
str(dados_forbes)

#----------Remover algumas colunas nao necessárias
dados_forbes$year=NULL #todos apresentam o mesmo ano de 2022
dados_forbes$month=NULL #todos apresentam o mesmo mes de 4
dados_forbes$state=NULL #usaremos apenas o país
dados_forbes$city=NULL #apenas país
dados_forbes$bio=NULL
dados_forbes$title=NULL
dados_forbes$about=NULL
dados_forbes$birthDate=NULL
dados_forbes$organization=NULL
dados_forbes$residenceMsa = NULL
#dados_forbes$personName=NULL

#----------Renomeando as colunas
dados_bilion = dados_forbes %>% rename(idade = 3,
                                       fortuna = 4,
                                       categoria = 5,
                                       fonte = 6,
                                       pais = 7,
                                       pais_cidad = 8,
                                       self_made = 9,
                                       genero = 10,
                                       filantropia = 11,
                                       qntd_irmaos = 12
)

View(dados_bilion)
glimpse(dados_bilion)

#----------Existe 'NAS'?
is.na(dados_bilion)
is.na(dados_bilion$idade)
is.na(dados_bilion$fortuna)
is.na(dados_bilion$categoria)
is.na(dados_bilion$fonte)
is.na(dados_bilion$pais)
is.na(dados_bilion$self_made)
is.na(dados_bilion$genero)
is.na(dados_bilion$filantropia)
is.na(dados_bilion$qntd_irmaos)

#----------Visualizando classes dos dados
str(dados_bilion)

#----------Visualizando as colunas
colnames(dados_bilion)
#10 colunas

#-------------------------------------------------#
#                                                 #
#         CRIANDO VARIAVEIS PADRONIZADAS          #
#                                                 #
#-------------------------------------------------#

#----------Pontuacao padronizada
#(X-Xmax)/(Xmax - Xmin)
max(dados_bilion$fortuna) #219000
min(dados_bilion$fortuna) #1000
dados_bilion_new = dados_bilion %>% mutate (fortuna_padronizada = (fortuna - (1000))/((219000)-(1000)))
View(dados_bilion_new)


#-------------------------------------------------#
#                                                 #
#               ANÁLISE DE CLUSTER                #
#                                                 #
#-------------------------------------------------#

View(dados_cluster)

dados_cluster = dados_bilion
dados_cluster$personName = NULL
dados_cluster$categoria = NULL
dados_cluster$fonte = NULL
dados_cluster$pais = NULL
dados_cluster$pais_cidad = NULL
dados_cluster$self_made = NULL
dados_cluster$genero = NULL
dados_cluster$filantropia = NULL
dados_cluster$qntd_irmaos = NULL
dados_cluster$fortuna_padronizada = NULL

#Tratando NA
table(is.na(dados_cluster$idade))
dados_cluster_new = dados_cluster %>% 
  mutate(idade = ifelse((is.na(idade)),(mean(idade)),idade))
View(dados_cluster_new)
dados = drop_na(dados_cluster_new)
sum(is.na(dados$idade))

#Rodar o modelo
mcdonalds.k2 <- kmeans(dados, centers = 2)

#Visualizar os clusters
fviz_cluster(mcdonalds.k2, data = dados, main = "Cluster K2")

#Criar clusters
mcdonalds.k3 <- kmeans(dados, centers = 3)
fviz_cluster(mcdonalds.k3, data = dados, main = "Cluster K3")
mcdonalds.k4 <- kmeans(dados, centers = 4)
fviz_cluster(mcdonalds.k4, data = dados, main = "Cluster K4")
mcdonalds.k5 <- kmeans(dados, centers = 5)
fviz_cluster(mcdonalds.k5, data = dados, main = "Cluster K5")
mcdonalds.k6 <- kmeans(dados, centers = 6)
fviz_cluster(mcdonalds.k6, data = dados, main = "Cluster K6")
mcdonalds.k7 <- kmeans(dados, centers = 7)
fviz_cluster(mcdonalds.k7, data = dados, main = "Cluster K7")
mcdonalds.k8 <- kmeans(dados, centers = 8)
fviz_cluster(mcdonalds.k8, data = dados, main = "Cluster K8")

#Criar graficos
G1 <- fviz_cluster(mcdonalds.k2, geom = "point", data = dados) + ggtitle("k = 2")
G2 <- fviz_cluster(mcdonalds.k3, geom = "point",  data = dados) + ggtitle("k = 3")
G3 <- fviz_cluster(mcdonalds.k4, geom = "point",  data = dados) + ggtitle("k = 4")
G4 <- fviz_cluster(mcdonalds.k5, geom = "point",  data = dados) + ggtitle("k = 5")
G5 <- fviz_cluster(mcdonalds.k6, geom = "point",  data = dados) + ggtitle("k = 6")
G6 <- fviz_cluster(mcdonalds.k7, geom = "point",  data = dados) + ggtitle("k = 7")
#Imprimir graficos na mesma tela
grid.arrange(G1, G2, G3, G4, G5,G6, nrow = 3)

#VERIFICANDO ELBOW 
fviz_nbclust(dados, kmeans, method = "wss")

#padronizar dados
dados.padronizados <- scale(dados)

#juntando dados
dados_bilion %>% drop_na(idade)
dados_bilion_ = dados_bilion %>% drop_na(idade)
sum(is.na(dados_bilion_$idade))

dados_bilion_cluster = data.frame(mcdonalds.k5$cluster)
View(dados_bilion_cluster)

#Agrupar cluster e base
dados_bilion_final =  cbind(dados_bilion_, dados_bilion_cluster)
View(dados_bilion_final)


#FAZENDO ANALISE DESCRITIVA
#MEDIAS das variaveis por grupo

mediagrupo <- dados_bilion_final %>% 
  group_by(mcdonalds.k5$cluster) %>% 
  summarise(n = n(),
            bilionarios_fortuna = mean(fortuna), 
            sd_bilionarios_fortuna = sd(fortuna),
            bilionarios_idade = mean(idade),
            sd_bilionarios_idade = sd(idade))
mediagrupo
fviz_cluster(mcdonalds.k5, geom = "point", data = dados) + ggtitle("Cluster k = 5")

grupo1 = dados_bilion_final %>%
  filter(mcdonalds.k5$cluster == 1)

idade_grupo1 = hist(grupo1$idade,
     main="Histograma Idade Bilionários Grupo 1",
     xlab="Idade",ylab="Frequência",
     col=c("#1FA662"))
fortuna_grupo1 = hist(grupo1$fortuna,
                    main="Histograma Fortuna Bilionários Grupo 1",
                    xlab="Fortuna",ylab="Frequência",
                    col=c("#1FA662"))


grupo2 = dados_bilion_final %>%
  filter(mcdonalds.k5$cluster == 2)

idade_grupo2 = hist(grupo2$idade,
                    main="Histograma Idade Bilionários Grupo 2",
                    xlab="Idade",ylab="Frequência",
                    col=c("#1FA662"))
fortuna_grupo2 = hist(grupo2$fortuna,
                      main="Histograma Fortuna Bilionários Grupo 2",
                      xlab="Idade",ylab="Frequência",
                      col=c("#1FA662"))


grupo3 = dados_bilion_final %>%
  filter(mcdonalds.k5$cluster == 3)

idade_grupo3 = hist(grupo3$idade,
                    main="Histograma Idade Bilionários Grupo 3",
                    xlab="Idade",ylab="Frequência",
                    col=c("#1FA662"))
fortuna_grupo3 = hist(grupo3$fortuna,
                      main="Histograma Fortuna Bilionários Grupo 3",
                      xlab="Idade",ylab="Frequência",
                      col=c("#1FA662"))


grupo4 = dados_bilion_final %>%
  filter(mcdonalds.k5$cluster == 4)

idade_grupo4 = hist(grupo4$idade,
                    main="Histograma Idade Bilionários Grupo 4",
                    xlab="Idade",ylab="Frequência",
                    col=c("#1FA662"))
fortuna_grupo4 = hist(grupo4$fortuna,
                      main="Histograma Fortuna Bilionários Grupo 4",
                      xlab="Idade",ylab="Frequência",
                      col=c("#1FA662"))

grupo5 = dados_bilion_final %>%
  filter(mcdonalds.k5$cluster == 5)

idade_grupo5 = hist(grupo5$idade,
                    main="Histograma Idade Bilionários Grupo 5",
                    xlab="Idade",ylab="Frequência",
                    col=c("#1FA662"))
fortuna_grupo5 = hist(grupo5$fortuna,
                      main="Histograma Fortuna Bilionários Grupo 5",
                      xlab="Idade",ylab="Frequência",
                      col=c("#1FA662"))



#---------- Histogramas Idade
par(mfrow=c(2,3))
idade_grupo1 = hist(grupo1$idade,
                    main="Idade Grupo 1",
                    xlab="Idade",ylab="Frequência",
                    col=c("#1FA662"))
idade_grupo2 = hist(grupo2$idade,
                    main="Idade Grupo 2",
                    xlab="Idade",ylab="Frequência",
                    col=c("#1FA662"))
idade_grupo3 = hist(grupo3$idade,
                    main="Idade Grupo 3",
                    xlab="Idade",ylab="Frequência",
                    col=c("#1FA662"))
idade_grupo4 = hist(grupo4$idade,
                    main="Idade Grupo 4",
                    xlab="Idade",ylab="Frequência",
                    col=c("#1FA662"))
idade_grupo5 = hist(grupo5$idade,
                    main="Idade Grupo 5",
                    xlab="Idade",ylab="Frequência",
                    col=c("#1FA662"))
summary(grupo1$idade)
summary(grupo2$idade)
summary(grupo3$idade)
summary(grupo4$idade)
summary(grupo5$idade)


#---------- Histogramas Fortunas
par(mfrow=c(2,3))
fortuna_grupo1 = hist(grupo1$fortuna,
                      main="Fortuna Grupo 1",
                      xlab="Fortuna",ylab="Frequência",
                      col=c("#1FA662"))
fortuna_grupo2 = hist(grupo2$fortuna,
                      main="Fortuna Grupo 2",
                      xlab="Fortuna",ylab="Frequência",
                      col=c("#1FA662"))
fortuna_grupo3 = hist(grupo3$fortuna,
                      main="Fortuna Grupo 3",
                      xlab="Fortuna",ylab="Frequência",
                      col=c("#1FA662"))
fortuna_grupo4 = hist(grupo4$fortuna,
                      main="Fortuna Grupo 4",
                      xlab="Fortuna",ylab="Frequência",
                      col=c("#1FA662"))
fortuna_grupo5 = hist(grupo5$fortuna,
                      main="Fortuna Grupo 5",
                      xlab="Fortuna",ylab="Frequência",
                      col=c("#1FA662"))

summary(grupo1$fortuna)
summary(grupo2$fortuna)
summary(grupo3$fortuna)
summary(grupo4$fortuna)
summary(grupo5$fortuna)

  

#---------- Sexo
par(mfrow=c(2,3))

#Grupo 1
grupo1_genero = ggplot(data = grupo1) +
  geom_bar(aes(x = genero, y = (..count..)/sum(..count..),
               fill = genero),show.legend = F)+
  ggtitle("Gênero Grupo 1 ")+
  labs(y = "Percentual de Bilionários", x = "Gênero") +
  scale_y_continuous(expand = expansion(add = c(0,0.02)),
                     labels = scales::percent_format()) +
  coord_flip()+scale_fill_manual(values=c("#9FF9CC","#167646"))
grupo1_genero
table(dados_bilion_final$genero)
table(grupo1$genero) #15% mulher e 84.9% homem

#Grupo 2
grupo2_genero = ggplot(data = grupo2) +
  geom_bar(aes(x = genero, y = (..count..)/sum(..count..),
               fill = genero),show.legend = F)+
  ggtitle("Gênero Grupo 2 ")+
  labs(y = "Percentual de Bilionários", x = "Gênero") +
  scale_y_continuous(expand = expansion(add = c(0,0.02)),
                     labels = scales::percent_format()) +
  coord_flip()+scale_fill_manual(values=c("#9FF9CC","#167646"))
grupo2_genero
table(grupo2$genero) #12% mulher e homem 88%  %homem

#Grupo 3
grupo3_genero = ggplot(data = grupo3) +
  geom_bar(aes(x = genero, y = (..count..)/sum(..count..),
               fill = genero),show.legend = F)+
  ggtitle("Gênero Grupo 3")+
  labs(y = "Percentual de Bilionários", x = "Gênero") +
  scale_y_continuous(expand = expansion(add = c(0,0.02)),
                     labels = scales::percent_format()) +
  coord_flip()+scale_fill_manual(values=c("#9FF9CC","#167646"))
grupo3_genero
table(grupo3$genero) #10.8% mulher e 89.2% homem

#Grupo 4
grupo4_genero = ggplot(data = grupo4) +
  geom_bar(aes(x = genero, y = (..count..)/sum(..count..),
               fill = genero),show.legend = F)+
  ggtitle("Gênero Grupo 4")+
  labs(y = "Percentual de Bilionários", x = "Gênero") +
  scale_y_continuous(expand = expansion(add = c(0,0.02)),
                     labels = scales::percent_format()) +
  coord_flip()+scale_fill_manual(values=c("#167646"))
grupo4_genero
table(grupo4$genero) #14% mulher e 86% homem

#Grupo 5
grupo5_genero = ggplot(data = grupo5) +
  geom_bar(aes(x = genero, y = (..count..)/sum(..count..),
               fill = genero),show.legend = F)+
  ggtitle("Gênero Grupo 5")+
  labs(y = "Percentual de Bilionários", x = "Gênero") +
  scale_y_continuous(expand = expansion(add = c(0,0.02)),
                     labels = scales::percent_format()) +
  coord_flip()+scale_fill_manual(values=c("#9FF9CC","#167646"))
grupo5_genero
table(grupo5$genero) #11% mulher e #89% homens 1973

grupo1$self_made

#---------- Self Made

#Grupo 1
grupo1_selfmade = ggplot(data = grupo1) +
  geom_bar(aes(x = self_made, y = (..count..)/sum(..count..),
               fill = self_made),show.legend = F)+
  ggtitle("Self Made Grupo 1")+
  labs(y = "Percentual de Bilionários", x = "Self Made") +
  scale_y_continuous(expand = expansion(add = c(0,0.02)),
                     labels = scales::percent_format()) +
  coord_flip()+
  scale_fill_manual(values=c("#9FF9CC","#167646"))
grupo1_selfmade
table(grupo1$self_made) #36% false e 64% true

#Grupo 2
grupo2_selfmade = ggplot(data = grupo2) +
  geom_bar(aes(x = self_made, y = (..count..)/sum(..count..),
               fill = self_made),show.legend = F)+
  ggtitle("Self Made Grupo 2")+
  labs(y = "Percentual de Bilionários", x = "Self Made") +
  scale_y_continuous(expand = expansion(add = c(0,0.02)),
                     labels = scales::percent_format()) +
  coord_flip()+
  scale_fill_manual(values=c("#9FF9CC","#167646"))
grupo2_selfmade
table(grupo2$self_made) #29%false e 71%true

#Grupo 3
grupo3_selfmade = ggplot(data = grupo3) +
  geom_bar(aes(x = self_made, y = (..count..)/sum(..count..),
               fill = self_made),show.legend = F)+
  ggtitle("Self Made Grupo 3")+
  labs(y = "Percentual de Bilionários", x = "Self Made") +
  scale_y_continuous(expand = expansion(add = c(0,0.02)),
                     labels = scales::percent_format()) +
  coord_flip()+
  scale_fill_manual(values=c("#9FF9CC","#167646"))
grupo3_selfmade
table(grupo3$self_made) #42% false e 58% true 

#Grupo 4
grupo4_selfmade = ggplot(data = grupo4) +
  geom_bar(aes(x = self_made, y = (..count..)/sum(..count..),
               fill = self_made),show.legend = F)+
  ggtitle("Self Made Grupo 4")+
  labs(y = "Percentual de Bilionários", x = "Self Made") +
  scale_y_continuous(expand = expansion(add = c(0,0.02)),
                     labels = scales::percent_format()) +
  coord_flip()+
  scale_fill_manual(values=c("#9FF9CC","#167646"))
grupo4_selfmade
table(grupo4$self_made) #12%true e 88% false

#Grupo 5
grupo5_selfmade = ggplot(data = grupo5) +
  geom_bar(aes(x = self_made, y = (..count..)/sum(..count..),
               fill = self_made),show.legend = F)+
  ggtitle("Self Made Grupo 5")+
  labs(y = "Percentual de Bilionários", x = "Self Made") +
  scale_y_continuous(expand = expansion(add = c(0,0.02)),
                     labels = scales::percent_format()) +
  coord_flip()+
  scale_fill_manual(values=c("#9FF9CC","#167646"))
grupo5_selfmade
table(grupo5$self_made) #27%false e 72% true












