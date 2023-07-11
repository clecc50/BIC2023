library(ggplot2)
library(dplyr)
iris
data <- iris
ggplot(data ,aes(x=Sepal.Width, y=Sepal.Length))+ giom_point()
library(esquisse)
install.packages("esquisse")
library(esquisse)
install.packages("dados")
library(dados)
dados_atmosfera
dados <- dados_atmosfera
dados |> esquisser()
#############
ggplot(dados) +
  aes(x = nuvem_media, y = nuvem_alta, colour = ano) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_gradient() +
  theme_minimal()
####################
install.packages("readxl")
library(readxl)
iris
data <- iris
library(esquisse)
dados |> esquisser()

library(ggplot2)

ggplot(dados) +
  aes(x = Sepal.Length, y = Sepal.Width, colour = Species) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal()
?esquisse
##############
library(ggplot2)
library(readxl)
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("readr")
library(readr)
read.csv()
top50_atv_and_correspondente_ros.csv
EmpSal <- read.csv('top50_atv_and_correspondente_ros.csv')
top50_atv_and_correspondente_ros.csv
read.csv(file, top50_atv_and_correspondente_ros.csv)
setwd("C:\\Users\\Elis\\OneDrive\\Documentos\\BioInfoClass")
read.csv("top50_atv_and_correspondente_ros.csv", header = TRUE
