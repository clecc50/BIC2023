library(ggplot2)
library(dplyr)
#dataset
names(iris)

#plot
ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length))+
  geom_point()+
  theme()

#new dataset
EmpSal <- read.csv('top50_atv_and_correspondente_ros.csv',
                   sep = ";")
head(EmpSal)

#new plot
p1=ggplot(EmpSal, aes(x=log2FC_ATV, y=pvalue_ATV))+
  geom_point()+
  theme(axis.title = element_text(size = 22,face = "bold"),
        axis.text = element_blank())
p1
