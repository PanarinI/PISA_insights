library(rio)
library(tidyverse)
library(corrplot)
library(Hmisc)
library(dplyr)
library(forcats)
library (ggplot2)
library(ggalt)
library(skimr)
library(cluster) # кластерный анализ
library(factoextra) # визуализации
library(pvclust) # тоже визуализация
library(NbClust) # визуализации и неиерархический анализ
library(dendextend) # будем раскрашивать дерево



data <- import("/Users/Игорь/Desktop/ВШЭ/Курс 1/Модуль 2/АнДан_Капуза/Кластерный анализ/univer_2017.xlsx")

                               ### Кластеризация ###
### 1. Доходы ВУЗа/общая численность НР
#Выберем переменные, стандартизуем, оставим 3 сигма (чтобы выбросы не влияли)
task1 <- select (data, TOTINC, SCIENTIST)
task1$st_TOTINC <- scale(data$TOTINC)
task1$st_SCIENTIST <- scale(data$SCIENTIST)
task1$TOTINC <- NULL
task1$SCIENTIST <-  NULL
task1 <- na.omit(task1)   
task1 <- filter(task1, st_TOTINC >-3  & st_TOTINC <3)
task1 <- filter(task1, st_SCIENTIST >-3  & st_SCIENTIST <3)
#Посмотрим распределения каждой переменной. Распределения экспоненциальные
# (это относится ко всем рассмотренным далее переменным, что говорит там о
# типичности распределения ВУЗов по всем параметрам)
hist(task1)
# Посмотрим диаграмму рассеяния:
ggplot(task1,
       aes(x=st_TOTINC, y=st_SCIENTIST)) +
  geom_point () +
  xlab("Доход") +
  ylab("Кол-во НР")
skim(task1)
# Визуально хочется выделить 3 кластера - основная масса слева снизу, разреженная область
# справа снизу и несколько случаев справа сверху. Проведем неиерархический КА (k-means)

#k-means
cl <- kmeans(task1, 3)
#Присвоим наблюдениям номера кластеров
task1$kmeans2 <- cl$cluster
task1 %>% filter(kmeans2 == 1) %>% skim # N=44,   mean доход=1.86 mean НР=0.635
task1 %>% filter(kmeans2 == 2) %>% skim # N=229,  mean доход=0.4 mean НР=0.03
task1 %>% filter(kmeans2 == 3) %>% skim # N=1163, mean доход=-0.275 mean НР=-0.09

# Запустим функцию NbClust для определения оптимального числа кластеров.
# Предлагает 6 кластеров
res <- NbClust(task1, min.nc = 2, max.nc = 8, method = "kmeans")
fviz_nbclust(res)
#Попробуем 6:
cl_6 <- kmeans(task1, 6)
task1$kmeans6 <- cl_6$cluster
# Визуализируем 6:
clusterplot <- par(mfrow = c(1, 2))      
plot(task1[c("st_TOTINC", "st_SCIENTIST")], col=cl_6$cluster, xlab="Доход", ylab = "Кол-во НР")
points(cl_6$centers[,c("st_TOTINC", "st_SCIENTIST")], col=10:20, pch=20, cex=2)
#Визуализируем 3:
clusterplot <- par(mfrow = c(1, 2))      
plot(task1[c("st_TOTINC", "st_SCIENTIST")], col=cl$cluster, xlab="Доход", ylab = "Кол-во НР")
points(cl$centers[,c("st_TOTINC", "st_SCIENTIST")], col=4:6, pch=20, cex=2)

#Содержательное описание (для 3 кластеров):
# "Богатые и крупные" N=44,   mean доход=1.86 mean НР=0.635
# "Средний класс, средний размер" N=229,   mean доход=0.4 mean НР=0.03
# "Основная масса - скромный доход, небольшие", N=1163, mean доход=-0.275 mean НР=-0.09

###2. Кол-во публикаций в WoS / кол-во цитирований в WoS

#Выберем переменные, стандартизуем, оставим 3 сигма (чтобы выбросы не влияли)
task2 <- select (data, WOSPUBL, WOSCIT)
task2$st_WOSPUBL <- scale(data$WOSPUBL)
task2$st_WOSCIT <- scale(data$WOSCIT)
task2$WOSPUBL <- NULL
task2$WOSCIT <-  NULL
task2 <- na.omit(task2)   
task2 <- filter(task2, st_WOSPUBL >-3  & st_WOSPUBL <3)
task2 <- filter(task2, st_WOSCIT >-3  & st_WOSCIT <3)
skim(task2$st_WOSCIT)
#Посмотрим распределения:
hist(task2$st_WOSPUBL)
hist(task2$st_WOSCIT)
#
quantile(task2$st_WOSPUBL, .90)
# Посмотрим диаграмму рассеяния:
ggplot(task2,
       aes(x=st_WOSPUBL, y=st_WOSCIT)) +
  geom_point () +
  xlab("Публикации") +
  ylab("Цитирования")
skim(task2)
# Визуально выделяются 2 различных по плотности кластера по оси X (Публикации. 
# Первый кластер ~ до 0.7 сигма, второй - от 0.7 сигма. По количеству цитирований
#имеются одиночные выбросы, но 99% значений - в пределах 0.25 сигма
hist(task2)

# Но предположим, что у нас нет идеи, сколько должно быть кластеров > иерархический КА
#Считаем дистанцию (попробуем метод Manhattan)
md_task2 <- dist(task2,method =  "manhattan")
head(md_task2)
tail(md_task2)
#Кластерный анализ, Wards
clusters_task2 <- hclust (md_task2, method = "ward.D2")
plot(clusters_task2, labels = F, hang = -1)
#Покрасим дендрограмму 
dendr_task2 <- as.dendrogram(clusters_task2)
col_dendr_task2 <- color_branches(dendr_task2, k = 3, groupLabels = T)
plot(col_dendr_task2)
# Похоже, что кластеров должно быть 2 или 3
rect.hclust(clusters_task2, k = 3)
# Проверим методом локтя
fviz_nbclust(task2, kmeans, method = "wss")+
  labs(subtitle = "Elbow method")
#Присвоим наблюдениям номера кластеров
groups2_task2 <- cutree(clusters_task2, k = 2)
task2$groups2 <- factor(cutree(clusters_task2, k = 2))
#Описательные статистики по кластерам, содержание
task2 %>% filter(groups3 == 1) %>% skim # N=1280, mean PUBL=-0.17 mean CIT=-0.06
task2 %>% filter(groups3 == 2) %>% skim # N=165,  mean PUBL=0.57 mean CIT=0.039 
#Визуализируем 2 кластера на графике
clusterplot_task2 <- par(mfrow = c(1, 2))      
plot(task2[c("st_WOSPUBL", "st_WOSCIT")], col=task2$groups2, xlab="Публикации", ylab = "Цитирование")
points(clusters_task2$centers[,c("st_WOSPUBL", "st_WOSCIT")], col=4:6, pch=20, cex=2)

# Запустим функцию NbClust для определения оптимального числа кластеров.
res <- NbClust(task2, min.nc = 2, max.nc = 8, method = "kmeans")
fviz_nbclust(res)
# Оптимально - 3. Попробуем
groups3_task2 <- cutree(clusters_task2, k = 3)
task2$groups3 <- factor(cutree(clusters_task2, k = 3))
clusters_task2_k <- kmeans(task2, 3)
#Визуализируем кластеры на графике
clusterplot_task2 <- par(mfrow = c(1, 2))      
plot(task2[c("st_WOSPUBL", "st_WOSCIT")], col=task2$groups3, xlab="Публикации", ylab = "Цитирование") +
points(clusters_task2$centers[,c("st_WOSPUBL", "st_WOSCIT")], col=7:9, pch=20, cex=2)

#Описательные статистики по 3 кластерам, содержание
task2 %>% filter(groups3 == 1) %>% skim # N=1280, mean PUBL=-0.17 mean CIT=-0.06
task2 %>% filter(groups3 == 2) %>% skim # N=136,  mean PUBL=0.378 mean CIT=0.00 
task2 %>% filter(groups3 == 3) %>% skim # N=29,  mean PUBL=1.49 mean CIT=0.20 
#  Группа 1 не изменилась. Группа 2 разделилась на 2. 
# Вычислим соотношения CIT и PUBL в кластерах 1 и 2:
#Группа 1: -0.06/-0.17 = 0.35
#Группа 2: 0.004/0.378 = 0.01

# В целом можно признать группу 1 "эффективнее" по КПД, чем группа 2, потому что
# при меньшем кол-ве публикаций в WoS цитируемость здесь примерно та же
# В малочисленной группе 3 больше пишут и их больше цитируют

