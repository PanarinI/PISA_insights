Инструкции
Придумайте и проверьте три гипотезы для трёх исследовательских вопросов: одну гипотезу 
проверьте с помощью хи-квадрата, вторую с помощью корреляции Спирмена, третью — Пирсона.
Переменные вы выбираете сами. 



Задание выполняется на данных PISA-2015, база лежит в учебных материалах.


# Загрузим библиотеки и базу
library(rio)
library(tidyverse)
library(corrplot)
library(Hmisc)
library(dplyr)
library(forcats)
library (ggplot2)
library(skimr)

pisa <- import("/Users/Игорь/Desktop/Test_R/PISA.sav")

###ИВ 1. Существует ли связь между наличием классической литературы дома 
# и количеством электронных книг в России?
# H0: статистически-значимой связи нет; H1: статистически-значимая связь есть

# Выберем переменные, проверим их тип: ST011Q07Ta (наличие classic literature)
# и ST012Q08NA (кол-во e-books)

class(pisa$ST012Q08NA)
table(pisa$ST012Q08NA)
class(pisa$ST011Q07TA)
table(pisa$ST011Q07TA)

# Обе переменные разделены по категориям. Это значит, что оптимально использовать 
# метод хи-квадрат через таблицы частотности
# 1. Изменим класс переменных на factor
pisa$classlitf <- factor(pisa$ST011Q07TA, labels =c("yes","no"))
class(pisa$classlitf)
pisa$ebooksf <- factor(pisa$ST012Q08NA, labels =c("none","one", "two", "three or more"))
class(pisa$ebooksf)

short <- select (pisa, CNT, classlitf, ebooksf)
short <- short %>% 
  filter (CNT == "RUS")

# Построим таблицу сопряженности
table <- table(short$classlitf, short$ebooksf)
table
addmargins(table(short$classlitf, short$ebooksf))
prop.table(table(short$classlitf, short$ebooksf))*100
# Совместим категории "one", "two" и "three or more" no e-books
short$ebooksf <- fct_collapse(short$ebooksf, "one or more" = c("one", "two", "three or more"))
table <- table(short$classlitf, short$ebooksf)
table
addmargins(table(short$classlitf, short$ebooksf))
# Посчитаем процентные соотношения
prop.table(table(short$classlitf, short$ebooksf))*100
# Посмотрим распределение наличия классической литературы по кол-ву e-books
prop.table(table(short$classlitf, short$ebooksf), margin=2)*100
# Проверяем хи-квадрат:
chisq.test(short$classlitf, short$ebooksf)
#Статистический вывод: принимаем H1 > существует значимая связь между наличием 
# классической лит-ры и количеством эл. книг дома

# Создадим лист
chi <- chisq.test(short$classlitf, short$ebooksf)
chi$expected
chi$observed
chi$p.value
# результаты - в таблицу
library(sjPlot)
sjt.xtab(short$classlitf, short$ebooksf, 
         var.labels = c("Classic literature", "No. of e-books"),
         show.row.prc = TRUE, show.cell.prc =TRUE)

##ИВ 2. Есть ли связь между возрастом, в котором учащийся прибыл в страну, где 
# он сейчас учится, и уровнем его читательской компетенции?
# Выберем, посмотрим тип и переименуем переменные: ST021Q01TA (возраст, когда прибыл);
#READ (среднее по PV_READ'ам)


short2 <- select (pisa,
                  ST021Q01TA,
                  starts_with("PV") & ends_with ("READ"),
                  starts_with("PV") & ends_with ("MATH"),
                  starts_with("PV") & ends_with ("SCIE")
                  )
short2 <- rename(short2, age_arrived = ST021Q01TA)
short2$math <- rowMeans(short2[, c("PV1MATH","PV2MATH","PV3MATH","PV4MATH","PV5MATH","PV6MATH","PV7MATH","PV8MATH","PV9MATH","PV10MATH")])
short2$sci <- rowMeans(short2[, c("PV1SCIE","PV2SCIE","PV3SCIE","PV4SCIE","PV5SCIE","PV6SCIE","PV7SCIE","PV8SCIE","PV9SCIE","PV10SCIE")])
short2$read <- rowMeans(short2[, c("PV1READ","PV2READ","PV3READ","PV4READ","PV5READ","PV6READ","PV7READ","PV8READ","PV9READ","PV10READ")])

short2 <- select (short2, age_arrived, read, sci, math)
short2 <- na.omit(short2)
class(short2$age_arrived)
table(short2$age_arrived)
class(short2$read)
table(short2$read)

#Посмотрим на распределение reading, math и science: распределения близки к нормальному

ggplot(short2)+
  geom_density(aes(x=math)) +
  geom_density(aes(x=sci), col = "red") +
  geom_density(aes(x=read), col = "green") +
  xlab ("Score") +
  ylab ("Density") 

# У нас интервальная переменная reading и порядковая age_arrived. 
# Соответственно, выбираем коэффициент Спирмена
# Построим scatterplot чтобы визуализировать связь

ggplot(short2,
       aes(x=age_arrived, y=read)) +
  geom_point () +
  geom_smooth(method = "loess")

ggplot(short2) +
  #geom_smooth(aes(x=age_arrived, y=math),method = "lm", col = "red") +
  #geom_smooth(aes(x=age_arrived, y=sci),method = "lm", col = "blue") +
  geom_smooth(aes(x=age_arrived, y=read),method = "lm", col = "green") +
  xlab("Age_arrived") + 
  ylab("Average scores")

# связь немонотонна, сперва положительна, потом отрицательна. Какой силы эта связь?
# Применим коэффициент корреляции Спирмена
cor(short2$read, short2$age_arrived, method = "spearman")
cor(short2$math, short2$age_arrived, method = "spearman")
cor(short2$sci, short2$age_arrived, method = "spearman")
cor.test(short2$read, short2$age_arrived, method = "spearman")

# Коэф. корреляции = -0,13 (read), -0,11(math), -0.09(science): слабая отрицательная корреляция. H1 подтверждается. (почему-то
# Пирсон дал более правдоподобый (как кажется) результат: -0,18(read))

#Визуализируем как матрицу:
cor_m <- cor(short2)
cor_m
cor.test(cor_m)
corrplot(cor_m, type = "lower")

# Содержательный вывод: имеется слабая связь между тем, насколько давно переехал
# учащийся в страну и уровнем его читательской компетенции.
# Ограничения: с 11 до 17 лет все выборки - маленькие (N<40), что может повлиять на корреляцию


### ИВ№3 Связано ли количество уроков в неделю по предметам (math - MMINS; science - SMINS);
# READ - LMEANS) с уровнем компетенции - read, science, math. Сравним связь в России и в Великобритании.

#Проанализируем переменные MMINS, LMINS, SMINS:
class(pisa$MMINS)
table(pisa$MMINS)
math <- short2$math
sci <- short2$sci
read <- short2$read
short3 <- select (pisa, CNT, MMINS, LMINS, SMINS, math, sci, read)
short3 <- na.omit(short3)
short3_RUS <- short3 %>% 
  filter (CNT == "RUS")
ggplot(short3_RUS)+
  geom_density(aes(x=MMINS))
# Имеются сильные выбросы, обрежем их, стандартизуем шкалы и признаем распределениe MMINS и LMINS
# относительно-нормальным (вопрос - допустимо ли это в данном случае?):
short3_RUS <- short3_RUS %>% 
  filter (MMINS <420 & MMINS > 50,
          LMINS <420
          )
ggplot(short3_RUS)+
  geom_density(aes(x=MMINS, col = 'blue')) +
  geom_density(aes(x=LMINS, col = 'red')) + 
  xlab("MMINS / LMINS") + 
  ylab("density")
short3_RUS <- select (short3_RUS, MMINS, LMINS, math, read)

# Построим scatterplot
ggplot(short3_RUS,
       aes(x=MMINS, y=math)) +
  geom_point () +
  geom_smooth(method = "lm")
# В России что-то не то: reading skills коррелирует отрицательно с количеством уроков в неделю!
ggplot(short3_RUS,
       aes(x=LMINS, y=read)) +
  geom_point () +
  geom_smooth(method = "lm")

#По TUR

short3_TUR <- short3 %>% 
  filter (CNT == "TUR")
ggplot(short3_TUR)+
  geom_density(aes(x=LMINS))

short3_TUR <- short3_TUR %>% 
  filter (MMINS <420 & MMINS > 50,
          LMINS <420
  )
ggplot(short3_TUR)+
  geom_density(aes(x=MMINS, col = 'blue')) +
  geom_density(aes(x=LMINS, col = 'red'))
short3_TUR <- select (short3_TUR, MMINS, LMINS, math, read)


ggplot(short3_TUR,
       aes(x=MMINS, y=math)) +
  geom_point () +
  geom_smooth(method = "lm")
# В Турции читательская компетенция зависит от кол-ва уроков!
ggplot(short3_TUR,
       aes(x=LMINS, y=read)) +
  geom_point () +
  geom_smooth(method = "lm")

# Примерим коэффициент Пирсона
cor(short3_RUS$MMINS, short3_RUS$math, use = "complete.obs")
cor.test(short3_RUS$MMINS, short3_RUS$math)
cor(short3_RUS$LMINS, short3_RUS$read, use = "complete.obs")
cor.test(short3_RUS$LMINS, short3_RUS$read)
cor(short3_TUR$MMINS, short3_TUR$math, use = "complete.obs")
cor.test(short3_RUS$MMINS, short3_RUS$math)
cor(short3_TUR$LMINS, short3_TUR$read, use = "complete.obs")
cor.test(short3_TUR$LMINS, short3_TUR$read)
#Визуализируем матрицей
cor_RUS <- cor(short3_RUS)
cor_TUR <- cor(short3_TUR)
corrplot(cor_RUS, type = "lower")
corrplot(cor_TUR, type = "lower")
# Результат: Корреляция имеется, принимаем H1. 
# Содержательный вывод: количество часов занятий положительно коррелирует
# с баллом по предмету, однако по России видна парадоксальная
# обратная корреляция по русскому языку. Выдвигается гипотеза:
# переменная описана test language, возможно это часы подготовки к экзаменам,
# и если так, то, возможно, школьная подготовка к ЕГЭ по русскому не эффективна