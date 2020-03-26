# R code for problem 2:
library(knitr)
library(rmarkdown)
library(ggplot2)
library(ggfortify)
library(MASS)
library(dplyr)
library(ISLR)

# Loading College data:
set.seed(1)
train.ind = sample(1:nrow(College), 0.5 * nrow(College))
college.train = College[train.ind, ]
college.test = College[-train.ind, ]
str(College)

predictors = c("Private", "Room.Board", "Terminal", "perc.alumni", "Expend", "Grad.Rate")
response = c("Outstate")
College_2 = select(College, predictors, response)
names(College_2)

color_data = College_2$Private
p_Private = ggplot(College_2, aes(x = Private, y = Outstate, color=color_data)) + geom_point() + theme(legend.position="top")
p_Room.Board = ggplot(College_2, aes(x = Room.Board, y = Outstate, color=color_data)) + geom_point() + theme(legend.position="none")
p_Terminal = ggplot(College_2, aes(x = Terminal, y = Outstate, color=color_data)) + geom_point() + theme(legend.position="none")
p_perc.alumni = ggplot(College_2, aes(x = perc.alumni, y = Outstate, color=color_data)) + geom_point() + theme(legend.position="none") 
p_Expend = ggplot(College_2, aes(x = Expend, y = Outstate, color=color_data)) + geom_point() + theme(legend.position="none")
p_Grad.Rate = ggplot(College_2, aes(x = Grad.Rate, y = Outstate, color=color_data)) + geom_point() + theme(legend.position="none")

gridExtra::grid.arrange(p_Private, p_Room.Board, p_Terminal, p_perc.alumni, p_Expend, p_Grad.Rate, nrow=3, ncol=2)

