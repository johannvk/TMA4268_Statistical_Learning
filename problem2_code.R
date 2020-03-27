# R code for problem 2:
library(knitr)
library(rmarkdown)
library(ggplot2)
library(ggfortify)
library(MASS)
library(dplyr)
library(ISLR)
library(reshape2)

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

lm_d_list = list()
poly_fitted_values = list()
poly_mse = list()
for(i in 1:10){
  cat("i:", i, "\n")
  lm_i = lm(Outstate ~ poly(Terminal, i), data=College_2[train.ind, ])
  lm_d_list[[i]] = lm_i
  poly_fitted_values[[i]] = lm_i$fitted.values
  poly_mse = sum(lm_i$residuals**2)/length(train.ind)
  cat("RSE: ", sum(lm_d_list[[i]]$residuals**2)/length(train.ind), "\n")
}

# poly_fitted_value_array = 
Outstate_fitted_df = do.call(cbind.data.frame, poly_fitted_values)
names(Outstate_fitted_df) = c("d=1", "d=2", "d=3", "d=4", "d=5", "d=6", "d=7", "d=8", "d=9", "d=10")
Outstate_fitted_df$Terminal = College_2[train.ind, ]$Terminal

names(Outstate_fitted_df)

# Outstate_df$Terminal
View(Outstate_fitted_df)
head(College_2[train.ind, ])

# "melt" the dataframe:
melted_Outstate_fitted_df = melt(Outstate_fitted_df, id.vars = "Terminal")

poly_plot = ggplot(data=College_2[train.ind, ], aes(x = Terminal, y = Outstate), type='p')
poly_plot = poly_plot + geom_line(data=melted_Outstate_fitted_df, aes(x=Terminal, y=value, col=variable)) + geom_line(size=1.0) + 
            ylab("Outstate")
print(poly_plot)

# test_plot =ggplot() + 
# print(test_plot)
# Scatter plot of original data:

# More manul approach:
# Add the degree d polynomial regression lines:
# colors = c("3300FF", "339933", "#FFCC00")
# for (i in c(1, 2, 3)){
#   poly_plot_df = data.frame(x_val = College_2[train.ind, ]$Terminal, y_val = lm_d_list[[i]]$fitted.values)
#   poly_plot = poly_plot + geom_line(data = poly_plot_df, aes(x = x_val, y = y_val), color=colors[[i]], size=1.2)
# }

print(poly_plot)

# TODO:
# Plot the results. Big plot, with 10 lines on might be messy. Maybe do d = 1, 2, 3, 6, 8, 10. A bit more pleasant to look at?
# Choose an optimal degree of freedom, AIC, BIC or something. Possibly CV.
# Use that number of degrees of freedom to compute a cubic spline interpolation, at the required quantiles.
