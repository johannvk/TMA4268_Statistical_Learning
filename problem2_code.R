# R code for problem 2:
library(knitr)
library(rmarkdown)
library(ggplot2)
library(ggfortify)
library(MASS)
library(dplyr)
library(ISLR)
library(reshape2)
library(RColorBrewer)

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

# Initializing storage for the polynomial regression models and predicted values:
poly_lm_list = list()
poly_predicted_values = list()
poly_mse = list()


Terminal_range = range(College_2[train.ind, ]$Terminal)
Terminal_seq = seq(from=Terminal_range[1], to=Terminal_range[2], length.out = 100)
# Making the d_max polynomial regression models:
d_max = 10
for(i in 1:d_max){
  cat("i:", i, "\n")
  lm_i = lm(Outstate ~ poly(Terminal, i), data=College_2[train.ind, ])
  poly_lm_list[[i]] = lm_i
  poly_predicted_values[[i]] = predict(lm_i, newdata = list(Terminal=Terminal_seq))
  poly_mse[[i]] = sum(lm_i$residuals**2)/length(train.ind)
  # cat("RSE: ", sum(poly_lm_list[[i]]$residuals**2)/length(train.ind), "\n")
}

Outstate_fitted_df = do.call(cbind.data.frame, poly_predicted_values)
names(Outstate_fitted_df) = c("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10")
Outstate_fitted_df$Terminal_seq = Terminal_seq

names(Outstate_fitted_df)

# Outstate_df$Terminal
View(Outstate_fitted_df)

# Abandoned:
# "melt" the dataframe:
# melted_Outstate_fitted_df = melt(Outstate_fitted_df, id.vars = "Terminal")
# poly_plot = poly_plot + geom_line(data=melted_Outstate_fitted_df, aes(x=Terminal, y=value, col=variable)) + geom_line(size=1.0) + 
#   ylab("Outstate")

# Add the original scatter-plot data:
col = heat.colors(10)
poly_plot = ggplot(data=College_2[train.ind, ], aes(x = Terminal, y = Outstate)) + geom_point()

# Manuel test av å legge til to linjer: Begge vises.
poly_plot = poly_plot + geom_line(data=Outstate_fitted_df, aes(x=Terminal_seq, y=Outstate_fitted_df[, 1]), size=1.1, color=col[[1]])
poly_plot = poly_plot + geom_line(data=Outstate_fitted_df, aes(x=Terminal_seq, y=Outstate_fitted_df[, 2]), size=1.1, color=col[[2]])

# Problematiske stedet: Får bare at den siste linjen som legges til på plottet blir igjen:
# for(i in 1:d_max){
#  cat("i:", i, "\n")
#  poly_plot = poly_plot + geom_line(data=Outstate_fitted_df, aes(x=Terminal_seq, y=Outstate_fitted_df[, i]), size=1.1, color=col[[i]])
#  print(poly_plot)
#}

print(poly_plot)

# TODO:
# Plot the results. Big plot, with 10 lines on might be messy. Maybe do d = 1, 2, 3, 6, 8, 10. A bit more pleasant to look at?
# Choose an optimal degree of freedom, AIC, BIC or something. Possibly CV.
# Use that number of degrees of freedom to compute a cubic spline interpolation, at the required quantiles.
