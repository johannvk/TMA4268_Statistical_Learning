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
library(splines)
library(caret)


# Loading College data:
set.seed(1)
train.ind = sample(1:nrow(College), 0.5 * nrow(College))
college.train = College[train.ind, ]
college.test = College[-train.ind, ]
str(College)

predictors = c("Private", "Room.Board", "Terminal", "perc.alumni", "Expend", "Grad.Rate")
response = c("Outstate")
College_2 = select(college.train, predictors, response)
names(College_2)

color_data = College_2$Private
p_Private = ggplot(College_2, aes(x = Private, y = Outstate, color=color_data)) + geom_point() + theme(legend.position="top")
p_Room.Board = ggplot(College_2, aes(x = Room.Board, y = Outstate, color=color_data)) + geom_point() + theme(legend.position="none")
p_Terminal = ggplot(College_2, aes(x = Terminal, y = Outstate, color=color_data)) + geom_point() + theme(legend.position="none")
p_perc.alumni = ggplot(College_2, aes(x = perc.alumni, y = Outstate, color=color_data)) + geom_point() + theme(legend.position="none") 
p_Expend = ggplot(College_2, aes(x = Expend, y = Outstate, color=color_data)) + geom_point() + theme(legend.position="none")
p_Grad.Rate = ggplot(College_2, aes(x = Grad.Rate, y = Outstate, color=color_data)) + geom_point() + theme(legend.position="none")

gridExtra::grid.arrange(p_Private, p_Room.Board, p_Terminal, p_perc.alumni, p_Expend, p_Grad.Rate, nrow=3, ncol=2)


### POLYNOMIAL REGRESSION ###
# Initializing storage for the polynomial regression models and predicted values:
poly_predicted_values = list()
poly_mse = list()

# Making the d_max polynomial regression models and find their prediction on a sequence of Terminal values:
Terminal_range = range(College_2$Terminal)
Terminal_seq = seq(from=Terminal_range[1], to=Terminal_range[2], length.out = 100)
d_max = 10
for(i in 1:d_max){
  cat("i:", i, "\n")
  lm_i = lm(Outstate ~ poly(Terminal, i), data=College_2)
  poly_predicted_values[[i]] = predict(lm_i, newdata = list(Terminal=Terminal_seq))
  poly_mse[[i]] = sum(lm_i$residuals**2)/length(train.ind)
}

# Buil a dataframe out of the predicted values from the polynomial regression:
Outstate_fitted_df = do.call(cbind.data.frame, poly_predicted_values)
names(Outstate_fitted_df) = c("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10")
Outstate_fitted_df$Terminal_seq = Terminal_seq

# Plot a few of the polynomial fits along with the original data:
col = brewer.pal(n=8, name="Dark2")
poly_plot = ggplot(data = Outstate_fitted_df, aes(x = Terminal_seq)) + geom_point(data=College_2, aes(x = Terminal, y=Outstate), color="blue") +
      xlab("Terminal")
str_degrees = names(Outstate_fitted_df)[1:10]
poly_plot = poly_plot +
      geom_line(aes(x = Terminal_seq, y = d1, colour="d1"), size=1.1) + 
      geom_line(aes(x = Terminal_seq, y = d2, colour="d2"), size=1.1) +
      geom_line(aes(x = Terminal_seq, y = d5, colour="d5"), size=1.1) +
      geom_line(aes(x = Terminal_seq, y = d7, colour="d7"), size=1.1) +
      geom_line(aes(x = Terminal_seq, y = d10, colour="d10"), size=1.1) +
      scale_colour_manual("",
                          breaks = c("d1", "d2", "d5", "d7", "d10"),
                          values = c("d1"=col[[1]], "d2"=col[[2]], "d5"=col[[3]], "d7"=col[[4]], "d10"=col[[5]])) +
      labs(title="Polynomial Regression, Outstate ~ Terminal")
print(poly_plot)


# TODO:
# Plot the results. Big plot, with 10 lines on might be messy. Maybe do d = 1, 2, 3, 6, 8, 10. A bit more pleasant to look at?
# Choose an optimal degree of freedom, AIC, BIC or something. Possibly CV.
# Use that number of degrees of freedom to compute a cubic spline interpolation, at the required quantiles.

### SPLINE REGRESSION ###
# Cross validate to find the best number of degrees of freedom for Spline-interpolation.
# Use cubic splines, with uniformly spaced knots.

# Performs LOOCV by setting cv=T:
smooth_spline_model = smooth.spline(x = College_2$Expend, y = College_2$Outstate, cv=T)
smooth_predict = predict(smooth_spline_model,College_2$Expend)

length(College_2$Expend)
length(smooth_predict$y)

smooth_df = do.call(data.frame, smooth_predict)
smooth_plot = ggplot(data=College_2, aes(x=Expend, y=Outstate)) + geom_point(size=1.1, col="blue") 
smooth_plot = smooth_plot +
              geom_line(data=smooth_df, aes(x=x, y=y, color="Smoothing spline"), size=1.1) +
              scale_color_manual("", values = c("Smoothing spline" = "red")) + 
              labs(title="Smoothing Splines, Outstate ~ Expend")
print(smooth_plot)
cat("Degrees of freedom for smoothing splines, chosen by LOOCV:", smooth_spline_model$df, "\n")


# Find the polynomial regression with the highest RSE:
length(College_2$Outstate)
length(Outstate_fitted_df$d10)
min_poly_MSE = do.call(min, poly_mse)
smoothing_MSE = mean((College_2$Outstate - smooth_predict$y)^2)

cat("Degree 10 polynomail regression MSE:\n", min_poly_MSE, "\n")
cat("Smoothing spline regression MSE:\n", smoothing_MSE, "\n")

# Not terribly surprised by the fact that the smoothing splines regression has less than half the MSE compared to the polynomial regression.
# The correlation between Outstate and Termnial is much lower than between Outstate and Expend. 


# Book plotting:
# plot(College_2$Expend, College_2$Outstate)
# lines(smooth_spline_model, col="red", lwd=2)


