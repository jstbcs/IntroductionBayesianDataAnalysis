x_lower <- 0
x_upper <- 1


ggplot(data.frame(x = c(x_lower, x_upper))
       , aes(x = x)) + 
  xlim(x_lower, x_upper) + 
  stat_function(fun = dbeta
                , args = list(shape1 = 4, shape2 = 4), geom = "area", 
                fill = "lightblue", alpha = 0.25) + 
  stat_function(fun = dbeta
                , args = list(shape1 = 4, shape2 = 4)) + 
  labs(x = "\n x", y = "f(x) \n", 
       title = "Beta Distribution") 

