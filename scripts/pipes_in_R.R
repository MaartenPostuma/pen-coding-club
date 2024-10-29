################# 
###   pipes in R
#     PEN coding club
#     21-03-2023, Rúna Magnússon
#     https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

############################################
### loading packages and data

# here I implement pipes using functionalities from the dplyr and broom library
library(dplyr)
library(broom)
library(ggplot2)

# example dataset: "iris"
data(iris)
head(iris)
dim(iris)


############################################
### general structure of a pipe command

# the general idea
#df %>%                                # dataframe on which you want to perform operations
#  do_this_operation %>%               # operation you want to perform
#  then_do_this_operation %>%
#  then_do_this_operation 

# the %>% is the "forward pipe operator"
# all packages in the tidyverse load the "%>%" command automatically
# you can also just install the whole tidyverse: https://r4ds.had.co.nz/

# but why would you do this?


############################################
### WHY: to tidy up and present data

# suppose i want to first remove all flowers with a sepal length of 7 or larger, then compute mean and sd of 
# SL per species group and then present those in a table in alphabetic order

# using base R functions
iris_subs <- subset(iris, Sepal.Length < 7)
iris_avg <- aggregate(Sepal.Length~Species,iris_subs, 
                      FUN = function(x) c(avg_SL = mean(x), sd_SL = sd(x) ) )
iris_avg <- iris_avg[order(iris_avg$Species),]
iris_avg

# using pipes --> no intermediate products, efficient "recipe"
iris_avg <- iris %>%
  filter(Sepal.Length < 7) %>% 
  group_by(Species) %>% 
  summarise(avg_SL = mean(Sepal.Length),
            sd_SL = sd(Sepal.Length)) %>%
  arrange(Species)

iris_avg  

### the output

# a tibble
print(iris_avg)

# is a kind of simpler version of a data frame. it also throws errors a bit quicker, based on the idea that it 
# stimulates you to fix code in an early stage

# key differences
# - never converts strings to factors
# - easier in some respects (column names, allowed characters, printing to console)
# - stricter in others

print(iris)
print(tibble(iris))

# some functions from base R or other packages don't work on tibbles since data frames are still the gold
# standard. in thatcase it is easy to convert back to data frame, using:

iris_avg_df <- as.data.frame(iris_avg)



############################################
### dplyr functions commonly used in pipe commands

# overview
# https://github.com/rstudio/cheatsheets/blob/main/data-transformation.pdf


# filtering
iris %>% 
  filter(Species == "setosa")

iris_setosa <- iris %>% 
  filter(Species == "setosa")


# sorting
iris %>%
  arrange(Species, Sepal.Length)


# selecting columns
iris %>% dplyr::select(Sepal.Length)# watch out; many packages have a select function..


# creating new columns
iris %>%
  mutate(log_SL=log(Sepal.Length))


# grouping
iris %>% group_by(Species)


# ..  then you probably want to do something with the groups, like summarizing:
iris_summary <- iris %>%  # summary data in new data frame
  group_by(Species) %>%  # grouping variable
  summarise(mean_SL = mean(Sepal.Length),  # group mean
            sd = sd(Sepal.Length), # standard deviation
            n = n())  # nr per group


# ... or get correlations per group:
iris %>%
  group_by(Species) %>%
  summarise(correlation = cor(Sepal.Length, Sepal.Width))
# stronger correlation for setosa!



############################################
### plotting

# ggplot can efficiently handle tibble output of pipe commands
iris_summary

ggplot(data=iris_summary, aes(x=Species, fill=Species, y=mean_SL)) + 
  geom_col() + 
  geom_errorbar(aes(ymin=mean_SL-sd, ymax=mean_SL+sd, x=Species), 
                             width=.2, position=position_dodge(.9)) +
  geom_text(aes(x = Species, y = -0.2, label = n), size = 4) +
  theme_bw()


############################################
### using custom functions in the pipe


# example 1: how regular R functions can be used in a pipe command:
# f(x, y) can be rewritten as x %>% f(y)

iris %>% 
  dplyr::select(Sepal.Length) %>% 
  log()

iris %>% 
  filter(Species=="setosa") %>%
  dplyr::select(Sepal.Length) %>%
  sum()


# example 2: apply a custom function to data in a pipe

func <- function(x) { (x+10)/2 }

iris %>% filter(Species=="setosa") %>%
  dplyr::select(Sepal.Length) %>%
  func()


func2 <- function(x) { sum(x[x>5]) }

output <- iris %>%  
  group_by(Species) %>%  # grouping variable
  summarise(sum5 = func2(Sepal.Length))

as.data.frame(output)


##########################################
### fancy way to use regression models in the pipe with broom

# version 1) manually extract coefficients and p?
# ---> this looks ugly and is prone to mistakes!
iris %>% 
  group_by(Species) %>%
  summarise(coef = coef(lm(Sepal.Width ~ Sepal.Length))[2],
            p = summary(lm(Sepal.Width ~ Sepal.Length))$coefficients[2,4])


# version 2; add a list of "lm" objects to the output
iris_fit <- iris %>% 
  group_by(Species) %>%
  do(model = lm(Sepal.Width ~ Sepal.Length, data = .))




##########################################
###  summarize the output with broom

# fit statistics per group
iris %>% 
  group_by(Species) %>%
  group_modify(.f = ~ broom::tidy(lm(Sepal.Width ~ Sepal.Length, data = .x)))

# omit the intercept
iris %>% 
  group_by(Species) %>%
  group_modify(.f = ~ broom::tidy(lm(Sepal.Width ~ Sepal.Length, data = .x))) %>%
  filter(term=="Sepal.Length")

# how tidy() from broom works:
diff <- lm(Sepal.Length ~ Species, iris)
summary(diff)
tidy(diff)



### thoughts on using pipes

# when to use it
# 1. when you have a particular sequence of operations you want to perform on a dataset, and you don't want to
# name or store every intermediate step

# 2. you want an intuitive and interchangable overview of operations on the data


# when not to use it
# 1. when your pipes are very long, then the pipe command becomes very hard to debug if you need to

# 2. You have multiple inputs or outputs. pipes work on a single dataset and return a single product. Combining
# datasets probably won't work or will be difficult

# 3. your operations aren't simple, linear steps (e.g. conditional check in between, more complex operation 
# structure.then maybe you can write a function?


#### BONUS
library(dadjoke)
library(praise)
dadjoke()
praise()


### bottom line

# pipes can be elegant and clear bits of code that avoid clogging up your directory with intermediate products
# but they won't do anything that base R or other approaches (e.g. functions) couldn't do for you as well :)


#############################################
### assignments to try

# add a new row that indicates whether this is a "large" or "small" iris, based 
# on a threshold petal length of 3. compute the correlation coefficient between
# petal width and petal length for both groups

# compute the linear regression coefficient and p value between petal width and 
# length for each species

# ..... something with our own dataset?
