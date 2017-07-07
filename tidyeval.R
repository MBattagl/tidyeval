library(tidyverse)
library(rlang)

data(mtcars)

head(mtcars)


### Basic intro to !! quo and sym

mtcars %>%
  filter(gear == 4)


x <- "gear"

mtcars %>%
  filter(x == 4)

expr(
  mtcars %>%
    filter(x == 4)
)

mtcars %>%
  filter((!!x) == 4)

expr(
  mtcars %>%
    filter((!!x) == 4)
)

# !! makes R evaluate x a step before evaluating the whole expression

# However we don't want a string there, we want the object name to be a symbol. 

x <- sym("gear")

mtcars %>%
  filter((!!x) == 4)

expr(
  mtcars %>%
    filter((!!x) == 4)
)


# If we want our input to be plain text, not a string, use quo

x <- quo(gear)

mtcars %>%
  filter((!!x) == 4)

expr(
  mtcars %>%
    filter((!!x) == 4)
)


### Enquo and sym example

mtcars %>% 
  count(cyl) %>% 
  mutate(prop = n / sum(n))


categorical_summary <- function(df, count_var) {
  count_var <- enquo(count_var)
  
  df %>% 
    count(!!count_var) %>%
    mutate(prop = n / sum(n))
  
}

mtcars %>% 
  categorical_summary(cyl)

mtcars %>% 
  categorical_summary("cyl")

categorical_summary_string <- function(df, count_var) {
  
  count_var <- sym(count_var)
  
  df %>% 
    count(!!count_var) %>%
    mutate(prop = n / sum(n))
  
}

mtcars %>% 
  categorical_summary_string("cyl")




### Big example

mtcars %>%
  filter(vs == 0) %>%
  group_by(cyl) %>%
  summarise(hp = mean(hp))

# Goal is to functionize replacing everything with arguments


# We'll start trying to replace cyl with group_var

summariser <- function(df, group_var) {
  
    df %>%
    filter(vs == 0) %>%
    group_by(group_var) %>%
    summarise(hp = mean(hp))
  
}

summariser(mtcars, cyl)


summariser <- function(df, group_var) {
  
  expr(
    df %>%
      filter(vs == 0) %>%
      group_by(group_var) %>%
      summarise(hp = mean(hp))
  )
  
}

summariser(mtcars, cyl)


# It tries to group by "group_var", where we want it to group by the user specified "cyl". 

# group_by (and other dplyr verbs) does not evaluate the input, it quotes it and then uses the quoted object

# We need to quote the input ourselves and then tell group_by to evaluate it prior to using it


summariser <- function(df, group_var) {
  
  group_var <- enquo(group_var)
  
  df %>%
    filter(vs == 0) %>%
    group_by(!!group_var) %>%
    summarise(hp = mean(hp))
  
}

summariser(mtcars, cyl)

# enquo takes a symbol refering to a fuction argument, and returns a quosure with the R code that was supplied by the user to this argument. 

# !! tells group_by to evaluate (unquote) group_var before using it 


summariser <- function(df, group_var) {
  
  group_var <- enquo(group_var)
  
  expr(  
    df %>%
      filter(vs == 0) %>%
      group_by(!!group_var) %>%
      summarise(hp = mean(hp))
  )
  
}

summariser(mtcars, cyl)



# We can also use quosures to assign an expression directly to an argument

summariser <- function(df, expression, group_var) {
  group_var <- enquo(group_var)
  expression <- enquo(expression)

  df %>%
    filter(!!expression) %>%
    group_by(!!group_var) %>%
    summarise(hp = mean(hp))
  
}

summariser(mtcars, vs == 0, cyl)



summariser <- function(df, expression, group_var, summary_var) {
  
  group_var <- enquo(group_var)
  summary_var <- enquo(summary_var)
  expression <- enquo(expression)
  
  df %>%
    filter(!!expression) %>%
    group_by(!!group_var) %>%
    summarise(x = mean(!!summary_var))
  
}

summariser(mtcars, vs == 0, cyl, hp)

# This does what we expect, but how do we dynamically name the summary variable to match the input?


summariser <- function(df, expression, group_var, summary_var) {
  
  group_var <- enquo(group_var)
  summary_var <- enquo(summary_var)
  expression <- enquo(expression)
  
  df %>%
    filter(!!expression) %>%
    group_by(!!group_var) %>%
    summarise(!!summary_var = mean(!!summary_var))
  
}

# !!summary_var = mean(!!summary_var) is not valid R code, need to use := operator to unquote on both RHS and LHS

summariser <- function(df, expression, group_var, summary_var) {
  
  group_var <- enquo(group_var)
  summary_var <- enquo(summary_var)
  expression <- enquo(expression)
  
    df %>%
      filter(!!expression) %>%
      group_by(!!group_var) %>%
      summarise(!!summary_var := mean(!!summary_var))
  
}

summariser(mtcars, vs == 0, cyl, hp)


summariser <- function(df, expression, group_var, summary_var) {
  
  group_var <- enquo(group_var)
  summary_var <- enquo(summary_var)
  expression <- enquo(expression)
  
  expr(
    df %>%
      filter(!!expression) %>%
      group_by(!!group_var) %>%
      summarise(!!summary_var := mean(!!summary_var))
  )
  
}

summariser(mtcars, vs == 0, cyl, hp)



summariser <- function(df, expression, group_var, summary_var) {
  
  group_var <- enquo(group_var)
  summary_var <- enquo(summary_var)
  expression <- enquo(expression)
  
  df %>%
    filter(!!expression) %>%
    group_by(!!group_var) %>%
    summarise(!!quo_name(summary_var) := mean(!!summary_var))
  
}

summariser(mtcars, vs == 0, cyl, hp)



summariser <- function(df, expression, group_var, summary_var) {
  
  group_var <- enquo(group_var)
  summary_var <- enquo(summary_var)
  expression <- enquo(expression)
  
  expr(  
    df %>%
      filter(!!expression) %>%
      group_by(!!group_var) %>%
      summarise(!!quo_name(summary_var) := mean(!!summary_var))
  )
  
}

summariser(mtcars, vs == 0, cyl, hp)

# The quo_name function replaces the quosure ~hp with "hp"


### Quos example

example <- tibble(group=c(rep("A",10), rep("B",10)), val = c(1:5, NA, 7:19, 1000))

grouped_mean <- function(df, grouping_var, val_var, ...) {
  
  args <- quos(...)
  grouping_var <- enquo(grouping_var)
  val_var <- enquo(val_var)
  
  df %>%
    group_by(!!grouping_var) %>%
    summarise(value = mean(!!val_var, !!!args))
  
}

grouped_mean(example, group, val)
grouped_mean(example, group, val, na.rm = TRUE)
grouped_mean(example, group, val, trim = .1)
grouped_mean(example, group, val, na.rm = TRUE, trim = .1)

