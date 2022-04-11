raw_data <- read_csv(here::here("inputs/data/raw_data.csv"))
library(tidyverse)
library(dplyr)
library(testthat)

test_data <- raw_data

test_data |>
  ggplot(aes(y = Background_characteristic, x = no_children)) +
  geom_point()

test_data |>
  ggplot(aes(y = Background_characteristic, x = one_children)) +
  geom_point()

test_data |>
  ggplot(aes(y = Background_characteristic, x = two_children)) +
  geom_point()

test_data |>
  ggplot(aes(y = Background_characteristic, x = three_children)) +
  geom_point()

test_data |>
  ggplot(aes(y = Background_characteristic, x = four_children)) +
  geom_point()

test_data |>
  ggplot(aes(y = Background_characteristic, x = five_children)) +
  geom_point()

test_data |>
  ggplot(aes(y = Background_characteristic, x = six_and_more_children)) +
  geom_point()

test_data |>
  ggplot(aes(y = Background_characteristic, x = total)) +
  geom_point()

### The data of x should be in the range of 0-100, all of this looks good.
expect_equal(class(test_data$Background_characteristic), "character")
expect_equal(class(test_data$no_children), "numeric")
expect_equal(class(test_data$one_children), "numeric")
expect_equal(class(test_data$two_children), "numeric")
expect_equal(class(test_data$three_children), "numeric")
expect_equal(class(test_data$four_children), "numeric")
expect_equal(class(test_data$five_children), "numeric")
expect_equal(class(test_data$six_and_more_children), "numeric")
expect_equal(class(test_data$total), "numeric")

### The class all of columns of data frame are in expectation.
cleaned_data <-
  test_data 

write.csv(cleaned_data,"C:/Users/Yukiu/Documents/Indonesia-Demographic-and-Health-Survey-1987/inputs/data/cleaned_data.csv.csv", row.names = FALSE)
