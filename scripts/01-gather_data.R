library(pdftools)
library(tidyverse)
library(tidyr)
library(readr)
library(strex)

dhs_1987 <- pdf_text("C:/Users/Yukiu/Documents/Indonesia-Demographic-and-Health-Survey-1987/inputs/literature/FR19_40.pdf")

dhs_1987 <- tibble(raw_data = dhs_1987)
dhs_1987 <- 
  dhs_1987 |> 
  separate_rows(raw_data, sep = "\\n", convert = FALSE)

dhs_1987_new <-
  dhs_1987 |> 
  slice(6:34)

dhs_1987_a <- 
  dhs_1987_new |> 
  mutate(raw_data = str_squish(raw_data)) |>
  separate_rows(raw_data, sep = "\\n", convert = FALSE) |>
  separate(
    col = raw_data,
    into = c("Background_characteristic", 
             "Data"),
    sep = "(?<=[A-Za-z])(?=[0-9])",
    remove = FALSE,
    fill = "right")
           
dhs_1987_a <- 
  dhs_1987_a |>
  mutate(Background_characteristic = str_extract_non_numerics(raw_data, 
                                                              decimals = TRUE, 
                                                              commas = TRUE)) 


dhs_1987_a <-
  dhs_1987_a |>
  mutate(Background_characteristic = str_remove_all(Background_characteristic, 
                                                    "[[:punct:]]")) |>
  mutate(Background_characteristic = if_else(substring(Background_characteristic,1,1) == "c", 
                                             substring(Background_characteristic, 2), 
                                             Background_characteristic)) |>
  mutate(Background_characteristic = str_trim(Background_characteristic)) |>
  mutate(raw_data = str_remove_all(raw_data, ":")) |>
  mutate(raw_data = str_remove_all(raw_data, ";")) |>
  mutate(raw_data = if_else(substring(raw_data,1,1) == "c", 
                            substring(raw_data, 2), 
                            raw_data)) |>
  mutate(raw_data = str_trim(raw_data)) |>
  mutate(Data = gsub("[a-zA-Z ]", " ", raw_data)) |>
  mutate(Data = str_remove_all(Data, "-")) |>
  mutate(Data = str_remove_all(Data, "\\[")) |>
  mutate(Data = str_squish(Data))

dhs_1987_b <-
  dhs_1987_a |>
  separate(col = Data,
           into = c("no_children",
                    "one_children",
                    "two_children",
                    "three_children",
                    "four_children",
                    "five_children",
                    "six_and_more_children",
                    "total"),
           sep = "\\s",
           remove = FALSE
           )
  
dhs_1987_b <-
  dhs_1987_b |> 
  slice(6:29) |>
  select(Background_characteristic, 
         no_children, 
         one_children, 
         two_children, 
         three_children, 
         four_children, 
         five_children, 
         six_and_more_children, 
         total) 

dhs_1987_b <-
  dhs_1987_b |>
  mutate(Background_characteristic = str_replace_all(Background_characteristic, 
                                                     'Urban        i', 'Urban'),
         Background_characteristic = str_replace_all(Background_characteristic, 
                                                     'Rural       I          I', 
                                                     'Rural'),
         Background_characteristic = str_replace_all(Background_characteristic, 
                                                     'Outer JavaBali II     i', 
                                                     'Outer Java-Bali II'),
         Background_characteristic = str_replace_all(Background_characteristic, 
                                                     'Outer JavaBali I', 
                                                     'Outer Java-Bali I'),
         Background_characteristic = str_replace_all(Background_characteristic, 
                                                     'Jakarta        I', 
                                                     'Jakarta'),
         Background_characteristic = str_replace_all(Background_characteristic, 
                                                     'West Java       I', 
                                                     'West Java'),
         Background_characteristic = str_replace_all(Background_characteristic, 
                                                     'Yogyakarta   i', 
                                                     'Yogyakarta'),
         Background_characteristic = str_replace_all(Background_characteristic, 
                                                     'None             ~', 
                                                     'None education'),
         Background_characteristic = str_replace_all(Background_characteristic, 
                                                     'Some primary     i        I', 
                                                     'Some primary'),
         Background_characteristic = str_replace_all(Background_characteristic, 
                                                     'Primary completed     i', 
                                                     'Primary completed'),
         Background_characteristic = str_replace_all(Background_characteristic, 
                                                     'Secondary or more   IOOO', 
                                                     'Secondary or more'),
         Background_characteristic = str_replace_all(Background_characteristic, 
                                                     'Tara', 'Total')
)

dhs_1987_b <-
  dhs_1987_b |> 
  slice(1:2, 5:7, 10:15, 18:21, 24)

dhs_1987_b[15,3] <- "100.0"
dhs_1987_b[2,8] <- "91.5"
dhs_1987_b[12,8] <- "86.4"
dhs_1987_b$no_children <- as.numeric(dhs_1987_b$no_children)
dhs_1987_b$one_children <- as.numeric(dhs_1987_b$one_children)
dhs_1987_b$two_children <- as.numeric(dhs_1987_b$two_children)
dhs_1987_b$three_children <- as.numeric(dhs_1987_b$three_children)
dhs_1987_b$four_children <- as.numeric(dhs_1987_b$four_children)
dhs_1987_b$five_children <- as.numeric(dhs_1987_b$five_children)
dhs_1987_b$six_and_more_children <- as.numeric(dhs_1987_b$six_and_more_children)
dhs_1987_b$total <- as.numeric(dhs_1987_b$total)

write.csv(dhs_1987_b,"C:/Users/Yukiu/Documents/Indonesia-Demographic-and-Health-Survey-1987/inputs/data/raw_data.csv", row.names = FALSE)

