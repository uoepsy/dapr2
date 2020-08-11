library(MASS)
library(tidyverse)
mu <- c(5.5,3.6, 0.5)
Sigma <- matrix(c(1.0, 0.8, 0.5,
                  0.8, 1.0, 0.5,
                  0.5, 0.5, 1.0), byrow = T, ncol = 3)

rawvars <- mvrnorm(n=10000, mu=mu, Sigma=Sigma)

df <- rawvars[sample(nrow(rawvars), 150),]

df1 <- tibble(
  ID = c(paste(rep("ID", 150), 1:150, sep="")),
  score = round(df[,1],0),
  hours = round(df[,2],1),
  study = round(df[,3],0)
  )

df2 <- df1 %>%
  mutate(
    study = ifelse(study <= 0, 0, 1)
  )


write_csv(df2, "C:/Work/Teaching/psychstats/dapr2/dapr2_lectures/dapr2_lec07.csv")
