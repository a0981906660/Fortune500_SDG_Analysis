library(fixest)
library(dplyr)


rm(list = ls())

n <- 1000

random_sampler <- function(n = 1000) {
  mu <- runif(1, -5, 5)
  sd <- runif(1, 0, 1)
  res <- rnorm(n, mean = mu, sd = sd)
  res <- sample(c(res, rep(NA, n/10)), size = n, replace = T)
  return(res)
}
random_sampler(100)

set.seed(1234)
data <- data.frame(resource_dep = random_sampler(),
                   autocracy = random_sampler(),
                   env_nar = random_sampler(),
                   hr_nar = random_sampler(),
                   international = random_sampler(),
                   year = sample(2008:2022, size = n, replace = T),
                   permno = sample(1:50, size = n, replace = T)
                   )

data <- data %>% 
  arrange(permno, year) 
  
# run regression

reg1 <- feols(env_nar ~ resource_dep + autocracy + international | year + permno, 
              data = data)
reg2 <- feols(hr_nar ~ resource_dep + autocracy + international | year + permno, 
              data = data)

# fixed effect ordinary least square
reg3 <- feols(env_nar ~ resource_dep + autocracy + international 
              + resource_dep*international + autocracy*international | year + permno, 
              data = data)
reg4 <- feols(hr_nar ~ resource_dep + autocracy + international 
              + resource_dep*international + autocracy*international | year + permno, 
              data = data)

# export table
etable(reg1, reg2, reg3, reg4)
colnames(data)
keyvalues = c("resource_dep"="Home resource dependency",
              "autocracy" = "Home autocracy",
              "env_nar" = "Env. Nar.",
              "hr_nar" = "HR Nar.",
              "international" = "internationalization"
              )
etable(reg1, reg2, reg3, reg4, 
       tex = TRUE,
       dict = keyvalues,
       style.tex = style.tex("aer"),
       fitstat = ~ r2 + n,
       file = "./stats101/result/tab_1.tex")


