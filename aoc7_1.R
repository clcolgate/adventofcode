library(plyr)
library(dplyr)

x <- read.csv("aoc7.txt", header = FALSE, sep = "\n")
x$V1 <- as.character(x$V1)

x <- strsplit(unlist(x), split = "[\\[\\]]", perl = TRUE)

xdf <- ldply(x, rbind) %>% 
  mutate_each(funs(as.character(.)), everything()) %>%
  select(-.id)

for (i in 1:ncol(xdf)) {
  for (j in 1:nrow(xdf)) {
  xdf[j, i] <- ifelse(length(
    regmatches(xdf[j, i], regexpr("(\\w)(\\w)\\2\\1", xdf[j, i]))) > 0,
    regmatches(xdf[j, i], regexpr("(\\w)(\\w)\\2\\1", xdf[j, i])), NA)
  }
}

for (i in 1:ncol(xdf)) {
  for (j in 1:nrow(xdf)) {
    xdf[j, i] <- ifelse(!is.na(xdf[j, i]) & 
      length(regmatches(xdf[j, i], regexpr("(\\w)\\1\\1\\1", xdf[j, i]))) > 0, 
      NA, xdf[j, i])
  }
}

xdf %>% 
  mutate(inbrack = ifelse(!is.na(`2`) | !is.na(`4`) | !is.na(`6`), "yes", "no")) %>%
  filter((!is.na(`1`) | !is.na(`3`) | !is.na(`5`) | !is.na(`7`)) & 
          inbrack == "no") %>% 
  count()

#####################################

library(plyr)
library(dplyr)

x <- read.csv("aoc7.txt", header = FALSE, sep = "\n")
x$V1 <- as.character(x$V1)

x <- strsplit(unlist(x), split = "[\\[\\]]", perl = TRUE)

xdf <- ldply(x, rbind) %>% 
  mutate_each(funs(as.character(.)), everything()) %>%
  select(-.id)

for (i in 1:ncol(xdf)) {
  for (j in 1:nrow(xdf)) {
    xdf[j, i] <- ifelse(length(
      regmatches(xdf[j, i], regexpr("(\\w)(\\w)\\1", xdf[j, i]))) > 0,
      regmatches(xdf[j, i], regexpr("(\\w)(\\w)\\1", xdf[j, i])), NA)
  }
}

for (i in 1:ncol(xdf)) {
  for (j in 1:nrow(xdf)) {
    xdf[j, i] <- ifelse(!is.na(xdf[j, i]) & 
                length(regmatches(xdf[j, i], regexpr("(\\w)\\1\\1", xdf[j, i]))) > 0, 
                        NA, xdf[j, i])
  }
}

