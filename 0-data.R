# Load csv file
ww <- read.table("./input/winequality-white.csv", header=T, sep=";")

# Define original variable names
ORIGINAL <- colnames(ww)
# Define independent variable names
INDEPENDENT <- colnames(ww)[1:11]

# 
ww$quality.f <- as.factor(ww$quality)

# [Quality conversion 1]
# Create a simpler classification of quality.
#   3, 4, 5 -> bad
#      6    -> normal
#   7, 8, 9 -> good
ww$quality.f2 <- ifelse(ww$quality == 3 | 
                          ww$quality == 4 | 
                          ww$quality == 5, 
                        "bad", 
                        ifelse(ww$quality == 6, 
                               "normal",
                               "good"))
ww$quality.f2 <- factor(ww$quality.f2, levels=c("bad", "normal", "good"))
summary(ww$quality.f2)


# [Quality conversion 2]
# Create factor variables for extreme qualities.
#   excellent: 9,8 -> True
#   inferior:  3,4 -> True
ww$excellent <- ifelse(ww$quality == 8 | ww$quality == 9, T, F)
ww$inferior <- ifelse(ww$quality == 3 | ww$quality == 4, T, F)

