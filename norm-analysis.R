# Packages
library(dplyr)
library(ggpubr)
library(rcompanion)

# Constants
EIGHT_MM_VOL = 2.24
NINE_MM_VOL = 3.04

# Import data
norm_black_data = read.csv("./data/Norm-black.csv")

# Multiply by volume to obtain balls per bin
# First bin is 8mm in width while the rest are 9mm
norm_black_data$X1 <- norm_black_data$X1 * EIGHT_MM_VOL
norm_black_data <- norm_black_data %>%
  mutate_at(vars(X2:X9),
            .funs = funs(. * NINE_MM_VOL))

# Sum by columns to obtain totals for each bin
bin_totals <- colSums(norm_black_data)
bin_totals <- round(bin_totals)

# Construct new dataframe for normality tests
Ball <- c(1:sum(bin_totals))

Bin <- c()
for (i in 1:length(bin_totals)) {
  Bin <- append(Bin, rep(i, bin_totals[i]))
}

norm_black_df <- data.frame(Ball,Bin)


# Plot histogram
ggplot(norm_black_df, aes(x=Bin))+
  geom_histogram(binwidth = 1, color = "black", fill = "grey", alpha = 0.6) +
  ggtitle("Histograma de la entrada normal") +
  ylab("Esferas") +
  xlab("Canasta")

# QQ plot
ggqqplot(norm_black_df$Bin) +
  ggtitle("Gráfico QQ de la entrada normal") +
  ylab("Muestra") +
  xlab("Teórico")

# Run Shapiro-Wilk to test for normality
shapiro.test(norm_black_df$Bin)

# Run Chi Squared test to check if its an uniform distribution instead
chisq.test(bin_totals)
