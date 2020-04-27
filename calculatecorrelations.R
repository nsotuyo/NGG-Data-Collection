library(tidyverse)

setwd("/Users/nate/Dropbox/_ PHD/NGG 998 Data Collection/")

# Load data
sp <- read.csv("spontaneous.csv")
te <- read.csv("tempinduced.csv")

# -----------------------------------------------------------------------------
# 1. Clean data
# -----------------------------------------------------------------------------
# Take only the first 8 rows and first 3 columns of te
final <- te[1:8,1:3]

# Count number of spontaneous seizures and average seizure time for each 
# mouse ID
sp_count <- sp %>%
  group_by(mouse_ID) %>%
  summarise(sp_count = n(),
            sp_time = mean(elapsed_time_s))

# Join spontaneous count to final data frame
final <- left_join(final, sp_count, by = "mouse_ID")
final$sp_count[is.na(final$sp_count)] <- 0

# Make two-way scatter plots between variables
plot(final$Thresh1, final$sp_count)
plot(final$Diff1, final$sp_count)
plot(final$Diff1, final$sp_time)

# Take away ID's that were experimentally different
final2 <- filter(final, ! mouse_ID %in% c("NS2", "NS7"))

# -----------------------------------------------------------------------------
# 2. Test correlations 
# -----------------------------------------------------------------------------
cor.test(final$Thresh1, final$sp_count, method = "spearman")
# rho 
# 0.2848101 
# p-value 0.4942

cor.test(final$Diff1, final$sp_count, method = "spearman")
# rho 
# -0.2284647 
# p-value = 0.5863

cor.test(final2$Thresh1, final2$sp_count, method = "spearman")
# rho 
# 0.6323529  
# p-value 0.1779

cor.test(final2$Diff1, final2$sp_count, method = "spearman")
# rho 
# -0.5 
# p-value 0.3125


# -----------------------------------------------------------------------------
# 3. Scatter plots
# -----------------------------------------------------------------------------
final$sp_time_size <- final$sp_time
final$sp_time_size[is.na(final$sp_time_size)] <- 10
plot(final$Diff1, final$sp_count, cex = final$sp_time_size*0.1, 
     xlab = "Temperature difference (C)", ylab = "Spontaneous count")
plot(final$Thresh1, final$sp_count, cex = final$sp_time_size*0.1,
     xlab = "Threshold temperature (C)", ylab = "Spontaneous count")

final2$sp_time_size <- final2$sp_time
final2$sp_time_size[is.na(final2$sp_time_size)] <- 10
plot(final2$Diff1, final2$sp_count, cex = final2$sp_time_size*0.1, 
     xlab = "Temperature difference (C)", ylab = "Spontaneous count")
plot(final2$Thresh1, final2$sp_count, cex = final2$sp_time_size*0.1,
     xlab = "Threshold temperature (C)", ylab = "Spontaneous count")

