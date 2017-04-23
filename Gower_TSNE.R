##### Clustering mixed data types #############
##### Use t-SNE for plotting ############

set.seed(1680) # for reproducibility

library(dplyr) # for data cleaning
library(ISLR) # for college dataset
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization

#####################################

head(College)

college_clean <- College %>% mutate(name=row.names(.), Acc_rate = Accept/Apps, 
                                    isElite = cut(Top10perc, breaks=c(0,50,100), labels = c("Not Elite", "Elite"), include.lowest = TRUE)) %>%
  mutate(isElite=factor(isElite)) %>% select(name, Acc_rate, Outstate, Enroll, Grad.Rate, Private, isElite)

glimpse(college_clean)

hist(log(college_clean$Enroll), 100)

gower_dist <- daisy(college_clean[,-1], metric = 'gower', type = list(logratio = 3))
summary(gower_dist)


gower_mat <- as.matrix(gower_dist)

# Output most similar pair

college_clean[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# dis similar pair

college_clean[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]



# silhoutte distance to determine number of clusters

sil_width <- c(NA)
for(i in 2:20){
  pam_fit <- pam(gower_dist, diss=TRUE, k=i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}


plot(1:20, sil_width, xlab = "Number of Clusters", ylab="silhoutte distance")
lines(1:20, sil_width)


pam_fit <- pam(gower_dist, diss=TRUE, k=3)
college_clean$cluster <- pam_fit$clustering

#Just wowed!
pam_summary <- college_clean %>% group_by(cluster) %>% do(the_summary = summary(.))
pam_summary$the_summary


tsne_obj <- Rtsne(gower_dist, distance=TRUE)
tnse_data <- tsne_obj$Y %>% data.frame() %>% setNames(c("X","Y")) %>% mutate(cluster = factor(pam_fit$clustering), name = college_clean$name)

ggplot(aes(x=X, y=Y,), data=tnse_data)+geom_point(aes(color=cluster))