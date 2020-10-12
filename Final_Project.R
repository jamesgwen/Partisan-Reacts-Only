# Gabrielle Kirlew and James Wen
# Gov 19 Final Project
# R Code 

# Initial Settings --------------------------------------------------------

#rm(list = ls())
#setwd("~/Desktop/18W/Gov_19.01/final_project")
library(dplyr)
library(arm)
library(lubridate)
library(rms)
library(miceadds)
library(multiwayvcov)
library(stringr)
library(gridExtra)
library(ggthemes)
library(stargazer)
library(cowplot)


# Initial Data Wrangling --------------------------------------------------

# data set for gender, race, twitter following, state pop 
senator <- read.csv("Senator_Demographics.csv") %>% 
  rename(user = User)
twitter_senator <- read.csv("senators.csv")

# ideolog number 
ideology_rank <- read.csv("Ideology_Percentile.csv") %>% 
  dplyr::select(user, ideology, rank)

# combine previous 2 data sets 
data1 <- left_join(senator, twitter_senator, by = "user")
data2 <- left_join(data1 , ideology_rank, by = "user")

# calculate how many tweets they made in AUG, 2017
data_last_month <- data2 %>%
  mutate(date = mdy_hm(created_at),
         after_AUG012017 = ifelse(date > mdy("08/01/17") & date < mdy("08/31/17"), TRUE, FALSE)) %>% 
  filter(after_AUG012017 == TRUE) %>% 
  group_by(user) %>% 
  summarise(count = n())

# add Aug 2017 data to main data set 
data3 <- left_join(data2, data_last_month, by = "user")


# Data for Tweet Model ---------------------------------------------

# set range for prime time (9am - 3pm)
prime_time_range <- 14:20 

# add varibles for retweet and favorite ratios
# recoce party, add a variable for followers vs state pop.
# add variables for hashtags, tags, and links 
data_by_tweet <- data3 %>% 
  mutate(ratio_retweet = retweets/replies,
         ratio_favorite = favorites/replies,
         partyr = ifelse(party == "I", 0, party),
         partyr = ifelse(party == "R", 1, 0),
         Twitter.Following = as.numeric(Twitter.Following),
         follower_ratio = as.numeric(Twitter.Following)/as.numeric(State.Population),
         text = as.character(text),
         hashtag = grepl("\\#", as.character(text)),
         tag =  grepl("\\@", as.character(text)),
         link = grepl("https", as.character(text))) %>% 
  filter (replies > 10,
          retweets > 10) %>% 
  mutate(hour = format(mdy_hm(created_at), format = "%H"),
         prime_time = ifelse(hour %in% prime_time_range, 1, 0)) 


# Data for Senator Model -------------------------------------------

# make variables for retweet and favorite ratios
# recode party, make a variable for followers vs state pop.
# create an extreme variable (measure distance of sentator from center)
data_by_senator <- data3 %>% 
  filter(replies > 0) %>% 
  mutate(ratio_retweet = retweets/replies,
         ratio_favorite = favorites/replies,
         partyr = ifelse(party == "I", 0, party),
         partyr = ifelse(party == "R", 1, 0),
         follower_ratio = as.numeric(Twitter.Following)/as.numeric(State.Population),
         extreme = abs(ideology - .5) * 100) %>% 
  group_by(user, Female, White, Age, Twitter.Following, State.Population, partyr,state, ideology, follower_ratio, extreme) %>%
  summarise(ratio_retweet = mean(ratio_retweet),
            ratio_favorite = mean(ratio_favorite)) %>% 
  ungroup()

# combine data of senators w/ data of how many times they tweeted 
data_by_senator <- left_join(data_by_senator, data_last_month, by = "user")

# Tweet Models ------------------------------------------------------------

# note: lm.cluster would only work on one of our computers, may or may not work
# on yours

# Retweet Model
twitter_retweet <- lm(ratio_retweet ~ partyr + hashtag + tag + link,
                            data = data_by_tweet)

# cluster standard errors
twitter_retweet <-lm.cluster(data_by_tweet,twitter_retweet,data_by_tweet$user)


# Favorite Model
twitter_favorite <- lm(ratio_favorite ~ partyr + hashtag + tag + link,
                                   data = data_by_tweet)

# cluster standard errors 
twitter_favorite <-lm.cluster(data_by_tweet,twitter_favorite, data_by_tweet$user)

# Senator Models ----------------------------------------------------------

# Retweet Model, Female-Ideology
female_ideo_senator_retweet <- lm(ratio_retweet ~ Female + White + Age + 
                        follower_ratio + ideology + count + Female*ideology,
                      data = data_by_senator)

# Favorite Model, Female-Ideology
female_ideo_senator_favorite <- lm(ratio_favorite ~ Female + White + Age + 
                         follower_ratio + ideology + count + Female*ideology,
                       data = data_by_senator)

# Retweet Model, Party-Extreme
party_extreme_senator_retweet <- lm(ratio_retweet ~ Female + White + Age + 
                                    follower_ratio + partyr + extreme + count + partyr*extreme,
                                  data = data_by_senator)

# Favorite Model, Party-Extreme
party_extreme_senator_favorite <- lm(ratio_favorite ~ Female + White + Age + 
                                      follower_ratio + partyr + extreme + count + partyr*extreme,
                                    data = data_by_senator)

# By Senator, retweet, Female-Ideology, Predicted Probability ------------------------------------------------------------------

ideology_val <- unique(data_by_senator$ideology)

Female <- rep(0, 100)
White <- rep(1, 100)
Age <- mean(data_by_senator$Age)
follower_ratio <- mean(data_by_senator$follower_ratio)
ideology <- ideology_val
count <- mean(data_by_senator$count)

graph1_male_data <- cbind(Female, White, Age, follower_ratio, ideology, count)

graph1_male_data <- as.data.frame(graph1_male_data) %>% 
  mutate(FemaleXideology = Female *ideology)

graph1_male_predict <- as.data.frame(predict(female_ideo_senator_retweet, se.fit = TRUE, newdata = graph1_male_data))

graph1_male_data = cbind(graph1_male_data, graph1_male_predict) %>% 
  mutate(fit = as.numeric(fit))

Female <- rep(1, 100)
White <- rep(1, 100)
Age <- mean(data_by_senator$Age)
follower_ratio <- mean(data_by_senator$follower_ratio)
ideology <- ideology_val
count <- mean(data_by_senator$count)

graph1_female_data <- cbind(Female, White, Age, follower_ratio, ideology, count)

graph1_female_data <- as.data.frame(graph1_female_data) %>% 
  mutate(FemaleXideology = Female *ideology)

graph1_female_predict <- as.data.frame(predict(female_ideo_senator_retweet, se.fit = TRUE, newdata = graph1_female_data))

graph1_female_data = cbind(graph1_female_data, graph1_female_predict) %>% 
  mutate(fit= as.numeric(fit))

graph1 <- ggplot() +
  geom_line(data = graph1_male_data, aes(x = ideology, y = fit, linetype = "Male")) +
  geom_line(data = graph1_female_data, aes(x = ideology, y = fit, linetype = "Female"))+
  scale_linetype_manual(name = "Legend",
                 values = c("Male" = "dotted", 
                            "Female" = "dashed")) +
  labs(x = "Ideology Score",
       y = "Retweet Ratio",
       title = "Effect of Ideology on Retweet Ratio")

# ggsave(filename = "female_ideo_retweet.pdf", plot = graph1, width = 10, height = 7.5 )


# By Senator, favorite, Female-Ideology, Predicted Probability ------------------------------------


Female <- rep(0, 100)
White <- rep(1, 100)
Age <- mean(data_by_senator$Age)
follower_ratio <- mean(data_by_senator$follower_ratio)
ideology <- ideology_val
count <- mean(data_by_senator$count)

graph2_male_data <- cbind(Female, White, Age, follower_ratio, ideology, count)

graph2_male_data <- as.data.frame(graph2_male_data) %>% 
  mutate(FemaleXideology = Female *ideology)

graph2_male_predict <- as.data.frame(predict(female_ideo_senator_favorite, se.fit = TRUE, newdata = graph2_male_data))

graph2_male_data = cbind(graph2_male_data, graph2_male_predict) %>% 
  mutate(fit = as.numeric(fit))

Female <- rep(1, 100)
White <- rep(1, 100)
Age <- mean(data_by_senator$Age)
follower_ratio <- mean(data_by_senator$follower_ratio)
ideology <- ideology_val
count <- mean(data_by_senator$count)

graph2_female_data <- cbind(Female, White, Age, follower_ratio, ideology, count)

graph2_female_data <- as.data.frame(graph2_female_data) %>% 
  mutate(FemaleXideology = Female *ideology)

graph2_female_predict <- as.data.frame(predict(female_ideo_senator_favorite, se.fit = TRUE, newdata = graph2_female_data))

graph2_female_data = cbind(graph2_female_data, graph2_female_predict) %>% 
  mutate(fit = as.numeric(fit))


graph2 <- ggplot() +
  geom_line(data = graph2_male_data, aes(x = ideology, y = fit, linetype = "Male")) +
  geom_line(data = graph2_female_data, aes(x = ideology, y = fit, linetype = "Female"))+
  scale_linetype_manual(name = "Legend",
                        values = c("Male" = "dotted", 
                                   "Female" = "dashed")) +
  labs(x = "Ideology Score",
       y = "Favorite Ratio",
       title = "Effect of Ideology on Favorite Ratio")

# ggsave(filename = "female_ideo_favorite.pdf", plot = graph2, width = 10, height = 7.5 )

# By Senator, retweet, Party-Extreme, Predicted Probability --------------------------------------

Female <- 0 
White <- 1 
Age <- mean(data_by_senator$Age)
follower_ratio <- mean(data_by_senator$follower_ratio)
partyr <- 0
extreme <- unique(data_by_senator$extreme)
count <- mean(data_by_senator$count)

graph3_dem_data <- cbind(Female, White, Age, follower_ratio, partyr, extreme, count)

graph3_dem_data <- as.data.frame(graph3_dem_data) %>% 
  mutate(partyrXextreme = partyr * extreme)

graph3_dem_predict <- as.data.frame(predict(party_extreme_senator_retweet, se.fit = TRUE, newdata = graph3_dem_data))

graph3_dem_data= cbind(graph3_dem_data, graph3_dem_predict) %>% 
  mutate(fit = as.numeric(fit))


Female <- 0 
White <- 1 
Age <- mean(data_by_senator$Age)
follower_ratio <- mean(data_by_senator$follower_ratio)
partyr <- 1
extreme <- unique(data_by_senator$extreme)
count <- mean(data_by_senator$count)

graph3_rep_data <- cbind(Female, White, Age, follower_ratio, partyr, extreme, count)

graph3_rep_data <- as.data.frame(graph3_rep_data) %>% 
  mutate(partyrXextreme = partyr * extreme)

graph3_rep_predict <- as.data.frame(predict(party_extreme_senator_retweet, se.fit = TRUE, newdata = graph3_rep_data))

graph3_rep_data= cbind(graph3_rep_data, graph3_rep_predict) %>% 
  mutate(fit = as.numeric(fit))

graph3 <- ggplot() +
  geom_line(data = graph3_dem_data, aes(x = extreme, y = fit, color = "Democrats")) +
  geom_line(data = graph3_rep_data, aes(x = extreme, y = fit, color = "Republicans")) +
  theme_economist() +
  scale_y_continuous(limits = c(2,8)) +
  scale_x_continuous(breaks = c(0,50),
                     labels = c("Not Extreme", "Very Extreme")) +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15)) +
  scale_color_manual(name = "",
                     values = c("Democrats" = "Blue", 
                                "Republicans" = "Red")) +
  labs(x = "Ideological Extremism",
       y = "Retweet Ratio",
       title = "Ideological Extremism's Effect on Retweet Ratio")

# ggsave(filename = "party_extreme_retweet.jpg", plot = graph3, width = 10, height = 7.5 )


# By Senator, favorite, Party-Extreme, Predicted Probability -------------------------------------

Female <- 0 
White <- 1 
Age <- mean(data_by_senator$Age)
follower_ratio <- mean(data_by_senator$follower_ratio)
partyr <- 0
extreme <- unique(data_by_senator$extreme)
count <- mean(data_by_senator$count)

graph4_dem_data <- cbind(Female, White, Age, follower_ratio, partyr, extreme, count)

graph4_dem_data <- as.data.frame(graph4_dem_data) %>% 
  mutate(partyrXextreme = partyr * extreme)

graph4_dem_predict <- as.data.frame(predict(party_extreme_senator_favorite , se.fit = TRUE, newdata = graph4_dem_data))

graph4_dem_data= cbind(graph4_dem_data, graph4_dem_predict) %>% 
  mutate(fit = as.numeric(fit))


Female <- 0 
White <- 1 
Age <- mean(data_by_senator$Age)
follower_ratio <- mean(data_by_senator$follower_ratio)
partyr <- 1
extreme <- unique(data_by_senator$extreme)
count <- mean(data_by_senator$count)

graph4_rep_data <- cbind(Female, White, Age, follower_ratio, partyr, extreme, count)

graph4_rep_data <- as.data.frame(graph4_rep_data) %>% 
  mutate(partyrXextreme = partyr * extreme)

graph4_rep_predict <- as.data.frame(predict(party_extreme_senator_favorite , se.fit = TRUE, newdata = graph4_rep_data))

graph4_rep_data= cbind(graph4_rep_data, graph4_rep_predict) %>% 
  mutate(fit = as.numeric(fit))

graph4 <- ggplot() +
  geom_line(data = graph4_dem_data, aes(x = extreme, y = fit, color = "Democrats")) +
  geom_line(data = graph4_rep_data, aes(x = extreme, y = fit, color = "Republicans")) +
  theme_economist() + 
  scale_y_continuous(limits = c(0,20)) +
  scale_x_continuous(breaks = c(0,50),
                     labels = c("Not Extreme", "Very Extreme")) +
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15)) +
  scale_color_manual(name = "",
                        values = c("Democrats" = "Blue", 
                                   "Republicans" = "red")) +
  labs(x = "Ideological Extremism",
       y = "Favorite Ratio",
       title = "Ideological Extremism's Effect on Favorite Ratio")

# ggsave(filename = "party_extreme_favorite.jpg", plot = graph4, width = 10, height = 7.5 )

# By Tweet, Retweet Model -------------------------------------------------------------

retweet_var <- as.data.frame(c("Party", "Hashtag", "Tag", "Link"))
retweet_beta <- as.data.frame(c(-3.36346081, -0.08946602, 0.68076093, -0.49033374))
retweet_se <- as.data.frame(c(0.6565224, 0.2388695, 0.2633048, 0.1540444))

retweet_colnames <- c("variable", "beta", "se")

retweet_airplane <- cbind(retweet_var, retweet_beta)
retweet_airplane <- cbind(retweet_airplane, retweet_se)
colnames(retweet_airplane) <- retweet_colnames

retweet_airplane$upper <- (retweet_airplane$beta + 1.96 * retweet_airplane$se)
retweet_airplane$lower <- (retweet_airplane$beta - 1.96 * retweet_airplane$se)

retweet_airplane$variable <- factor(retweet_airplane$variable,levels = c("Party", "Hashtag", "Tag", "Link"))

retweet_beta_graph <- ggplot(retweet_airplane, aes(x = variable, y= beta)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-15,5)) +
  theme_economist() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  labs(x = "Variables",
       y = "Slope",
       title = "Slope Effects on Retweet Ratio") +
  coord_flip()

# ggsave(filename = "retweet_beta.jpg", plot = retweet_beta_graph, width = 10, height = 7.5 )


# By Tweet, Favorite Model ----------------------------------------------------------

favorite_var <- as.data.frame(c("Party", "Hashtag", "Tag", "Link"))
favorite_beta <- as.data.frame(c(-9.3833531, -1.6031779, 0.9544924, -0.4095581))
favorite_se <- as.data.frame(c(1.6547253, 0.5502249, 0.4893916, 0.3576517))

favorite_colnames <- c("variable", "beta", "se")

favorite_airplane <- cbind(favorite_var, favorite_beta)
favorite_airplane <- cbind(favorite_airplane, favorite_se)
colnames(favorite_airplane) <- favorite_colnames

favorite_airplane$upper <- (favorite_airplane$beta + 1.96 * favorite_airplane$se)
favorite_airplane$lower <- (favorite_airplane$beta - 1.96 * favorite_airplane$se)

favorite_airplane$variable <- factor(favorite_airplane$variable,levels = c("Party", "Hashtag", "Tag", "Link"))

favorite_beta_graph <- ggplot(favorite_airplane, aes(x = variable, y= beta)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(-15,5)) +
  theme_economist() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  labs(x = "Variables",
       y = "Slope",
       title = "Slope Effects on Favorite Ratio") +
  coord_flip()

# ggsave(filename = "favorite_beta.jpg", plot = favorite_beta_graph, width = 10, height = 7.5 )


# Tweet Graphs ------------------------------------------------------------

# Party
party_tweet <- data_by_tweet %>% 
  dplyr::select(partyr) %>% 
  mutate(partyr = as.numeric(partyr),
         partyr = factor(partyr,
                         levels = 0:1,
                         labels = c("Democrat","Republican")))


party_graph <- ggplot(data = party_tweet) + 
  geom_bar(aes(x = partyr)) +
  labs(title = "Number of Tweets by Party, Senate",
       x = "Party",
       y = "Number of Tweets")

# ggsave(filename = "party_graph.jpg", plot = party_graph, width = 10, height = 7.5 )

# Hashtab
hashtag_tweet <- data_by_tweet %>% 
  dplyr::select(hashtag) %>% 
  mutate(hashtag = ifelse(hashtag == FALSE,"No Hashtag","Hashtag"),
         hashtag = factor(hashtag,
                          levels = c("No Hashtag","Hashtag")))


hashtag_graph <- ggplot(data = hashtag_tweet) + 
  geom_bar(aes(x = hashtag)) +
  scale_y_continuous(limits = c(0,60000)) +
  labs(title = "Number of Tweets that Used Hashtags, Senate",
       x = "",
       y = "Number of Tweets")

# ggsave(filename = "hashtag_graph.jpg", plot = hashtag_graph, width = 10, height = 7.5 )

# Tag
tag_tweet <- data_by_tweet %>% 
  dplyr::select(tag) %>% 
  mutate(tag = ifelse(tag == FALSE,"No Tag","Tag"),
         tag = factor(tag,
                      levels = c("No Tag","Tag")))


tag_graph <- ggplot(data = tag_tweet ) + 
  geom_bar(aes(x = tag)) +
  scale_y_continuous(limits = c(0,60000)) +
  labs(title = "Number of Tweets that Used Tags, Senate",
       x = "",
       y = "Number of Tweets")

# ggsave(filename = "tag_graph.jpg", plot = tag_graph, width = 10, height = 7.5 )

# Link
link_tweet <- data_by_tweet %>% 
  dplyr::select(link) %>% 
  mutate(link = ifelse(link == FALSE,"No Link","Link"),
         link = factor(link,
                       levels = c("No Link","Link")))

link_graph <- ggplot(data = link_tweet) + 
  geom_bar(aes(x = link)) +
  scale_y_continuous(limits = c(0,60000)) +
  labs(title = "Number of Tweets that Used Links, Senate",
       x = "",
       y = "Number of Tweets")

# ggsave(filename = "link_graph.jpg", plot = link_graph, width = 10, height = 7.5 )

tweet_graph_combined <- plot_grid(party_graph, hashtag_graph, tag_graph, link_graph,
                                  ncol = 2, 
                                  nrow = 2)
# ggsave(filename = "tweet_graph_combined.jpg", plot = tweet_graph_combined, width = 11, height = 7.5 )
# Senator Graphs ----------------------------------------------------------

# Female
female_senator <- data_by_senator %>% 
  dplyr::select(Female) %>% 
  mutate(Female = factor(Female,
                         levels = 0:1,
                         labels = c("Male", "Female")))

female_senator <- ggplot(female_senator) +
  geom_bar(aes(x = Female)) +
  labs(x = "Gender",
       y = "Number",
       title = "Gender of Senators")

# White
white_senator <- data_by_senator %>% 
  dplyr::select(White) %>% 
  mutate(White = factor(White,
                         levels = 0:1,
                         labels = c("Minority", "White")))

white_senator <- ggplot(white_senator) +
  geom_bar(aes(x = White)) +
  labs(x = "Race",
       y = "Number",
       title = "Race of Senators")

# Age 
age_senator <- ggplot(data_by_senator) +
  geom_histogram(aes(x = Age), bins = 10) +
  labs(x = "Age",
       y = "Number",
       title ="Age Distribution of Senators")

# Follower Ratio 
follower_senator <- ggplot(data_by_senator) +
  geom_histogram(aes(x = follower_ratio)) +
  xlim(0, .00015) +
  labs(x = "Follower Ratio",
       y = "Number of Senators",
       title = "Follower Ratio Distribution of Senators",
       subtile = "Follower Ratio = Twitter Followers/State Population")

# Ideology 
ideology_senator <- ggplot(data_by_senator) +
  geom_histogram(aes(x = ideology)) +
  xlim(-.05, 1.05) +
  labs(x = "Ideology, 0 = Extreme Democrat, 1 = Extreme Republican",
       y = "Number of Senators",
       title = "Ideology Scores of Senators",
       subtitle = "Source: GovTrack.US")

# Count
count_senator <- ggplot(data_by_senator) +
  geom_histogram(aes(x = count), bins = 20) +
  labs(x = "Number of Tweets per Month",
       y = "Senators",
       title = "Tweeting Frequency of Senators",
       subtitle = "Frequency is based off of data from Aug 2017")

senator_graph_combined <- plot_grid(female_senator, white_senator, age_senator, 
                                    follower_senator,ideology_senator, count_senator,
                                    ncol = 2, 
                                    nrow = 3)

# ggsave(filename = "senator_graph_combined.jpg", plot = senator_graph_combined, width = 11, height = 11 )

# Stargazer Tables  -------------------------------------------------------

stargazer(female_ideo_senator_retweet, female_ideo_senator_favorite,
          title="Ideology and Female",
          dep.var.labels=c("Retweet Ratio", "Favorite Ratio"),
          covariate.labels=c("Female", "White", "Age",
                             "Follower Ratio", "Ideology", "Tweet Count", "Female*Ideology"),
          omit.stat=c("LL","f"),
          star.cutoffs = c(0.10, 0.05, 0.01),
          type = "latex",
          digits = 3)

stargazer(party_extreme_senator_retweet, party_extreme_senator_favorite,
          title="Party and Ideological Extremism",
          dep.var.labels=c("Retweet Ratio", "Favorite Ratio"),
          covariate.labels=c("Female", "White", "Age",
                             "Follower Ratio", "Party", "Ideological Extremism", "Tweet Count",
                             "Party*Extremism"),
          omit.stat=c("LL","f"),
          star.cutoffs = c(0.10, 0.05, 0.01),
          type = "latex",
          digits = 3)
