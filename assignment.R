#############################
## Load Zone
#############################
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
    knitr, ggplot2, ggpubr, png,
    tidyverse, rlist, contextual,
    lubridate, zoo, roll, scales,
    fastDummies, gridExtra,
    stats, data.table, formula.tools
)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# get the dataframe from the csv file
df_zozo <- read.csv("zozo_Context_80items.csv")

source("toolbox.R")

set.seed(123)

#############################
## Descrpitive Analysis of user features
#############################

# transform the position variable to three dummies
df_zozo <- df_zozo %>%
    dummy_cols(select_columns = "position", remove_first_dummy = TRUE)

# check the unique values of 'user_feature_0'
unique(df_zozo$user_feature_0)
# make the values easier to read
df_zozo$user_feature_0 <- as.factor(df_zozo$user_feature_0)
df_zozo$user_feature_0 <- as.numeric(df_zozo$user_feature_0)

# do the same for the other user features
unique(df_zozo$user_feature_1)
df_zozo$user_feature_1 <- as.factor(df_zozo$user_feature_1)
df_zozo$user_feature_1 <- as.numeric(df_zozo$user_feature_1)

unique(df_zozo$user_feature_2)
df_zozo$user_feature_2 <- as.factor(df_zozo$user_feature_2)
df_zozo$user_feature_2 <- as.numeric(df_zozo$user_feature_2)

unique(df_zozo$user_feature_3)
df_zozo$user_feature_3 <- as.factor(df_zozo$user_feature_3)
df_zozo$user_feature_3 <- as.numeric(df_zozo$user_feature_3)

# turn user features into dummies
df_zozo <- dummy_cols(df_zozo, select_columns = c("user_feature_0", "user_feature_1",
                       "user_feature_2", "user_feature_3"), remove_first_dummy = TRUE)

# check the number of occurrences for each item
df_zozo %>%
    group_by(item_id) %>%
    summarise(n = n()) %>%
    arrange(desc(n))

# Create the variables for hour and day of observation
df_zozo$day  <- day(df_zozo$timestamp)
df_zozo$hour <- hour(df_zozo$timestamp)

# Create an indicator variable for the "decision hours"
# Which are defined as 18h and 19h, when there is a clear peak in click rates.
# As seen from the plots made below.
df_zozo$hour_dec <- ifelse(df_zozo$hour == 18 | df_zozo$hour == 19, 1, 0)

# Only 1.7% of purchases are made in these decision hours.
mean(df_zozo$hour_dec)

#################################
## Click rates per level of context.

# Computing the mean clicks per level of each context variable.
# I want to plot these means together with the number of observations in a 
# single plot for each context. So each plot contains two lines: they share the 
# x-axis denoting levels of the context variable, and they have separate y-axes
# for number of observations and mean clicks.

contexts <- c("position", "user_feature_0", "user_feature_1", "user_feature_2", "user_feature_3", "day", "hour")

# Next, we compute the click rate per item for each level of the categorical contexts.
# Here we also count the number of observations for each level.
reward_rates <- list()
plots <- list()
for (var in contexts) {
  reward_rates[[var]] <- aggregate(df_zozo[["click"]], by = list(df_zozo[[var]]),  function(x) c(mean = mean(x), count = length(x)))
  reward_rates[[var]] <- cbind(reward_rates[[var]][["Group.1"]], reward_rates[[var]][["x"]])
  colnames(reward_rates[[var]]) <- c(var, "mean", "count")
  reward_rates[[var]] <- as.data.frame(reward_rates[[var]])
  
  # Here we plot the mean click rate and number of observations per level of context.
  p <- ggplot(reward_rates[[var]], aes_string(x = var)) +
    geom_line(aes(y = mean, color = "Mean click rate")) +
    geom_line(aes(y = count/1e8, color = "Number of observations"), linetype = "dashed") +
    scale_y_continuous(name = "Mean click rate",
                       limits = c(0, 0.0075),
                       sec.axis = sec_axis(~. * 1e8, name = "Number of observations", 
                                           labels = function(x) format(x, scientific = FALSE, big.mark = ","))) +
    scale_x_continuous(breaks= extended_breaks()) +
    labs(y = "Mean click rate", color = NULL) +
    theme_minimal() +
    theme(legend.position = "top")
  
  ggsave(filename = paste0("mean_clicks_of_", var, ".pdf"), plot = p)
  plots[[var]] <- p
}

user_feature_plots = plots[c("user_feature_0", "user_feature_1", "user_feature_2", "user_feature_3")]
user_feature_plot <- ggpubr::ggarrange(plotlist = user_feature_plots, 
                               ncol = 2, nrow = 2,
                               common.legend = TRUE, align = "hv"
                               )
ggsave(filename = paste0("plot_grid_user_feature_plot.pdf"), plot = user_feature_plot, width = 12)
user_feature_plot

position_and_time_plots = plots[contexts[!grepl(c("user_feature", "kmeans"), contexts)]]
position_and_time_plot <- ggpubr::ggarrange(plotlist = position_and_time_plots, 
                               ncol = 3, nrow = 1, common.legend = TRUE)
ggsave(filename = paste0("plot_grid_position_and_time.pdf"), plot = position_and_time_plot, 
       width = 12, height = 4)
position_and_time_plot




cluster_contexts <- c("kmeans_2_clusters", "kmeans_4_clusters")


reward_rates_clusters <- list()
plots <- list()
for (var in cluster_contexts) {
  reward_rates_clusters[[var]] <- aggregate(df_zozo[["click"]], by = list(df_zozo[[var]]),  function(x) c(mean = mean(x), count = length(x)))
  reward_rates_clusters[[var]] <- cbind(reward_rates_clusters[[var]][["Group.1"]], reward_rates_clusters[[var]][["x"]])
  colnames(reward_rates_clusters[[var]]) <- c(var, "mean", "count")
  reward_rates_clusters[[var]] <- as.data.frame(reward_rates_clusters[[var]])
  
  # Here we plot the mean click rate and number of observations per level of context.
  p <- ggplot(reward_rates_clusters[[var]], aes_string(x = var)) +
    geom_line(aes(y = mean, color = "Mean click rate")) +
    geom_line(aes(y = count/1.5e8, color = "Number of observations"), linetype = "dashed") +
    scale_y_continuous(name = "Mean click rate",
                       limits = c(0, 0.0075),
                       sec.axis = sec_axis(~. * 1.5e8, name = "Number of observations", 
                                           labels = function(x) format(x, scientific = FALSE, big.mark = ","))) +
    scale_x_continuous(labels = function(x) format(round(x), scientific = FALSE), breaks= extended_breaks()) +
    labs(y = "Mean click rate", color = NULL) +
    theme_minimal() +
    theme(legend.position = "top")
  
  ggsave(filename = paste0("mean_clicks_of_", var, ".pdf"), plot = p)
  plots[[var]] <- p
}


cluster_plots = plots[c("kmeans_2_clusters", "kmeans_4_clusters")]
cluster_plot <- ggpubr::ggarrange(plotlist = cluster_plots, 
                                       ncol = 2, nrow = 1,
                                       common.legend = TRUE, align = "hv"
)
ggsave(filename = paste0("plot_grid_cluster_plot.pdf"), plot = cluster_plot, width = 12, height = 6)
cluster_plot




#################################
## Reward rates per item per level of context.

# I find that these plots are actually not very informative. They seem indicative
# of relative homogeneity, but that cannot be argued for well, because the 
# results may simply reflect erratic behavior by some groups having few
# observations per item.

# Compute the reward rate for each item. Both on an aggregate level and per 
# level of each context variable. Next, print histograms.
reward_rates_by_item <- list()
for (var in c("total", contexts)) {
  if (var != "total"){
    reward_rates_by_item[[var]] <- aggregate(df_zozo[["click"]], by = list(df_zozo[[var]], df_zozo[["item_id"]]),  function(x) c(mean = mean(x), count = length(x)))
    reward_rates_by_item[[var]] <- cbind(reward_rates_by_item[[var]][["Group.1"]], reward_rates_by_item[[var]][["Group.2"]], reward_rates_by_item[[var]][["x"]])
    colnames(reward_rates_by_item[[var]]) <- c(var, "item", "mean", "count")
    reward_rates_by_item[[var]] <- as.data.frame(reward_rates_by_item[[var]])
    
    p <- ggplot(reward_rates_by_item[[var]], aes(x = mean, fill = factor(.data[[var]]))) +
      geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
      labs(x = "Mean click rate", y = "Frequency", fill = var) +
      theme_minimal() +
      theme(legend.position = "top") 
    
    ggsave(filename = paste0("mean_clicks_per_item_", var, ".pdf"), plot = p)
    
  } else {reward_rates_by_item[[var]] <- aggregate(df_zozo[["click"]], by = list(df_zozo[["item_id"]]),  function(x) c(mean = mean(x), count = length(x)))
    reward_rates_by_item[[var]] <- cbind(reward_rates_by_item[[var]][["Group.1"]], reward_rates_by_item[[var]][["x"]])
    colnames(reward_rates_by_item[[var]]) <- c("item", "mean", "count")
    reward_rates_by_item[[var]] <- as.data.frame(reward_rates_by_item[[var]])
    p <- ggplot(reward_rates_by_item[[var]], aes(x = mean)) +
      geom_histogram(bins = 30, position = "identity", alpha = 0.5, fill = "darkblue") +
      geom_vline(xintercept = mean(df_zozo$click), color = "red", linetype = "dashed", size = 0.8) +
      labs(x = "Mean click rate", y = "Frequency") +
      theme_minimal() +
      theme(legend.position = "top") 
    
    ggsave(filename = paste0("mean_clicks_per_item_aaa_", var, ".pdf"), plot = p)
  }
}

#############################
## Vanilla Thompson Sampling
#############################

# initialize the bandit
bandit_TS_vanilla <- OfflineReplayEvaluatorBandit$new(click ~ item_id, df_zozo)

# initialize simulation parameters
size_sim <- 8000000
n_sim <- 10

# here we define the Thompson Sampling policy object
### 
# Parameters for tuning:
# - alpha: the alpha parameter of the beta distribution
# - beta: the beta parameter of the beta distribution
policy_TS_vanilla <- ThompsonSamplingPolicy$new()

# create an agent to make arm choice based on the policy
agent_TS_vanilla <- Agent$new(
    policy_TS_vanilla, # add policy
    bandit_TS_vanilla
) # add bandit

# initialize the simulator
sim_TS_vanilla <- Simulator$new(agent_TS_vanilla, # set our agent
    horizon = size_sim, # set size of sim
    do_parallel = TRUE, # run in parallel
    worker_max = 5, # set the number of workers
    simulations = n_sim
) # simulate it n_sim times

# run the simulation
history_TS_vanilla <- sim_TS_vanilla$run()

summary(history_TS_vanilla)
# gather results
df_TS_vanilla <- history_TS_vanilla$data %>%
    select(t, sim, choice, reward, agent)

df_TS_vanilla_agg <- cum_reward(df_TS_vanilla)

# save the results
write.csv(df_TS_vanilla, "df_TS_vanilla_max.csv")

#############################
## Vanilla UCB
#############################
bandit_UCB_vanilla <- OfflineReplayEvaluatorBandit$new(click ~ item_id, df_zozo)

# initialize the policy
### 
# Parameters for tuning:
# - alpha: the coefficient for the confidence interval
alpha <- 0.2
policy_UCB_vanilla <- LinUCBDisjointOptimizedPolicy$new(alpha = alpha)

# create an agent to make arm choice based on the policy
agent_UCB_vanilla <- Agent$new(
    policy_UCB_vanilla, # add policy
    bandit_UCB_vanilla
) # add bandit

# initialize the simulator
sim_UCB_vanilla <- Simulator$new(agent_UCB_vanilla, # set our agent
    horizon = size_sim, # set size of sim
    do_parallel = TRUE, # run in parallel
    worker_max = 5, # set the number of workers
    simulations = n_sim
) # simulate it n_sim times

# run the simulation
history_UCB_vanilla <- sim_UCB_vanilla$run()

# gather results
df_UCB_vanilla <- history_UCB_vanilla$data %>%
    select(t, sim, choice, reward, agent)

df_UCB_vanilla_agg <- cum_reward(df_UCB_vanilla)

# save the results
write.csv(df_UCB_vanilla, "df_UCB_vanilla_max.csv")

######################################
## Contextual analysis
######################################
## Clustering on user features. 

cluster_columns <- c("user_feature_0_2", "user_feature_0_3", "user_feature_0_4",
                     "user_feature_1_2", "user_feature_1_3", "user_feature_1_4", "user_feature_1_5",
                     "user_feature_1_6", "user_feature_2_2", "user_feature_2_3", "user_feature_2_4",
                     "user_feature_2_5", "user_feature_2_6", "user_feature_2_7", "user_feature_2_8",
                     "user_feature_2_9", "user_feature_2_10", "user_feature_3_2", "user_feature_3_3",
                     "user_feature_3_4", "user_feature_3_5", "user_feature_3_6", "user_feature_3_7",
                     "user_feature_3_8", "user_feature_3_9", "user_feature_3_10")


df_zozo <- as.data.frame(df_zozo)   # How has this not been done earlier? Not moved it up yet because test running requires reloading. 

df_zozo["kmeans_2_clusters"] <- kmeans(df_zozo[, cluster_columns], centers = 2)$cluster
df_zozo["kmeans_4_clusters"] <- kmeans(df_zozo[, cluster_columns], centers = 4)$cluster

# One-hot encoding the 4 level dummies.
df_zozo <- dummy_cols(df_zozo, select_columns = c("kmeans_4_clusters"), remove_first_dummy = TRUE)


######################################
# Contextual Thompson Sampling

# Initializing bandits.
bandit_TS_contextual_k2 <- OfflineReplayEvaluatorBandit$new(click ~ item_id | hour_dec + kmeans_2_clusters, df_zozo)
bandit_TS_contextual_k4 <- OfflineReplayEvaluatorBandit$new(click ~ item_id | hour_dec + kmeans_4_clusters_2 + kmeans_4_clusters_3 + kmeans_4_clusters_4, df_zozo)

df_TS_contextual_agg <- list()    # Initializing list to store cumulative rewards.

for (k in c(2,4)){ 
  
  # Initializing policy
  policy_TS_contextual <- ContextualLinTSPolicy$new()
  
  # Create an agent to make arm choice based on the policy
  agent_TS_contextual <- Agent$new(
    policy_TS_contextual, # Adding policy
    # Adding bandit. 
    if (k == 2){bandit_TS_contextual_k2} else{bandit_TS_contextual_k4}
  ) 
  
  # Running the simulation. Simulating n_sim times
  sim_TS_contextual <- Simulator$new(agent_TS_contextual, # set our agent
                                     horizon = size_sim, # set size of sim
                                     do_parallel = TRUE, # run in parallel
                                     worker_max = 5, # set the number of workers
                                     simulations = n_sim
  )
  history_TS_contextual <- sim_TS_contextual$run()
  
  # Gathering results.
  df_TS_contextual <- history_TS_contextual$data %>%
    select(t, sim, choice, reward, agent)
  
  # Saving the results. 
  if (k == 2){
    write.csv(df_TS_contextual, paste0("df_TS_contextual_max_kmeans_2_clusters.csv"))
    df_TS_contextual_agg["kmeans_2_clusters"] <- list(cum_reward(df_TS_contextual))
  } else {
    write.csv(df_TS_contextual, paste0("df_TS_contextual_max_kmeans_4_clusters.csv"))
    df_TS_contextual_agg["kmeans_4_clusters"] <- list(cum_reward(df_TS_contextual))
  }
  
  summary(history_TS_contextual)
}

# Plotting the cumulative rewards of all three Thompson Sampling algorithms:
# Vanilla, k2, and k4.
p <- ggplot(df_TS_vanilla_agg, aes(x = t, y = mean_cum_reward)) +
  geom_line(aes(y = mean_cum_reward, color = "Vanilla")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "orange", alpha = 0.1) +
  geom_line(data = df_TS_contextual_agg$kmeans_2_clusters, aes(x = t, y = mean_cum_reward, color = "Peak hours + two user feature clusters")) +
  geom_ribbon(data = df_TS_contextual_agg$kmeans_2_clusters, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.1) +
  geom_line(data = df_TS_contextual_agg$kmeans_4_clusters, aes(x = t, y = mean_cum_reward, color = "Peak hours + four user feature clusters")) +
  geom_ribbon(data = df_TS_contextual_agg$kmeans_4_clusters, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "green", alpha = 0.1) +
  scale_color_manual(name = "Algorithm", values = c("Vanilla" = "orange", 
                                                    "Peak hours + two user feature clusters" = "blue", 
                                                    "Peak hours + four user feature clusters" = "green")) +
  labs(x = "Rounds", y = "Cumulative Reward") +
  ylim(0, 75) +
  theme_bw() +
  theme(text = element_text(size = 18), legend.position = "top")


ggsave(filename = "Contextual_TS_clusters_cumrewards.pdf", plot = p)
print(p)


######################################
# Contextual UCB

bandit_UCB_contextual_k2 <- OfflineReplayEvaluatorBandit$new(click ~ item_id | hour_dec + kmeans_2_clusters, df_zozo)
bandit_UCB_contextual_k4 <- OfflineReplayEvaluatorBandit$new(click ~ item_id | hour_dec + kmeans_4_clusters_2 + kmeans_4_clusters_3 + kmeans_4_clusters_4, df_zozo)

df_UCB_contextual_agg <- list()    # Initializing list to store cumulative rewards.
for (k in c(2,4)){ 
  # Initializing policy
  policy_UCB_contextual <- LinUCBDisjointOptimizedPolicy$new(alpha = alpha)
  
  # Create an agent to make arm choice based on the policy
  agent_UCB_contextual <- Agent$new(
    policy_UCB_contextual, # Adding policy
    # Adding bandit. 
    if (k == 2){bandit_UCB_contextual_k2} else{bandit_UCB_contextual_k4}
  ) 
  
  # Running the simulation. Simulating n_sim times
  sim_UCB_contextual <- Simulator$new(agent_UCB_contextual, # set our agent
                                     horizon = size_sim, # set size of sim
                                     do_parallel = TRUE, # run in parallel
                                     worker_max = 5, # set the number of workers
                                     simulations = n_sim
  )
  history_UCB_contextual <- sim_UCB_contextual$run()
  
  # Gathering results.
  df_UCB_contextual <- history_UCB_contextual$data %>%
    select(t, sim, choice, reward, agent)
  
  # Saving the results. 
  if (k == 2){
    write.csv(df_UCB_contextual, paste0("df_UCB_contextual_max_kmeans_2_clusters.csv"))
    df_UCB_contextual_agg["kmeans_2_clusters"] <- list(cum_reward(df_UCB_contextual))
  } else {
    write.csv(df_UCB_contextual, paste0("df_UCB_contextual_max_kmeans_4_clusters.csv"))
    df_UCB_contextual_agg["kmeans_4_clusters"] <- list(cum_reward(df_UCB_contextual))
  }
  
  summary(history_UCB_contextual)
}

# Plotting the cumulative rewards of all three UCB algorithms:
# Vanilla, k2, and k4.
p <- ggplot(df_UCB_vanilla_agg, aes(x = t, y = mean_cum_reward)) +
  geom_line(aes(y = mean_cum_reward, color = "Vanilla")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "orange", alpha = 0.1) +
  geom_line(data = df_UCB_contextual_agg$kmeans_2_clusters, aes(x = t, y = mean_cum_reward, color = "Peak hours + two user feature clusters")) +
  geom_ribbon(data = df_UCB_contextual_agg$kmeans_2_clusters, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.1) +
  geom_line(data = df_UCB_contextual_agg$kmeans_4_clusters, aes(x = t, y = mean_cum_reward, color = "Peak hours + four user feature clusters")) +
  geom_ribbon(data = df_UCB_contextual_agg$kmeans_4_clusters, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "green", alpha = 0.1) +
  scale_color_manual(name = "Algorithm", values = c("Vanilla" = "orange", 
                                                    "Peak hours + two user feature clusters" = "blue", 
                                                    "Peak hours + four user feature clusters" = "green")) +
  labs(x = "Rounds", y = "Cumulative Reward") +
  ylim(0, 75) +
  theme_bw() +
  theme(text = element_text(size = 18), legend.position = "top")


ggsave(filename = "Contextual_UCB_clusters_cumrewards.pdf", plot = p)
print(p)


######################################
# Random policy benchmark
# run a random policy to compare the results
bandit_random <- OfflineReplayEvaluatorBandit$new(click ~ item_id, df_zozo)

policy_random <- RandomPolicy$new()

agent_random <- Agent$new(
    policy_random, # add policy
    bandit_random
) # add bandit

sim_random <- Simulator$new(agent_random, # set our agent
    horizon = size_sim, # set size of sim
    do_parallel = TRUE, # run in parallel
    worker_max = 5, # set the number of workers
    simulations = n_sim
) # simulate it n_sim times

history_random <- sim_random$run()

df_random <- history_random$data %>%
    select(t, sim, choice, reward, agent)

df_random_agg <- cum_reward(df_random)

# save the results
write.csv(df_random, "df_random_max.csv")

# Plotting the cumulative rewards of all bandits in one plot.
plot <- ggplot() +
    geom_line(data = df_TS_vanilla_agg, aes(x = t, y = mean_cum_reward, color = "TS Vanilla")) +
    geom_ribbon(data = df_TS_vanilla_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "green", alpha = 0.1) +
    geom_line(data = df_TS_contextual_agg$kmeans_2_clusters, aes(x = t, y = mean_cum_reward, color = "TS Context k=2")) +
    geom_ribbon(data = df_TS_contextual_agg$kmeans_2_clusters, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.1) +
    geom_line(data = df_TS_contextual_agg$kmeans_4_clusters, aes(x = t, y = mean_cum_reward, color = "TS Context k=4")) +
    geom_ribbon(data = df_TS_contextual_agg$kmeans_4_clusters, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "cyan", alpha = 0.1) +
  
    geom_line(data = df_UCB_vanilla_agg, aes(x = t, y = mean_cum_reward, color = "UCB Vanilla")) +
    geom_ribbon(data = df_UCB_vanilla_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "orange", alpha = 0.1) +
    geom_line(data = df_UCB_contextual_agg$kmeans_2_clusters, aes(x = t, y = mean_cum_reward, color = "UCB Context k=2")) +
    geom_ribbon(data = df_UCB_contextual_agg$kmeans_2_clusters, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "red", alpha = 0.1) +
    geom_line(data = df_UCB_contextual_agg$kmeans_4_clusters, aes(x = t, y = mean_cum_reward, color = "UCB Context k=4")) +
    geom_ribbon(data = df_UCB_contextual_agg$kmeans_4_clusters, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "purple", alpha = 0.1) +
  
    geom_line(data = df_random_agg, aes(x = t, y = mean_cum_reward, color = "Random")) +
    geom_ribbon(data = df_random_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci, fill = "Random"), alpha = 0.1) +
  
    scale_color_manual(name = "Algorithm", values = c("TS Vanilla" = "green", "TS Context k=2" = "blue", "TS Context k=4" = "cyan",
                                                      "UCB Vanilla" = "orange", "UCB Context k=2" = "red", "UCB Context k=4" = "purple", 
                                                      "Random" = "gray47")) +
    scale_fill_manual(name = "Algorithm", values = c("TS Vanilla" = "green", "TS Context k=2" = "blue", "TS Context k=4" = "cyan",
                                                     "UCB Vanilla" = "orange", "UCB Context k=2" = "red", "UCB Context k=4" = "purple", 
                                                     "Random" = "gray47"), guide = FALSE) +
    labs(x = "Rounds", y = "Cumulative Reward") +
    xlim(0, length(df_TS_vanilla_agg$t)) +
    ylim(0, 75) +
    theme_bw() +
    theme(text = element_text(size = 18), legend.position = "right")


ggsave("rewards_all_together.pdf", plot)
print(plot)


# Plotting the cumulative rewards of all TS bandits and random in one plot.
rewards_TS_and_random_plot <- ggplot() +
  geom_line(data = df_TS_vanilla_agg, aes(x = t, y = mean_cum_reward, color = "TS Vanilla")) +
  geom_ribbon(data = df_TS_vanilla_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "orange", alpha = 0.1) +
  geom_line(data = df_TS_contextual_agg$kmeans_2_clusters, aes(x = t, y = mean_cum_reward, color = "TS Context k=2")) +
  geom_ribbon(data = df_TS_contextual_agg$kmeans_2_clusters, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "cyan", alpha = 0.1) +
  geom_line(data = df_TS_contextual_agg$kmeans_4_clusters, aes(x = t, y = mean_cum_reward, color = "TS Context k=4")) +
  geom_ribbon(data = df_TS_contextual_agg$kmeans_4_clusters, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "purple", alpha = 0.1) +
  
  geom_line(data = df_random_agg, aes(x = t, y = mean_cum_reward, color = "Random")) +
  geom_ribbon(data = df_random_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci, fill = "Random"), alpha = 0.1) +
  
  scale_color_manual(name = "Algorithm", values = c("TS Vanilla" = "orange", "TS Context k=2" = "cyan", "TS Context k=4" = "purple",
                                                    "Random" = "gray47")) +
  scale_fill_manual(name = "Algorithm", values = c("TS Vanilla" = "orange", "TS Context k=2" = "cyan", "TS Context k=4" = "purple",
                                                   "Random" = "gray47"), guide = FALSE) +
  labs(x = "Rounds", y = "Cumulative Reward") +
  xlim(0, length(df_TS_vanilla_agg$t)) +
  ylim(0, 75) +
  theme_bw() +
  theme(text = element_text(size = 18), legend.position = "right")


ggsave("rewards_TS_and_random.pdf", rewards_TS_and_random_plot)
print(rewards_TS_and_random_plot)

# Plotting the cumulative rewards of all UCB bandits and random in one plot.
rewards_UCB_and_random_plot <- ggplot() +
  geom_line(data = df_UCB_vanilla_agg, aes(x = t, y = mean_cum_reward, color = "UCB Vanilla")) +
  geom_ribbon(data = df_UCB_vanilla_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "red", alpha = 0.1) +
  geom_line(data = df_UCB_contextual_agg$kmeans_2_clusters, aes(x = t, y = mean_cum_reward, color = "UCB Context k=2")) +
  geom_ribbon(data = df_UCB_contextual_agg$kmeans_2_clusters, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.1) +
  geom_line(data = df_UCB_contextual_agg$kmeans_4_clusters, aes(x = t, y = mean_cum_reward, color = "UCB Context k=4")) +
  geom_ribbon(data = df_UCB_contextual_agg$kmeans_4_clusters, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "green", alpha = 0.1) +
  
  geom_line(data = df_random_agg, aes(x = t, y = mean_cum_reward, color = "Random")) +
  geom_ribbon(data = df_random_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci, fill = "Random"), alpha = 0.1) +
  
  scale_color_manual(name = "Algorithm", values = c("UCB Vanilla" = "red", "UCB Context k=2" = "blue", "UCB Context k=4" = "green", 
                                                    "Random" = "gray47")) +
  scale_fill_manual(name = "Algorithm", values = c("UCB Vanilla" = "red", "UCB Context k=2" = "blue", "UCB Context k=4" = "green", 
                                                   "Random" = "gray47"), guide = FALSE) +
  labs(x = "Rounds", y = "Cumulative Reward") +
  xlim(0, length(df_TS_vanilla_agg$t)) +
  ylim(0, 75) +
  theme_bw() +
  theme(text = element_text(size = 18), legend.position = "right")


ggsave("rewards_UCB_and_random.pdf", rewards_UCB_and_random_plot)
print(rewards_UCB_and_random_plot)



legend1 <- get_legend(rewards_UCB_and_random_plot)
legend2 <- get_legend(rewards_TS_and_random_plot)
legends <- ggpubr::ggarrange(legend1,legend2, nrow = 2)
reward_plot <- ggpubr::ggarrange(rewards_UCB_and_random_plot, rewards_TS_and_random_plot, 
                                 ncol = 2, nrow = 1,
                                 legend.grob = legends, legend = "top"
)
ggsave(filename = paste0("reward_plot.pdf"), plot = reward_plot, width = 12, height = 6)
reward_plot





dev.off()        # What does this do?



#############################
## Thompson Sampling sensitivity analysis
#############################

## Alphas values
alpha_values <- seq(0.01, 5, length.out = 7)
average_rewards <- vector("list", length(alpha_values))

# Loop over each alpha value
for (i in seq_along(alpha_values)) {
    policy_TS_vanilla <- ThompsonSamplingPolicy$new(alpha = alpha_values[i])
    agent_TS_contextual <- Agent$new(
        policy_TS_vanilla, # add policy
        bandit_TS_vanilla
    ) # add bandit
    # initialize the simulator
    sim_TS_vanilla <- Simulator$new(agent_TS_vanilla, # set our agent
        horizon = size_sim, # set size of sim
        do_parallel = TRUE, # run in parallel
        simulations = 1
    ) # simulate it n_sim times

    # Run the simulation for the current alpha value
    history_TS_vanilla <- sim_TS_vanilla$run()

    # gather results
    df_TS_vanilla <- history_TS_vanilla$data %>%
        select(t, sim, choice, reward, agent)

    # Calculate the average rewards for the first 700 steps
    avg_reward <- mean(history_TS_vanilla$data$reward[history_TS_vanilla$data$t <= 700])

    # Store the average reward for the current alpha value
    average_rewards[[i]] <- avg_reward
}
print(average_rewards)

## Betas values
beta_values <- seq(0.01, 5, length.out = 7)
average_rewards_beta <- vector("list", length(beta_values))

for (i in seq_along(beta_values)) {
    policy_TS_vanilla <- ThompsonSamplingPolicy$new(alpha = beta_values[i])
    agent_TS_contextual <- Agent$new(
        policy_TS_vanilla, # add policy
        bandit_TS_vanilla
    ) # add bandit
    # initialize the simulator
    sim_TS_vanilla <- Simulator$new(agent_TS_vanilla, # set our agent
        horizon = size_sim, # set size of sim
        do_parallel = TRUE, # run in parallel
        simulations = 1
    ) # simulate it n_sim times

    # Run the simulation for the current alpha value
    history_TS_vanilla <- sim_TS_vanilla$run()

    # Calculate the average rewards for the first 700 steps
    avg_reward <- mean(history_TS_vanilla$data$reward[history_TS_vanilla$data$t <= 700])

    # Store the average reward for the current alpha value
    average_rewards_beta[[i]] <- avg_reward
}
print(average_rewards_beta)

average_rewards

# Plot average_rewards and average_rewards_beta against alpha_values
plot(alpha_values, average_rewards, type = "l", lwd = 6, col = rgb(0, 0, 1, alpha = 0.5), xlab = "Values", ylab = "Average Rewards", main = "Average rewards over parameter values")
lines(alpha_values, average_rewards_beta, col = rgb(1, 0, 0, alpha = 0.5), lwd = 2)
legend("topright",
    legend = c(expression(paste(alpha, " values, ", beta, " fixed")), expression(paste(beta, " values, ", alpha, " fixed"))),
    col = c("blue", "red"),
    lty = 1,
    cex = 0.8
)

#############################
## Aggregation vs. heterogeneity sensitivity analysis
#############################

# clustering analysis based on user features
# k-means clustering
# we use the elbow method to determine the number of clusters
set.seed(123)
wss <- vector("numeric", 10)
alpha <- 0.2

cluster_columns <- c("user_feature_0_2", "user_feature_0_3", "user_feature_0_4",
    "user_feature_1_2", "user_feature_1_3", "user_feature_1_4", "user_feature_1_5",
    "user_feature_1_6", "user_feature_2_2", "user_feature_2_3", "user_feature_2_4",
    "user_feature_2_5", "user_feature_2_6", "user_feature_2_7", "user_feature_2_8",
    "user_feature_2_9", "user_feature_2_10", "user_feature_3_2", "user_feature_3_3",
    "user_feature_3_4", "user_feature_3_5", "user_feature_3_6", "user_feature_3_7",
    "user_feature_3_8", "user_feature_3_9", "user_feature_3_10")

# loop over the number of clusters
for (i in 1:10) {
    kmeans_model <- kmeans(df_zozo[, cluster_columns], centers = i)
    wss[i] <- sum(kmeans_model$withinss)
}

# plot the within-cluster sum of squares against the number of clusters and save the plot with high resolution
png("elbow_method.png", width = 1000, height = 800)
plot(1:10, wss, type = "b", pch = 6, frame = TRUE, xlab = "Number of clusters",
    ylab = "Within-cluster sum of squares", cex.lab = 2, cex.axis = 1, cex.main = 1.5)
dev.off()


size_sim <- 500000
n_sim <- 10

# read df_UCB_vanilla
df_UCB_vanilla <- read.csv("df_UCB_vanilla_max.csv")

# use two clusters 
kmeans_model <- kmeans(df_zozo[, cluster_columns], centers = 2)

df_zozo$cluster <- kmeans_model$cluster

df_zozo_1 <- df_zozo %>%
    filter(cluster == 1)

df_zozo_2 <- df_zozo %>%
    filter(cluster == 2)

# descriptive analysis of the click-through rate of the two subgroups
df_zozo_1 %>%
    summarise(obs = n(),
        click_rate = mean(click),
        sd_click = sd(click))

df_zozo_2 %>%
    summarise(obs = n(),
        click_rate = mean(click),
        sd_click = sd(click))

df_zozo %>%
    summarise(click_rate = mean(click))

# t test to compare the click-through rate of the two subgroups
t.test(df_zozo_1$click, df_zozo_2$click)

# train vanilla UCB on each subgroup

bandit_UCB_vanilla_1 <- OfflineReplayEvaluatorBandit$new(click ~ item_id, df_zozo_1)

bandit_UCB_vanilla_2 <- OfflineReplayEvaluatorBandit$new(click ~ item_id, df_zozo_2)

policy_UCB_vanilla_1 <- LinUCBDisjointOptimizedPolicy$new(alpha = alpha)

policy_UCB_vanilla_2 <- LinUCBDisjointOptimizedPolicy$new(alpha = alpha)

agent_UCB_vanilla_1 <- Agent$new(
    policy_UCB_vanilla_1, # add policy
    bandit_UCB_vanilla_1
) # add bandit

agent_UCB_vanilla_2 <- Agent$new(
    policy_UCB_vanilla_2, # add policy
    bandit_UCB_vanilla_2
) # add bandit

sim_UCB_vanilla_1 <- Simulator$new(agent_UCB_vanilla_1, # set our agent
    horizon = size_sim, # set size of sim
    do_parallel = TRUE, # run in parallel
    worker_max = 5, # set the number of workers
    simulations = n_sim
) # simulate it n_sim 

sim_UCB_vanilla_2 <- Simulator$new(agent_UCB_vanilla_2, # set our agent
    horizon = size_sim, # set size of sim
    do_parallel = TRUE, # run in parallel
    worker_max = 5, # set the number of workers
    simulations = n_sim
) # simulate it n_sim times

history_UCB_vanilla_1outof2 <- sim_UCB_vanilla_1$run()
history_UCB_vanilla_2outof2 <- sim_UCB_vanilla_2$run()

# save rdata
save(history_UCB_vanilla_1outof2, file = "history_UCB_vanilla_1outof2.RData")
save(history_UCB_vanilla_2outof2, file = "history_UCB_vanilla_2outof2.RData")

# read the results
load("history_UCB_vanilla_1outof2.RData")
load("history_UCB_vanilla_2outof2.RData")

# gather results
df_UCB_vanilla_1outof2 <- history_UCB_vanilla_1outof2$data %>%
    select(t, sim, choice, reward, agent)
df_UCB_vanilla_2outof2 <- history_UCB_vanilla_2outof2$data %>%
    select(t, sim, choice, reward, agent)  
df_UCB_vanilla_1outof2_agg <- cum_reward(df_UCB_vanilla_1outof2)
df_UCB_vanilla_2outof2_agg <- cum_reward(df_UCB_vanilla_2outof2)

# plot the two results and df_UCB_vanilla_agg in the same plot
plot <- ggplot() +
    geom_line(data = df_UCB_vanilla_1outof2_agg, aes(x = t, y = mean_cum_reward, color = "subgroup1")) +
    geom_ribbon(data = df_UCB_vanilla_1outof2_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "orange", alpha = 0.1) +
    geom_line(data = df_UCB_vanilla_2outof2_agg, aes(x = t, y = mean_cum_reward, color = "subgroup2")) +
    geom_ribbon(data = df_UCB_vanilla_2outof2_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.1) +
    geom_line(data = df_UCB_vanilla_agg, aes(x = t, y = mean_cum_reward, color = "all individuals")) +
    geom_ribbon(data = df_UCB_vanilla_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "darkolivegreen4", alpha = 0.1) +
    scale_color_manual(values = c("subgroup1" = "orange", "subgroup2" = "blue", "all individuals" = "darkolivegreen4")) +
    labs(x = "Rounds", y = "Cumulative Reward") +
    xlim(0, length(df_UCB_vanilla_1outof2_agg$t)) +
    ylim(0, 30) +
    theme_bw() + # set the theme
    theme(text = element_text(size = 18))

# save the plot
ggsave("reward_subgroups_1outof2.png", plot, width = 10, height = 8)

# check the best arms for the two subgroups
df_UCB_vanilla_1outof2 %>%
    group_by(choice) %>%
    summarise(reward_rate = mean(reward),
    se_reward = sd(reward)/sqrt(length(reward))) %>%
    arrange(desc(reward_rate)) %>%
    filter(choice == 60)

df_UCB_vanilla_2outof2 %>%
    group_by(choice) %>%
    summarise(reward_rate = mean(reward),
    se_reward = sd(reward)/sqrt(length(reward))) %>%
    arrange(desc(reward_rate))

df_UCB_vanilla %>% 
    group_by(choice) %>%
    summarise(reward_rate = mean(reward),
    se_reward = sd(reward)/sqrt(length(reward))) %>%
    arrange(desc(reward_rate))


#############################
## Fairness Analysis
#############################

# read df_random and df_UCB_vanilla
df_random <- read.csv("df_random_max.csv")
df_UCB_vanilla <- read.csv("df_UCB_vanilla_max.csv")

# trim df_random to the same length as df_UCB_vanilla
df_random <- df_random %>%
    filter(t <= nrow(df_UCB_vanilla))

# fairness for items

# plot the number of exposures for each item
exposure_random <- df_random %>%
    group_by(choice) %>%
    summarise(n_exposures = n()/length(unique(sim)))

exposure_UCB <- df_UCB_vanilla %>%
    group_by(choice) %>%
    summarise(n_exposures = n()/length(unique(sim)))

# take per 800 rounds as a window, compute the number of exposures for each item in each window

df_random <- df_random %>%
    mutate(window = t %/% 800)

df_UCB_vanilla <- df_UCB_vanilla %>%
    mutate(window = t %/% 800)

exposure_random <- df_random %>%
    group_by(choice, window) %>%
    summarise(n_exposures = n()/length(unique(sim))) %>%
    group_by(window) %>%
    summarise(max = max(n_exposures),
    min = min(n_exposures))

exposure_UCB <- df_UCB_vanilla %>%
    group_by(choice, window) %>%
    summarise(n_exposures = n()/length(unique(sim))) %>%
    group_by(window) %>%
    summarise(max = max(n_exposures),
    min = min(n_exposures))

# plot the quantiles for the two policies in the same plot, and save the plot
plot1 <- ggplot() +
    geom_line(data = exposure_random, aes(x = window, y = min, color = "Random"), size = 1) +
    geom_point(data = exposure_random, aes(x = window, y = min, color = "Random"), size = 2) +
    geom_line(data = exposure_random, aes(x = window, y = max, color = "Random"), size = 1) +
    geom_point(data = exposure_random, aes(x = window, y = max, color = "Random"), size = 2) +
    geom_line(data = exposure_UCB, aes(x = window, y = min, color = "UCB"), size = 1) +
    geom_point(data = exposure_UCB, aes(x = window, y = min, color = "UCB"), size = 2) +
    geom_line(data = exposure_UCB, aes(x = window, y = max, color = "UCB"), size = 1) +
    geom_point(data = exposure_UCB, aes(x = window, y = max, color = "UCB"), size = 2) +
    scale_color_manual(values = c("Random" = "lightblue", "UCB" = "salmon")) +
    labs(x = "Window", y = "Number of exposures") +
    theme_bw() + # set the theme
    theme(text = element_text(size = 16)) +
    xlim(0, 15)

# save the plot
ggsave("exposures_range.png", plot1, width = 10, height = 8)

# plot the number of exposures for each item for the two policies in the same plot, and save the plot
plot1 <- ggplot() +
    geom_point(data = exposure_random, aes(x = choice, y = n_exposures, color = "Random"), size = 2) +
    geom_point(data = exposure_UCB, aes(x = choice, y = n_exposures, color = "UCB"), size = 2) +
    labs(x = "Item", y = "Number of exposures") +
    scale_color_manual(values = c("Random" = "lightblue", "UCB" = "salmon")) +
    theme_bw() + # set the theme
    theme(text = element_text(size = 16)) +
    labs(title = "All rounds")

# do the same analysis with only the first 1000 rounds
exposure_random <- df_random %>%
    filter(t <= 1000) %>%
    group_by(choice) %>%
    summarise(n_exposures = n()/length(unique(sim)))

exposure_UCB <- df_UCB_vanilla %>%
    filter(t <= 1000) %>%
    group_by(choice) %>%
    summarise(n_exposures = n()/length(unique(sim)))

# plot the number of exposures for each item for the two policies in the same plot, and save the plot
plot2 <- ggplot() +
  geom_point(data = exposure_random, aes(x = choice, y = n_exposures, color = "Random"), size = 2) +
  geom_point(data = exposure_UCB, aes(x = choice, y = n_exposures, color = "UCB"), size = 2) +
  labs(x = "Item", y = "Number of exposures") +
  scale_color_manual(values = c("Random" = "lightblue", "UCB" = "salmon")) +
  theme_bw() + # set the theme
  theme(text = element_text(size = 16)) +
  labs(title = "First 1000 rounds")

# put plot1 and plot2 in the same plot horizontally and save it
combined_plot <- grid.arrange(plot1, plot2, ncol = 2)

# save the plot
ggsave("exposures.png", combined_plot, width = 16, height = 8)

# fairness for users (compare how the best arm selected by the bandit performs for different user features)

# Vanilla UCB
# turn user_feature_0,1,2,3 into dummies
df_zozo <- dummy_cols(df_zozo, select_columns = c("user_feature_0", "user_feature_1",
    "user_feature_2", "user_feature_3"), remove_first_dummy = TRUE)

bandit_UCB_vanilla <- OfflineReplayEvaluatorBandit$new(click ~ item_id | user_feature_0_2 + 
                                                        user_feature_0_3 + user_feature_0_4 +
                                                        user_feature_1_2 + user_feature_1_3 +
                                                        user_feature_1_4 + user_feature_1_5 +
                                                        user_feature_1_6 + user_feature_2_2 +
                                                        user_feature_2_3 + user_feature_2_4 +
                                                        user_feature_2_5 + user_feature_2_6 +
                                                        user_feature_2_7 + user_feature_2_8 +
                                                        user_feature_2_9 + user_feature_2_10 +
                                                        user_feature_3_2 + user_feature_3_3 +
                                                        user_feature_3_4 + user_feature_3_5 +
                                                        user_feature_3_6 + user_feature_3_7 +
                                                        user_feature_3_8 + user_feature_3_9 +
                                                        user_feature_3_10, df_zozo)

# initialize the policy
### !!
# Parameters for tuning:
# - alpha: the coefficient for the confidence interval
alpha <- 0.2
policy_UCB_vanilla <- UCB2Policy$new(alpha = alpha)

# create an agent to make arm choice based on the policy
agent_UCB_vanilla <- Agent$new(
    policy_UCB_vanilla, # add policy
    bandit_UCB_vanilla
) # add bandit

# initialize the simulator
sim_UCB_vanilla <- Simulator$new(agent_UCB_vanilla, # set our agent
    horizon = size_sim, # set size of sim
    do_parallel = TRUE, # run in parallel
    worker_max = 5, # set the number of workers
    simulations = n_sim,
    save_context = TRUE
) # simulate it n_sim times

# run the simulation
history_UCB_vanilla <- sim_UCB_vanilla$run()

# save history_UCB_vanilla to R data
save(history_UCB_vanilla, file = "history_UCB_vanilla.RData")

# read history_UCB_vanilla
load("history_UCB_vanilla.RData")

# gather results
df_UCB_vanilla <- history_UCB_vanilla$data

# transform the dummy variables (X.2, X.3, X.4) into one variable
df_UCB_vanilla <- df_UCB_vanilla %>%
    mutate(user_feature_0 = ifelse(X.2 == 1, 2, ifelse(X.3 == 1, 3, ifelse(X.4 == 1, 4, 1))))
# transform the dummy variables (X.5, X.6, X.7, X.8, X.9) into one variable
df_UCB_vanilla <- df_UCB_vanilla %>%
    mutate(user_feature_1 = ifelse(X.5 == 1, 2, ifelse(X.6 == 1, 3, ifelse(X.7 == 1, 4, ifelse(X.8 == 1, 5, ifelse(X.9 == 1, 6, 1))))))
# transform the dummy variables (X.10, X.11, X.12, X.13, X.14, X.15, X.16, X.17, X.18) into one variable
df_UCB_vanilla <- df_UCB_vanilla %>%
    mutate(user_feature_2 = ifelse(X.10 == 1, 2, ifelse(X.11 == 1, 3, ifelse(X.12 == 1, 4, ifelse(X.13 == 1, 5, ifelse(X.14 == 1, 6, ifelse(X.15 == 1, 7, ifelse(X.16 == 1, 8, ifelse(X.17 == 1, 9, ifelse(X.18 == 1, 10, 1))))))))))
# transform the dummy variables (X.19, X.20, X.21, X.22, X.23, X.24, X.25, X.26, X.27) into one variable
df_UCB_vanilla <- df_UCB_vanilla %>%
    mutate(user_feature_3 = ifelse(X.19 == 1, 2, ifelse(X.20 == 1, 3, ifelse(X.21 == 1, 4, ifelse(X.22 == 1, 5, ifelse(X.23 == 1, 6, ifelse(X.24 == 1, 7, ifelse(X.25 == 1, 8, ifelse(X.26 == 1, 9, ifelse(X.27 == 1, 10, 1))))))))))

#-----compare fairness across user_feature_0
# compute the reward rate for different user_feature_0
df1_reward <- df_UCB_vanilla %>%
    group_by(user_feature_0) %>%
    summarise(reward_rate = mean(reward), 
    se = sd(reward)/sqrt(length(reward)))

df2_reward <- df_zozo %>%
    group_by(user_feature_0) %>%
    summarise(reward_rate = mean(click),
    se = sd(click)/sqrt(length(click)))

# plot the mean reward rate for different user_feature_0 in the two dataframes in the same plot, with error bars
plot1 <- ggplot() +
    geom_point(data = df1_reward, aes(x = user_feature_0, y = reward_rate, color = "UCB"), size = 2.5) +
    geom_errorbar(data = df1_reward, aes(x = user_feature_0, ymin = reward_rate - 1.96 * se, ymax = reward_rate + 1.96 * se, color = "UCB"), width = 0.2) +
    geom_point(data = df2_reward, aes(x = user_feature_0, y = reward_rate, color = "Overall"), size = 2.5) +
    geom_errorbar(data = df2_reward, aes(x = user_feature_0, ymin = reward_rate - 1.96 * se, ymax = reward_rate + 1.96 * se, color = "Overall"), width = 0.2) +
    labs(x = "User feature 0", y = "Reward rate") +
    scale_color_manual(values = c("UCB" = "salmon", "Overall" = "darkolivegreen4")) +
    theme_bw() + # set the theme
    theme(text = element_text(size = 16))

# save the plot
ggsave("user_feature_0.png", plot1, width = 8, height = 6)

# ANOVA test to see if the reward rate is the same for each user_feature_0
aov_UCB_vanilla_0 <- aov(reward ~ user_feature_0, data = df_UCB_vanilla)
summary(aov_UCB_vanilla)

# test for user_feature_1
df1_reward <- df_UCB_vanilla %>%
    group_by(user_feature_1) %>%
    summarise(reward_rate = mean(reward), 
    se = sd(reward)/sqrt(length(reward)))

df2_reward <- df_zozo %>%
    group_by(user_feature_1) %>%
    summarise(reward_rate = mean(click),
    se = sd(click)/sqrt(length(click)))

# plot the mean reward rate for different user_feature_1 in the two dataframes in the same plot, with error bars
plot2 <- ggplot() +
    geom_point(data = df1_reward, aes(x = user_feature_1, y = reward_rate, color = "UCB"), size = 2.5) +
    geom_errorbar(data = df1_reward, aes(x = user_feature_1, ymin = reward_rate - 1.96 * se, ymax = reward_rate + 1.96 * se, color = "UCB"), width = 0.2) +
    geom_point(data = df2_reward, aes(x = user_feature_1, y = reward_rate, color = "Overall"), size = 2.5) +
    geom_errorbar(data = df2_reward, aes(x = user_feature_1, ymin = reward_rate - 1.96 * se, ymax = reward_rate + 1.96 * se, color = "Overall"), width = 0.2) +
    labs(x = "User feature 1", y = "Reward rate") +
    scale_color_manual(values = c("UCB" = "salmon", "Overall" = "darkolivegreen4")) +
    theme_bw() + # set the theme
    theme(text = element_text(size = 16))

# save the plot
ggsave("user_feature_1.png", plot2, width = 8, height = 6)

# ANOVA test to see if the reward rate is the same for each user_feature_1
aov_UCB_vanilla_1 <- aov(reward ~ user_feature_1, data = df_UCB_vanilla)
summary(aov_UCB_vanilla_1)

# test for user_feature_2
df1_reward <- df_UCB_vanilla %>%
    group_by(user_feature_2) %>%
    summarise(reward_rate = mean(reward), 
    se = sd(reward)/sqrt(length(reward)))

df2_reward <- df_zozo %>%
    group_by(user_feature_2) %>%
    summarise(reward_rate = mean(click),
    se = sd(click)/sqrt(length(click)))

# plot the mean reward rate for different user_feature_2 in the two dataframes in the same plot, with error bars
plot3 <- ggplot() +
    geom_point(data = df1_reward, aes(x = user_feature_2, y = reward_rate, color = "UCB"), size = 2.5) +
    geom_errorbar(data = df1_reward, aes(x = user_feature_2, ymin = reward_rate - 1.96 * se, ymax = reward_rate + 1.96 * se, color = "UCB"), width = 0.2) +
    geom_point(data = df2_reward, aes(x = user_feature_2, y = reward_rate, color = "Overall"), size = 2.5) +
    geom_errorbar(data = df2_reward, aes(x = user_feature_2, ymin = reward_rate - 1.96 * se, ymax = reward_rate + 1.96 * se, color = "Overall"), width = 0.2) +
    labs(x = "User feature 2", y = "Reward rate") +
    scale_color_manual(values = c("UCB" = "salmon", "Overall" = "darkolivegreen4")) +
    theme_bw() + # set the theme
    theme(text = element_text(size = 16))

# save the plot
ggsave("user_feature_2.png", plot3, width = 8, height = 6)

# ANOVA test to see if the reward rate is the same for each user_feature_2
aov_UCB_vanilla_2 <- aov(reward ~ user_feature_2, data = df_UCB_vanilla)
summary(aov_UCB_vanilla_2)

# test for user_feature_3
df1_reward <- df_UCB_vanilla %>%
    group_by(user_feature_3) %>%
    summarise(reward_rate = mean(reward), 
    se = sd(reward)/sqrt(length(reward)))

df2_reward <- df_zozo %>%
    group_by(user_feature_3) %>%
    summarise(reward_rate = mean(click),
    se = sd(click)/sqrt(length(click)))

# plot the mean reward rate for different user_feature_3 in the two dataframes in the same plot, with error bars
plot4 <- ggplot() +
    geom_point(data = df1_reward, aes(x = user_feature_3, y = reward_rate, color = "UCB"), size = 2.5) +
    geom_errorbar(data = df1_reward, aes(x = user_feature_3, ymin = reward_rate - 1.96 * se, ymax = reward_rate + 1.96 * se, color = "UCB"), width = 0.2) +
    geom_point(data = df2_reward, aes(x = user_feature_3, y = reward_rate, color = "Overall"), size = 2.5) +
    geom_errorbar(data = df2_reward, aes(x = user_feature_3, ymin = reward_rate - 1.96 * se, ymax = reward_rate + 1.96 * se, color = "Overall"), width = 0.2) +
    labs(x = "User feature 3", y = "Reward rate") +
    scale_color_manual(values = c("UCB" = "salmon", "Overall" = "darkolivegreen4")) +
    theme_bw() + # set the theme
    theme(text = element_text(size = 16))

# save the plot
ggsave("user_feature_3.png", plot4, width = 8, height = 6)

# ANOVA test to see if the reward rate is the same for each user_feature_3
aov_UCB_vanilla_3 <- aov(reward ~ user_feature_3, data = df_UCB_vanilla)
summary(aov_UCB_vanilla_3)

