#############################
## Load Zone
#############################
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
    knitr, ggplot2, png,
    tidyverse, rlist, contextual,
    lubridate, zoo, roll,
    fastDummies
)

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

# check the number of repeats for each item
df_zozo %>%
    group_by(item_id) %>%
    summarise(n = n()) %>%
    arrange(desc(n))

# compute the reward rate for each item
df_zozo %>%
    group_by(item_id) %>%
    summarise(reward_rate = mean(click))

# compute mean reward rate
mean(df_zozo$click)

# turn user features into dummies
# df_zozo <- dummy_cols(df_zozo, select_columns = c("user_feature_0", "user_feature_1",
#                        "user_feature_2", "user_feature_3"), remove_first_dummy = TRUE)

#############################
## Thompson Sampling
#############################

# Vanilla Thompson Sampling

# initialize the bandit
bandit_TS_vanilla <- OfflineReplayEvaluatorBandit$new(click ~ item_id, df_zozo)

# initialize simulation parameters
size_sim <- 8000000
n_sim <- 10

# here we define the Thompson Sampling policy object
### !!
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

# Contextual Thompson Sampling

# initialize the bandit

bandit_TS_contextual <- OfflineReplayEvaluatorBandit$new(click ~ item_id | position_2 + position_3, df_zozo)

# initialize the policy
policy_TS_contextual <- ContextualLinTSPolicy$new()

# create an agent to make arm choice based on the policy
agent_TS_contextual <- Agent$new(
    policy_TS_contextual, # add policy
    bandit_TS_contextual
) # add bandit

# initialize the simulator
sim_TS_contextual <- Simulator$new(agent_TS_contextual, # set our agent
    horizon = size_sim, # set size of sim
    do_parallel = TRUE, # run in parallel
    worker_max = 5, # set the number of workers
    simulations = n_sim
) # simulate it n_sim times

# run the simulation
history_TS_contextual <- sim_TS_contextual$run()

# gather results
df_TS_contextual <- history_TS_contextual$data %>%
    select(t, sim, choice, reward, agent)

df_TS_contextual_agg <- cum_reward(df_TS_contextual)

summary(history_TS_contextual)

# plot df_TS_vanilla_agg and df_TS_contextual_agg in the same plot
ggplot(df_TS_vanilla_agg, aes(x = t, y = mean_cum_reward)) +
    geom_line(aes(y = mean_cum_reward), color = "orange") +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "orange", alpha = 0.1) +
    geom_line(data = df_TS_contextual_agg, aes(x = t, y = mean_cum_reward), color = "blue") +
    geom_ribbon(data = df_TS_contextual_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.1) +
    scale_color_manual(values = c("orange", "blue")) +
    labs(x = "Rounds", y = "Cumulative Reward") +
    theme_bw() + # set the theme
    theme(text = element_text(size = 18))

# save the results
write.csv(df_TS_contextual, "df_TS_contextual_max.csv")

#############################
## UCB
#############################

# Vanilla UCB
bandit_UCB_vanilla <- OfflineReplayEvaluatorBandit$new(click ~ item_id, df_zozo)

# initialize the policy
### !!
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

# Contextual UCB
bandit_UCB_contextual <- OfflineReplayEvaluatorBandit$new(click ~ item_id | position_2 + position_3, df_zozo)

# initialize the policy
policy_UCB_contextual <- LinUCBDisjointOptimizedPolicy$new(alpha = alpha)

# create an agent to make arm choice based on the policy
agent_UCB_contextual <- Agent$new(
    policy_UCB_contextual, # add policy
    bandit_UCB_contextual
) # add bandit

# initialize the simulator
sim_UCB_contextual <- Simulator$new(agent_UCB_contextual, # set our agent
    horizon = size_sim, # set size of sim
    do_parallel = TRUE, # run in parallel
    worker_max = 5, # set the number of workers
    simulations = n_sim
) # simulate it n_sim times

# run the simulation
history_UCB_contextual <- sim_UCB_contextual$run()

# gather results
df_UCB_contextual <- history_UCB_contextual$data %>%
    select(t, sim, choice, reward, agent)

df_UCB_contextual_agg <- cum_reward(df_UCB_contextual)

# save the results
write.csv(df_UCB_contextual, "df_UCB_contextual_max.csv")

# plot df_UCB_vanilla_agg and df_UCB_contextual_agg in the same plot
ggplot(df_UCB_vanilla_agg, aes(x = t, y = mean_cum_reward)) +
    geom_line(aes(y = mean_cum_reward), color = "orange") +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "yellow", alpha = 0.1) +
    geom_line(data = df_UCB_contextual_agg, aes(x = t, y = mean_cum_reward), color = "blue") +
    geom_ribbon(data = df_UCB_contextual_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "grey", alpha = 0.1) +
    scale_color_manual(values = c("orange", "blue")) +
    labs(x = "Rounds", y = "Cumulative Reward") +
    theme_bw() + # set the theme
    theme(text = element_text(size = 18))

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

# plot the four results in the same plot
plot <- ggplot() +
    geom_line(data = df_TS_vanilla_agg, aes(x = t, y = mean_cum_reward, color = "TS Vanilla")) +
    geom_ribbon(data = df_TS_vanilla_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci, fill = "TS Vanilla"), alpha = 0.1) +
    geom_line(data = df_TS_contextual_agg, aes(x = t, y = mean_cum_reward, color = "TS Contextual")) +
    geom_ribbon(data = df_TS_contextual_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci, fill = "TS Contextual"), alpha = 0.1) +
    geom_line(data = df_UCB_vanilla_agg, aes(x = t, y = mean_cum_reward, color = "UCB Vanilla")) +
    geom_ribbon(data = df_UCB_vanilla_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci, fill = "UCB Vanilla"), alpha = 0.1) +
    geom_line(data = df_UCB_contextual_agg, aes(x = t, y = mean_cum_reward, color = "UCB Contextual")) +
    geom_ribbon(data = df_UCB_contextual_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci, fill = "UCB Contextual"), alpha = 0.1) +
    geom_line(data = df_random_agg, aes(x = t, y = mean_cum_reward, color = "Random")) +
    geom_ribbon(data = df_random_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci, fill = "Random"), alpha = 0.1) +
    scale_color_manual(name = "Algorithm", values = c("TS Vanilla" = "orange", "TS Contextual" = "blue", "UCB Vanilla" = "green", "UCB Contextual" = "red", "Random" = "gray47")) +
    scale_fill_manual(name = "Algorithm", values = c("TS Vanilla" = "orange", "TS Contextual" = "blue", "UCB Vanilla" = "green", "UCB Contextual" = "red", "Random" = "gray47"), guide = FALSE) +
    labs(x = "Rounds", y = "Cumulative Reward") +
    xlim(0, length(df_TS_vanilla_agg$t)) +
    ylim(0, 55) +
    theme_bw() +
    theme(text = element_text(size = 18))

# set the size of the plot and save it
png("reward.png", width = 1000, height = 800)
print(plot)
dev.off()

# save the plot
ggsave("reward.png", plot, width = 10, height = 8)

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
## Fairness Analysis
#############################

# use contextual TS to compute the fairness for items because it performed the best

# fairness for items

# count the number of exposures for each item in each simulation, then plot the mean and standard deviation of the number of exposures for each item
df_TS_contextual %>%
    group_by(sim, choice) %>%
    summarise(n = n()) %>%
    group_by(choice) %>%
    summarise(mean_exposures = mean(n), sd_exposures = sd(n))

# plot the mean and standard deviation of the number of exposures for each item
df_TS_contextual %>%
    group_by(sim, choice) %>%
    summarise(n = n()) %>%
    group_by(choice) %>%
    summarise(mean_exposures = mean(n), sd_exposures = sd(n)) %>%
    ggplot(aes(x = choice, y = mean_exposures)) +
    geom_point(aes(y = mean_exposures), color = "orange") +
    geom_errorbar(aes(ymin = mean_exposures - sd_exposures, ymax = mean_exposures + sd_exposures), width = 0.2, color = "orange") +
    labs(x = "Item", y = "Mean number of exposures") +
    theme_bw() + # set the theme
    theme(text = element_text(size = 18))

# select only the first 2000 rounds, do the same analysis
df_TS_contextual %>%
    filter(t <= 2000) %>%
    group_by(sim, choice) %>%
    summarise(n = n()) %>%
    group_by(choice) %>%
    summarise(mean_exposures = mean(n), sd_exposures = sd(n)) %>%
    ggplot(aes(x = choice, y = mean_exposures)) +
    geom_point(aes(y = mean_exposures), color = "orange") +
    geom_errorbar(aes(ymin = mean_exposures - sd_exposures, ymax = mean_exposures + sd_exposures), width = 0.2, color = "orange") +
    labs(x = "Item", y = "Mean number of exposures") +
    theme_bw() + # set the theme
    theme(text = element_text(size = 18))

# fairness for users (compare how the best arm selected by the bandit performs for different user features)

# compute the reward rate per arm
df_TS_contextual %>%
    group_by(choice) %>%
    summarise(reward_rate = mean(reward))

# select the best arm (highest reward rate)
best_arm <- df_TS_contextual %>%
    group_by(choice) %>%
    summarise(reward_rate = mean(reward)) %>%
    arrange(desc(reward_rate)) %>%
    head(1)

df_best_arm <- df_zozo %>%
    filter(item_id == best_arm$choice)

# test whether the reward rate is the same for each user feature
df_best_arm %>%
    group_by(user_feature_0) %>%
    summarise(reward_rate = mean(click))

# see user_feature_1
df_best_arm %>%
    group_by(user_feature_1) %>%
    summarise(reward_rate = mean(click))

# see user_feature_2
df_best_arm %>%
    group_by(user_feature_2) %>%
    summarise(reward_rate = mean(click))

# see user_feature_3
df_best_arm %>%
    group_by(user_feature_3) %>%
    summarise(reward_rate = mean(click))
