#############################
## Load Zone
#############################
if (!require("pacman")) install.packages("pacman")

pacman::p_load(knitr, ggplot2, png,
               tidyverse, rlist, contextual,
               lubridate, zoo, roll,
               fastDummies)

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
# df_zozo <- dummy_cols(df_zozo, select_columns = c("user_feature_0", "user_feature_1", 
#                        "user_feature_2", "user_feature_3"), remove_first_dummy = TRUE)

#############################
## Thompson Sampling
#############################

# Vanilla Thompson Sampling

# initialize the bandit
bandit_TS_vanilla <- OfflineReplayEvaluatorBandit$new(click ~ item_id, df_zozo)

# initialize simulation parameters
size_sim <- 100000
n_sim <- 10

# here we define the Thompson Sampling policy object
###!!
# Parameters for tuning:
# - alpha: the alpha parameter of the beta distribution
# - beta: the beta parameter of the beta distribution
policy_TS_vanilla <- ThompsonSamplingPolicy$new()

# create an agent to make arm choice based on the policy
agent_TS_vanilla <- Agent$new(policy_TS_vanilla, # add policy
                              bandit_TS_vanilla) # add bandit

# initialize the simulator
sim_TS_vanilla <- Simulator$new(agent_TS_vanilla, # set our agent
                                horizon = size_sim, # set size of sim
                                do_parallel = TRUE, # run in parallel
                                simulations = n_sim)  # simulate it n_sim times

# run the simulation
history_TS_vanilla <- sim_TS_vanilla$run()

# gather results
df_TS_vanilla <- history_TS_vanilla$data %>%
    select(t, sim, choice, reward, agent)

df_TS_vanilla_agg <- cum_reward(df_TS_vanilla)

# Contextual Thompson Sampling

# initialize the bandit

bandit_TS_contextual <- OfflineReplayEvaluatorBandit$new(click ~ item_id | position_2 + position_3, df_zozo)

# initialize the policy
policy_TS_contextual <- ContextualLinTSPolicy$new()

# create an agent to make arm choice based on the policy
agent_TS_contextual <- Agent$new(policy_TS_contextual, # add policy
                                 bandit_TS_contextual) # add bandit

# initialize the simulator
sim_TS_contextual <- Simulator$new(agent_TS_contextual, # set our agent
                                   horizon = size_sim, # set size of sim
                                   do_parallel = TRUE, # run in parallel
                                   simulations = n_sim)  # simulate it n_sim times

# run the simulation
history_TS_contextual <- sim_TS_contextual$run()

# gather results
df_TS_contextual <- history_TS_contextual$data %>%
    select(t, sim, choice, reward, agent)

df_TS_contextual_agg <- cum_reward(df_TS_contextual)

# plot df_TS_vanilla_agg and df_TS_contextual_agg in the same plot
ggplot(df_TS_vanilla_agg, aes(x = t, y = mean_cum_reward)) +
    geom_line(aes(y = mean_cum_reward), color = "orange") +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "yellow", alpha = 0.1) +
    geom_line(data = df_TS_contextual_agg, aes(x = t, y = mean_cum_reward), color = "blue") +
    geom_ribbon(data = df_TS_contextual_agg, aes(x = t, ymin = lower_ci, ymax = upper_ci), fill = "grey", alpha = 0.1) +
    scale_color_manual(values = c("orange", "blue")) +
    labs(x = "Rounds", y = "Cumulative Reward") +
    theme_bw()+ # set the theme
    theme(text = element_text(size=18))


#############################
## UCB
#############################


#############################
## Thompson Sampling sensitivity analysis
#############################

## Alphas values
alpha_values <- seq(0.01, 5, length.out = 7)
average_rewards <- vector("list", length(alpha_values))

# Loop over each alpha value
for (i in seq_along(alpha_values)) {
    policy_TS_vanilla <- ThompsonSamplingPolicy$new(alpha = alpha_values[i])
    agent_TS_contextual <- Agent$new(policy_TS_vanilla, # add policy
                                     bandit_TS_vanilla) # add bandit
    # initialize the simulator
    sim_TS_vanilla <- Simulator$new(agent_TS_vanilla, # set our agent
                                    horizon = size_sim, # set size of sim
                                    do_parallel = TRUE, # run in parallel
                                    simulations = 1)  # simulate it n_sim times
    
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
  agent_TS_contextual <- Agent$new(policy_TS_vanilla, # add policy
                                   bandit_TS_vanilla) # add bandit
  # initialize the simulator
  sim_TS_vanilla <- Simulator$new(agent_TS_vanilla, # set our agent
                                  horizon = size_sim, # set size of sim
                                  do_parallel = TRUE, # run in parallel
                                  simulations = 1)  # simulate it n_sim times
  
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
       legend = c(expression(paste( alpha, " values, ", beta, " fixed")), expression(paste(beta, " values, ", alpha, " fixed"))), 
       col = c("blue", "red"), 
       lty = 1, 
       cex = 0.8)

