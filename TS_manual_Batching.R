
# Packes required for subsequent analysis. P_load ensures these will be installed and loaded.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,
               tidyr,
               ggplot2,
               reshape2,
               latex2exp,
               xtable,
               urca,
               lubridate, 
               contextual
               )

# reformat data for batching analysis
dfZozo80$index = dfZozo80$X
dfZozo80$arm = dfZozo80$item_id
dfZozo80$reward = dfZozo80$click
dfForBatching= dfZozo80[,c("arm", "reward", "index")]

n_arms=79
policy_TS <- function(alpha, beta, n_arms=79, n_sample=100){
  arm <- 1:n_arms
  
  # dataframe with alpha, beta per arm
  df_alpha_beta <- data.frame(arm=arm,alpha = alpha, beta = beta)
  
  # create dataframe with sampled values
  df_sampled <- mapply(rbeta, n_sample, df_alpha_beta$alpha, df_alpha_beta$beta) 
  
  # average per sample
  avg_from_sampled <- apply(df_sampled, 2, mean)
  
  # get the arm
  chosen_arm <- which.max(avg_from_sampled)
  
# return the chosen arm
  return(chosen_arm)
}

sim_TS <- function(df, n_before_sim, n_sim, interval=1){
  
  ## Part 1: create two dataframes, one with data before start of policy, and one with data after
  
  # define the number of observations of all data available
  n_obs <- nrow(df)
  
  # Give user a warning: the size of the intended experiment is bigger than the data provided
  if(n_sim > (n_obs-n_before_sim)){
    stop("The indicated size of the experiment is bigger than the data provided - shrink the size ")
  }
  
  # find n_before_sim random observations to be used before start policy
  index_before_sim <- sample(1:n_obs,n_before_sim)
  
  # using indexing, create dataframe with data before start policy
  df_before_policy <- df[index_before_sim,]
  
  # create dataframe with data that we can sample from during policy
  df_during_policy <- df[-index_before_sim,]
  
  # dataframe where the results of storing the policy are stored
  df_results_policy <- data.frame(matrix(NA, nrow = n_sim, ncol = 2))
  colnames(df_results_policy) <- c('arm', 'reward')
  
  # initialize alpha, beta to ones
  # alpha per arm
  alpha <-rep(1,79)
  
  # beta per arm
  beta <- rep(1,79)
  
  ## part 2: apply TS algorithm, updating at interval
  alpha_batch <- rep(1,79)
  beta_batch <- rep(1,79)
  
  i = 1
  while(i <= n_sim){
    chosen_arm <- policy_TS(alpha, beta)
    
    # select from the data for experiment the arm chosen
    df_during_policy_arm <- df_during_policy %>% 
      filter(arm==chosen_arm)
    
    # randomly sample from this arm and observe the reward
    sampled_arm <- sample(1:nrow(df_during_policy_arm), 1)
    reward <- df_during_policy_arm$reward[sampled_arm ]
    
    # important: remove the reward from the dataset to prevent repeated sampling
    index_result <- df_during_policy_arm$index[sampled_arm]
    df_during_policy_arm <- df_during_policy_arm %>% filter(index == index_result)
    
    alpha_batch[chosen_arm] <- alpha_batch[chosen_arm] + reward
    beta_batch[chosen_arm] <- beta_batch[chosen_arm] + 1-reward
    
    # update at interval
    if((i==1) || ((i %% interval)==0)){
      alpha <- alpha_batch
      beta <- beta_batch
      }
    
    
    # warn the user to increase dataset or downside the size of experiment, 
    # in the case that have sampled all observations from an arm
    if(length(reward) == 0){
      stop("You have run out of observations from a chosen arm")
      break
    }
    
    # get a vector of results from chosen arm (arm, reward)
    result_policy_i <- c(chosen_arm, reward)
    
    # add to dataframe to save the result
    df_results_policy[i,] <- result_policy_i
    
    # onto the next
    i <- i + 1
    
  }
  
  
  # save results in list
  results <- list(df_results_of_policy = df_results_policy,
                  df_sample_of_policy = df[-index_before_sim,],
                  alpha = alpha,
                  beta = beta)
  
  return(results)
  
}

# set parameters: observations before start simulation, then how many observations for simulation
n_before_sim = 100
n_sim = 16500 # equivalent to horizons, the simulation stop at this hoziron when we use all the data as one of the arms runs out of data at this time step
n_runs <- 10 # equivalent to n_sim in other code files

# set the seed
set.seed(0)

runs_int_1 <- data.frame(matrix(ncol=n_runs, nrow=n_sim))   
runs_int_10 <- data.frame(matrix(ncol=n_runs, nrow=n_sim))
runs_int_100 <- data.frame(matrix(ncol=n_runs, nrow=n_sim))
runs_int_1000 <- data.frame(matrix(ncol=n_runs, nrow=n_sim))

for (i in 1:n_runs){
  result_sim_int_1 <- sim_TS(dfForBatching,
                             n_before_sim=n_before_sim,
                             n_sim=n_sim,
                             interval=1)
  
  result_sim_int_10 <- sim_TS(dfForBatching,
                             n_before_sim=n_before_sim,
                             n_sim=n_sim,
                             interval=10)
  
  result_sim_int_100 <- sim_TS(dfForBatching,
                             n_before_sim=n_before_sim,
                             n_sim=n_sim,
                             interval=100)
  
  result_sim_int_1000 <- sim_TS(dfForBatching,
                               n_before_sim=n_before_sim,
                               n_sim=n_sim,
                               interval=1000)
  
  runs_int_1[i] <- cumsum(result_sim_int_1$df_results_of_policy$reward)
  runs_int_10[i] <- cumsum(result_sim_int_10$df_results_of_policy$reward)
  runs_int_100[i] <- cumsum(result_sim_int_100$df_results_of_policy$reward)
  runs_int_1000[i] <- cumsum(result_sim_int_1000$df_results_of_policy$reward)
}

int_1_avg <- apply(runs_int_1,1,mean)
int_1_se <- apply(runs_int_1,1,sd)/sqrt(n_runs)
int_1_lower_CI =int_1_avg - 1.96*int_1_se
int_1_upper_CI =int_1_avg + 1.96*int_1_se

int_10_avg <- apply(runs_int_10,1,mean)
int_10_se <- apply(runs_int_10,1,sd)/sqrt(n_runs)
int_10_lower_CI =int_10_avg - 1.96*int_10_se
int_10_upper_CI =int_10_avg + 1.96*int_10_se

int_100_avg <- apply(runs_int_100,1,mean)
int_100_se <- apply(runs_int_100,1,sd)/sqrt(n_runs)
int_100_lower_CI =int_100_avg - 1.96*int_100_se
int_100_upper_CI =int_100_avg + 1.96*int_100_se

int_1000_avg <- apply(runs_int_1000,1,mean)
int_1000_se <- apply(runs_int_1000,1,sd)/sqrt(n_runs)
int_1000_lower_CI =int_1000_avg - 1.96*int_1000_se
int_1000_upper_CI =int_1000_avg + 1.96*int_1000_se

int_1_res <- data.frame(t=1:n_sim,
                        avg=int_1_avg,
                        se=int_1_se,
                        lower_ci=int_1_lower_CI,
                        upper_ci = int_1_upper_CI)

int_10_res <- data.frame(t=1:n_sim,
                         avg=int_10_avg,
                         se=int_10_se,
                         lower_ci=int_10_lower_CI,
                         upper_ci = int_10_upper_CI)

int_100_res <- data.frame(t=1:n_sim,
                         avg=int_100_avg,
                         se=int_100_se,
                         lower_ci=int_100_lower_CI,
                         upper_ci = int_100_upper_CI)

int_1000_res <- data.frame(t=1:n_sim,
                          avg=int_1000_avg,
                          se=int_1000_se,
                          lower_ci=int_1000_lower_CI,
                          upper_ci = int_1000_upper_CI)

plot <- ggplot() +
  geom_line(data = int_1_res, aes(x = t, y = avg, color = "1")) +
  geom_ribbon(data = int_1_res, aes(x = t, ymin = lower_ci, ymax = upper_ci, fill = "1"), alpha = 0.1) +
  geom_line(data = int_10_res, aes(x = t, y = avg, color = "10")) +
  geom_ribbon(data = int_10_res, aes(x = t, ymin = lower_ci, ymax = upper_ci, fill = "10"), alpha = 0.1) +
  geom_line(data = int_100_res, aes(x = t, y = avg, color = "100")) +
  geom_ribbon(data = int_100_res, aes(x = t, ymin = lower_ci, ymax = upper_ci, fill = "100"), alpha = 0.1) +
  geom_line(data = int_1000_res, aes(x = t, y = avg, color = "1000")) +
  geom_ribbon(data = int_1000_res, aes(x = t, ymin = lower_ci, ymax = upper_ci, fill = "1000"), alpha = 0.1) +
  scale_color_manual(name = "Batch size", values = c("1" = "orange", "10" = "blue", "100" = "green", "1000" = "red")) +
  scale_fill_manual(name = "Algorithm", values = c("1" = "orange", "10" = "blue", "100" = "green", "1000" = "red"), guide = FALSE) +
  labs(x = "Rounds", y = "Cumulative Reward") +
  theme_bw() + xlim(0,16500)+ ylim(0,87)
  theme(text = element_text(size = 12))

plot
