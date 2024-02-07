
#' @title Cumulative reward
#' @description This function calculates the cumulative rewards from simulation results.
#' @param df_res_sim A data frame containing the simulation results, which must include the columns t, sim, and reward.
#' @return A data frame containing the cumulative rewards.
#' @output A plot of the average cumulative reward of all simulations.
cum_reward <- function(df_res_sim) {
    # caculate the maximum number of rounds
    max_rounds <- df_res_sim %>%
        group_by(sim) %>%
        summarise(max_t = max(t))

    # compute the maximum number of rounds
    max_obs <- min(max_rounds$max_t)

    df_res_sim_agg <- df_res_sim %>%
        filter(t <= max_obs) %>%
        group_by(sim) %>%
        mutate(cum_reward = cumsum(reward)) %>%
        group_by(t) %>%
        summarise(mean_cum_reward = mean(cum_reward),
                  se_cum_reward = sd(cum_reward,na.rm = TRUE)/sqrt(n_sim)) %>%
        mutate(upper_ci = mean_cum_reward + 1.96*se_cum_reward,
               lower_ci = mean_cum_reward - 1.96*se_cum_reward)

    # plot the average cumulative reward of all simulations
    plot_cum <- ggplot(df_res_sim_agg, aes(x = t, y = mean_cum_reward)) +
                geom_line(aes(y = mean_cum_reward), color = "orange") +
                geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "gray", alpha = 0.1) +
                scale_color_manual(values = legend) +
                labs(x = "Rounds", y = "Cumulative Reward") +
                theme_bw()+ # set the theme
                theme(text = element_text(size=18))

    print(plot_cum)

    return(df_res_sim_agg)
}