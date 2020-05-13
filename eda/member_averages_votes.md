Trump Score
================
Mac Bagwell
2020-02-24

``` r
# Libraries
library(tidyverse)
library(lubridate)
# Parameters
START_115 <- ymd("2017-02-02")
END_115 <- ymd("2018-12-31")
votes_file <- here::here("c01-own/data/votes.rds")
member_averages_file <- here::here("c01-own/data/member_averages.rds")
window <- 180L
#===============================================================================
votes_df <- read_rds(votes_file)
member_averages_df <- read_rds(member_averages_file)
```

## Trump Scores Distribution

Unsurprisingly, Democrats and Republicans occupy distinct intervals on
the Trump Score scale. There is a bit more variance in Democrats’ Trump
Score, though, with some senators agreeing with Trump more often than
not during the 115th congress.

``` r
member_averages_df %>% 
  filter(congress == 115, party != "Independent") %>% 
  ggplot(aes(agree_pct, fill = party)) +
  geom_vline(xintercept = 0.5) +
  geom_dotplot(binwidth = 0.02) +
  scale_x_continuous(
    breaks = scales::breaks_width(0.1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  scale_fill_manual(
    breaks = c("Republican", "Democrat"),
    values = c("red", "royalblue")
  ) +
  coord_cartesian(xlim = c(0.05, 1.05), expand = FALSE) +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "Trump Score Distribution, 115th Congress",
    x = "Trump Score",
    y = "",
    fill = ""
  ) +
  ggsave("figures/trump_score_distribution.png")
```

    ## Saving 7 x 5 in image

![](member_averages_votes_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Trump Score vs. Trump Support

Much of the variation in Democrats’ Trump Scores can be attributed to
the political tastes of their constituent state. The Democrats with
outlier Trump Scores hailed from states that Trump took by a veritable
landslide in 2016. This is not surprising, but it complicates our
analysis of the effects of protesting on senatorial votes, since
presumably, protest activity will correlate significantly with political
tastes.

``` r
member_averages_df %>% 
  filter(congress == 115, party != "Independent") %>% 
  ggplot(aes(net_trump_vote, agree_pct)) +
  geom_hline(yintercept = 0.5) +
  geom_vline(xintercept = 0) +
  geom_smooth(aes(color = party), method = "lm") +
  geom_point(aes(color = party)) +
  ggrepel::geom_text_repel(
    data = . %>% filter(party == "Democrat") %>% top_n(5, net_trump_vote),
    mapping = aes(label = str_c(last_name, " (D-", state, ")"))
  ) +
  ggrepel::geom_text_repel(
    data = . %>% filter(party == "Republican") %>% top_n(-5, agree_pct),
    mapping = aes(label = str_c(last_name, " (R-", state, ")"))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(0.1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  scale_color_manual(
    breaks = c("Republican", "Democrat"),
    values = c("red", "royalblue")
  ) +
  theme_bw() +
  labs(
    title = "Trump Score vs. State Net Trump Vote",
    x = "State Net Trump Vote (2016)",
    y = "Trump Score",
    color = ""
  ) +
  ggsave("figures/trump_score_vs_net_trump_vote.png")
```

    ## Saving 7 x 5 in image

![](member_averages_votes_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
member_averages_df %>% 
  filter(congress == 115L) %>% 
  ggplot(aes(votes)) +
  geom_dotplot(binwidth = 1) +
  labs(
    title = "Number of Votes by Senator, 115th Congress",
    x = "Number of Votes",
    y = ""
  ) +
  ggsave("figures/trump_score_votes.png")
```

    ## Saving 7 x 5 in image

![](member_averages_votes_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Trump Score over time

``` r
trump_score <- 
  tibble(
    date = 
      seq(START_115, END_115, by = "1 day") %>% as.Date()
  ) %>% 
  left_join(
    votes_df %>% 
      mutate(date = as.Date(voted_at)) %>% 
      group_by(date, bioguide, last_name) %>% 
      summarize(
        agrees = sum(agree),
        votes = n()
      ) %>% 
      pivot_wider(
        names_from = c(bioguide, last_name),
        values_from = c(agrees, votes)
      ),
    by = "date"
  ) %>% 
  mutate_at(
    vars(starts_with("agrees"), starts_with("votes")),
    RcppRoll::roll_sum,
    n = window,
    align = "right",
    fill = NA_integer_,
    na.rm = TRUE
  ) %>% 
  slice(-(1:window)) %>% 
  pivot_longer(
    cols = -date,
    names_to = c("variable", "bioguide", "last_name"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  filter(votes > 0) %>% 
  mutate(trump_score = agrees / votes) %>% 
  left_join(
    member_averages_df %>% 
      filter(congress == 115) %>% 
      select(bioguide, party),
    by = "bioguide"
  )

trump_score %>% 
  drop_na(trump_score) %>% 
  filter(party != "Independent") %>% 
  group_by(date, party) %>% 
  summarize(
    pct_25 = quantile(trump_score, 0.25),
    pct_50 = quantile(trump_score, 0.5),
    pct_75 = quantile(trump_score, 0.75)
  ) %>% 
  ggplot(aes(date, fill = party)) +
  geom_line(aes(y = pct_25, group = party)) +
  geom_line(aes(y = pct_75, group = party)) +
  geom_ribbon(aes(ymin = pct_25, ymax = pct_75, alpha = "middle-50")) +
  geom_line(aes(y = pct_50, group = party)) +
  geom_hline(yintercept = 0.5) +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %y"
  ) +
  scale_y_continuous( 
    breaks = scales::breaks_width(0.1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  scale_fill_manual(
    breaks = c("Republican", "Democrat"),
    values = c("red", "royalblue")
  ) +
  scale_alpha_manual(
    breaks = c("middle-50"),
    labels = c("Middle 50%"),
    values = c(0.85)
  ) +
  coord_cartesian(
    ylim = c(0, 1),
    expand = FALSE
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    title = str_glue("Middle 50% of {window}D Rolling Trump Scores by Party, 115th Congress"),
    x = "",
    y = "Trump Score",
    fill = "",
    alpha = ""
  ) +
  ggsave("figures/trump_score_party.png")
```

    ## Saving 7 x 5 in image

![](member_averages_votes_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
