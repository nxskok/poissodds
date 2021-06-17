# functions

# get odds from website and scale into probabilities. mult downscales draws (if less than 1).

make_prob_df = function(url, mult = 1) {
  read_html(url) -> html
  html %>% html_nodes("td") %>% 
    html_attr("data-best-dig") -> odds
  if (length(odds)==0) stop("no odds!")
  html %>% html_nodes("td") %>% 
    html_attr("title") -> title
  tibble(title, odds) %>% drop_na() %>% 
    extract(title, into = "team", regex = "Add (.*) to betslip") %>% 
    mutate(what = rep(c("H", "D", "A"), length.out = nrow(.))) %>% 
    mutate(row = gl(nrow(.)/3, 3)) %>% 
    mutate(prob = 1/as.numeric(odds)) %>%
    group_by(row) %>%
    mutate(prob = prob / sum(prob)) %>%
    ungroup() %>%
    select(-odds) %>%
    pivot_wider(names_from = what,
                values_from = c(team, prob)) %>%
    mutate(game = str_c(team_H, " v ", team_A)) %>%
    mutate(prob_D = prob_D * mult,
           prob_notD = 1 - prob_D,
           prob_H = prob_H / prob_notD,
           prob_A = prob_A / prob_notD) %>% 
    select(game, prob_H, prob_D, prob_A)
}

# poisson probabilities of home win, draw, away win based on means l1 (home team), l2 (away team). 
# Works out probability of each score 0-0 to 10-10 assuming independence.

poiss_probs <- function(l1, l2)  {
  crossing(s1 = 0:10, s2 = 0:10) %>% 
    mutate(p1 = dpois(s1, l1), p2 = dpois(s2, l2)) %>% 
    mutate(p = p1*p2) %>% 
    mutate(res = case_when(
      s1>s2 ~ "H",
      s1<s2 ~ "A",
      TRUE  ~ "D"
    )) %>% 
    group_by(res) %>% 
    summarize(prob = sum(p)) %>% 
    arrange(desc(res))
}

# given two poisson means as input along with target probabilities of home win, draw, away win,
# work out actual probabilities of each result using poiss_probs and return sum of squared 
# errors from target

from_target <- function(lambda, target) {
  poiss_probs(lambda[1], lambda[2]) %>% 
    mutate(target = target) %>% 
    summarize(ssq = sum((prob-target)^2)) %>% 
    pull(ssq)
}

# for input target probabilities of home win, draw, away win, find the poisson means that come closest to
# producing those probabilities. Default initial values are about right for soccer.

opt_lambda <- function(probs, start = c(1.5, 1.5)) {
  ans <- optim(start, from_target, target = probs)
  ans$par
}

# run everything from dataframe leagues, with optional discounting of draws (default 1 means no discounting)

make_lambdas <- function(leagues, discount = 1) {
  leagues %>% 
    rowwise() %>% 
    mutate(p = list(make_prob_df(sheet_url, discount))) %>% 
    unnest(p) %>% 
    rowwise() %>% 
    mutate(lambda = list(opt_lambda(c(prob_H, prob_D, prob_A)))) %>% 
    unnest_wider(lambda, names_sep = "_") %>% 
    select(short, game, starts_with("prob"), starts_with("lambda"))
}

