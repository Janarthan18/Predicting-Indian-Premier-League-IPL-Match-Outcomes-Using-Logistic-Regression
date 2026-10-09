# =============================================================================
# 01_generate_ipl_dataset.R
# Builds ipl_matches.csv (500 matches): a SYNTHETIC IPL-style match dataset (2008-2024)
# whose columns mirror the public Kaggle "IPL Complete Dataset" schema.
# To use the REAL Kaggle file instead, download it, rename it ipl_matches.csv,
# keep these column names, and skip this script.
# =============================================================================
set.seed(2024)

teams <- c("Chennai Super Kings", "Mumbai Indians", "Royal Challengers Bangalore",
           "Kolkata Knight Riders", "Delhi Capitals", "Rajasthan Royals",
           "Sunrisers Hyderabad", "Punjab Kings", "Lucknow Super Giants", "Gujarat Titans")

home_venue <- c("Chennai Super Kings" = "MA Chidambaram Stadium",
                "Mumbai Indians" = "Wankhede Stadium",
                "Royal Challengers Bangalore" = "M Chinnaswamy Stadium",
                "Kolkata Knight Riders" = "Eden Gardens",
                "Delhi Capitals" = "Arun Jaitley Stadium",
                "Rajasthan Royals" = "Sawai Mansingh Stadium",
                "Sunrisers Hyderabad" = "Rajiv Gandhi Stadium",
                "Punjab Kings" = "PCA Stadium Mohali",
                "Lucknow Super Giants" = "Ekana Stadium",
                "Gujarat Titans" = "Narendra Modi Stadium")
neutral_venues <- c("Dubai International Stadium", "Sharjah Cricket Stadium", "Sheikh Zayed Stadium")
venue_city <- c("Chennai", "Mumbai", "Bengaluru", "Kolkata", "Delhi", "Jaipur", "Hyderabad",
                "Mohali", "Lucknow", "Ahmedabad", "Dubai", "Sharjah", "Abu Dhabi")
names(venue_city) <- c(unname(home_venue), neutral_venues)

# hidden "true" team strength + small home and toss effects (drives the outcomes)
strength <- setNames(rnorm(length(teams), 0, 0.30), teams)
umpires  <- sprintf("Umpire_%02d", 1:15)

rows <- list(); id <- 0
for (season in 2008:2024) {
  n_matches <- ifelse(season >= 2022, 36, 28)
  avail <- if (season >= 2022) teams else teams[1:8]
  for (m in seq_len(n_matches)) {
    id <- id + 1
    pair <- sample(avail, 2); t1 <- pair[1]; t2 <- pair[2]
    if (season == 2020) {                                   # UAE bubble season
      venue <- sample(neutral_venues, 1)
    } else if (runif(1) < 0.75) {
      venue <- unname(home_venue[sample(c(t1, t2), 1)])
    } else venue <- sample(c(unname(home_venue[avail]), neutral_venues[1]), 1)
    toss_winner   <- sample(c(t1, t2), 1)
    toss_decision <- sample(c("field", "bat"), 1, prob = c(0.62, 0.38))
    h1 <- as.numeric(venue == home_venue[t1]); h2 <- as.numeric(venue == home_venue[t2])
    logit <- strength[t1] - strength[t2] + 0.35 * h1 - 0.35 * h2 +
             ifelse(toss_winner == t1, 0.15, -0.15)
    t1_wins <- runif(1) < plogis(logit)
    r <- round(rnorm(2, 168, 22)); r <- r + ifelse(runif(2) < 0.04, 30, 0)
    hi <- max(r); lo <- min(r); if (hi == lo) hi <- hi + 1
    t1_runs <- ifelse(t1_wins, hi, lo); t2_runs <- ifelse(t1_wins, lo, hi)
    rows[[id]] <- data.frame(
      match_id = id, season = season,
      date = format(as.Date(paste0(season, "-03-22")) + floor(m * 66 / n_matches)),
      city = unname(venue_city[venue]), venue = venue, team1 = t1, team2 = t2,
      toss_winner = toss_winner, toss_decision = toss_decision,
      winner = ifelse(t1_wins, t1, t2), team1_runs = t1_runs, team2_runs = t2_runs,
      team1_wickets = min(10, rbinom(1, 10, ifelse(t1_wins, 0.35, 0.65))),
      umpire1 = sample(umpires, 1), umpire2 = sample(umpires, 1),
      stringsAsFactors = FALSE)
  }
}
ipl <- do.call(rbind, rows)

# inject a few realistic missing values
ipl$city[sample(nrow(ipl), round(0.03 * nrow(ipl)))]           <- NA
ipl$umpire2[sample(nrow(ipl), round(0.02 * nrow(ipl)))]        <- NA
ipl$team1_wickets[sample(nrow(ipl), round(0.015 * nrow(ipl)))] <- NA

write.csv(ipl, "ipl_matches.csv", row.names = FALSE)
cat("ipl_matches.csv written:", nrow(ipl), "rows x", ncol(ipl), "columns\n")
