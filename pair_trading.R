library(tidyquant)

# Bank tickers
bank_tickers <- c("HDFCBANK.NS", "ICICIBANK.NS", "AXISBANK.NS", "SBIN.NS", 
                  "KOTAKBANK.NS", "BANKBARODA.NS", "BANKINDIA.NS", "CANBK.NS",
                  "IDFCFIRSTB.NS", "PNB.NS", "INDUSINDBK.NS", "FEDERALBNK.NS",
                  "IDBI.NS", "RBLBANK.NS", "DCBBANK.NS", "UNIONBANK.NS", "YESBANK.NS")

# Date range
start_date <- "2015-05-13"
end_date   <- "2025-05-13"

# Folder to store CSVs
dir.create("bank_ohlc_data", showWarnings = FALSE)

# Loop and assign to Global Environment + save CSV
for (ticker in bank_tickers) {
  try({
    data <- tq_get(ticker, from = start_date, to = end_date, get = "stock.prices")
    
    # Save to CSV
    write.csv(data, file = paste0("bank_ohlc_data/", ticker, ".csv"), row.names = FALSE)
    
    # Assign to global environment (removes . to make it a valid R object name)
    assign(gsub("\\.", "_", ticker), data, envir = .GlobalEnv)
    
  }, silent = TRUE)
}
library(dplyr)
library(tidyr)
library(tibble)

# 1. Identify all loaded bank datasets
bank_objects <- ls(pattern = "_NS$")

# 2. Calculate daily log returns and store in a list
returns_list <- list()

for (ticker_obj in bank_objects) {
  df <- get(ticker_obj)
  
  if (!"adjusted" %in% colnames(df)) next  # safety check
  
  df_returns <- df %>%
    arrange(date) %>%
    mutate(return = log(adjusted / lag(adjusted))) %>%
    select(date, return)
  
  # Rename return column
  colnames(df_returns)[2] <- ticker_obj
  returns_list[[ticker_obj]] <- df_returns
}

# 3. Merge all returns on date
returns_merged <- Reduce(function(x, y) full_join(x, y, by = "date"), returns_list)

# 4. Drop NA rows (incomplete return rows)
returns_clean <- na.omit(returns_merged)

# 5. Compute Pearson correlation
cor_matrix <- cor(returns_clean[,-1], method = "pearson")

# 6. Find highly correlated pairs > 0.90
high_corr_pairs <- which(cor_matrix > 0.90 & cor_matrix < 1, arr.ind = TRUE)

# 7. Extract and print pairs
printed <- c()

cat("Highly correlated pairs (Pearson > 0.90):\n")
for (i in seq_len(nrow(high_corr_pairs))) {
  row <- high_corr_pairs[i, 1]
  col <- high_corr_pairs[i, 2]
  
  # Avoid duplicate pairs
  key <- paste(sort(c(rownames(cor_matrix)[row], colnames(cor_matrix)[col])), collapse = "-")
  if (key %in% printed) next
  
  corr_val <- cor_matrix[row, col]
  cat(sprintf("%s - %s : %.3f\n", rownames(cor_matrix)[row], colnames(cor_matrix)[col], corr_val))
  printed <- c(printed, key)
}
library(zoo)
library(dplyr)
library(tidyr)

# 1. Data: returns_clean already has date + returns
returns_xts <- zoo::zoo(returns_clean[,-1], order.by = returns_clean$date)

# 2. Get all unique ticker pairs
tickers <- colnames(returns_xts)
pair_indices <- combn(tickers, 2, simplify = FALSE)

# 3. Rolling correlation function
rolling_corrs <- list()

for (pair in pair_indices) {
  x <- returns_xts[, pair[1]]
  y <- returns_xts[, pair[2]]
  
  roll_corr <- zoo::rollapply(
    cbind(x, y),
    width = 60,
    FUN = function(z) cor(z[, 1], z[, 2], use = "complete.obs"),
    by.column = FALSE,
    align = "right",
    fill = NA
  )
  
  # Store result
  df <- data.frame(date = index(roll_corr),
                   correlation = coredata(roll_corr),
                   ticker1 = pair[1],
                   ticker2 = pair[2])
  
  key <- paste(pair, collapse = "_")
  rolling_corrs[[key]] <- df
}

# Example: preview one
head(rolling_corrs[["HDFCBANK_NS_ICICIBANK_NS"]])
rolling_corrs[["HDFCBANK_NS_ICICIBANK_NS"]] %>%
  filter(!is.na(correlation)) %>%
  head()
library(ggplot2)

rolling_corrs[["HDFCBANK_NS_ICICIBANK_NS"]] %>%
  filter(!is.na(correlation)) %>%
  ggplot(aes(x = date, y = correlation)) +
  geom_line(color = "steelblue") +
  labs(title = "60-Day Rolling Correlation: HDFC vs ICICI",
       y = "Pearson Correlation", x = "Date") +
  theme_minimal()
library(dplyr)
library(ggplot2)

# Extract adjusted prices
bob <- BANKBARODA_NS %>% select(date, adj_bob = adjusted)
canbk <- CANBK_NS %>% select(date, adj_canbk = adjusted)

# Merge and compute log prices
pair_data <- inner_join(bob, canbk, by = "date") %>%
  mutate(log_bob = log(adj_bob),
         log_canbk = log(adj_canbk))

# Estimate hedge ratio (beta) using OLS regression
hedge_model <- lm(log_bob ~ log_canbk, data = pair_data)
beta <- coef(hedge_model)[2]
cat("Estimated β:", round(beta, 4), "\n")

# Compute spread and Z-score
pair_data <- pair_data %>%
  mutate(spread = log_bob - beta * log_canbk,
         spread_mean = mean(spread, na.rm = TRUE),
         spread_sd = sd(spread, na.rm = TRUE),
         z_score = (spread - spread_mean) / spread_sd)

# Plot Z-score
ggplot(pair_data, aes(x = date, y = z_score)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = c(-1, 0, 1), linetype = "dashed", color = "darkred") +
  labs(title = "Z-Score of Price Spread: Bank of Baroda vs Canara Bank",
       y = "Z-Score", x = "Date") +
  theme_minimal()
# Assume pair_data has date, z_score, spread, adj_bob, adj_canbk

# Initialize state tracking
pair_data <- pair_data %>%
  mutate(signal = 0,   # 1 = long spread, -1 = short spread
         position = 0,
         pnl = 0)

# Generate trading signals
for (i in 2:nrow(pair_data)) {
  z = pair_data$z_score[i]
  z_prev = pair_data$z_score[i - 1]
  position = pair_data$position[i - 1]
  
  # Entry Signals
  if (position == 0) {
    if (z < -1) pair_data$signal[i] = 1  # Long spread
    if (z > 1)  pair_data$signal[i] = -1 # Short spread
  }
  
  # Exit Signals
  if (position != 0 && sign(z) != sign(z_prev)) {
    pair_data$signal[i] = 0  # Exit
  }
  
  # Update position
  if (pair_data$signal[i] != 0) {
    pair_data$position[i] = pair_data$signal[i]
  } else {
    pair_data$position[i] = pair_data$position[i - 1]
  }
  
  # Calculate spread PnL
  spread_today = pair_data$spread[i]
  spread_yest  = pair_data$spread[i - 1]
  pair_data$pnl[i] = pair_data$position[i - 1] * (spread_today - spread_yest)
}

# Compute cumulative PnL
pair_data <- pair_data %>%
  mutate(cum_pnl = cumsum(replace_na(pnl, 0)))

# Plot Equity Curve
library(ggplot2)

ggplot(pair_data, aes(x = date, y = cum_pnl)) +
  geom_line(color = "forestgreen", size = 1) +
  labs(title = "Equity Curve of Pair Trading: BoB vs Canara Bank",
       x = "Date", y = "Cumulative PnL (Spread Units)") +
  theme_minimal()
# Initialize variables
trades <- list()
in_trade <- FALSE
entry_idx <- NA
position <- 0  # 1 = long spread, -1 = short spread

for (i in 2:nrow(pair_data)) {
  z = pair_data$z_score[i]
  z_prev = pair_data$z_score[i - 1]
  
  # --- Entry ---
  if (!in_trade) {
    if (z < -1) {
      in_trade <- TRUE
      position <- 1  # Long spread
      entry_idx <- i
    } else if (z > 1) {
      in_trade <- TRUE
      position <- -1  # Short spread
      entry_idx <- i
    }
  }
  
  # --- Exit ---
  else if (in_trade && (z * z_prev < 0)) {
    exit_idx <- i
    
    # Record trade
    trade <- tibble(
      entry_date   = pair_data$date[entry_idx],
      exit_date    = pair_data$date[exit_idx],
      entry_z      = pair_data$z_score[entry_idx],
      exit_z       = z,
      position     = position,
      entry_spread = pair_data$spread[entry_idx],
      exit_spread  = pair_data$spread[exit_idx],
      pnl          = position * (pair_data$spread[exit_idx] - pair_data$spread[entry_idx])
    )
    
    trades[[length(trades) + 1]] <- trade
    
    # Reset state
    in_trade <- FALSE
    entry_idx <- NA
    position <- 0
  }
}

# Combine trades
trade_log <- bind_rows(trades)

# Print each trade
print(trade_log)

# Total PnL
cat("\n==== Total PnL ====\n")
cat("Total Trades:", nrow(trade_log), "\n")
cat("Cumulative PnL:", round(sum(trade_log$pnl), 5), "spread units\n")
library(zoo)
library(dplyr)
library(tibble)

# 1. Calculate 60-day rolling mean and SD of spread
pair_data <- pair_data %>%
  mutate(
    spread_mean_60 = rollmean(spread, 60, fill = NA, align = "right"),
    spread_sd_60   = rollapply(spread, 60, sd, fill = NA, align = "right"),
    z_score_60     = (spread - spread_mean_60) / spread_sd_60
  )

# 2. Initialize trade tracking
trades <- list()
in_trade <- FALSE
entry_idx <- NA
position <- 0  # 1 = long spread, -1 = short spread

# 3. Backtest with tighter Z logic
for (i in 61:nrow(pair_data)) {  # start after day 60 due to rolling window
  z = pair_data$z_score_60[i]
  z_prev = pair_data$z_score_60[i - 1]
  
  # --- Entry Logic ---
  if (!in_trade) {
    if (z < -0.75) {
      in_trade <- TRUE
      position <- 1  # Long spread
      entry_idx <- i
    } else if (z > 0.75) {
      in_trade <- TRUE
      position <- -1  # Short spread
      entry_idx <- i
    }
  }
  
  # --- Exit Logic (Z-score reverts to ±0.25) ---
  if (in_trade && abs(z) < 0.25) {
    exit_idx <- i
    
    trade <- tibble(
      entry_date   = pair_data$date[entry_idx],
      exit_date    = pair_data$date[exit_idx],
      entry_z      = pair_data$z_score_60[entry_idx],
      exit_z       = z,
      position     = position,
      entry_spread = pair_data$spread[entry_idx],
      exit_spread  = pair_data$spread[exit_idx],
      pnl          = position * (pair_data$spread[exit_idx] - pair_data$spread[entry_idx])
    )
    
    trades[[length(trades) + 1]] <- trade
    
    # Reset
    in_trade <- FALSE
    entry_idx <- NA
    position <- 0
  }
}

# 4. Combine and summarize results
trade_log_tight <- bind_rows(trades)

print(trade_log_tight)
cat("\n==== Total PnL ====\n")
cat("Total Trades:", nrow(trade_log_tight), "\n")
cat("Cumulative PnL:", round(sum(trade_log_tight$pnl), 10), "spread units\n")
# Assume each trade uses ₹100,000 capital
capital <- 100000

trade_log_tight <- trade_log_tight %>%
  mutate(
    pnl_inr = pnl * capital
  )

# Print trade log with INR PnL
print(trade_log_tight %>% select(entry_date, exit_date, position, pnl, pnl_inr))

# Total
total_pnl_inr <- sum(trade_log_tight$pnl_inr)
cat("\n==== Capital-Based Results ====\n")
cat("Total Trades:", nrow(trade_log_tight), "\n")
cat("Total PnL (log spread):", round(sum(trade_log_tight$pnl), 5), "\n")
cat("Total PnL (INR): ₹", round(total_pnl_inr, 2), "\n")
trades <- list()
in_trade <- FALSE
entry_idx <- NA
position <- 0

for (i in 61:nrow(pair_data)) {
  z = pair_data$z_score_60[i]
  
  # --- Entry Logic ---
  if (!in_trade) {
    if (z < -1) {
      in_trade <- TRUE
      position <- 1  # Long spread
      entry_idx <- i
    } else if (z > 1) {
      in_trade <- TRUE
      position <- -1  # Short spread
      entry_idx <- i
    }
  }
  
  # --- Exit Logic (opposite side reached) ---
  if (in_trade) {
    if ((position == 1 && z > 1) || (position == -1 && z < -1)) {
      exit_idx <- i
      
      trade <- tibble(
        entry_date   = pair_data$date[entry_idx],
        exit_date    = pair_data$date[exit_idx],
        entry_z      = pair_data$z_score_60[entry_idx],
        exit_z       = z,
        position     = position,
        entry_spread = pair_data$spread[entry_idx],
        exit_spread  = pair_data$spread[exit_idx],
        pnl          = position * (pair_data$spread[exit_idx] - pair_data$spread[entry_idx])
      )
      
      trades[[length(trades) + 1]] <- trade
      
      # Reset
      in_trade <- FALSE
      entry_idx <- NA
      position <- 0
    }
  }
}

# Combine & summarize
trade_log_swing <- bind_rows(trades)
trade_log_swing <- trade_log_swing %>%
  mutate(pnl_inr = pnl * 100000)

print(trade_log_swing %>% select(entry_date, exit_date, entry_z, exit_z, pnl, pnl_inr))

cat("\n==== Swing Strategy PnL ====\n")
cat("Total Trades:", nrow(trade_log_swing), "\n")
cat("Total Spread PnL:", round(sum(trade_log_swing$pnl), 5), "\n")
cat("Total INR PnL: ₹", round(sum(trade_log_swing$pnl_inr), 2), "\n")


#pair arbitrage micro scale project
