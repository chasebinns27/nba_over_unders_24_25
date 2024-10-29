library(dplyr)
library(shiny)
#library(nbastatR)
library(tidyr)
library(gt)
library(gtExtras)
library(do)
library(XML)
library(rvest)
library(xml2)

#normal picks
Team <- c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", "DN", "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MIA", "MIL", "MIN", "NYK", "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")

win_total_vector <- c(36.5, 58.5, 19.5, 31.5, 28.5, 48.5, 49.5, 50.5, 25.5, 43.5, 42.5, 46.5, 35.5, 42.5, 43.5, 49.5, 51.5, 53.5, 57.5, 47.5, 49.5, 48.5, 20.5, 46.5, 35.5, 28.5, 27.5, 19.5)

Name <- c("Nick", "Charles", "Nick", "Nick", "Charles", "Nick", "Jason", "Ben", "Jason", "Charles", "Chase", "Jason", "Ben", "Jason", "Chase", "John", "John", "John", "Zach", "Charles", "John", "Ben", "Zach", "Chase", "Zach", "Ben", "Chase", "Zach")

O_U_vector <- c("U", "O", "U", "U", "U", "O", "U", "O", "O", "U", "O", "O", "O", "O", "O", "O", "U", "U", "O", "U", "U", "O", "U", "O", "U", "U", "U", "U")

# Create the data frame
all_picks <- data.frame(team = Team, proj_win_total = win_total_vector, name = Name, pick = O_U_vector)


#WC picks
wc_team <- c("MEM", "MEM", "MEM", "MEM", "MEM", "MEM", "MEM", "NOP", "NOP", "NOP", "NOP", "NOP", "NOP", "NOP")

wc_win_total_vector <- c(46.5, 46.5, 46.5, 46.5, 46.5, 46.5, 46.5, 45.5, 45.5, 45.5, 45.5, 45.5, 45.5, 45.5)

wc_Name <- c("Charles", "Ben", "Nick", "John", "Chase", "Jason", "Zach", "Charles", "Ben", "Nick", "John", "Chase", "Jason", "Zach")

wc_O_U_vector <- c('U', 'O', 'U', 'U', 'O', 'U', 'O', 'U', 'U', 'U', 'U', 'U', 'U', 'U')

# Create the data frame
wc_picks <- data.frame(wc_team, wc_proj_win_total = wc_win_total_vector, name = wc_Name, wc_pick = wc_O_U_vector)



#bring in schedule data
website <- read_html('https://www.basketball-reference.com/leagues/NBA_2025_standings.html')

tables <- html_table(website)

east <- tables[[1]] %>%
  mutate(team = case_when(substr(`Eastern Conference`, 1, 3) == "Bos" ~ 'BOS',
                          substr(`Eastern Conference`, 1, 3) == "Phi" ~ 'PHI',
                          substr(`Eastern Conference`, 1, 3) == "Mil" ~ 'MIL',
                          substr(`Eastern Conference`, 1, 3) == "Mia" ~ 'MIA',
                          substr(`Eastern Conference`, 1, 3) == "Orl" ~ 'ORL',
                          substr(`Eastern Conference`, 1, 3) == "Ind" ~ 'IND',
                          substr(`Eastern Conference`, 1, 3) == "New" ~ 'NYK',
                          substr(`Eastern Conference`, 1, 3) == "Cle" ~ 'CLE',
                          substr(`Eastern Conference`, 1, 3) == "Atl" ~ 'ATL',
                          substr(`Eastern Conference`, 1, 3) == "Bro" ~ 'BKN',
                          substr(`Eastern Conference`, 1, 3) == "Tor" ~ 'TOR',
                          substr(`Eastern Conference`, 1, 3) == "Chi" ~ 'CHI',
                          substr(`Eastern Conference`, 1, 3) == "Cha" ~ 'CHA',
                          substr(`Eastern Conference`, 1, 3) == "Was" ~ 'WAS',
                          substr(`Eastern Conference`, 1, 3) == "Det" ~ 'DET')) %>%
  dplyr::select(-`Eastern Conference`)

west <- tables [[2]] %>%
  mutate(team = case_when(substr(`Western Conference`, 1, 3) == "Min" ~ 'MIN',
                          substr(`Western Conference`, 1, 3) == "Den" ~ 'DN',
                          substr(`Western Conference`, 1, 3) == "Okl" ~ 'OKC',
                          substr(`Western Conference`, 1, 3) == "Dal" ~ 'DAL',
                          substr(`Western Conference`, 1, 3) == "Sac" ~ 'SAC',
                          substr(`Western Conference`, 13, 18) == "Lakers" ~ 'LAL',
                          substr(`Western Conference`, 1, 3) == "Pho" ~ 'PHX',
                          substr(`Western Conference`, 1, 3) == "Hou" ~ 'HOU',
                          substr(`Western Conference`, 1, 3) == "New" ~ 'NOP',
                          substr(`Western Conference`, 1, 3) == "Gol" ~ 'GSW',
                          substr(`Western Conference`, 13, 20) == "Clippers" ~ 'LAC',
                          substr(`Western Conference`, 1, 3) == "Uta" ~ 'UTA',
                          substr(`Western Conference`, 1, 3) == "Mem" ~ 'MEM',
                          substr(`Western Conference`, 1, 3) == "Por" ~ 'POR',
                          substr(`Western Conference`, 1, 3) == "San" ~ 'SAS')) %>%
  dplyr::select(-`Western Conference`)

standings <- union(east, west)

wc_data <- wc_picks %>%
  left_join(standings, by = c('wc_team' = 'team')) %>%
  mutate(wins = as.numeric(W),
         losses = as.numeric(L)) %>%
  mutate(games_remaining = 82 - wins - losses,
         preseason_exp_win_pct = wc_proj_win_total / 82,
         current_win_pct = wins / (wins + losses),
         games_played = wins + losses) %>%
  mutate(current_win_pct = ifelse(is.na(current_win_pct) == TRUE, 0, current_win_pct)) %>%
  mutate(exp_win_pct = ((games_remaining/82)*preseason_exp_win_pct) + 
           ((games_played/82) * current_win_pct)) %>%
  mutate(exp_wins = wins + ((games_remaining * exp_win_pct))) %>%
  mutate(pick_success = ifelse(wc_pick == 'O', exp_wins-wc_proj_win_total, wc_proj_win_total-exp_wins)) %>%
  mutate(pred_win = ifelse(pick_success > 0, 1, 0),
         pred_loss = ifelse(pick_success < 0, 1, 0),
         pred_push = ifelse(pick_success == 0, 1, 0)) %>%
  group_by(name) %>%
  summarize(wc_pred_wins = sum(pred_win),
            wc_pred_loss = sum(pred_loss))

#join data
combined_data <- all_picks %>%
  dplyr::left_join(standings, by = 'team') %>%
  mutate(wins = as.numeric(W),
         losses = as.numeric(L)) %>%
  mutate(games_remaining = 82 - wins - losses,
         preseason_exp_win_pct = proj_win_total / 82,
         current_win_pct = wins / (wins + losses),
         games_played = wins + losses) %>%
  mutate(current_win_pct = ifelse(is.na(current_win_pct) == TRUE, 0, current_win_pct)) %>%
  mutate(exp_win_pct = ((games_remaining/82)*preseason_exp_win_pct) + 
           ((games_played/82) * current_win_pct)) %>%
  mutate(exp_wins = wins + ((games_remaining * exp_win_pct))) %>%
  mutate(pick_success = ifelse(pick == 'O', exp_wins-proj_win_total, proj_win_total-exp_wins)) %>%
  mutate(pred_win = ifelse(pick_success > 0, 1, 0),
         pred_loss = ifelse(pick_success < 0, 1, 0),
         pred_push = ifelse(pick_success == 0, 1, 0)) %>%
  mutate(clinched_over_w = ifelse(pick == 'O' & wins > proj_win_total, 1, 0),
         clinched_over_l = ifelse(pick == 'O' & losses > 82 - proj_win_total, 1, 0),
         clinched_under_w = ifelse(pick == 'U' & losses > 82 - proj_win_total, 1, 0),
         clinched_under_l = ifelse(pick == 'U' & wins > proj_win_total, 1, 0)) %>%
  mutate(clinched_w = ifelse(clinched_over_w == 1 | clinched_under_w == 1,1, 0),
         clinched_l = ifelse(clinched_over_l == 1 | clinched_under_l == 1,1, 0))


summary <- combined_data %>%
  group_by(name) %>%
  summarize(pred_wins = sum(pred_win),
            pred_losses = sum(pred_loss),
            pred_pushes = sum(pred_push),
            clinched_wins = sum(clinched_w),
            clinched_losses = sum(clinched_l),
            strength_of_record = sum(pick_success)) %>%
  ungroup() %>%
  left_join(wc_data, by = 'name') %>%
  dplyr::select(name, pred_wins, pred_losses, pred_pushes, wc_pred_wins, strength_of_record) %>%
  arrange(desc(pred_wins), pred_losses, desc(wc_pred_wins), desc(strength_of_record)) %>%
  gt() %>%
  gt_theme_538() %>%
  gt_color_rows(strength_of_record,
                palette = c("red", "white", "green")) %>%
  tab_header(title = 'Predicted End of Year Standings')

details <- combined_data %>%
  dplyr::select(name, team, proj_win_total, preseason_exp_win_pct, pick, wins, losses, 
                current_win_pct, games_remaining, exp_win_pct, exp_wins, clinched_w, clinched_l, pick_success) %>%
  arrange(desc(pick_success)) %>%
  gt() %>%
  gt_theme_538() %>%
  gt_color_rows(pick_success,
                palette = c("red", "white", "green")) %>%
  tab_header(title = 'Pick Breakdown')


# Define UI for application
ui <- fluidPage(
  #convert to Vertical Layout
  verticalLayout(
    # Application title
    gt_output("summary_table"),
    br(),
    gt_output("details_table")
    
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$summary_table <- render_gt({
    summary
  })
  
  output$details_table <- render_gt({
    details
  })
}

# Run the application 
shinyApp(ui = ui, server = server)