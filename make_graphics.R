source("~/setup_elliott.R")

# Demographics of elected officials -------

# Number of House D vs R women over time
# Number of Senate D vs R women over time
# Number of House D vs R white men over time
# Number of Senate D vs R white men over time

# Control over state legislature & governorships -------
library(dtplyr)
df <- fread('data/clean_ncsl_data.csv')
colnames(df) <- colnames(df) %>% tolower %>% gsub(' ', '_', .) %>% gsub('\\.', '', .)

df <- df %>% 
  mutate(total_house = as.numeric(total_house), 
         total_senate = as.numeric(total_senate),
         senate_dem = as.numeric(senate_dem),
         senate_rep = as.numeric(senate_rep)) %>% 
  mutate(house_other = total_house - house_dem - house_rep) %>% 
  mutate(senate_other = total_senate - senate_dem - senate_rep)

yearly.seat.totals <- df %>% 
  select(year, state, matches('house'), matches('senate')) %>% 
  melt(c('year', 'state')) %>% 
  .[, sum(value, na.rm=T), by=.(year, variable)] %>% 
  dcast(year ~ variable, value.var='V1') %>% 
  mutate(pct_house_dem = house_dem / total_house * 100,
         pct_senate_dem = senate_dem / total_senate * 100,
         pct_all_seats_dem = (house_dem + senate_dem) / (total_house + total_senate) * 100)


# compute state control ---------------------------------------------------
df <- df %>% 
  mutate(legis_control = gsub('\\*', '', legis_control),
         state_control = gsub('\\*', '', state_control)) %>% 
  mutate(legis_control = ifelse(legis_control=='Divided', 'Split', legis_control),
         state_control = ifelse(state_control=='Divided', 'Split', state_control))

# graphs ------------------------------------------------------------------
yearly.seat.totals %>% 
  select(year, house_dem, senate_dem) %>% 
  melt('year') %>% 
  ggplot(aes(year, value, colour=variable)) +
  # facet_wrap(~variable, scales='free') +
  geom_point() +
  geom_line() +
  expand_limits(y=0) +
  geom_vline(xintercept = 2010, colour='red', linetype=2) +
  labs(x='', y='Total number of seats', title='Total state legislative seats held by Democrats over time')

yearly.seat.totals %>% 
  select(year, matches('pct')) %>% 
  select(-pct_all_seats_dem) %>% 
  melt('year') %>% 
  ggplot(aes(year, value, colour=variable)) +
  # facet_wrap(~variable, scales='free') +
  geom_point() +
  geom_line() +
  expand_limits(y=0) +
  geom_vline(xintercept = 2010, colour='red', linetype=2) +
  labs(x='', y='Share of seats, %', title='Democratic share of state legislative seats, by chamber')

d1 <- df[year>2003][state!='Nebraska'][, .N, by = .(year, legis_control)] %>% 
  dplyr::rename(control = legis_control,
         N_leg = N)
d2 <- df[year>2003][state!='Nebraska'][, .N, by = .(year, state_control)] %>% 
  dplyr::rename(control = state_control,
         N_state = N)
state_control <- merge(d1, d2, by=c('year', 'control'))
state_control %>% 
  dplyr::rename(`Control of legislature` = N_leg, `Control of states` = N_state) %>% 
  melt(c('year', 'control')) %>% 
  ggplot(aes(year, value, colour=control)) +
  facet_wrap(~variable) +
  geom_point() +
  geom_line() +
  expand_limits(y=0) +
  geom_vline(xintercept = 2010, colour='red', linetype=2) +
  labs(x='', y='Number of states held')

# Control of state House
# Control of state Senate
# Control of governorships

# House scatters ------------------
# Change in House vote x pres vote
# Change in House vote x education
# Change in House vote x annual income
# Change in House vote x working-class whites
# Change in House vote x nonwhite %
# Change in votes cast x pres vote 
# Change in votes cast x education
# Change in votes cast x annual income
# Change in votes cast x working-class whites
# Change in votes cast x nonwhite %

# Predictions analysis -------------
# Error x Trump v Clinton
# Error x college v noncolled
# etc

# Polarization ------------------

# DIME scores for winning House candidates
# DIME scores for winning Senate candidates


# Gerrymandering ----------------
# Efficiency gap by state