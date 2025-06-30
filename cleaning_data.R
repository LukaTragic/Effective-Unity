library(tidyverse)


voteview_data <- read.csv("data/VV_H111-118_DW.csv")
cel_data <- read.csv("data/CEL_H111-118_LES.csv")

territories = c("AS", "DC", "GU", "MP", "PR", "VI")
house_ideology <- voteview_data %>% filter(chamber == 'House', !state_abbrev %in% territories)
house_effectiveness <- cel_data %>% filter(!Two.letter.state.code %in% territories)


house_effectiveness <- house_effectiveness %>%
  rename(
    icpsr = `ICPSR.number..according.to.Poole.and.Rosenthal`,
    congress = `Congress.number`
  )

merged_house <- inner_join(
  house_ideology,
  house_effectiveness,
  by = c('icpsr', 'congress')
)

colnames(merged_house)
features <- c("congress", "chamber", "icpsr", "district_code", "state_abbrev", "party_code",
              "bioname", "bioguide_id", "nominate_dim1", "nominate_dim2", 
              "nokken_poole_dim1", "nokken_poole_dim2", "Year.first.elected.to.House",
              "Percent.vote.received.to.enter.this.Congress", "Size.of.House.delegation.from.member.s.state",
              "X1...majority.party.member", "Seniority..number.of.terms.served.counting.current",
              "Number.of.members.in.Party..0...Indep.", "LES.Classic.Rank.within.Party",
              "LES.1.0", "Benchmark.score..Classic..based.on.majority..seniority..chairs", "LES.benchmark..Classic.",
              "X1...Below..2...Meets..3...Above..Based.on.Classic.LES")


filtered_houses <- select(merged_house, features)



filtered_houses <- filtered_houses %>%
  rename(
    district = `district_code`,
    state = `state_abbrev`,
    party = `party_code`,
    DW_nominate_dim1 = `nominate_dim1`,
    DW_nominate_dim2 = `nominate_dim2`,
    year_first_elected = `Year.first.elected.to.House`,
    victory_margin = `Percent.vote.received.to.enter.this.Congress`,
    size_state_delegation = `Size.of.House.delegation.from.member.s.state`,
    majority_party_member = `X1...majority.party.member`,
    num_terms_served = `Seniority..number.of.terms.served.counting.current`,
    num_party_members = `Number.of.members.in.Party..0...Indep.`,
    LES_rank_within_party = `LES.Classic.Rank.within.Party`,
    LES_1.0 = `LES.1.0`,
    benchmark_score = `Benchmark.score..Classic..based.on.majority..seniority..chairs`,
    score = `LES.benchmark..Classic.`,
    score_rank = `X1...Below..2...Meets..3...Above..Based.on.Classic.LES`
  )


filtered_houses <- filtered_houses %>%
  select(
    # Identity
    congress, chamber, icpsr, bioname, bioguide_id,
    # State & District
    state, district, size_state_delegation,
    # Party Info
    party, majority_party_member, num_party_members,
    # Ideology
    DW_nominate_dim1, DW_nominate_dim2, nokken_poole_dim1, nokken_poole_dim2,
    # Career
    year_first_elected, num_terms_served, victory_margin,
    # Legislative Effectiveness
    LES_1.0, LES_rank_within_party, benchmark_score, score, score_rank
  )


majority_by_congress <- filtered_houses %>%
  group_by(congress, party) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(congress) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  mutate(
    house_majority_party = case_when(
      party == 100 ~ "Democratic",
      party == 200 ~ "Republican",
      TRUE ~ "Other"
    )
  ) %>%
  select(congress, house_majority_party)

merged_house <- filtered_houses %>%
  left_join(majority_by_congress, by = "congress")
