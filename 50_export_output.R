# Export output as RDS files

saveRDS(df_female_us_teams, file = "final_output/female_results.RDS")
saveRDS(df_female_us_teams, file = "gymnastics_app/data/female_results.RDS")
write_csv2(df_female_us_teams, file = "final_output/female_results.csv")
write_csv2(df_female_us_teams, file = "gymnastics_app/data/female_results.csv")
saveRDS(out_female_medal_winners, file = "gymnastics_app/data/female_medals.RDS")

saveRDS(df_male_us_teams, file = "final_output/male_results.RDS")
saveRDS(df_male_us_teams, file = "gymnastics_app/data/male_results.RDS")
write_csv2(df_male_us_teams, file = "final_output/male_results.csv")
write_csv2(df_male_us_teams, file = "gymnastics_app/data/male_results.csv")
saveRDS(out_male_medal_winners, file = "gymnastics_app/data/male_medals.RDS")
