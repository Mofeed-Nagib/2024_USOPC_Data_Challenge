# Export output as RDS files

saveRDS(df_female_us_teams, file = "Case_Study_1-Gymnastics/final_output/female_results.RDS")
write_csv2(df_female_us_teams, file = "Case_Study_1-Gymnastics/final_output/female_results.csv")
saveRDS(out_female_medal_winners, file = "Case_Study_1-Gymnastics/gymnastics_app/data/female_medals.RDS")

saveRDS(df_male_us_teams, file = "Case_Study_1-Gymnastics/final_output/male_results.RDS")
write_csv2(df_male_us_teams, file = "Case_Study_1-Gymnastics/final_output/male_results.csv")
