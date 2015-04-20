df = read.csv('bau_year_count_staff.csv')
df$Count = df$Count /  10

write.csv(df, 'bau_year_count_staff.csv')

