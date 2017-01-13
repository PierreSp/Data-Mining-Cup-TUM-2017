# Import data
df <- read.csv("DMC1/training_yzCDTHz.csv")

# first summary:

summary(df)

# Observerations:
# Outcome many NA

# Create new field duration (is duration of the phonecall) Bucketize?
as.difftime(c(df$CallStart[1], df$CallEnd[1]), units = "mins")
