# Loading ----

# Data handling
library(scales) # Scaling the data
library(tidyverse) # Rich data manipulation

# Data frame handling
library(dplyr) # Data frame handling

# Plotting
library(ggplot2) # Rich visualization
library(ggdendro) # Rich dendrogram
library(igraph) # Ngram graphics
library(plotly) # Interactive plotting

# Google
library(googlesheets4) # Connect with google sheets

# Table
library(data.table) # Rich data table

# Morphologic
library(RMeCab) # Morphological Analysis

# Cosine Similarity
library(lsa) # Cosine function

# Tree clustering
library(stats)

# Constants ----

# Gppgle
SSID_SURVEY <- Sys.getenv("SSID_SURVEY_RESPONSES")
SSID_GROUPS <- Sys.getenv("SSID_OUTPUT")
SHTNAME_FORM <- "form"
SHTNAME_STUDENTS <- "respondees"
SHTNAME_THEMES <- "themes"
SHTNAME_DD1_SURVEY <- "anothersurvey"

# Grouping
MEMBERS_PER_GROUP <- 4
STRICT <- FALSE # How to deal with the exceeded members
## i.e.1. when 10 names and members per group given as 3, if STRICT is FALSE, the last group goes as 4 members, and if TRUE, the last group has only 1 member.
## i.e.2 Calculating the strict with round() function.

# Import the data ----

# Get the data
sheet.form.raw <- read_sheet(SSID_SURVEY, SHTNAME_FORM)
sheet.students <- read_sheet(SSID_SURVEY, SHTNAME_STUDENTS)
sheet.dd1.raw <- read_sheet(SSID_SURVEY, SHTNAME_DD1_SURVEY)

# # Join the data
sheet.form <- left_join( sheet.students, sheet.form.raw |> select(!タイムスタンプ))
sheet.dd1 <- left_join( sheet.students, sheet.dd1.raw |> select(!c(タイムスタンプ, 名前)))

# Get the short_names
shortNames <- unique(sheet.students$省略名_かな漢字)
numberOfResponses <- length(sheet.form$タイムスタンプ)


# Calculate cosine similarity ----

## Absentables ----
df.absentable <- sheet.students |>
  mutate(
    absentable = ifelse(名前 %in% ABSENTABLES, TRUE, FALSE)
  ) |>
  select(name = 省略名_かな漢字, absentable)

## Gender ----
df.gender <- sheet.students |>
  select(name = "省略名_かな漢字", gender = 性別) |>
  mutate(gender = as.factor(gender))
cs.gender <- df.gender |>
  table() |>
  t() |>
  cosine()


## Rolls ----
df.roll <- sheet.form |>
  select(name =省略名_かな漢字, roll = 自分の特性) |>
  mutate(
    roll = stringi::stri_match_first_regex(roll, "(.*?)：")[,2]
  ) |>
  mutate(
    roll = ifelse(is.na(roll), "なんでも屋", roll)
  )
df.roll |> table() |> colSums()

cs.roll <- df.roll |>
  table() |>
  t() |>
  cosine()

## Preference  ----
df.pref <- sheet.form |>
  select(name = 省略名_かな漢字, preference = 好きな進め方) |>
  separate_rows(preference, sep = ",") |>
  mutate(
    preference = stringi::stri_match_first_regex(preference, "(.*?)：")[,2]
  )
cs.pref <- df.pref |>
  table() |>
  t() |>
  cosine()
cs.pref[is.nan(cs.pref)] <- 0


## Member ----
df.member <- sheet.form |>
  select(name = 省略名_かな漢字, member = 一緒にやりたい人)
cs.member <- df.member |>
  table() |>
  t() |>
  cosine()
cs.member[is.nan(cs.member)] <- 0

## Themes ----
df.theme.raw <- left_join(sheet.form, sheet.dd1) |>
  select(name = "省略名_かな漢字" , themes = やりたいテーマ, like1 = `好きなものは何ですか？（1つ目）`, like2 = `好きなものは何ですか？（2つ目）`, like3 = `好きなものは何ですか？（3つ目）`) |>
  mutate(
    themes = paste(themes, like1, like2, like3, sep = "、")
  ) |>
  select(name, themes)

# Extra data shapings
df.theme.raw$themes <- gsub("・", "", as.character(df.theme.raw$themes))
df.theme.raw$themes <- gsub("，", "", as.character(df.theme.raw$themes))
df.theme.raw$themes <- gsub(",", "", as.character(df.theme.raw$themes))
df.theme.raw$themes <- gsub("）", "", as.character(df.theme.raw$themes))
df.theme.raw$themes <- gsub(")", "", as.character(df.theme.raw$themes))
df.theme <- df.theme.raw |>
  separate_rows(themes,  sep = "、") |>
  separate_rows(themes,  sep = "\\n") |>
  separate_rows(themes,  sep = "（") |>
  separate_rows(themes,  sep = "\\(") |>
  separate_rows(themes, sep = "　") |>
  separate_rows(themes, sep = " ") |>
  filter(themes != "") |>
  mutate(theme = themes, .keep = "unused") |>
  unique()

## Categorize with GPT-3
sheet.themes <- read_sheet(SSID_SURVEY, SHTNAME_THEMES)
if(!"category" %in% colnames(sheet.themes) ){
  write_sheet(df.theme, SSID_SURVEY, sheet = "themes")
  #### Categorize with ChatGPT:
  #### Prompt
  # #要求
  # 以下の回答を10個のカテゴリに分類して、以下の列を含む表を作成してください。
  # ・回答
  # ・カテゴリ名
  #
  # #回答群
  sheet.themes <- read_sheet(SSID_SURVEY, SHTNAME_THEMES)
}

# Calculation
df.category <- left_join(df.theme, sheet.themes)
cs.category <- df.category |>
  select(name, category) |>
  table() |>
  t() |>
  cosine()
cs.category[is.nan(cs.category)] <- 0

# Merge cosine tables ----

## Set hyper parameter
weight.category <- 5.0
weight.preference <- 3.0
weight.member <- 2.0
weight.gender <- 3.0
weight.roll <- 8.0 # Highly affectable
diversity.category <- 1.0 ## -1.0: More diversity or 1.0 less diversity *Not to be 0
diversity.perference <- 1.0
diversity.member <- 1.0
diversity.gender <- -1.0
diversity.roll <- -1.0

## Merge cosine tables
mtx.score.raw <- (1-cs.category)*weight.category*diversity.category  +
  (1-cs.pref)*weight.preference*diversity.perference +
  (1-cs.member)*weight.member*diversity.member +
  (1-cs.gender)*weight.gender*diversity.gender +
  (1-cs.roll)*weight.roll*diversity.roll
mtx.score <- mtx.score.raw - min(mtx.score.raw)
mtx.score <- rescale(mtx.score, to = c(0,1))

# Grouping ----

# Function
grouping_with_clusters <- function(nn.mtx, df.absent = df.absent, df.roll = df.roll,   members_per_group = 4, strict = FALSE){

  # Pre-processions
  names <- rownames(nn.mtx)
  numbersOfGroups <- ifelse(isTRUE(strict), ceiling( length(rownames(nn.mtx)) / members_per_group ), round( length(rownames(nn.mtx)) / members_per_group) )
  data.remains <-  nn.mtx # Temporary data, which show the remains to be dealt with
  data.fin <- data.frame() # Data frame where the completed values be stored
  names.remains <- names # The same above for names
  groupNum <- 0 # Stack the number of groups

  # Grouping loop
  while(length(names.remains) > 2){ # continue until no-name remains

    # Generate a cluster
    h.this <-  as.dist(data.remains) |> hclust()

    # Loop within the cluster
    no.groups.yet <- TRUE # Flag for while loop
    height.selected <- 1 # The number of the height in cluster
    while(no.groups.yet){ # Continue until any grouping has been made

      # Cut the tree with its height
      hcut.this <- cutree(h.this, h = h.this$height[height.selected]) # Cut with the lowest height

      # Grouping
      calc.this <- data.frame(
          name = names.remains
        ) |>
        mutate(
          cut = hcut.this[name]
        ) |>
        group_by(cut) |>
        mutate(
          n = n()
        ) |>
        filter(
          n >= members_per_group
        )

      # Check whether there is any conditions-filled group
      if(length(calc.this$name) == 0){
        height.selected <- height.selected + 1 # Check the next height cutting

      }else{

        # Pick up each cluster
        cuts.this <- unique(calc.this$cut)

        # Check ever cut
        for(cut.this in cuts.this){

          # Grouping within the cut
          grouping.this <- calc.this |>
            filter(
              cut == cut.this
            ) |>
            mutate(
              group = groupNum + ceiling(row_number()/members_per_group)
            ) |>
            group_by(group) |>
            mutate(
              groupMembers = n()
            ) |>
            filter(groupMembers >= members_per_group) |>
            select(name, group)

          # Push the data to the main
          data.fin <- bind_rows(data.fin, grouping.this)
          names.remains <- names.remains[!names.remains %in% grouping.this$name]
          data.remains <- data.remains[names.remains, names.remains]
          groupNum <- max(grouping.this$group)

          # Change the flag and stop looping
          no.groups.yet <- FALSE
        }
      }
    }
  }

  # Push the last remains to the data
  data.fin <- bind_rows(
    data.fin,
    data.frame(
      name = names.remains,
      group = rep(numbersOfGroups, length(names.remains))
    )
  )

  return(data.fin)
}

# Calculate
df.res <- grouping_with_clusters(mtx.score)

## Check about the absentables and leaders and if its has any issues, adjust the hyper parameter above
df.check <- left_join(df.res, df.absentable) |>
  left_join(df.roll) |>
  left_join(df.gender)
df.check |> filter(absentable == TRUE) # Not in the same group
df.check |> filter(roll == "リーダー") # Not in the same group
df.check$roll[duplicated(df.check |> select(group, roll))] |> unique() # No duplication in leader or subleader
df.check |> filter(gender == "male") |> select(group) |> table() |> max() # Should be less than or equal to 2

# Visualize ----

## Total score
h <- as.dist(mtx.score) |> hclust()
ggdendrogram(h, rotate = 0) +
  # coord_polar() +
  labs(
    title = "Distance Tree",
    caption = "Data from the survey"
  ) +
  theme(
    axis.text.x = element_text(
      colour = df.res$group,
      angle = 60,
      size = 7
    )
  )

# Export to Google sheets ----

## Pre-procession
df.pref.all <-  df.pref |>
  filter(!is.na(preference)) |>
  group_by(name) |>
  mutate(preferences = paste(preference, collapse = ", "), .keep = "unused") |>
  unique()
df.theme.all <- df.theme |>
  filter(!is.na(theme)) |>
  group_by(name) |>
  mutate(themes = paste(theme, collapse = ", "), .keep = "unused") |>
  unique()
df.all <- left_join(df.res, df.roll) |>
  left_join(df.pref.all) |>
  left_join(df.theme.all)

## Exporting
write_sheet(df.all, SSID_GROUPS, paste0("GROUPING_", format(Sys.Date(), "%Y%m")))

write_sheet(cs.category |> as.data.frame(row.names = rownames(cs.category), make.names = T) |> rownames_to_column(var = "name"), SSID_GROUPS, "CosineSimilarity_themes")
write_sheet(cs.pref |> as.data.frame(row.names = rownames(cs.pref), make.names = T) |> rownames_to_column(var = "name"), SSID_GROUPS, "CosineSimilarity_preferences")
