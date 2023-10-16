# Loadings ----
library(data.table) # Rich data table
library(dplyr) # Rich Data frame manipulation
library(gt) # Rich table output
library(kableExtra) # Export tables
library(tidyverse) # Rich data manipulation
library(googlesheets4) # Google Spreadsheets

# Import the data ----

# Get constants
email <- Sys.getenv("MYEMAIL")
ssid_students <- Sys.getenv("SSID_STUDENTS_LIST")
ssid_group.info <- Sys.getenv("SSID_GROUP_INFO")
ssid_responses <- Sys.getenv("SSID_FORM_RESPONSES")

# Authentication
gs4_auth(email = Sys.getenv("MYEMAIL"))

# Read sheets
( sht.names_students <- sheet_names(ssid_students))
sht_students <- read_sheet(ssid_students, "NAME_LIST_SHEET")
sht.studyAbroadings <- read_sheet(ssid_students, "STUDENTS_INFO_LIST")
( sht.names_group.info <- sheet_names(ssid_group.info) )
sht_group.info <- read_sheet(ssid_group.info, "GROUPING_INFO")
sht_questions <- read_sheet(ssid_group.info, 'QUESTIONS_LIST')
( sht.names_responses <- sheet_names(ssid_responses) )
sht_reflections <- read_sheet(ssid_responses, "FORM_RESPONSES")


# Shape the data ----

## Shape the main data ----

# Merge the data
data.raw <- left_join(sht_students, sht_group.info) |>
  left_join(sht_reflections)

# Extract the columns to use
data.picked <- data.raw |>
  dplyr::select(
    name = 名前,
    class = クラス,
    group = サイクル１,
    current.interest = 問い,
    members.synergy = メンバー,
    groupwork.process = 進め方,
    discussion.diversity = 多様性,
    total.score = 総合点,
    praising.others = メンバーを褒める,
    praising.oneself = 自分を褒める,
    problems =グループの改善点,
    change.desire = シャッフル要望,
    interesting.questions = 気になる問い
  )

# Shape the data
data <- data.picked |>

  # Extract not-perticipated students
  mutate(group = unlist(group)) |>
  filter(!group %in% c("expired", "abroadOut")) |> #

  # Add a column to check answer or not
  mutate(is.answered = ifelse(is.na(current.interest), FALSE, TRUE), .before = name) |>

  # Shape the data types:  class and group to factor, numeric values to integer, shufflable to ordered.factor
  mutate(
    class = factor(class, ordered = F),
    group = factor(group, levels = seq(min(as.numeric(group)), max(as.numeric(group)), 1)),
    across(where(is.numeric), function(x) ordered(x, levels = 1:5)),
    change.desire = factor(change.desire, levels = c("別の問いにしたい", "なんでもいい", "今の問いでいい"), ordered = F)
  )

data.answered <- data |>
  filter(is.answered == TRUE) |>
  dplyr::select(!is.answered)

data.quantity <- data.answered |>
  dplyr::select(!where(is.character))

## Shape the questions data ----
data.selected.questions <- data.answered |>
  dplyr::select(name, group, change.desire, interesting.questions) |>
  separate_rows(interesting.questions, sep =", ")
data.questions <- sht_questions |>
  mutate(
    q.group = factor(グループ, levels = seq(min(as.numeric(グループ)), max(as.numeric(グループ)))),
    question = サイクル１_発表,
    .keep = "none"
  )

data.interests <- left_join(data.selected.questions, data.questions, by = join_by(interesting.questions == question))

# Grouping ----
## STRATEGY ##
## 1. Keep groups where at least one person want to remain
## 2. Stay students who are not want to be shuffled
## 3. Distribute students who want to move
## 4. Distribute students who have not answered

# 1. Settle which groups to keep ----

# Groups to keep
tbl.selections <- table(data |> dplyr::select(group, change.desire))
df.selections <- data.frame(tbl.selections)
groups.toKeep <- df.selections |>
  filter(change.desire == "今の問いでいい", Freq > 0) |>
  select(group)
(n.groups <-  nrow(groups.toKeep) )
nmax.perGroup <- ceiling(100 / n.groups )

# Visualize groups to keep
cols <- c("#ffdddd", "#ffcccc", "#11aaff")
ggplot(df.selections, aes(x = group, y = Freq, fill = change.desire)) +
  geom_bar(stat = "identity", position = "fill", alpha = .9) +
  geom_hline(yintercept = .5, col = "grey", alpha = .7, show.legend = F) +
  scale_x_discrete(limits = rev(levels(df.selections$group))) +
  scale_fill_manual(values = cols) +
  coord_flip() +
  labs(
    title = "グループごとのシャッフル要望",
    subtitle = paste0("作成日：", format.Date(Sys.Date(), "%Y年%m月%d日")),
    caption = paste0("個人フィードバックフォーム回答より集計（n=", nrow(data |> filter(is.answered == TRUE)), "）"),
    x = "グループ",
    y = element_blank(),
    fill = element_blank()
  ) +
  theme_minimal() +
  theme(
    text = element_text(colour = "#333333"),
    plot.title = element_text(hjust = .5, face = "bold"),
    plot.subtitle = element_text(hjust = .5),
    axis.text.y = element_text(face = "bold", colour = ifelse(rev(df.selections$group) %in% groups.toKeep$group, cols[3], cols[1])),
    legend.position = "top"
  )

# 2. Keep who want to stay ----

## 今の問いでいい: stay the same group ----

# Distribute
term2 <- data |>
  mutate(サイクル2 = ifelse(change.desire == "今の問いでいい", group, NA)) |>
  select(名前 = name, サイクル1 = group, サイクル2, シャッフル要望 = change.desire)

# Check
( data.table(term2) )
( term2 |> group_by(サイクル2) |> summarise(n = n()))

## なんでもいい: if group exists, then stay, else move ----

# Distribute
for(student in term2 |> filter(シャッフル要望 == "なんでもいい") |> select(名前) |> as.vector() |> unlist() ){

  groupings <- term2 |>
    group_by(サイクル2) |>
    summarise(n = n())
  groups.remains <- groupings |>
    filter(サイクル2 %in% groups.toKeep$group, n <= nmax.perGroup)

  group.this <- term2$サイクル1[term2$名前 == student]
  if( group.this %in% groups.toKeep$group) {
    term2$サイクル2[term2$名前 == student] <- group.this

  }else{
    for(g.toMove in data.interests$q.group[data.interests$name == student]) {
      if(g.toMove %in% groups.remains$サイクル2){
        term2$サイクル2[term2$名前 == student] <- g.toMove
      }
    }
  }
}

# Check
( data.table(term2) )
( term2 |> group_by(サイクル2) |> summarise(n = n()))

## 別の問いにしたい: if group exists, move ----

# Distribute
for(student in term2 |> filter(シャッフル要望 == "別の問いにしたい") |> select(名前) |> as.vector() |> unlist() ){

  groupings <<- term2 |>
    group_by(サイクル2) |>
    summarise(n = n())
  groups.remains <<- groupings |>
    filter(サイクル2 %in% groups.toKeep$group, n < nmax.perGroup)

  for(g.toMove in data.interests$q.group[data.interests$name == student]) {
    if(g.toMove %in% groups.remains$サイクル2){
      term2$サイクル2[term2$名前 == student] <- g.toMove
    }
  }
}

# Check
( data.table(term2) )
( term2 |> group_by(サイクル2) |> summarise(n = n()))

## 未回答: distribute to the lowest ----

# Distribute
for(student in term2 |> filter(is.na(サイクル2)) |> select(名前) |> as.vector() |> unlist() ) {

  groupings <<- term2 |>
    group_by(サイクル2) |>
    summarise(n = n()) |>
    filter(!is.na(サイクル2)) |>
    arrange(n)
  term2$サイクル2[term2$名前 == student] <- groupings$サイクル2[1]
}

# Check
( data.table(term2) )
( term2 |> group_by(サイクル2) |> summarise(n = n()))

# Complete check ----
term2.df <- term2 |>
  mutate(
    シャッフル要望 = ifelse( is.na(シャッフル要望), "未回答", as.character(シャッフル要望) ) |>
             factor(levels = c("未回答", "別の問いにしたい", "なんでもいい", "今の問いでいい") ),
    サイクル2 = as.numeric(サイクル2)
  ) |>
  arrange(サイクル2) |>
  mutate(
    サイクル2 = factor(サイクル2),
    グループ変更 = ifelse(as.character(サイクル1) == as.character(サイクル2), "無", "有") |> factor(levels = c("有", "無"), ordered = F)
  )

( data.table(term2.df) )
(term2.tbl <-  table(term2.df |> select(サイクル2, グループ変更) ) )
term2.shuffled <- term2.tbl |>
  prop.table(margin = 1) |>
  as.data.frame() |>
  filter(グループ変更 == "有")

# Visualize
cols2 <- c("#11aaff", "#aaaa11")
ggplot(term2.df, aes(x = サイクル2, fill = グループ変更)) +
  geom_bar(stat = "count", position = "fill", alpha = .9) +
  geom_hline(yintercept = .5, col = "grey", alpha = .7, show.legend = F) +
  scale_x_discrete(limits = rev(levels(term2.df$サイクル2))) +
  scale_fill_manual(values = cols2) +
  coord_flip() +
  labs(
    title = "サイクル2各グループのシャッフル割合",
    subtitle = paste0("作成日：", format.Date(Sys.Date(), "%Y年%m月%d日")),
    caption = paste0("個人フィードバックフォーム回答より振り分け（n=", nrow(data |> filter(is.answered == TRUE)), "）"),
    x = "グループ",
    y = element_blank()
  ) +
  theme_minimal() +
  theme(
    text = element_text(colour = "#333333"),
    plot.title = element_text(hjust = .5, face = "bold"),
    plot.subtitle = element_text(hjust = .5),
    axis.text.y = element_text(face = "bold", colour = ifelse(rev(term2.shuffled$Freq) > .5 , cols2[1], cols2[2])),
    legend.position = "top"
  )

# Export the results ----
term2.export <- right_join( sht_students |> select(名前, 省略名_かな漢字), term2.df)
write_sheet(term2.export,　ssid_group.info, "グループ分け_〜サイクル２")
