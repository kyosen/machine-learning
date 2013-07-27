# ■ ４章の概要
# ・複数の素性を組み合わせて順位付けを行うモデルを設計する。
#　 * ３章は素性は１つだけ。(あやしい単語が入っているか入っていないか)
# ・Google が提案する４種類のカテゴリのうち、ラベル素性以外を使用する。
# 　* 順位付けに１つだけのカテゴリを用いると偏りが出てしまう。例えば、MLから大量のメールがあるとソーシャル素性がでかくなる。
#
# ・(米田のコメント)今回はベイズの定理は関係ない??
# ・(米田のコメント)よくわからないのは、P.ix(はじめに) で「ここでは二値分類だけにとどまらず複数のタイプへの分類を行う」
# 　　　　　　　　　あたかも、多値分類がテーマかのように書いてあるのに、最終的な結果は優先度１、０の二値分類になっている。
#                個人的には多値分類は全く関係がないと思う。
#　　　　　　　　　 複数の素性を用いるモデルについて述べているだけな気がする。
#
# ■ 流れ
# Google が提案する４種類のカテゴリのうち、ラベル素性以外を使用する。
# (順位付けに１つだけのカテゴリを用いると偏りが出てしまう。例えば、MLから大量のメールがあるとソーシャル素性がでかくなる。)
# カテゴリと今回使うデータとの対応は下記
# 1.ソーシャル素性：送信者と受信者の関係の強さ
#　　− 特定の人物から送信されたメールの量(本来は送受信メールと返信の早さなども用いる。今回はデータが受信メールしかない)
# 2.スレッド素性：スレットとユーザーの関係
#　　− スレッドの活動量（活発さ)。活発なスレッドほど重要であると仮定
#　　− データは２つあり
#　　　 * スレッド内で活動的な送信者
#　　　 * スレッドの活動量は単位時間あたりに何通メールが来ているか
# 3.コンテンツ素性：受信者がメール上で行うことと高い相関を持つ最近の単語
#　　− ユーザーが受け取ったメールの本文や件名に共通の用語が多く含まれるかどうか。
#    　テキストマイニング技術を使用する
#　　− データは２つあり
#　　　 * 件名(スレッド名)に対するコンテンツ素性
#　　　 * 本文に体するコンテンツ素性
# NA.ラベル素性：ユーザがフィルタを用いてメールに適用したラベル
#　　− 今回のデータでは判別できないためなし
# 素性の値は順位付けする際に計算され、学習の為に一時的に保存される。

# Load libraries
library('tm')
library('ggplot2')
library('plyr')

# Set the global paths
data.path <- file.path("..", "03-Classification", "data")
easyham.path <- file.path(data.path, "easy_ham")

#-- 0. メールを構造化された矩形に変換する

# メール本文を取得
# Simply returns the full text of a given email message
msg.full <- function(path)
{
  con <- file(path, open = "rt", encoding = "latin1")
  msg <- readLines(con)
  close(con)
  return(msg)
}

# 送信者アドレスを取得
# Retuns the email address of the sender for a given
# email message
get.from <- function(msg.vec)
{
  # grepl は true, false を返す
  from <- msg.vec[grepl("From: ", msg.vec)]
  # [":<>]半角スペースで文字列を区切る
  from <- strsplit(from, '[":<> ]')[[1]]
  from <- from[which(from  != "" & from != " ")] # 空文字、半角スペース以外の要素を取り出す
  return(from[grepl("@", from)][1]) # @ がついている要素を返す
}

# メールの件名を取得
# Retuns the subject string for a given email message
get.subject <- function(msg.vec)
{
  subj <- msg.vec[grepl("Subject: ", msg.vec)]
  if(length(subj) > 0)
  {
    # [[1]] はベクトルの名前??
    # [1] が空白文字(Subject: が区切りになったため)、[2] に件名本体が入っている
    return(strsplit(subj, "Subject: ")[[1]][2])
  }
  else
  {
    return("")　# 件名が空の場合は空文字を返す
  }
}

# メールの本文を返す(３章で既出)
# Similar to the function from Chapter 3, this returns
# only the message body for a given email.
get.msg <- function(msg.vec)
{
  msg <- msg.vec[seq(which(msg.vec == "")[1] + 1, length(msg.vec), 1)]
  return(paste(msg, collapse = "\n"))
}

# メールの日時を取得
# Retuns the date a given email message was received
get.date <- function(msg.vec)
{
  date.grep <- grepl("^Date: ", msg.vec)
  date.grep <- which(date.grep == TRUE)
  date <- msg.vec[date.grep[1]]
  # Date: 行の内容を分割
  # ..."Data: 04 Dec 2002 11:49:23 +0000" という行は下記に分割される
  # ......"Date" "04 Dec 2002 11:49:23 " "0000"
  date <- strsplit(date, "\\+|\\-|: ")[[1]][2]
  # 連続する半角スペースを削除
  date <- gsub("^\\s+|\\s+$", "", date)
  # 付加情報部分を削除して返す
  # ...Date: の形式が "Date：Wed, 04 Dec 2002 11:36:32 GMT" の場合に " GMT" を削除する
  return(strtrim(date, 25))
}

# 日付、送信者、件名、本文、メールへのパス の行列を返す
# This function ties all of the above helper functions together.
# It returns a vector of data containing the feature set
# used to categorize data as priority or normal HAM
parse.email <- function(path)
{
  full.msg <- msg.full(path)
  date <- get.date(full.msg)
  from <- get.from(full.msg)
  subj <- get.subject(full.msg)
  msg <- get.msg(full.msg)
  return(c(date, from, subj, msg, path))
}

# easy_ham フォルダ以下のメールに対して (日付、送信者、件名、本文、メールパス)の行列を作成
# In this case we are not interested in classifiying SPAM or HAM, so we will take
# it as given that is is being performed.  As such, we will use the EASY HAM email
# to train and test our ranker.
easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
easyham.parse <- lapply(easyham.docs,
                        function(p) parse.email(file.path(easyham.path, p)))

# Convert raw data from list to data frame
ehparse.matrix <- do.call(rbind, easyham.parse) # easy_ham のマトリクスを連結
allparse.df <- data.frame(ehparse.matrix, stringsAsFactors = FALSE) # データフレームに変換
names(allparse.df) <- c("Date", "From.EMail", "Subject", "Message", "Path") # カラム名を設定

# 文字列を POSIX オブジェクトに変換する
# Convert date strings to POSIX for comparison. Because the emails data
# contain slightly different date format pattners we have to account for
# this by passining them as required partmeters of the function.
date.converter <- function(dates, pattern1, pattern2)
{
  # strptime：文字列を POSIX形式に変換する。変換できなかった場合 NA が帰って来る
  # ??　サンプルを書いてみたが、なぜか変換できず。。。
  pattern1.convert <- strptime(dates, pattern1)
  pattern2.convert <- strptime(dates, pattern2)
  # pattern1, pattern2 とで NA でない方を返す
  # ...↓右辺左辺、両方とも is.na(pattern"1".convert) であることがポイント
  pattern1.convert[is.na(pattern1.convert)] <- pattern2.convert[is.na(pattern1.convert)]
  return(pattern1.convert)
}

pattern1 <- "%a, %d %b %Y %H:%M:%S"
pattern2 <- "%d %b %Y %H:%M:%S"

allparse.df$Date <- date.converter(allparse.df$Date, pattern1, pattern2)

# 件名とアドレスを小文字に変換
# Convert emails and subjects to lower-case
allparse.df$Subject <- tolower(allparse.df$Subject)
allparse.df$From.EMail <- tolower(allparse.df$From.EMail)

# データを日付で昇順にソート
# ..."with(allparse.df, order(Date))" で、ソート後のインデックスのリストが帰って来る
# ......つまり、4 2 6 5 7 という配列に対して with と order を使うと
# ......2 1 4 3 5 という値が帰って来る
# ...最後の ", ]" を省くとエラーになる。なぜかは知らない。。。
# Order the messages chronologically
priority.df <- allparse.df[with(allparse.df, order(Date)), ]

# 最初の半分のみを訓練用に使用する。
# ...nrow：行数
# ...round: 丸め。小数点以下切り捨て
# ...1:n： 1番目から n 番目まで取り出す
# We will use the first half of the priority.df to train our priority in-box algorithm.
# Later, we will use the second half to test.
priority.train <- priority.df[1:(round(nrow(priority.df) / 2)), ]

#-- 1. ソーシャル素性を求める

# 電子メールの送信者毎に重み付け
# The first step is to create rank weightings for all of the features.
# We begin with the simpliest: who the email is from.

# ddply はデータフレームを操作する関数。
# ...第２引数：From.EMail 列でグループ化する。
# ...第３引数：summarise グループ化したデータを集計する
# ...第４引数：Freq という列を作成して集計結果を出力する。列の長さは Subject 列と同じ
# ...        (どの列も同じ長さなので、Subject でなくてもよい)
# Calculate the frequency of correspondence with all emailers in the training set
from.weight <- ddply(priority.train, .(From.EMail),
                     summarise, Freq = length(Subject))
# Freq の昇順にソート
from.weight <- from.weight[with(from.weight, order(Freq)), ]

# Freq が 6 より大きい行を取得
# We take a subset of the from.weight data frame to show our most frequent 
# correspondents.
from.ex <- subset(from.weight, Freq > 6)

# 表示
from.scales <- ggplot(from.ex) +
  geom_rect(aes(xmin = 1:nrow(from.ex) - 0.5,
                xmax = 1:nrow(from.ex) + 0.5,
                ymin = 0,
                ymax = Freq,
                fill = "lightgrey",
                color = "darkblue")) +
  scale_x_continuous(breaks = 1:nrow(from.ex), labels = from.ex$From.EMail) +
  coord_flip() +
  scale_fill_manual(values = c("lightgrey" = "lightgrey"), guide = "none") +
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  ylab("Number of Emails Received (truncated at 6)") +
  xlab("Sender Address") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 5, hjust = 1))
ggsave(plot = from.scales,
       filename = file.path("images", "0011_from_scales.pdf"),
       height = 4.8,
       width = 7)

# Log weight scheme, very simple but effective

# log は自然対数、log10は常用対数
# 自然対数の結果をWeight列に、常用対数の結果をlog10Weight列に
#...対数を取らないとソーシャル素性の値が大きくなりすぎてしまい、他の素性(スレッド、コンテンツ)が効かなくなってしまう
from.weight <- transform(from.weight,
                         Weight = log(Freq + 1),
                         log10Weight = log10(Freq + 1))

# 表示
from.rescaled <- ggplot(from.weight, aes(x = 1:nrow(from.weight))) +
  geom_line(aes(y = Weight, linetype = "ln")) +
  geom_line(aes(y = log10Weight, linetype = "log10")) +
  geom_line(aes(y = Freq, linetype = "Absolute")) +
  scale_linetype_manual(values = c("ln" = 1,
                                   "log10" = 2,
                                   "Absolute" = 3),
                        name = "Scaling") +
  xlab("") +
  ylab("Number of emails Receieved") +
  theme_bw() +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank())
ggsave(plot = from.rescaled,
       filename = file.path("images", "0012_from_rescaled.pdf"),
       height = 4.8,
       width = 7)

#-- 2. スレッド素性を求める
#--    ...２種類あり「スレッド内で活動的な送信者」「単位時間内でどれだけのメールが飛び交っているか」

# To calculate the rank priority of an email we should calculate some probability that 
# the user will respond to it.  In our case, we only have one-way communication data.
# In this case, we can calculate a weighting based on words in threads that have a lot
# of activity.

# スレッドの先頭のベクトルを作成
# This function is used to find threads within the data set.  The obvious approach
# here is to use the 're:' cue from the subject line to identify message threads.
find.threads <- function(email.df)
{
  # re: があると、response.threads の要素数が２になる。１つめは空文字
  response.threads <- strsplit(email.df$Subject, "re: ")
  # is.thread "re: "があるメールは TRUE が入るベクトル
  is.thread <- sapply(response.threads,
                      function(subj) ifelse(subj[1] == "", TRUE, FALSE))
  # re: があるメール(スレッドの先頭メール)のベクトルを作成
  threads <- response.threads[is.thread]
  # スレッドの先頭メールの送信者アドレスのベクトル作成
  senders <- email.df$From.EMail[is.thread]
  # 分割した件名を元に戻す
  # ...paste はベクトルを文字列として連結する。 collapse は連結文字
  # ...?? [2:length(t)] となっているが [1:length(t)] が正しいのでは?
  threads <- sapply(threads,
                    function(t) paste(t[2:length(t)], collapse = "re: "))
  # 送信者, スレッド のベクトルを作成
  return(cbind(senders,threads))
}

threads.matrix <- find.threads(priority.train)

# スレッド中で活動的な送信者に基づいて重み付けしたマトリクスを作成
# Using the matrix of threads generated by the find.threads function this function
# creates a data from of the sender's email, the frequency of emails from that
# sender, and a log-weight for that sender based on the freqeuncy of corresponence.
email.thread <- function(threads.matrix)
{
  senders <- threads.matrix[, 1] # thread.matrix[, 1] は送信者名
  # table は 列：送信者、行：同一送信者の数 の表
  senders.freq <- table(senders)
  # 送信者、同一送信者の数、←の自然対数
  senders.matrix <- cbind(names(senders.freq),
                          senders.freq,
                          log(senders.freq + 1))
  senders.df <- data.frame(senders.matrix, stringsAsFactors=FALSE)
  # 行に番号を付加
  row.names(senders.df) <- 1:nrow(senders.df)
  # 列名付加
  names(senders.df) <- c("From.EMail", "Freq", "Weight")
  senders.df$Freq <- as.numeric(senders.df$Freq)
  senders.df$Weight <- as.numeric(senders.df$Weight)
  return(senders.df)
}

senders.df <- email.thread(threads.matrix)

# 指定されたスレッドの活動量で重みを計算(単位時間内でどれだけのメールが飛び交っているか)
# As an additional weight, we can enhance our notion of a thread's importance
# by measuring the time between responses for a given email.  This function
# takes a given thread and the email.df data frame to generate a weighting 
# based on this activity level.  This function returns a vector of thread
# activity, the time span of a thread, and its log-weight.
thread.counts <- function(thread, email.df)
{
  # 引数のスレッド名と合致する件名のリストを作成
  # Need to check that we are not looking at the original message in a thread, 
  # so we check the subjects against the 're:' cue.
  thread.times <- email.df$Date[which(email.df$Subject == thread |
                                      email.df$Subject == paste("re:", thread))]
  freq <- length(thread.times) # スレッドの量を計算
  min.time <- min(thread.times) # スレッドの開始時刻
  max.time <- max(thread.times) # スレッドの終了時刻
  # スレッドの開始終了のタイムスパンを作成(単位：秒)
  time.span <- as.numeric(difftime(max.time, min.time, units = "secs"))
  if(freq < 2)
  {
    # スレッドの数が１以下なら NA
    return(c(NA, NA, NA))
  }
  else
  {
    # 1秒あたり何通メールのやりとりがあったか計算
    trans.weight <- freq / time.span
    # 常用対数で重み付け
    # ...trans.weight は１より小さい値になるので対数を取ると負の数になる
    # ...アフィン変換を使って解決する
    log.trans.weight <- 10 + log(trans.weight, base = 10)
    return(c(freq, time.span, log.trans.weight))
  }
}

# 全スレッドに対して重みを計算してマトリクスを作成
# This function uses the threads.counts function to generate a weights
# for all email threads.
get.threads <- function(threads.matrix, email.df)
{
  threads <- unique(threads.matrix[, 2]) # スレッドの一覧を重複を省いて作成
  thread.counts <- lapply(threads,
                          function(t) thread.counts(t, email.df))
  thread.matrix <- do.call(rbind, thread.counts)
  return(cbind(threads, thread.matrix))
}

# Now, we put all of these function to work to generate a training set
# based on our thread features.
thread.weights <- get.threads(threads.matrix, priority.train)
thread.weights <- data.frame(thread.weights, stringsAsFactors = FALSE)
names(thread.weights) <- c("Thread", "Freq", "Response", "Weight")
thread.weights$Freq <- as.numeric(thread.weights$Freq)
thread.weights$Response <- as.numeric(thread.weights$Response)
thread.weights$Weight <- as.numeric(thread.weights$Weight)
thread.weights <- subset(thread.weights, is.na(thread.weights$Freq) == FALSE)

#-- 3. コンテンツ素性を求める

# 全てのスレッド中に頻出する単語に対して重み付けをする
#...3章のcount.word 関数に似てる
#...rowSums(as.matrix(tdm) すると下記の結果が帰って来る
#...      単語A  単語B
#...       1     1    ← 行毎の Sum
# Similar to what we did in Chapter 3, we create a simple function to return a 
# vector of word counts.  This time, however, we keep the TDM as a free
# parameter of the function.
term.counts <- function(term.vec, control)
{
  vec.corpus <- Corpus(VectorSource(term.vec))
  vec.tdm <- TermDocumentMatrix(vec.corpus, control = control)
  return(rowSums(as.matrix(vec.tdm)))
}

# スレッド(の件名) の単語の学習データを作成
thread.terms <- term.counts(thread.weights$Thread,
                            control = list(stopwords = TRUE))
thread.terms <- names(thread.terms)

# thread.terms の語を１つずつ取り出してスレッド名を grep し、一致するスレッドの重みの平均を取る
term.weights <- sapply(thread.terms,
                       function(t) mean(thread.weights$Weight[grepl(t, thread.weights$Thread, fixed = TRUE)]))
term.weights <- data.frame(list(Term = names(term.weights),
                                Weight = term.weights),
                           stringsAsFactors = FALSE,
                           row.names = 1:length(term.weights))

# メール本文の単語の学習データを作成
# Finally, create weighting based on frequency of terms in email. 
# Will be similar to SPAM detection, but in this case weighting
# high words that are particularly HAMMMY.
msg.terms <- term.counts(priority.train$Message,
                         control = list(stopwords = TRUE,
                         removePunctuation = TRUE,
                         removeNumbers = TRUE))
msg.weights <- data.frame(list(Term = names(msg.terms),
                               Weight = log(msg.terms, base = 10)),
                          stringsAsFactors = FALSE,
                          row.names = 1:length(msg.terms))

# 重みが 0 以下のものは削除
# Remove words that have a zero weight
msg.weights <- subset(msg.weights, Weight > 0)

# 訓練データ(weight.df)を検索して重みを取得
# ...単語でもスレッドでもOK。
# ...スレッドの場合は第３引数を FALSE にする。
# This function uses our pre-calculated weight data frames to look up
# the appropriate weightt for a given search.term.  We use the 'term'
# parameter to dertermine if we are looking up a word in the weight.df
# for it message body weighting, or for its subject line weighting.
get.weights <- function(search.term, weight.df, term = TRUE)
{
  if(length(search.term) > 0)
  {
    if(term)
    {
      term.match <- match(names(search.term), weight.df$Term)
    }
    else
    {
      term.match <- match(search.term, weight.df$Thread)
    }
    match.weights <- weight.df$Weight[which(!is.na(term.match))]
    if(length(match.weights) < 1)
    {
      return(1)
    }
    else
    {
      return(mean(match.weights))
    }
  }
  else
  {
    return(1)
  }
}

#-- 4. これまでに作成した関数を使用して訓練データを作成

# 全素性の重みを考慮した順位を計算する
#...日付、送信者、件名、順位 が返る
# Our final step is to write a function that will assign a weight to each message based
# on all of our, we create a function that will assign a weight to each message based on
# the mean weighting across our entire feature set.
rank.message <- function(path)
{
  # msg はテストデータ
  msg <- parse.email(path)
  # Weighting based on message author

  # ソーシャル素性(送信者の出現頻度)
  # First is just on the total frequency
  from <- ifelse(length(which(from.weight$From.EMail == msg[2])) > 0, # msg[2] は送信者
                 from.weight$Weight[which(from.weight$From.EMail == msg[2])],
                 1)
  
  # スレッド素性(スレッド内の送信者の出現頻度)
  # Second is based on senders in threads, and threads themselves
  thread.from <- ifelse(length(which(senders.df$From.EMail == msg[2])) > 0,
                        senders.df$Weight[which(senders.df$From.EMail == msg[2])],
                        1)
  
  subj <- strsplit(tolower(msg[3]), "re: ")  # msg[3] は件名
  is.thread <- ifelse(subj[[1]][1] == "", TRUE, FALSE)
  
  # スレッド素性(スレッドの活動量の重みを取得)
  # (ぼやき) actigity は {} のスコープ外に定義しなくても、 {} の外側からアクセスできるのね。
  #         っというか、R のスコープの概念ってどうなってるんだっけ??
  if(is.thread)
  {
    activity <- get.weights(subj[[1]][2], thread.weights, term = FALSE)
  }
  else
  {
    activity <- 1
  }
  
  # Next, weight based on terms    

  # コンテンツ素性(スレッド名)
  # ...活動的なスレッドに含まれる単語による重み付け
  # Weight based on terms in threads
  thread.terms <- term.counts(msg[3], control = list(stopwords = TRUE))
  thread.terms.weights <- get.weights(thread.terms, term.weights)

  # コンテンツ素性(メール本文)
  # ...メール本文に出現する語に対して重み付けを計算
  # Weight based terms in all messages
  msg.terms <- term.counts(msg[4], # msg[4] は本文
                           control = list(stopwords = TRUE,
                           removePunctuation = TRUE,
                           removeNumbers = TRUE))
  msg.weights <- get.weights(msg.terms, msg.weights)
  
  # rank：順位。全部の重みを掛け合わせた値
  # Calculate rank by interacting all weights
  rank <- prod(from,
               thread.from,
               activity, 
               thread.terms.weights,
               msg.weights)
  
  return(c(msg[1], msg[2], msg[3], rank)) # 日付、送信者、件名、順位
}

# 訓練用のメールのパス、テスト用のメールのパス
# Find splits again
train.paths <- priority.df$Path[1:(round(nrow(priority.df) / 2))]
test.paths <- priority.df$Path[((round(nrow(priority.df) / 2)) + 1):nrow(priority.df)]

# 訓練データを作成
# ...suppressWarnings 警告のコンソールへの出力を抑制する
# ...lapply() は結果のリスト(sapply とちがって、属性(カラム名)付きではない)
# Now, create a full-featured training set.
train.ranks <- suppressWarnings(lapply(train.paths, rank.message))
train.ranks.matrix <- do.call(rbind, train.ranks)
train.ranks.matrix <- cbind(train.paths, train.ranks.matrix, "TRAINING")
train.ranks.df <- data.frame(train.ranks.matrix, stringsAsFactors = FALSE)
names(train.ranks.df) <- c("Message", "Date", "From", "Subj", "Rank", "Type") # パス、日付、送信者、件名、順位、タイプ(TRAINING)
train.ranks.df$Rank <- as.numeric(train.ranks.df$Rank)

# 順位から優先度のしきい値を決定
# ...中央値で設定する
# Set the priority threshold to the median of all ranks weights
priority.threshold <- median(train.ranks.df$Rank)

# Visualize the results to locate threshold
# 表示
threshold.plot <- ggplot(train.ranks.df, aes(x = Rank)) +
  stat_density(aes(fill="darkred")) +
  geom_vline(xintercept = priority.threshold, linetype = 2) +
  scale_fill_manual(values = c("darkred" = "darkred"), guide = "none") +
  theme_bw()
ggsave(plot = threshold.plot,
       filename = file.path("images", "01_threshold_plot.pdf"),
       height = 4.7,
       width = 7)

# 訓練データの Rank がしきい値より大きい場合は優先度１、それ以外は０
# Classify as priority, or not
train.ranks.df$Priority <- ifelse(train.ranks.df$Rank >= priority.threshold, 1, 0)

#-- 5. テストデータを解析する

# テストデータを作成。内容は訓練データと同じ
# Now, test our ranker by performing the exact same procedure on the test data
test.ranks <- suppressWarnings(lapply(test.paths,rank.message))
test.ranks.matrix <- do.call(rbind, test.ranks)
test.ranks.matrix <- cbind(test.paths, test.ranks.matrix, "TESTING")
test.ranks.df <- data.frame(test.ranks.matrix, stringsAsFactors = FALSE)
names(test.ranks.df) <- c("Message","Date","From","Subj","Rank","Type")
test.ranks.df$Rank <- as.numeric(test.ranks.df$Rank)
# しきい値は訓練データと同じ値を使用する
test.ranks.df$Priority <- ifelse(test.ranks.df$Rank >= priority.threshold, 1, 0)

# 訓練データとテストデータを連結
# Finally, we combine the data sets.
final.df <- rbind(train.ranks.df, test.ranks.df)
# 日付を変換
final.df$Date <- date.converter(final.df$Date, pattern1, pattern2)
# 日付順に並び替えて逆順に
final.df <- final.df[rev(with(final.df, order(Date))), ]

# Save final data set and plot results.
# 出力
write.csv(final.df, file.path("data", "final_df.csv"), row.names = FALSE)

testing.plot <- ggplot(subset(final.df, Type == "TRAINING"), aes(x = Rank)) +
  stat_density(aes(fill = Type, alpha = 0.65)) +
  stat_density(data = subset(final.df, Type == "TESTING"),
               aes(fill = Type, alpha = 0.65)) +
  geom_vline(xintercept = priority.threshold, linetype = 2) +
  scale_alpha(guide = "none") +
  scale_fill_manual(values = c("TRAINING" = "darkred", "TESTING" = "darkblue")) +
  theme_bw()
ggsave(plot = testing.plot,
       filename = file.path("images", "02_testing_plot.pdf"),
       height = 4.7,
       width = 7)
