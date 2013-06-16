#−− ベイズ分類器の仕組み
#−−   ・ ベイズの定理の一般式
#--         P(結果 | 原因) = P(結果) * P(原因 | 結果) / P(原因)
#−−          ※ P(A)：Aが起きる確率
#--          ※ P(結果 | 原因)　： 原因が発生したときに、結果が起きる条件付き確率
#--          ※ / P(原因)：は同じ値になる場合は省略してもOK
#--   ・ メールフィルタをベイズの定理に従うと下記の式になる
#−−       <スパム>
#--    　   P(スパムである | 与えられたメール) = P(スパムである) * P(メール | スパムである) / P(メール)
#−−       <非スパム>
#--    　   P(スパムでない | 与えられたメール) = P(スパムでない) * P(メール | スパムでない) / P(メール)
#--       ※「左辺」が事後確率にあたり、「P(スパムである/でない)」が事前確率にあたる(今回は50%と設定されている)
#--       ※ 「/ P(メール)」は同じ値になるので省略されている)
#--   ・ P(スパムである | 与えられたメール)　>　P(スパムでない | 与えられたメール)　のとき、スパムと判別する
#−−   ・ P(メール | スパムである/でない)は、下記の要領で算出する
#--       - (1)訓練データの作成：data/spam(easy_ham) 以下のメール内に含まれる単語のリストを作り、「単語が含まれているメール / 全メール」という割合を求めておく
#--       - (2)判別したいメール中の語と、訓練データ中の語とを比較し一致するものを洗い出す。その語の(1)で求めた割合を全て乗算する
#−−       - (3)訓練データに一致しない語の数回、0.0001% 乗算したものと、(2）の値を乗算する


#--テキストマイニングライブラリ
library('tm')
#--ジッタを入れてプロットするライブラリ
library('ggplot2')

#-- メールサンプルがあるフォルダのパス
spam.path <- file.path("data", "spam")
spam2.path <- file.path("data", "spam_2")
easyham.path <- file.path("data", "easy_ham")
easyham2.path <- file.path("data", "easy_ham_2")
hardham.path <- file.path("data", "hard_ham")
hardham2.path <- file.path("data", "hard_ham_2")

#-- get.msg ファイルからメッセージの部分だけを抽出
get.msg <- function(path)
{
  #-- ファイルを開いて内容を読み込む
  con <- file(path, open = "rt", encoding = "latin1")
  text <- readLines(con)

  #-- seq 普通の数列(配列)を生成する 引数(from, to, by(ステップ))
  #-- which 引数の条件式に一致するベクトルのインデックスの配列を返す。メールの仕様的に最初の空行以降がメッセージなので "" で判断
  #-- [] はベクトル、配列の要素のアクセス(他の言語と同じ)
  msg <- text[seq(which(text == "")[1] + 1, length(text), 1)]
  close(con)
  #-- paste ベクトルを文字列として連結させる。collapse はベクトル要素間の連結文字列
  return(paste(msg, collapse = "\n"))
}

#−− get.tdm 単語文書行列を生成。全文書に現れた単語１つ１つが行、各文書が列のN×M行列。
#-- 　　　　　[i, j] 番目の要素は、単語iが文書j に現れる回数に対応する
get.tdm <- function(doc.vec)
{
    control <- list(stopwords = TRUE,          #−− ストップワード：検索精度の向上のためには検索対象から除外せざるを得ない語。英語だと the, of, is など
                  removePunctuation = TRUE,  #-- 句読点
                  removeNumbers = TRUE,
                  minDocFreq = 2)            #-- コーパスに２回以上出現する単語のみが行に現れるようにする(コーパス：コンピュータによる検索が可能になっている大量の言語データのこと)
  #-- コーパスの生成
  #−− ??VectorSource は何をやっているのかイマイチつかめなかった。
  doc.corpus <- Corpus(VectorSource(doc.vec))
  #-- TDM 作成
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}

#-- pathに指定されたメールのファイル内に、term の語が何回登場するかカウントする
count.word <- function(path, term)
{
  msg <- get.msg(path)
  msg.corpus <- Corpus(VectorSource(msg))
  # Hard-coded TDM control
  control <- list(stopwords = TRUE,
                  removePunctuation = TRUE,
                  removeNumbers = TRUE)
  msg.tdm <- TermDocumentMatrix(msg.corpus, control)
  #-- rowSums 行毎に集計
  #--     as.matrix(tdm) とすると下記のようなデータになる
  #--             Docs
  #--       Term  1 2
  #--       単語A  0 1
  #--       単語B  1 0
  #--     rowSums(as.matrix(tdm) すると下記の結果が帰って来る
  #--       単語A  単語B
  #--        1     1    ← 行毎の Sum
  word.freq <- rowSums(as.matrix(msg.tdm))
  #−− which(names(word.freq) == term) で引数termとカラム名が一致する位置が帰って来るので、それがterm の出現回数
  #-- カラム名に一致するtermがない場合は NA　が帰って来る
  term.freq <- word.freq[which(names(word.freq) == term)]
  # We use ifelse here because term.freq = NA if nothing is found
  #−− term.freq が NA だった場合に 0 に変換して return
  return(ifelse(length(term.freq) > 0, term.freq, 0))
}

#-- ナイーブベイズ推定値を返す。(P(スパムである/でない | メール))
#--     第１引数：メールファイルパス、
#--     第２引数：訓練データのデータフレーム
#--     第３引数：事前確率、
#--     第４引数：固定値(訓練セットに含まれない単語(未知語)の確率)
classify.email <- function(path, training.df, prior = 0.5, c = 1e-6)
{
  # Here, we use many of the support functions to get the
  # email text data in a workable format
  msg <- get.msg(path)
  msg.tdm <- get.tdm(msg)
  msg.freq <- rowSums(as.matrix(msg.tdm))
  #-- intersect カラム名が一致する語を見つける
  #--    training.df は縦に単語が並んでいる
  #--    msg.freq カラム名が単語
  #−−    この２つを比較して一致するものが戻り値として帰って来る
  msg.match <- intersect(names(msg.freq), training.df$term)
  #−− ベイズ計算
  #--     ^ n は n乗
  if(length(msg.match) < 1)
  {
    #-- 一致するものがなかった場合。固定値にメール内の語の数乗したものを事前確率に掛けて返す
    return(prior * c ^ (length(msg.freq)))
  }
  else
  {
    #−− 一致した語の出現率(occurrence)の一覧を取得
    match.probs <- training.df$occurrence[match(msg.match, training.df$term)]
    #-- prod ベクトル内の数を全てかけ算
    #−−     事前確率 × 訓練データに存在した語の出現率のprod × 未知語の確率
    #--     prod(match.probs) * c ^ … = P(メール | スパムである/でない)
    return(prior * prod(match.probs) * c ^ (length(msg.freq) - length(msg.match)))
  }
}


#−− cmds 以外の全てのメールファイルのメッセージ部分を取り出す
spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs != "cmds")]
#-- sapply 結果の「 names 属性付きのベクトル」か「 names 属性付きの行列」を返す．
all.spam <- sapply(spam.docs,
                   function(p) get.msg(file.path(spam.path, p)))

# Create a DocumentTermMatrix from that vector
spam.tdm <- get.tdm(all.spam)

# Create a data frame that provides the feature set from the training SPAM data
spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix)
#-- spam.counts は 列に単語、行に頻度となっているので、行列を入れ替える
spam.df <- data.frame(cbind(names(spam.counts),
                            as.numeric(spam.counts)),  #-- rowSums を numeric 型に変換(数の配列になる)
                      stringsAsFactors = FALSE)
#−− ↑spam.df$frequency は numeric になっていないらしい。is.numeric をしたら FALSE が帰って来た
#−- カラム名を付ける
names(spam.df) <- c("term", "frequency")
spam.df$frequency <- as.numeric(spam.df$frequency)

#−− 出現率(単語が１回以上出現するメールの数 / 全メール)
spam.occurrence <- sapply(1:nrow(spam.matrix),   #-- nrow 行数
                          function(i)
                          {
                            #-- spam.matrix 各行で１以上の要素の数 / 全カラム数
                            length(which(spam.matrix[i, ] > 0)) / ncol(spam.matrix)
                          })
#−− 密度(全単語数の出現数のうち、各単語がどのくらいの割合で出ているか)
spam.density <- spam.df$frequency / sum(spam.df$frequency)

#−− spam.df に density と occurrence を追加
spam.df <- transform(spam.df,
                     density = spam.density,
                     occurrence = spam.occurrence)

#−− spam.df と同じように、非スパム(易)についても作成(実装はspam.dfと変わらない)
easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
all.easyham <- sapply(easyham.docs[1:length(spam.docs)],
                      function(p) get.msg(file.path(easyham.path, p)))

easyham.tdm <- get.tdm(all.easyham)

easyham.matrix <- as.matrix(easyham.tdm)
easyham.counts <- rowSums(easyham.matrix)
easyham.df <- data.frame(cbind(names(easyham.counts),
                               as.numeric(easyham.counts)),
                         stringsAsFactors = FALSE)
names(easyham.df) <- c("term", "frequency")
easyham.df$frequency <- as.numeric(easyham.df$frequency)
easyham.occurrence <- sapply(1:nrow(easyham.matrix),
                            function(i)
                            {
                              length(which(easyham.matrix[i, ] > 0)) / ncol(easyham.matrix)
                            })
easyham.density <- easyham.df$frequency / sum(easyham.df$frequency)

easyham.df <- transform(easyham.df,
                        density = easyham.density,
                        occurrence = easyham.occurrence)

#−− 作った訓練データを使って、非スパム(難)をスパムかどうか判別する
hardham.docs <- dir(hardham.path)
hardham.docs <- hardham.docs[which(hardham.docs != "cmds")]

#−− スパムである条件付き確率を算出
hardham.spamtest <- sapply(hardham.docs,
                           function(p) classify.email(file.path(hardham.path, p), training.df = spam.df))
#−− 非スパムである条件付き確率を算出
hardham.hamtest <- sapply(hardham.docs,
                          function(p) classify.email(file.path(hardham.path, p), training.df = easyham.df))
#−− スパムの確率のほうが非スパムの確率より大きければスパムと判断
hardham.res <- ifelse(hardham.spamtest > hardham.hamtest,
                      TRUE,
                      FALSE)
summary(hardham.res)

#−− スパムメールから、html, table の数をカウントして、行列を作成
html.spam <- sapply(spam.docs,
                    function(p) count.word(file.path(spam.path, p), "html"))
table.spam <- sapply(spam.docs,
                     function(p) count.word(file.path(spam.path, p), "table"))
spam.init <- cbind(html.spam, table.spam, "SPAM")

#−−非スパム(易)も同様にカウント
html.easyham <- sapply(easyham.docs,
                       function(p) count.word(file.path(easyham.path, p), "html"))
table.easyham <- sapply(easyham.docs,
                        function(p) count.word(file.path(easyham.path, p), "table"))
easyham.init <- cbind(html.easyham, table.easyham, "EASYHAM")

#−− 行を連結して、型変換
init.df <- data.frame(rbind(spam.init, easyham.init),
                      stringsAsFactors = FALSE)
names(init.df) <- c("html", "table", "type")
init.df$html <- as.numeric(init.df$html)
init.df$table <- as.numeric(init.df$table)
init.df$type <- as.factor(init.df$type)

#−− プロット
init.plot1 <- ggplot(init.df, aes(x = html, y = table)) +
  geom_point(aes(shape = type)) +
  scale_shape_manual(values = c("SPAM" = 1, "EASYHAM" = 3), name = "Email Type") +
  xlab("Frequency of 'html'") +
  ylab("Freqeuncy of 'table'") +
  stat_abline(yintersept = 0, slope = 1) +
  theme_bw()
ggsave(plot = init.plot1,
       filename = file.path("images", "01_init_plot1.pdf"),
       width = 10,
       height = 10)
    
init.plot2 <- ggplot(init.df, aes(x = html, y = table)) +
  geom_point(aes(shape = type), position = "jitter") +
  scale_shape_manual(values = c("SPAM" = 1, "EASYHAM" = 3), name = "Email Type") +
  xlab("Frequency of 'html'") +
  ylab("Freqeuncy of 'table'") +
  stat_abline(yintersept = 0, slope = 1) +
  theme_bw()
ggsave(plot = init.plot2,
       filename = file.path("images", "02_init_plot2.pdf"),
       width = 10,
       height = 10)

#−− 3.3.2 分類器をすべての種類の電子メールに対してテストする
#−− スパム判別をするロジックを関数化
#-- 戻り値：(スパムの確率, 非スパムの確率, スパムかどうかのbool)
#−−    (つぶやき)spam.df, easyham.df を引数で与えてないけど参照できるのか〜
spam.classifier <- function(path)
{
  pr.spam <- classify.email(path, spam.df)
  pr.ham <- classify.email(path, easyham.df)
  return(c(pr.spam, pr.ham, ifelse(pr.spam > pr.ham, 1, 0)))
}

# Get lists of all the email messages
#−− メールファイルのリスト作成
easyham2.docs <- dir(easyham2.path)
easyham2.docs <- easyham2.docs[which(easyham2.docs != "cmds")]

hardham2.docs <- dir(hardham2.path)
hardham2.docs <- hardham2.docs[which(hardham2.docs != "cmds")]

spam2.docs <- dir(spam2.path)
spam2.docs <- spam2.docs[which(spam2.docs != "cmds")]

# Classify them all!
#-- lapply() は結果のリスト(sapply とちがって、属性(カラム名)付きではない)
#−− suppressWarnings 警告のコンソールへの出力を抑制する
easyham2.class <- suppressWarnings(lapply(easyham2.docs,
                                   function(p)
                                   {
                                     spam.classifier(file.path(easyham2.path, p))
                                   }))
hardham2.class <- suppressWarnings(lapply(hardham2.docs,
                                   function(p)
                                   {
                                     spam.classifier(file.path(hardham2.path, p))
                                   }))
spam2.class <- suppressWarnings(lapply(spam2.docs,
                                function(p)
                                {
                                  spam.classifier(file.path(spam2.path, p))
                                }))

#−− spam_2, easy_ham_2, hard_ham_2 について条件付き確率を算出して行列作成
#-- do.call do.call(what, args)関数は，関数名whatと引数リストargsをもとに関数呼び出しを構成し，実行する．
#--         lapply()関数との違いは，lapplyはリストの各成分ごとに関数whatを適用して，リストとして結果を返す点．
easyham2.matrix <- do.call(rbind, easyham2.class)
easyham2.final <- cbind(easyham2.matrix, "EASYHAM")

hardham2.matrix <- do.call(rbind, hardham2.class)
hardham2.final <- cbind(hardham2.matrix, "HARDHAM")

spam2.matrix <- do.call(rbind, spam2.class)
spam2.final <- cbind(spam2.matrix, "SPAM")

class.matrix <- rbind(easyham2.final, hardham2.final, spam2.final)
class.df <- data.frame(class.matrix, stringsAsFactors = FALSE)
names(class.df) <- c("Pr.SPAM" ,"Pr.HAM", "Class", "Type")
class.df$Pr.SPAM <- as.numeric(class.df$Pr.SPAM)
class.df$Pr.HAM <- as.numeric(class.df$Pr.HAM)
class.df$Class <- as.logical(as.numeric(class.df$Class))
class.df$Type <- as.factor(class.df$Type)

# Create final plot of results
#−− プロット
class.plot <- ggplot(class.df, aes(x = log(Pr.HAM), log(Pr.SPAM))) +
    geom_point(aes(shape = Type, alpha = 0.5)) +
    stat_abline(yintercept = 0, slope = 1) +
    scale_shape_manual(values = c("EASYHAM" = 1,
                                  "HARDHAM" = 2,
                                  "SPAM" = 3),
                       name = "Email Type") +
    scale_alpha(guide = "none") +
    xlab("log[Pr(HAM)]") +
    ylab("log[Pr(SPAM)]") +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank())
ggsave(plot = class.plot,
       filename = file.path("images", "03_final_classification.pdf"),
       height = 10,
       width = 10)

#-- (全メール中の非スパムの割合, 全メール中のスパムの割合)のベクトルを作成
get.results <- function(bool.vector)
{
  results <- c(length(bool.vector[which(bool.vector == FALSE)]) / length(bool.vector),
               length(bool.vector[which(bool.vector == TRUE)]) / length(bool.vector))
  return(results)
}

# Save results as a 2x3 table
#−− col(非スパムメールの割合, スパムメールの割合)　row(非スパム(易), 非スパム(難), スパム)
easyham2.col <- get.results(subset(class.df, Type == "EASYHAM")$Class)
hardham2.col <- get.results(subset(class.df, Type == "HARDHAM")$Class)
spam2.col <- get.results(subset(class.df, Type == "SPAM")$Class)

class.res <- rbind(easyham2.col, hardham2.col, spam2.col)
colnames(class.res) <- c("NOT SPAM", "SPAM")
print(class.res)

# Save the training data for use in Chapter 4
write.csv(spam.df, file.path("data", "spam_df.csv"), row.names = FALSE)
write.csv(easyham.df, file.path("data", "easyham_df.csv"), row.names = FALSE)
