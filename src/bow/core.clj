(ns bow.core
  (:require [clojure.string :as str]
            [pppmap.core :refer :all]))


(def stop-words [  "-", "–","",".","a", "about", "above", "after", "again", "against", "all", "am", "an", "and", "any", "are", "as", "at", "be", "because", "been", "before", "being", "below", "between", "both", "but", "by", "could", "did", "do", "does", "doing", "down", "during", "each", "few", "for", "from", "further", "had", "has", "have", "having", "he", "he'd", "he'll", "he's", "her", "here", "here's", "hers", "herself", "him", "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if", "in", "into", "is", "it", "it's", "its", "itself", "let's", "me", "more", "most", "my", "myself", "nor", "of", "on", "once", "only", "or", "other", "ought", "our", "ours", "ourselves", "out", "over", "own", "same", "she", "she'd", "she'll", "she's", "should", "so", "some", "such", "than", "that", "that's", "the", "their", "theirs", "them", "themselves", "then", "there", "there's", "these", "they", "they'd", "they'll", "they're", "they've", "this", "those", "through", "to", "too", "under", "until", "up", "very", "was", "we", "we'd", "we'll", "we're", "we've", "were", "what", "what's", "when", "when's", "where", "where's", "which", "while", "who", "who's", "whom", "why", "why's", "with", "would", "you", "you'd", "you'll", "you're", "you've", "your", "yours", "yourself", "yourselves" ]
  )

(defn load-stopwords [stopwords-filepath]
  (-> stopwords-filepath
      (slurp)
      (str/split #"[\r\n]+")))


(defn remove-numbers [text-in]
  (.trim (str/replace text-in #"\d+" "")))



                                        ;(def stemmer (snowball/stemmer :english))

(defn remove-stopwords [words stop-words]
  (remove (set stop-words) words))


(defn remove-punctuation-in-string [s]
  (-> s
      (str/replace #"[\.,\?:;!\"'†+\/\(\)\\]+$" "") ;; end of the line
      (str/replace #"^[\.,\?:;!\"'†+\/\(\)\\]+" "") ;; beginning of the line
      (str/replace #"(\S)[,\?:;!\"'†+\(\)\\]+(\S)" "$1 $2") ;; with non-space chars either side
      (str/replace #"(\S)[\.,\?:;!\"'†+\/\(\)\\]+\s" "$1 ") ;; non-space before and space after
      (str/replace #"\s[\.,\?:;!\"'†+\/\(\)\\]+(\S)" " $1") ;; space before and non-space after
      (str/replace #"\s[-\.,\?:;!\"'†+\/\(\)\\]+\s" " ") ;; space either side
      (str/replace #" – " " ") ;; pesky em-dashes
      (str/replace #"\d+" "")
      (str/replace #"\W" "")))

(defn tokenize [s]
  (clojure.string/split s #" |\n"))

(defn preprocess-tokens [v]
  (->>  v
        (map clojure.string/lower-case)
        (map remove-punctuation-in-string)
        (map remove-numbers)
        (#(remove-stopwords % stop-words))
        )
  )


(defn text->word-freqs
  ([s]
   (text->word-freqs
    s 
    {:tokenize-fn tokenize
     :preproces-fn preprocess-tokens}))
  
  ([s opts]
   (->>  ((:tokenize-fn opts ) s)
         ((:preproces-fn opts))
         (frequencies)
         ))
  )


(defn text->ints
  ([s dic]
   (text->ints
    s dic
    {:tokenize-fn tokenize
     :preproces-fn preprocess-tokens}))
  ([s dic opts] 
   (->> ((:tokenize-fn opts ) s)
        ((:preproces-fn opts))
        (map #(get dic %)))))




(defn is-file-seq [dir]
  (->> (file-seq dir)
       (filter #(.isFile %))
       )
  )

(defn texts->ints [files dic]
  (->>  files
        (ppmap-with-progress "process"
                             10
                             #(text->ints (slurp %) dic))
        (filter (comp not nil?))
              ))
  


(defn texts->word-freqs
  ([files]
   (texts->word-freqs
    files
    {}
)
    )
  ([files opts]
   (let [opts (merge
                     {:tokenize-fn tokenize
                      :preproces-fn preprocess-tokens
                      :ppmap-grain-size 500
                      }
                     opts)
             ]
        (->>  files
                                        ;(take 100000)
              (ppmap-with-progress "process"
                                   (:ppmap-grain-size opts)
                                   #(text->word-freqs (slurp %) opts))
              (filter (comp not nil?))
              (apply (partial merge-with +))
              )))
  )


(defn text->bow-vec [s dictionary]
  (let [all-zeros (zipmap (vals dictionary) (repeat (count dictionary) 0))
        text-freqs  (dissoc (frequencies (text->ints s dictionary))
                            nil)
        ]
    (int-array (map second (sort-by first (merge all-zeros text-freqs))))
    ))

(defn text->token-freqs [s dictionary]
  (dissoc (frequencies (text->ints s dictionary))
          nil)
  
  )

(defn token-freqs->dics [all-freqs max-n-freqs]
  (let [sorted-freqs (reverse (sort-by second all-freqs))
        all-freqs-pruned  (take max-n-freqs sorted-freqs)
        dictionary (zipmap (keys all-freqs-pruned) (range))
        dictionary-inv (zipmap (range )(keys all-freqs-pruned))

        ]
    {:dic dictionary
     :inv-dic dictionary-inv}
    ))

(comment

  (def text-1 "this is a test of a nice text with quite some words and more unusual words")
  (def text-2 "and that is a new text about wikipedia and some more words")

  (def freqs-1 (text->word-freqs text-1))
  (def freqs-2 (text->word-freqs text-2))

  (def all-freqs (merge-with + freqs-1 freqs-2))

  (def dics (token-freqs->dics all-freqs 10))
  (def dic (:dic dics))
  (def dic-inv (:inv-dic dics))



  (def ints-1 (text->ints text-1 dic))
  (map  #(get dic-inv %)   ints-1)
  (text->bow-vec text-1 dic)
  (text->bow-vec "words" dic)

)

