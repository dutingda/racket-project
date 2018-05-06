;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname suggest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; OPTIONAL -- spellcheck.rkt provides the following function:
;;
;; (spellcheck? s) determines if s is spelled correctly
;;   (according to a medium-sized wordlist)
;; spellcheck: Str -> Bool
;;
;; You may use this function for your own experiments
;; (and to show off your program to your friends & family)

;; Do NOT leave this require in your file when you submit your code.
;; (require "spellcheck.rkt")
;; [this file will be available after the A07 deadline]
;; NOTE: You do not need to open spellcheck.rkt in DrRacket to use it
;;       (opening the file in DrRacket may slow down your computer).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Place your identifying information here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: you should complete the documentation & tests (design recipe)
;; for all functions (except remove-at and remove-letters)
;; But remember, because most of your functions will not have a cond
;; or much logic, exhaustive tests may not be required

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; A Word is a Str
;; requires: only lowercase letters appear in the word
;;           (no spaces, punctuation, etc.)

(define letters (string->list "abcdefghijklmnopqrstuvwxyz"))

;; Useful functions:
(define (valid? s)
  (member? s '("bright" "might" "fight" "rights"
                        "aright" "alright" "slight" "smight")))
  

;; 4a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (remove-dups slst) Produces a list of which all duplicates are removed which slst
;;   is the consumed list that are being removed
;; remove-dups: (listof Word) -> (listof Word)
;; requires: slst is sorted in non-decreasing order
;; Examples:
(check-expect (remove-dups empty) empty)
(check-expect (remove-dups '("abc")) '("abc"))


(define (remove-dups slst)
  (foldr (lambda (x y) (cond
                         [(empty? y) (cons x empty)]
                         [(string=? x (first y)) y]
                         [else (cons x y)])) empty slst))


;; Tests:
(check-expect (remove-dups '("apple" "apple" "apples" "banana" "cherry" "cherry"))
              '("apple" "apples" "banana" "cherry"))
(check-expect (remove-dups '("abc" "bcd" "bcd" "cdefg" "cdefg" "coghk" "ctoml"))
              '("abc" "bcd" "cdefg" "coghk" "ctoml"))
;; 4b ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (ifoldr: combine base lst) Abstracts lockstep recursion on both a list lst and a
;;    natural number (using a " countup" pattern). Combine is a function that take
;;    index and normal foldr parameters and combine the two part of the list.
;; ifoldr: (Nat X Y -> Y) Y (listof X) -> Y
;; Example:
(check-expect
 (ifoldr (lambda (i x y) (cons (list i x) y)) empty '(a b c))
 (list (list 0 'a) (list 1 'b) (list 2 'c)))

(define (ifoldr combine base lst)
  (local
    [;; (ifoldr/help combine base lst index) abstracts lockstep recursion
     ;;    on both a list and a natural number.
     ;; ifoldr/help: (Nat X Y -> Y) Y (listof X) Nat -> Y
     (define (ifoldr/help combine base lst index)
       (cond
         [(empty? lst) base]
         [else (combine index (first lst) (ifoldr/help combine base (rest lst) (add1 index)))]))]
    (ifoldr/help combine base lst 0)))


;; Tests:
(check-expect (ifoldr (lambda (i x y) (+ i y)) 0 '(e f g h i j k l)) 28)
(check-expect (ifoldr (lambda (i x y) (* i y)) 1 '(r f g h k)) 0)
              
;; example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (remove-at i lst) removes element with index i from lst
;; remove-at: Nat (listof Any) -> (listof Any)
;; Examples:
;;;; COMMENTED OUT FOR NOW: THESE EXAMPLES RELY ON ifoldr [above]
(check-expect (remove-at 0 '(a b c d)) '(b c d))
(check-expect (remove-at 3 '(a b c d)) '(a b c))
(check-expect (remove-at 0 '()) '())

(define (remove-at i lst)
  (ifoldr (lambda (k x y)
            (cond [(= i k) y]
                  [else (cons x y)]))
          empty lst))


;; (remove-letters s) produces a list of Words,
;;    each with one letter removed from s
;; remove-letters: Word -> (listof Word)
;; Examples:
;;;; COMMENTED OUT FOR NOW: THESE EXAMPLES RELY ON ifoldr [above]
(check-expect (remove-letters "abc") '("bc" "ac" "ab"))
(check-expect (remove-letters "") '())

(define (remove-letters s)
  (local [(define loc (string->list s))]
    (build-list (length loc) (lambda (i) (list->string (remove-at i loc))))))

;; 4c ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (insert-letters s) Produces every possible letter (a..z) inserted before each
;;   letter in the word
;; insert-letters: Word -> (listof Word)
;; Example:
(check-expect (insert-letters "tingda")
              (list
               "atingda"
               "taingda"
               "tiangda"
               "tinagda"
               "tingada"
               "tingdaa"
               "btingda"
               "tbingda"
               "tibngda"
               "tinbgda"
               "tingbda"
               "tingdba"
               "ctingda"
               "tcingda"
               "ticngda"
               "tincgda"
               "tingcda"
               "tingdca"
               "dtingda"
               "tdingda"
               "tidngda"
               "tindgda"
               "tingdda"
               "tingdda"
               "etingda"
               "teingda"
               "tiengda"
               "tinegda"
               "tingeda"
               "tingdea"
               "ftingda"
               "tfingda"
               "tifngda"
               "tinfgda"
               "tingfda"
               "tingdfa"
               "gtingda"
               "tgingda"
               "tigngda"
               "tinggda"
               "tinggda"
               "tingdga"
               "htingda"
               "thingda"
               "tihngda"
               "tinhgda"
               "tinghda"
               "tingdha"
               "itingda"
               "tiingda"
               "tiingda"
               "tinigda"
               "tingida"
               "tingdia"
               "jtingda"
               "tjingda"
               "tijngda"
               "tinjgda"
               "tingjda"
               "tingdja"
               "ktingda"
               "tkingda"
               "tikngda"
               "tinkgda"
               "tingkda"
               "tingdka"
               "ltingda"
               "tlingda"
               "tilngda"
               "tinlgda"
               "tinglda"
               "tingdla"
               "mtingda"
               "tmingda"
               "timngda"
               "tinmgda"
               "tingmda"
               "tingdma"
               "ntingda"
               "tningda"
               "tinngda"
               "tinngda"
               "tingnda"
               "tingdna"
               "otingda"
               "toingda"
               "tiongda"
               "tinogda"
               "tingoda"
               "tingdoa"
               "ptingda"
               "tpingda"
               "tipngda"
               "tinpgda"
               "tingpda"
               "tingdpa"
               "qtingda"
               "tqingda"
               "tiqngda"
               "tinqgda"
               "tingqda"
               "tingdqa"
               "rtingda"
               "tringda"
               "tirngda"
               "tinrgda"
               "tingrda"
               "tingdra"
               "stingda"
               "tsingda"
               "tisngda"
               "tinsgda"
               "tingsda"
               "tingdsa"
               "ttingda"
               "ttingda"
               "titngda"
               "tintgda"
               "tingtda"
               "tingdta"
               "utingda"
               "tuingda"
               "tiungda"
               "tinugda"
               "tinguda"
               "tingdua"
               "vtingda"
               "tvingda"
               "tivngda"
               "tinvgda"
               "tingvda"
               "tingdva"
               "wtingda"
               "twingda"
               "tiwngda"
               "tinwgda"
               "tingwda"
               "tingdwa"
               "xtingda"
               "txingda"
               "tixngda"
               "tinxgda"
               "tingxda"
               "tingdxa"
               "ytingda"
               "tyingda"
               "tiyngda"
               "tinygda"
               "tingyda"
               "tingdya"
               "ztingda"
               "tzingda"
               "tizngda"
               "tinzgda"
               "tingzda"
               "tingdza"))
(check-expect (insert-letters "abc")
              (list
               "aabc"
               "aabc"
               "abac"
               "babc"
               "abbc"
               "abbc"
               "cabc"
               "acbc"
               "abcc"
               "dabc"
               "adbc"
               "abdc"
               "eabc"
               "aebc"
               "abec"
               "fabc"
               "afbc"
               "abfc"
               "gabc"
               "agbc"
               "abgc"
               "habc"
               "ahbc"
               "abhc"
               "iabc"
               "aibc"
               "abic"
               "jabc"
               "ajbc"
               "abjc"
               "kabc"
               "akbc"
               "abkc"
               "labc"
               "albc"
               "ablc"
               "mabc"
               "ambc"
               "abmc"
               "nabc"
               "anbc"
               "abnc"
               "oabc"
               "aobc"
               "aboc"
               "pabc"
               "apbc"
               "abpc"
               "qabc"
               "aqbc"
               "abqc"
               "rabc"
               "arbc"
               "abrc"
               "sabc"
               "asbc"
               "absc"
               "tabc"
               "atbc"
               "abtc"
               "uabc"
               "aubc"
               "abuc"
               "vabc"
               "avbc"
               "abvc"
               "wabc"
               "awbc"
               "abwc"
               "xabc"
               "axbc"
               "abxc"
               "yabc"
               "aybc"
               "abyc"
               "zabc"
               "azbc"
               "abzc"))


(define (insert-letters s)
  (local
    [;; (insert-at index letter lst) Produces the list of characters lst that has been
     ;;   inserted by letter into in the certain index 
     ;; insert-at: Nat Char (listof Char) -> (listof Char)
     (define (insert-at index letter lst)
       (ifoldr (lambda (i x y) (cond
                                 [(= i index) (cons letter (cons x y))]
                                 [else (cons x y)])) empty lst))
     ;; (insert letter loc) Produces a list of string that is inserted at all
     ;;   available indices in the loc by the letter
     ;; insert: Char (listof Char) -> (listof Str)
     (define (insert letter loc)
       (build-list (length (string->list s))
                   (lambda (in) (list->string
                                 (insert-at in letter loc)))))]
    (foldr (lambda (a b) (append (insert a (string->list s)) b)) empty letters)))


;; (trailing-letters s) Produces a list of word with every possible letter
;;    inserted at the end of the word
;; trailing-letters: Word -> (listof Word)
;; Examples:
(check-expect (trailing-letters "tingda")
              (list
               "tingdaa"
               "tingdab"
               "tingdac"
               "tingdad"
               "tingdae"
               "tingdaf"
               "tingdag"
               "tingdah"
               "tingdai"
               "tingdaj"
               "tingdak"
               "tingdal"
               "tingdam"
               "tingdan"
               "tingdao"
               "tingdap"
               "tingdaq"
               "tingdar"
               "tingdas"
               "tingdat"
               "tingdau"
               "tingdav"
               "tingdaw"
               "tingdax"
               "tingday"
               "tingdaz"))
(check-expect (trailing-letters "abc")
              (list
               "abca"
               "abcb"
               "abcc"
               "abcd"
               "abce"
               "abcf"
               "abcg"
               "abch"
               "abci"
               "abcj"
               "abck"
               "abcl"
               "abcm"
               "abcn"
               "abco"
               "abcp"
               "abcq"
               "abcr"
               "abcs"
               "abct"
               "abcu"
               "abcv"
               "abcw"
               "abcx"
               "abcy"
               "abcz"))


(define (trailing-letters s)
  (map (lambda (x) (list->string x))
       (map (lambda (x) (append (string->list s) (list x))) letters)))


;; (replace-letters s) Produces a list of word with each letter replace with
;;    every possible letter
;; replace-letters: Word -> (listof Word)
;; Examples:
(check-expect (replace-letters "ide")
              (list "ade" "iae" "ida" "bde"
                    "ibe" "idb" "cde" "ice"
                    "idc" "dde" "ide" "idd"
                    "ede" "iee" "ide" "fde"
                    "ife" "idf" "gde" "ige"
                    "idg" "hde" "ihe" "idh"
                    "ide" "iie" "idi" "jde"
                    "ije" "idj" "kde" "ike"
                    "idk" "lde" "ile" "idl"
                    "mde" "ime" "idm" "nde"
                    "ine" "idn" "ode" "ioe"
                    "ido" "pde" "ipe" "idp"
                    "qde" "iqe" "idq" "rde"
                    "ire" "idr" "sde" "ise"
                    "ids" "tde" "ite" "idt"
                    "ude" "iue" "idu" "vde"
                    "ive" "idv" "wde" "iwe"
                    "idw" "xde" "ixe" "idx"
                    "yde" "iye" "idy" "zde"
                    "ize" "idz"))
(check-expect (replace-letters "123")
              (list
               "a23"
               "1a3"
               "12a"
               "b23"
               "1b3"
               "12b"
               "c23"
               "1c3"
               "12c"
               "d23"
               "1d3"
               "12d"
               "e23"
               "1e3"
               "12e"
               "f23"
               "1f3"
               "12f"
               "g23"
               "1g3"
               "12g"
               "h23"
               "1h3"
               "12h"
               "i23"
               "1i3"
               "12i"
               "j23"
               "1j3"
               "12j"
               "k23"
               "1k3"
               "12k"
               "l23"
               "1l3"
               "12l"
               "m23"
               "1m3"
               "12m"
               "n23"
               "1n3"
               "12n"
               "o23"
               "1o3"
               "12o"
               "p23"
               "1p3"
               "12p"
               "q23"
               "1q3"
               "12q"
               "r23"
               "1r3"
               "12r"
               "s23"
               "1s3"
               "12s"
               "t23"
               "1t3"
               "12t"
               "u23"
               "1u3"
               "12u"
               "v23"
               "1v3"
               "12v"
               "w23"
               "1w3"
               "12w"
               "x23"
               "1x3"
               "12x"
               "y23"
               "1y3"
               "12y"
               "z23"
               "1z3"
               "12z"))
(check-expect (replace-letters "") empty)


(define (replace-letters s)
  (local
    [;; (replace-at word letter ind) Replace the "word" that is at the "ind" of the letter
     ;;    with letter
     ;; replace-at (listof Char) Char Nat -> (listof Char)
     (define (replace-at word letter ind)
       (ifoldr (lambda (i x y) (cond
                                 [(= i ind) (cons letter y)]
                                 [else (cons x y)])) empty word))
     ;; (replace-each word letter) Produces the list of string that has been replaced by the
     ;;   letter in terms of each index
     ;; replace-each: (listof Char) Char -> (listof Str)
     (define (replace-each word letter)
       (ifoldr (lambda (i x y) (cons (list->string (replace-at word letter i)) y)) empty word))]
    (foldr (lambda (x y) (append (replace-each (string->list s) x) y)) empty letters)))


;; (swap-letters s) Produces a list of words with each adjacent pair of letters
;;    in the word swapped
;; swap-letters: Word -> (listof Word)
;; Examples:
(check-expect (swap-letters "tingda") (list "itngda" "tnigda" "tignda" "tindga" "tingad"))
(check-expect (swap-letters "chen") (list "hcen" "cehn" "chne"))

(define (swap-letters s)
  (local
    [;; (swap-at i lst) Produces a new list of characters with the ith index character
     ;;    and (i+1)th index character swapped
     ;; swap-at Nat (listof Char) -> (listof Char)
     (define (swap-at i lst)
       (ifoldr (lambda (index x y)
                 (cond [(= index i) (cons (first y) (cons x (rest y)))]
                       [else (cons x y)]))
               empty lst))
     (define loc (string->list s))]
    (build-list (- (length loc) 1) (lambda (i) (list->string (swap-at i loc))))))     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You are not required to modify the definition of suggest,
;; but you may if you wish

;; suggest: Word (Word -> Bool) -> (listof Word)

;; Example:
(check-expect (suggest "right" valid?)
              (list "aright" "bright" "fight" "might" "rights"))


(define (suggest s valid?)
  (local [(define words (append (remove-letters s)
                                (insert-letters s)
                                (trailing-letters s)
                                (replace-letters s)
                                (swap-letters s)))

          (define valid-words (filter valid? words))

          (define legal-words (filter (lambda (x) (and (not (string=? s x))
                                                       (not (string=? x ""))))
                                      valid-words))

          (define clean-words (remove-dups (sort legal-words string<=?)))]

    clean-words))


;; Tests:
(check-expect (suggest "mights" (lambda (s) true)) (list
                                                    "aights"
                                                    "amights"
                                                    "bights"
                                                    "bmights"
                                                    "cights"
                                                    "cmights"
                                                    "dights"
                                                    "dmights"
                                                    "eights"
                                                    "emights"
                                                    "fights"
                                                    "fmights"
                                                    "gights"
                                                    "gmights"
                                                    "hights"
                                                    "hmights"
                                                    "ights"
                                                    "iights"
                                                    "imghts"
                                                    "imights"
                                                    "jights"
                                                    "jmights"
                                                    "kights"
                                                    "kmights"
                                                    "lights"
                                                    "lmights"
                                                    "maghts"
                                                    "maights"
                                                    "mbghts"
                                                    "mbights"
                                                    "mcghts"
                                                    "mcights"
                                                    "mdghts"
                                                    "mdights"
                                                    "meghts"
                                                    "meights"
                                                    "mfghts"
                                                    "mfights"
                                                    "mgghts"
                                                    "mghts"
                                                    "mgights"
                                                    "mgihts"
                                                    "mhghts"
                                                    "mhights"
                                                    "miaghts"
                                                    "miahts"
                                                    "mibghts"
                                                    "mibhts"
                                                    "micghts"
                                                    "michts"
                                                    "midghts"
                                                    "midhts"
                                                    "mieghts"
                                                    "miehts"
                                                    "mifghts"
                                                    "mifhts"
                                                    "migahts"
                                                    "migats"
                                                    "migbhts"
                                                    "migbts"
                                                    "migchts"
                                                    "migcts"
                                                    "migdhts"
                                                    "migdts"
                                                    "migehts"
                                                    "migets"
                                                    "migfhts"
                                                    "migfts"
                                                    "migghts"
                                                    "miggts"
                                                    "mighas"
                                                    "mighats"
                                                    "mighbs"
                                                    "mighbts"
                                                    "mighcs"
                                                    "mighcts"
                                                    "mighds"
                                                    "mighdts"
                                                    "mighes"
                                                    "mighets"
                                                    "mighfs"
                                                    "mighfts"
                                                    "mighgs"
                                                    "mighgts"
                                                    "mighhs"
                                                    "mighhts"
                                                    "mighis"
                                                    "mighits"
                                                    "mighjs"
                                                    "mighjts"
                                                    "mighks"
                                                    "mighkts"
                                                    "mighls"
                                                    "mighlts"
                                                    "mighms"
                                                    "mighmts"
                                                    "mighns"
                                                    "mighnts"
                                                    "mighos"
                                                    "mighots"
                                                    "mighps"
                                                    "mighpts"
                                                    "mighqs"
                                                    "mighqts"
                                                    "mighrs"
                                                    "mighrts"
                                                    "mighs"
                                                    "mighss"
                                                    "mighst"
                                                    "mighsts"
                                                    "might"
                                                    "mighta"
                                                    "mightas"
                                                    "mightb"
                                                    "mightbs"
                                                    "mightc"
                                                    "mightcs"
                                                    "mightd"
                                                    "mightds"
                                                    "mighte"
                                                    "mightes"
                                                    "mightf"
                                                    "mightfs"
                                                    "mightg"
                                                    "mightgs"
                                                    "mighth"
                                                    "mighths"
                                                    "mighti"
                                                    "mightis"
                                                    "mightj"
                                                    "mightjs"
                                                    "mightk"
                                                    "mightks"
                                                    "mightl"
                                                    "mightls"
                                                    "mightm"
                                                    "mightms"
                                                    "mightn"
                                                    "mightns"
                                                    "mighto"
                                                    "mightos"
                                                    "mightp"
                                                    "mightps"
                                                    "mightq"
                                                    "mightqs"
                                                    "mightr"
                                                    "mightrs"
                                                    "mightsa"
                                                    "mightsb"
                                                    "mightsc"
                                                    "mightsd"
                                                    "mightse"
                                                    "mightsf"
                                                    "mightsg"
                                                    "mightsh"
                                                    "mightsi"
                                                    "mightsj"
                                                    "mightsk"
                                                    "mightsl"
                                                    "mightsm"
                                                    "mightsn"
                                                    "mightso"
                                                    "mightsp"
                                                    "mightsq"
                                                    "mightsr"
                                                    "mightss"
                                                    "mightst"
                                                    "mightsu"
                                                    "mightsv"
                                                    "mightsw"
                                                    "mightsx"
                                                    "mightsy"
                                                    "mightsz"
                                                    "mightt"
                                                    "mightts"
                                                    "mightu"
                                                    "mightus"
                                                    "mightv"
                                                    "mightvs"
                                                    "mightw"
                                                    "mightws"
                                                    "mightx"
                                                    "mightxs"
                                                    "mighty"
                                                    "mightys"
                                                    "mightz"
                                                    "mightzs"
                                                    "mighus"
                                                    "mighuts"
                                                    "mighvs"
                                                    "mighvts"
                                                    "mighws"
                                                    "mighwts"
                                                    "mighxs"
                                                    "mighxts"
                                                    "mighys"
                                                    "mighyts"
                                                    "mighzs"
                                                    "mighzts"
                                                    "migihts"
                                                    "migits"
                                                    "migjhts"
                                                    "migjts"
                                                    "migkhts"
                                                    "migkts"
                                                    "miglhts"
                                                    "miglts"
                                                    "migmhts"
                                                    "migmts"
                                                    "mignhts"
                                                    "mignts"
                                                    "migohts"
                                                    "migots"
                                                    "migphts"
                                                    "migpts"
                                                    "migqhts"
                                                    "migqts"
                                                    "migrhts"
                                                    "migrts"
                                                    "migshts"
                                                    "migsts"
                                                    "migths"
                                                    "migthts"
                                                    "migts"
                                                    "migtts"
                                                    "miguhts"
                                                    "miguts"
                                                    "migvhts"
                                                    "migvts"
                                                    "migwhts"
                                                    "migwts"
                                                    "migxhts"
                                                    "migxts"
                                                    "migyhts"
                                                    "migyts"
                                                    "migzhts"
                                                    "migzts"
                                                    "mihghts"
                                                    "mihgts"
                                                    "mihhts"
                                                    "mihts"
                                                    "miights"
                                                    "miihts"
                                                    "mijghts"
                                                    "mijhts"
                                                    "mikghts"
                                                    "mikhts"
                                                    "milghts"
                                                    "milhts"
                                                    "mimghts"
                                                    "mimhts"
                                                    "minghts"
                                                    "minhts"
                                                    "mioghts"
                                                    "miohts"
                                                    "mipghts"
                                                    "miphts"
                                                    "miqghts"
                                                    "miqhts"
                                                    "mirghts"
                                                    "mirhts"
                                                    "misghts"
                                                    "mishts"
                                                    "mitghts"
                                                    "mithts"
                                                    "miughts"
                                                    "miuhts"
                                                    "mivghts"
                                                    "mivhts"
                                                    "miwghts"
                                                    "miwhts"
                                                    "mixghts"
                                                    "mixhts"
                                                    "miyghts"
                                                    "miyhts"
                                                    "mizghts"
                                                    "mizhts"
                                                    "mjghts"
                                                    "mjights"
                                                    "mkghts"
                                                    "mkights"
                                                    "mlghts"
                                                    "mlights"
                                                    "mmghts"
                                                    "mmights"
                                                    "mnghts"
                                                    "mnights"
                                                    "moghts"
                                                    "moights"
                                                    "mpghts"
                                                    "mpights"
                                                    "mqghts"
                                                    "mqights"
                                                    "mrghts"
                                                    "mrights"
                                                    "msghts"
                                                    "msights"
                                                    "mtghts"
                                                    "mtights"
                                                    "mughts"
                                                    "muights"
                                                    "mvghts"
                                                    "mvights"
                                                    "mwghts"
                                                    "mwights"
                                                    "mxghts"
                                                    "mxights"
                                                    "myghts"
                                                    "myights"
                                                    "mzghts"
                                                    "mzights"
                                                    "nights"
                                                    "nmights"
                                                    "oights"
                                                    "omights"
                                                    "pights"
                                                    "pmights"
                                                    "qights"
                                                    "qmights"
                                                    "rights"
                                                    "rmights"
                                                    "sights"
                                                    "smights"
                                                    "tights"
                                                    "tmights"
                                                    "uights"
                                                    "umights"
                                                    "vights"
                                                    "vmights"
                                                    "wights"
                                                    "wmights"
                                                    "xights"
                                                    "xmights"
                                                    "yights"
                                                    "ymights"
                                                    "zights"
                                                    "zmights"))