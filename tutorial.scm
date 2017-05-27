(use awful
     yaml
     json
     doctype
     loops)

(require-extension utf8)
#;(import
  (except scheme
          string-length string-ref string-set! make-string string substring
          string->list list->string string-fill! write-char read-char display)
  (except chicken
          reverse-list->string print print*)
  (except data-structures
          ->string conc string-chop string-split string-translate
          substring=? substring-ci=? substring-index substring-index-ci)
  #;(except extras
          read-string write-string read-token)
  #;(except regexp
          grep regexp string-substitute string-substitute* string-split-fields
          string-match string-match-positions string-match-offsets
          string-search string-search-positions string-search-offsets) )

(enable-sxml #t)

(define (read-json-file a-path)
  (let ([a-port (open-input-file a-path)])
    (json-read a-port)))

(define (read-yaml-file a-path)
  (let ([a-port (open-input-file a-path)])
    (yaml-load a-port)))

#||#
(define HSK-1 (read-json-file "data/metadata-vocabulary/hsk-1.json"))
(define HSK-2 (read-json-file "data/metadata-vocabulary/hsk-2.json"))
(define HSK-3 (read-json-file "data/metadata-vocabulary/hsk-3.json"))
(define HSK-4 (read-json-file "data/metadata-vocabulary/hsk-4.json"))
(define HSK-5 (read-json-file "data/metadata-vocabulary/hsk-5.json"))
(define HSK-6 (read-json-file "data/metadata-vocabulary/hsk-6.json"))

(define HSK-1-YAML (read-yaml-file "data/vocabulary/hsk-1.yaml"))
(define HSK-2-YAML (read-yaml-file "data/vocabulary/hsk-2.yaml"))
(define HSK-3-YAML (read-yaml-file "data/vocabulary/hsk-3.yaml"))
(define HSK-4-YAML (read-yaml-file "data/vocabulary/hsk-4.yaml"))
(define HSK-5-YAML (read-yaml-file "data/vocabulary/hsk-5.yaml"))
(define HSK-6-YAML (read-yaml-file "data/vocabulary/hsk-6.yaml"))

;; returns a list of vectors
(define (get-words-list vocabulary-vector)
  (cdr (vector-ref vocabulary-vector 1)))

(define (get-nth-word words-list n)
  (list-ref words-list n))

(define (get-word-metadata a-word)
  ;; index is determined by how the data file is structured
  (cdr (vector-ref a-word 0)))

(define (get-word-translation-data a-word)
  ;; index is determined by how the data file is structured
  (cdr (vector-ref a-word 1)))

(define (get-translation-english translation-data)
  (cdr (vector-ref translation-data 0)))
(define (get-translation-pinyin-numbered translation-data)
  (cdr (vector-ref translation-data 1)))
(define (get-translation-pinyin translation-data)
  (cdr (vector-ref translation-data 2)))
(define (get-translation-simplified translation-data)
  (cdr (vector-ref translation-data 3)))
(define (get-translation-traditional translation-data)
  (cdr (vector-ref translation-data 4)))

(define (get-metadata-learned metadata)
  (cdr (vector-ref metadata 0)))
(define (get-metadata-description metadata)
  (cdr (vector-ref metadata 1)))

;; ================= ;;
;; GETTERS FROM WORD ;;
;; ================= ;;
(define (learned? a-word)
  (get-metadata-learned
   (get-word-metadata a-word)))

(define (description a-word)
  (get-metadata-description
   (get-word-metadata a-word)))

(define (english a-word)
  (get-translation-english
   (get-word-translation-data a-word)))

(define (pinyin-numbered a-word)
  (get-translation-pinyin-numbered
   (get-word-translation-data a-word)))

(define (pinyin a-word)
  (get-translation-pinyin
   (get-word-translation-data a-word)))

(define (simplified a-word)
  (get-translation-simplified
   (get-word-translation-data a-word)))

(define (traditional a-word)
  (get-translation-traditional
   (get-word-translation-data a-word)))

;; ======= ;;
;; UNICODE ;;
;; ======= ;;
(define (utf8-string->list a-string)
  (define (iter the-string ind result)
    (cond [(< ind (string-length the-string))
           (display "current character is: ") (display (string-ref the-string ind)) (newline)
           (iter the-string (+ ind 1) (cons (string-ref the-string ind) result))]
          [else
           result]))
  (reverse (iter a-string 0 (list))))

;; ========= ;;
;; RENDERING ;;
;; ========= ;;
(define (render-word a-word)
  `(div (@ (class "word-container"))
        (p (@ (class "word-translation-english-container"))
           ,(string-append "English: "
                           (english a-word)))))

(define (render-hanci hanci)
  (define (render-hanzi hanzi)
    `(div (@ (class "hanzi-container"))
          (p (@ (class "hanzi"))
             ,hanzi)))
  (map render-hanzi (utf8-string->list hanci)))

(define (render-word-table a-word)
  `(div (@ (class "word-table-container"))
        (table (@ (class "word-table"))
               (tr (td (@ (class "latinletters-cell") (rowspan 2))
                       (div (p (@ (class "word-attribute-label"))
                               ,"English:")
                            (p ,(english a-word))))
                   (td (@ (class "hiding-cell hanci-cell") (rowspan 3))
                       ,(render-hanci (simplified a-word))))
               (tr)
               (tr (td (@ (class "latinletters-cell") (rowspan 2))
                       (div (p (@ (class "word-attribute-label"))
                               ,"Pīnyīn:")
                            (p ,(pinyin a-word)))))

               (tr (td (@ (class "hiding-cell hanci-cell") (rowspan 3))
                       ,(render-hanci (traditional a-word))))
               (tr (td (@ (class "latinletters-cell") (rowspan 2))
                       (div (p (@ (class "hiding-cell word-attribute-label"))
                               ,"Pīnyīn #:")
                            (p ,(pinyin-numbered a-word)))))
               (tr))))

;; ====== ;;
;; ROUTES ;;
;; ====== ;;
(ajax-library "https://code.jquery.com/jquery-3.2.1.min.js")
(ajax-namespace "ajax")  ; default
(page-css
 (list
  #;"static/CSS/Skeleton/normalize.css"
  #;"static/CSS/Skeleton/skeleton.css"
  "static/CSS/custom/general.css"
  "static/CSS/custom/hiding.css"
  #;"static/CSS/custom/crossed.css"))

(define-page (main-page-path)
  (lambda ()
    `(div (@ (class "content-container"))
          (div (@ (class "words-container"))#;,(map render-word (get-words-list HSK-1))
               ,(map render-word-table (get-words-list HSK-1)))))
  doctype: doctype-html
  charset: "utf-8"
  use-ajax: #t)

;; (get-word-metadata (get-nth-word (get-words-list HSK-1) 0))
;; (display (time (do-times i 10000000
;;    (english (get-nth-word (get-words-list HSK-1) 0)))))

;; (define-page (main-page-path)
;;   (lambda ()
;;     (add-javascript "alert('Hello!');")
;;     ;; make a list of
;;     ;; - ajax code
;;     ;; - the raw: (div (@ (id "greetings-reply")))
;;     ;;   which is not evaluated for now
;;     ;; This is done so that it is in one return value of the lambda expression.
;;     ;; It would be possible to build a list with list operations and using the ' character, I guess.
;;     `(,(ajax-link "greetings" 'greetings "Hello, awful!"
;;                   (lambda ()
;;                     '(b "Hello, awful!"))
;;                   target: "greetings-reply")
;;       (div (@ (id "greetings-reply")))))
;;   use-ajax: #t)

;; (use awful
;;      #;html-tags
;;      #;spiffy
;;      #;spiffy-request-vars)

;; ;; enable AJAX for all pages
;; #;(enable-ajax #t)

;; (ajax-library "https://code.jquery.com/jquery-3.2.1.min.js")
;; (ajax-namespace "ajax")  ; default

;; (enable-sxml #t)

;; (define-page (main-page-path)
;;   (lambda ()
;;     (add-javascript "alert('Hello!');")
;;     `(,(ajax-link "greetings" 'greetings "Hello, awful!"
;;                   (lambda ()
;;                     '(b "Hello, awful!"))
;;                   target: "greetings-reply")
;;       (div (@ (id "greetings-reply")))))
;;   use-ajax: #t)

#;(define-page (main-page-path)
  (lambda ()
    `("Hello, " ,($ 'person "world") "!"))
  ;; enable AJAX for this page
  ;; links jQuery library
  use-ajax: #t)

#;(define-page "/02"
  ;; @ seems to begin specifications of attributes to tags

  ;; ($ 'symbol "string") seems to get a HTTP request variable
  ;; and if the variable does not exist use the default value given as a second argument "string"

  ;; there is no person yet, the default value of "world" will be used

  ;; html elements (actually sxml expressions) are started with their name (p ... )
  ;; they take attributes introduced with the @ syntax as the first argument  and
  ;; they take their content as second argument

  ;; about the ajax procedure:
  ;; (ajax <route handling a request> ;; relative to ajax-namespace: /<ajax-namespace>/<route handling an AJAX request>
  ;;       <id of DOM elem>
  ;;       <event>
  ;;       <precedure to be run>)

  ;;
  (lambda ()
    (ajax "greetings" 'greetings 'click
          ;; a procedure which produces content to go somewhere else
          (lambda ()
            '(b "Hello awful!"))
          ;; where shall the response content go?
          target: "greetings-reply"
          ;; use .on in JS instead of .bind, because .bind is now deprecated
          on: #t)

    ;; ` means "do not evaluate"
    ;; , means evaluate if before there was the effect of `
    `((a (@ (href "#") (id "greetings"))
         "Hello " ,($ 'person "world") "!")
      (div (@ (id "greetings-reply")))))
  use-ajax: #t)

#;(define-page "/03"  ; adding arbitrary JS to a page
  (lambda ()
    (add-javascript "alert('Hello!');")  ; arbitrary JS

    ;; specialized wayof adding an AJAX link
    `(,(ajax-link "greetings" 'greetings "Hello awful!"
                  (lambda () '(b "Hello awful!"))
                  target: "greetings-reply")
      (div (@ (id "greetings-reply")))))
  use-ajax: #t)
