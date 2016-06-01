;; finding seating arrangements for a classroom

(use extras)
(use srfi-1)
(use srfi-26)
(use srfi-69)

(define (random-gender)
  (let ((g (random 2)))
    (cond ((= g 0) 'm)
          (else 'f))))

(define (random-ethnicity)
  (let ((e (random 100)))
    (cond ((< e 10) 'a)
          ((< e 30) 'b)
          ((< e 60) 'c)
          (else 'd))))

(define (int->sym c) (string->symbol
                       (string
                        (integer->char c))))

;; make a database of random students- an alist of names
;; ((name . ((gender . <gender>)
;;            (ethnicity . <ethnicity>)))
;;  (name . <...>))
(define (make-student-list)
  (map (lambda (c)
         (list (int->sym c) `((gender  ,(random-gender))
                              (ethnicity ,(random-ethnicity)))))
       (iota 25 97)))

;; what's the ethnicity of student 'a?
;; (alist-ref 'ethnicity (car (alist-ref 'd student-list)))
       
;; given a list of k->v mappings, count total number of distinct v's
;; and return as a list of v->(count v)
(define (attr-count hash attr)
  (hash-table-fold hash
                   (lambda (k v a)
                     (let* ((this-attr (car (alist-ref attr (car v))))
                            (this-count (alist-ref this-attr a)))
                       (if this-count
                           (alist-update
                            this-attr
                            (add1 this-count)
                            a)
                           (cons (cons this-attr  1) a))))
                   (list)))

;; (define student-hash (alist->hash-table student-list))
;; (attr-count student-hash 'gender)
;; (apply min (map cdr (attr-count student-hash 'ethnicity)))

;; given a list of student names, score a table where those students sit, based on
;; various criteria. returns a list of scores, one for each test
(define (score-table arrangement student-list)
  (let* ((table (alist->hash-table (map (lambda (k) (cons k (alist-ref k student-list)))
                                        arrangement)))
         
         (score-list
          (list
           
           ;; at least two ethnicities
           (let* ((ethnicities (map car (attr-count table 'ethnicity)))
                 (eth-count (length ethnicities)))
            (cond
             ((< eth-count 2) 5)
             (else 0)))
          
          ;; at least two of any ethnicity
          (let* ((eth-counts (map cdr (attr-count table 'ethnicity)))
                 (min-count (apply min eth-counts)))
            (cond
             ((< min-count 2) 5)
             (else 0)))

          ;; at least two genders
          (let* ((genders (map car (attr-count table 'gender)))
                 (gender-count (length genders)))
            (cond
             ((< gender-count 2) 5)
             (else 0)))
          
          ;; at least two of any gender
          (let* ((gender-counts (map cdr (attr-count table 'gender)))
                 (min-count (apply min gender-counts)))
            (cond
             ((< min-count 2) 5)
             (else 0)))
          
          )))
      
    score-list))



;; http://codeimmersion.i3ci.hampshire.edu/2009/09/20/a-simple-scheme-shuffle/
;; this might actually be a chicken builtin
(define shuffle ; Returns a randomly re-ordered copy of list.
  (lambda (list)
    (if (< (length list) 2)
        list
        (let ((item (list-ref list (random (length list)))))
          (cons item (shuffle (remove (lambda (x) (equal? x item)) list)))))))


;; given a genome (an ordered list of student names), split in to table chunks
;; and score each table in turn
(define (score-genome g student-list)
  (fold
   +
   0
   (apply append (map (lambda (t) (score-table t student-list))
                      (chop g 5)))))

;; run and score n random permutations
;; returns a list of (permutation . score) pairs
(define (trial-run n student-list)
    (map (lambda (i)
           (let* ((g (shuffle (map car student-list)))
                  (score (score-genome g student-list)))
             (cons g score)))
         (iota n)))

;; http://stackoverflow.com/questions/23133351/position-of-minimum-element-in-list
;; given a list of solutions, as a list of (genome . score) pairs, find the minimum
;; solution. note that if there are multiple minimum-score solutions, this will
;; return the first one
(define (min-of-trial-run runs)
  (let* ((lst (map cdr runs))
         (min-index   (list-index (cute = (apply min lst) <>) lst)))
    (list-ref runs min-index)))

;; given a table (a list of alists), display table information
(define (display-table kids)
  (map (lambda (t) (begin (display "name ")
                          (display (alist-ref 'name t))
                          (display ", gender ")
                          (display (alist-ref 'gender t))
                          (display ", ethnicity ")
                          (display (alist-ref 'ethnicity t))
                          (newline)))
               kids))

;; given a solution as returned from min-of-trial-run (a (genome . score) pair),
;; report that solution
(define (report-score t student-list)
  (let* ((genome (car t))
         (total-score (cdr t))
         (tables (chop genome 5)))
    (begin (display "Total score ")
           (display total-score)
           (newline)
           (map (lambda (t)
                  (let ((score (score-table t student-list))
                        (kids (map (lambda (s) (cons `(name ,s) (car (alist-ref s student-list)))) t)))
                    (begin (display-table kids)
                           (newline)
                           (display score)
                           (newline) (newline))))
                tables))))
             
(define student-list (make-student-list))


(report-score
 (min-of-trial-run (trial-run 2000 student-list))
 student-list)


;; randomly choose a solution, inverse weighted its score
;; (lower-scoring solutions are more likely to be chosen)
(define (choose-a-solution trial-solutions)
  (lambda ()
  (let* (
         ;; find max score
         (max-score (apply max (map cdr trial-solutions)))
         (diff-scores (map (lambda (v) (expt (- max-score (cdr v)) 6))
                           trial-solutions))
         (z (begin (display (map cdr trial-solutions)) (newline)))
         (x (begin (display diff-scores) (newline)))
         (total-score (fold + 0 diff-scores))
       
         ;; chose a random point on the (0, total-score) line
         (point (random total-score)))
  
  (letrec (;; figure out what solution lies there
           (recc (lambda (solutions acc i)
                   (let* ((solution (car solutions))
                          (new-acc (+ acc (expt (- max-score (cdr solution)) 6))))
                     (if (> new-acc point)
                         i
                         (recc (cdr solutions) new-acc (+ i 1)))))))
    (begin (display total-score) (display " ") (display point) (newline)
           (list-ref
            trial-solutions
            (recc trial-solutions 0 0)))))))

(define trial-solutions (trial-run 2000 student-list))
(define chooser (choose-a-solution trial-solutions))
(chooser)
(min-of-trial-run trial-solutions)
(map cdr trial-solutions)
