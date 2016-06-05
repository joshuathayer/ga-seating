;; finding seating arrangements for a classroom

;; re: loading modules- https://lists.gnu.org/archive/html/chicken-users/2016-03/msg00019.html
(use extras)
(use srfi-1) ;; lists
(use srfi-26)
(use srfi-69)

;; compile this with $ csc -J -s students.scm
(use students)

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
  (let* ((student-names (map car student-list))
         (table (alist->hash-table (map (lambda (k) (cons k (alist-ref k student-list)))
                                        arrangement)))
         
         (score-list
          (list
           
           ;; at least two ethnicities
           (let* ((ethnicities (map car (attr-count table 'ethnicity)))
                 (eth-count (length ethnicities)))
            (cond
             ((< eth-count 2) 20)
             (else 0)))
          
          ;; at least two of any ethnicity
          (let* ((eth-counts (map cdr (attr-count table 'ethnicity)))
                 (min-count (apply min eth-counts)))
            (cond
             ((< min-count 2) 20)
             (else 0)))

          ;; at least two genders
          (let* ((genders (map car (attr-count table 'gender)))
                 (gender-count (length genders)))
            (cond
             ((< gender-count 2) 20)
             (else 0)))
          
          ;; at least two of any gender
          (let* ((gender-counts (map cdr (attr-count table 'gender)))
                 (min-count (apply min gender-counts)))
            (cond
             ((< min-count 2) 20)
             (else 0)))))

         ;; scores are (student-1 student-2 score)
         (avoid-pair-scores
          (map (lambda (p) (if (and (memq (car p) arrangement)
                                    (memq (cadr p) arrangement))
                               (caddr p)
                               0)) avoid-pairs))
         
         ;; this is bogus- we get a strong negative score if a student has
         ;; multiple compatible peers, when we probbaly only need one
         (prefer-pair-scores
          (map (lambda (p)
                 (if (and (memq (car p) arrangement)
                          (memq (cadr p) arrangement))
                     (caddr p)
                     10)) prefer-pairs)))
    
    (append score-list avoid-pair-scores prefer-pair-scores)))


;; (report-score
;;  (min-of-population (trial-run 2000 student-list))
;;  student-list)


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
(define (random-population n student-list)
    (map (lambda (i)
           (let* ((g (shuffle (map car student-list)))
                  (score (score-genome g student-list)))
             (cons g score)))
         (iota n)))

;; create n random student lists
(define (random-genomes n student-list)
  (map (lambda (i)
         (shuffle (map car student-list)))
       (iota n)))


;; http://stackoverflow.com/questions/23133351/position-of-minimum-element-in-list
;; given a list of solutions, as a list of (genome . score) pairs, find the minimum
;; solution. note that if there are multiple minimum-score solutions, this will
;; return the first one
(define (min-of-population runs)
  (let* ((lst (map cdr runs))
         (min-index   (list-index (cute = (apply min lst) <>) lst)))
    (list-ref runs min-index)))

(define (max-of-population runs)
  (let* ((lst (map cdr runs))
         (max-index   (list-index (cute = (apply max lst) <>) lst)))
    (list-ref runs max-index)))


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

;; given a solution as returned from min-of-population (a (genome . score) pair),
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
             
;; (define student-list (make-student-list))


;; (report-score
;;  (min-of-population (random-population 2000 student-list))
;;  student-list)

;; randomly choose a solution, inverse weighted its score
;; (lower-scoring solutions are more likely to be chosen)
(define (choose-a-solution trial-solutions)

  (let* (
         ;; find max score
         (max-score (apply max (map cdr trial-solutions)))
         (diff-scores (map (lambda (v) (expt (- max-score (cdr v)) 1))
                           trial-solutions))
         ;; (z (begin (display (map cdr trial-solutions)) (newline)))
         ;; (x (begin (display diff-scores) (newline)))
         (total-score (fold + 0 diff-scores)))


    (lambda ()
      
      (letrec ((recc (lambda (point solutions acc i)
                       (let* ((solution (car solutions))
                              (new-acc (+ acc (expt (- max-score (cdr solution)) 1))))
                         (if (> new-acc point)
                             i
                             (recc point (cdr solutions) new-acc (+ i 1)))))))
        (begin ;; (display total-score) (newline)
               (list-ref
                trial-solutions
                (recc (random total-score) trial-solutions 0 0)))))))

;; swap a single pair of elements in a list
(define (mutate-solution solution)
  (if (< (length solution) 2)
      solution
      (let ((a (list-ref solution (random (length solution))))
            (b (list-ref solution (random (length solution)))))
        (map (lambda (n) (cond ((equal? n a) b)
                               ((equal? n b) a)
                               (else n))) solution))))

;; chunk a in to groups of five
;; take the first three of each group
;; determine which memebers are missing
;; take somehow intelligently from b? that's weird, there are totally invalid
;; solutions if we, per group, just take a random
  

(define (repeatedly-mutate genome chance)
  (if (> chance (random 100))
      (repeatedly-mutate (mutate-solution genome) chance)
      (mutate-solution genome)))


;; (repeatedly-mutate '(a b c d e f g h i j k l m n o p q r s t u v w x y) 60)


(define shuffle ; Returns a randomly re-ordered copy of list.
  (lambda (list)
    (if (< (length list) 2)
        list
        (let ((item (list-ref list (random (length list)))))
          (cons item (shuffle (remove (lambda (x) (equal? x item)) list)))))))

;; ( (genome . score) .. ) -> ( (genome . score) .. )
(define (choose-and-recur chooser n acc)
  (if (= n 0)
      acc
      (cons (chooser)
            (choose-and-recur chooser (- n 1) acc))))


(define (evolve-inner student-list scored-genomes i mutation-chance chooser)
  (begin
    (display (cdr (min-of-population scored-genomes)))
    (display " ")
    (if (= i 0)
        (min-of-population scored-genomes)
        (let* ((new-genomes (choose-and-recur chooser
                                            (- (length scored-genomes) 1) '()))
             
             ;; ((genome, score), .. ) -> ((genome), .. )
             (mutated-genomes (map (lambda (sg)
                                        (repeatedly-mutate (car sg) mutation-chance))
                                   new-genomes))
             
             ;; ((genome), .. ) -> ((genome, score), ..)
             (new-scored-genomes (map (lambda (x)
                                           (cons x (score-genome x student-list)))
                                      mutated-genomes))
             )
          (evolve-inner
           student-list
           (cons
            ;; keep the most fit (what's that called? elitsm)
            (min-of-population scored-genomes)
        
            new-scored-genomes)
           (- i 1)
           mutation-chance
           chooser)))))

(define (evolve student-list scored-population i mutation-chance)
  (let ((chooser (choose-a-solution scored-population)))
    (evolve-inner student-list scored-population i mutation-chance chooser)))


(define solved
  (evolve student-list
          (random-population 100 student-list)
          500
          20))
(newline)
(report-score solved student-list)

;; (define trial-solutions (random-population 10 student-list))
;; (define chooser (choose-a-solution trial-solutions))
;; (chooser)
;; (map cdr (choose-and-recur chooser 5 '()))
;; (min-of-population trial-solutions)
;; (min-of-population (random-population 10 student-list))
;; (map cdr trial-solutions)
;; (map cdr trial-solutions)
