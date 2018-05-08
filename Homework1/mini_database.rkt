#lang racket

(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(define init-database
  (λ ()
    '()))

(define create-table
  (λ (table columns-name)
    (cons table (list columns-name))))

(define get-name
  (λ (table)
    (car table)))

(define get-columns
  (λ (table)
    (cadr table)))

(define get-tables
  (λ (db)
    db))

(define get-table
  (λ (db table-name)
    (if (null? (filter (λ (e)
              (equal? (car e) table-name)) db))
        '()
        (car (filter (λ (e)
              (equal? (car e) table-name)) db)))))

(define add-table
  (λ (db table)
    (cons table db)))

(define remove-table
  (λ (db table-name)
    (filter (λ (e)
              (not (equal? (car e) table-name))) db)))

;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================

(define db
  (list (list "Studenți"
              (list "Număr matricol" "Nume" "Prenume" "Grupă" "Medie")
              (list 123 "Ionescu" "Gigel" "321CA" 9.82)
              (list 124 "Popescu" "Maria" "321CB" 9.91)
              (list 125 "Popa" "Ionel" "321CC" 9.99)
              (list 126 "Georgescu" "Ioana" "321CD" 9.87))
        (list "Cursuri"
              (list "Anul" "Semestru" "Disciplină" "Număr credite" "Număr teme")
              (list "I" "I" "Programarea calculatoarelor" 5 2)
              (list "II" "II" "Paradigme de programare" 6 3)
              (list "III" "I" "Algoritmi paraleli și distribuiți" 5 3)
              (list "IV" "I" "Inteligență artificială" 6 3)
              (list "I" "II" "Structuri de date" 5 3)
              (list "III" "II" "Baze de date" 5 0))))

;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================

(define construct-record
  (λ (columns record acc)
    (if (null? columns)
        acc
        (construct-record (cdr columns)
                          record
                              (append acc
                                      ((λ (column record)
                                        (if (assoc column record)
                                            (list (cdr (assoc column record)))
                                            (list NULL))) (car columns) record)))))) 

(define insert-record-in-table
  (λ (table record)
    (cons (car table) (cons (cadr table)
                            (append (cddr table)
                                    (list (construct-record (get-columns table) record '())))))))

(define insert
  (λ (db table-name record)
    (add-table (remove-table db table-name)
               (insert-record-in-table (get-table db table-name) record))))


;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================

(define get-entries
  (λ (db table-name)
    (cddr (get-table db table-name))))

(define get-column
  (λ (entries total-len len)
    (if (equal? total-len len)
        (map car entries)
        (get-column (map cdr entries) (sub1 total-len) len))))

(define simple-select-helper
  (λ (columns table-columns entries total-len acc)
    (if (null? columns)
        acc
        (if (member (car columns) table-columns)
            (simple-select-helper (cdr columns)
                                  table-columns
                                  entries
                                  total-len
                                  (append acc (list (append (list (car columns)) (get-column
                                                                                  entries
                                                                                  total-len
                                                                                  (length (member
                                                                                           (car columns)
                                                                                           table-columns)))))))
            (simple-select-helper (cdr columns)
                                  table-columns
                                  entries
                                  total-len
                                  (append acc (list (append (list (cdar columns)) (get-column
                                                                                  entries
                                                                                  total-len
                                                                                  (length (member
                                                                                           (cdar columns)
                                                                                           table-columns)))))))))))

(define simple-select
  (λ (db table-name columns)
    (if (member '() (map cdr (simple-select-helper columns
                                   (get-columns (get-table db table-name))
                                   (get-entries db table-name)
                                   (length (get-columns (get-table db table-name)))
                                   '())))
        '()
        (map cdr (simple-select-helper columns
                                   (get-columns (get-table db table-name))
                                   (get-entries db table-name)
                                   (length (get-columns (get-table db table-name)))
                                   '())))))
   
;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================

(define member-in-condition
  (λ (name conditions total-len len)
    (if (equal? total-len len)
        (car conditions)
        (member-in-condition name (cdr conditions) (sub1 total-len) len))))

(define deleteNth
  (λ (n  l)
    (cond
      [(= n 0) (rest l)]
      [(< n (length l)) (append (take l n) (rest (drop l n)))]
      [else l])))
  

(define delete-column
  (λ (data n acc)
    (if (null? data)
        acc
        (delete-column (cdr data) n (append acc (list (deleteNth n (car data))))))))
    
(define get-pos
  (λ (column total-len comparator value acc)
    (if (null? column)
        (map add1 acc)
        (if (equal? (car column) NULL)
            (get-pos (cdr column) total-len comparator value (append acc (list (- total-len (length column)))))
            (if (not (comparator (car column) value))
                (get-pos (cdr column) total-len comparator value (append acc (list (- total-len (length column)))))
                (get-pos (cdr column) total-len comparator value acc))))))
    

(define remove-columns-select
  (λ (data positions)
    (if (null? positions)
        data
        (remove-columns-select (delete-column data (car positions) '()) (map sub1 (cdr positions))))))
        
(define constraints-select
  (λ (conditions data)
    (if (null? conditions)
        data
        (constraints-select (cdr conditions)
                            (remove-columns-select data
                                             (get-pos (cdr (assoc (second (car conditions)) data))
                                                      (sub1 (length (car data)))
                                                      (first (car conditions))
                                                      (third (car conditions))
                                                      '()))))))

(define select-helper
  (λ (data columns acc)
    (if (null? columns)
        acc
        (if (pair? (car columns))
            (select-helper data (cdr columns) (append acc (list (assoc (cdr (car columns)) data))))
            (select-helper data (cdr columns) (append acc (list (assoc (car columns) data))))))))

(define (remove-duplicates l)
  (foldr (lambda (x y) (cons x (filter (lambda (z) (not (equal? x z))) y))) empty l))

(define apply-operation
  (λ (data op)
    (cond
      ((equal? op 'min) (apply min data))
      ((equal? op 'max) (apply max data))
      ((equal? op 'count) (length (remove-duplicates data)))
      ((equal? op 'sum) (apply + data))
      ((equal? op 'avg) (/ (apply + data) (length data)))
      ((equal? op 'sort-asc) (sort data <))
      ((equal? op 'sort-desc) (sort data >))
      (else '()))))

(define operations 
  (λ (columns data acc)
    (if (null? columns)
        acc
        (if (pair? (car columns))
            (operations (cdr columns)
                        data
                        (append acc (list (apply-operation (cdr (assoc (cdr (car columns)) data)) (car (car columns))))))
            (operations (cdr columns)
                        data
                        (append acc (list (cdr (assoc (car columns) data)))))))))

(define select
  (λ (db table-name columns conditions)
    (operations columns
                (select-helper (constraints-select conditions
                                            (simple-select-helper (get-columns (get-table db table-name))
                                                                  (get-columns (get-table db table-name))
                                                                  (get-entries db table-name)
                                                                  (length (get-columns (get-table db table-name)))
                                                                  '()))
                               columns
                               '())
                '())))

;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================

(define add-index
  (λ (l acc seed)
    (if (null? l)
        acc
        (add-index (cdr l) (append acc (list (cons (car l) seed))) (add1 seed)))))

(define replace-info-helper
  (λ (l value positions)
    (if (null? positions)
        (map car l)
        (replace-info-helper (map (λ (e)
                                    (if (equal? (cdr e) (car positions))
                                        (cons value (cdr e))
                                        e)) l) value (cdr positions)))))

(define replace-info
  (λ (l value positions)
    (if (null? positions)
        l
        (replace-info-helper (add-index l '() 0) value positions))))

(define update-helper
  (λ (data values positions acc)
    (if (null? data)
        acc
        (if (false? (assoc (caar data) values))
            (update-helper (cdr data) values positions (append acc (list (car data))))
            (update-helper (cdr data) values positions (append acc (list (replace-info (car data) (cdr (assoc (caar data) values)) positions))))))))

(define get-pos-update
  (λ (column total-len comparator value acc)
    (if (null? column)
        (map add1 acc)
        (if (equal? (car column) NULL)
            (get-pos-update (cdr column) total-len comparator value acc)
            (if (comparator (car column) value)
                (get-pos-update (cdr column) total-len comparator value (append acc (list (- total-len (length column)))))
                (get-pos-update (cdr column) total-len comparator value  acc)))))) 
 
(define (intersection a b)
  (if (null? a)
      '()
      (if (member (car a) b)
          (cons (car a) (intersection (cdr a) b))
          (intersection (cdr a) b))))


(define constraints-update
  (λ (conditions values data acc)
    (if (null? conditions)
        (update-helper data values acc '())
        (constraints-update (cdr conditions) values data (intersection acc
                                                                (get-pos-update (cdr (assoc (second (car conditions)) data))
                                                                                (sub1 (length (car data)))
                                                                                (first (car conditions))
                                                                                (third (car conditions))
                                                                                '()))))))

(define update
  (λ (db table-name values conditions)
    (add-table (remove-table db table-name)
               (recreate-table table-name 
                               (constraints-update
                                conditions
                                values                   
                                (simple-select-helper (get-columns (get-table db table-name))
                                                      (get-columns (get-table db table-name))
                                                      (get-entries db table-name)
                                                      (length (get-columns (get-table db table-name)))
                                                      '())
                                (stream-take naturals (length (get-entries db table-name))))))))

;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================

(define remove-columns-delete
  (λ (data positions)
    (if (null? positions)
        data
        (remove-columns-delete (delete-column data (car positions) '()) (map sub1 (cdr positions))))))

(define get-pos-delete
  (λ (column total-len comparator value acc)
    (if (null? column)
        (map add1 acc)
        (if (equal? (car column) NULL)
            (get-pos-delete (cdr column) total-len comparator value acc)
            (if (comparator (car column) value)
                (get-pos-delete (cdr column) total-len comparator value (append acc (list (- total-len (length column)))))
                (get-pos-delete (cdr column) total-len comparator value  acc)))))) 


(define constraints-delete
  (λ (conditions data acc)
    (if (null? conditions)
        (remove-columns-delete data acc)
        (constraints-delete (cdr conditions) data (intersection acc
                                                                (get-pos-delete (cdr (assoc (second (car conditions)) data))
                                                                                (sub1 (length (car data)))
                                                                                (first (car conditions))
                                                                                (third (car conditions))
                                                                                '()))))))

(define recreate-table-helper
  (λ (data acc)db
    (if (member '() data)
        '()
        (if (equal? 1 (length (car data)))
            (append acc (list (map car data)))
            (recreate-table-helper (map cdr data) (append acc (list (map car data)))))))) 

(define recreate-table 
  (λ (table-name data)
    (cons table-name (append (list (map car data)) (recreate-table-helper (map cdr data) '())))))

; take n numbers from a stream
(define (stream-take s n)
  (cond ((zero? n) '())
        ((stream-empty? s) '())
        (else (cons (stream-first s)
                    (stream-take (stream-rest s) (- n 1))))))

; natural numbers stream
(define naturals
  (let loop ((seed 1))
    (stream-cons seed (loop (add1 seed)))))

(define delete
  (λ (db table-name conditions)
    (add-table (remove-table db table-name)
               (recreate-table table-name (constraints-delete conditions
                                                              (simple-select-helper (get-columns (get-table db table-name))
                                                                (get-columns (get-table db table-name))
                                                                (get-entries db table-name)
                                                                (length (get-columns (get-table db table-name)))
                                                                '())
                                                              (stream-take naturals (length (get-entries db table-name))))))))

;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================


(define natural-join
  (λ (db tables columns conditions)
     '()))		