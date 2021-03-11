;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; $Id$
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Code for auto-generating synchronized SQL defn's and Modula-3 code
;;
;; Copyright (c) 2008, 2009, Generation Capital Ltd.  All rights reserved.
;;
;; Author: Mika Nystrom <mika@alum.mit.edu>
;;
;;
;; A database consists of tables
;; tables consist of fields
;; e.g.
;;
;;
;; (make-database "db" 
;;  (make-table "table1" 
;;   (make-field 'writer "f1" 'data-type1 'data-option1 'data-option2 ...)
;;    ...
;;  )
;; )
;;
;; This code uses and is synchronized with:
;; 1. Database and DatabaseTable Modula-3 interfaces in htmltable lib.
;; 1a.DesynchronizedDB in ratsql (Rational SQL)
;; 2. templates in mscheme library
;; 3. generic TableMonitor.{im}g
;; 4. scheme-lib/src/scodegen.scm (that is this file, actually!)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-modules "basic-defs" "display" "time")

;; 
;; The fact that fields can be written by both client and server
;; can really complicate things.
;;
;; Because the server holds an internal cache (in Modula-3),
;; an incorrect design would have consistency issues.
;; For this reason, we limit ourselves to a subset of possible read
;; and write operations.
;;
;; Every table falls into one of the following categories:
;; 
;; 'server     written only by Modula-3 (central) server
;; 'client     written only by clients
;;
;; WE ARE MOVING AWAY FROM THESE RESTRICTIONS... most of what the
;; client and server can do is now identical.
;;

(define (map-filter-assoc tag lst)
  (map cdr (filter (lambda (x) (eq? (car x) tag)) lst)))

;;
;; PUBLIC ROUTINES FOR BUILDING THE INPUTS TO THE BELOW CODE
;;

(define (make-table name owner . x) (append (list 'table name owner) x))

(define (make-field name type . x)  (append (list 'field name type) x))

(define (make-index cols . opts)(append (list 'index cols) opts))

(define (symbol-intersection l1 l2) (filter (lambda (x1) (memq x1 l2)) l1))

(define (make-database name . x) (cons name x))

;;;;;

(define (db-name db) (car db))

(define (get-table-names db) (map car (cdr db)))

(define (get-tables db)      (map-filter-assoc 'table (cdr db)))

(define (get-fields tab)     (map-filter-assoc 'field (cddr tab)))

(define (get-indexes tab)    (map-filter-assoc 'index (cddr tab)))

;; after setting things up with the above routines, call
;;
;;   write-database-sql 
;;   write-database-m3 
;;
;; as desired.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define port '()) ;; this is an ugly global for now (makes things 
                  ;; a little bit easier to debug...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           INTER-LANGUAGE DATA TYPE DEFINITIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (m3-sym a b) (cons a b))
(define (m3-intf s) (car s))
(define (m3-nam s) (cdr s))

(define int-conversion
  ;; convert integers SQL<->M3
  (list identity
  (m3-sym "Fmt" "Int")
  (m3-sym "Scan" "Int") 
  (list (m3-sym "Lex" "Error")
        (m3-sym "FloatMode" "Trap"))))

(define lr-conversion
  ;; convert doubles SQL<->M3
  (list identity
  (m3-sym "Fmt" "LongReal")
  (m3-sym "Scan" "LongReal") 
  (list (m3-sym "Lex" "Error")
        (m3-sym "FloatMode" "Trap"))))

(define bool-conversion
  ;; convert booleans SQL<->M3
  (list identity 
        (m3-sym "Fmt" "Bool")
        (m3-sym "PGSQLScan" "Bool") 
        (list (m3-sym "Lex" " Error"))))

(define str-conversion
  ;; convert strings SQL<->M3
  (list identity 
        (m3-sym "Database" "EscapeQ")
        (m3-sym "Database" "Unescape")
        '()))

(define ts-conversion
  ;; convert timestamps SQL<->M3
  (list (lambda (fieldname) (string-append "extract (\'epoch\' from " 
             fieldname ")"))
  (m3-sym "Fmt"
    (lambda (x) (string-append "\"timestamptz \'epoch\' + \" & Fmt.LongReal(" 
              x 
              ") & \"* interval \'1 second\'\"")))
  (m3-sym "Scan" "LongReal") 
  (list (m3-sym "Lex" "Error")
        (m3-sym "FloatMode" "Trap"))))

;;
;; NOW LIST ALL THE TYPES WE KNOW ABOUT AND CAN HANDLE...
;;

(define types
  (list
   ;; sym-name, SQL-name, m3-name, m3-must-import, sql-m3-read
   (list 'serial           "serial"           "INTEGER"   #f  int-conversion  )
   (list 'varchar          "varchar"          "TEXT"      #f  str-conversion  )
   (list 'integer          "integer"          "INTEGER"   #f  int-conversion  )
   (list 'double-precision "double precision" "LONGREAL"  #f  lr-conversion   )
   (list 'boolean          "boolean"          "BOOLEAN"   #f  bool-conversion )
   (list 'timestamp        "timestamp with time zone" 
                                              "XTime"     #t  ts-conversion   )
   (list 'id               "integer"          "INTEGER"   #f  int-conversion  )  
   (list 'date             "date"             "TEXT"      #f  str-conversion  ) ;; hack
   )
)

(define (find-type type action types)
  ;; extract a type defn from types table
  (define (f lst t)
    (cond ((null? lst) (error "unknown type "  type))
    ((eq? (caar lst) t) (action (car lst)))
    (else (f (cdr lst) t))))

  (if (list? type) 
      (f types (car type))
      (f types type)))

;; get various details about a known type...
(define (get-conversion type) (nth type 4))

(define (type->sql-string type) (find-type type cadr types))

(define (type->m3-typename type) 
  (if (m3-import? type)
      (string-append (find-type type caddr types) ".T")
      (find-type type caddr types))
)

(define (type->m3-intfname type) (nth (find-type type identity types) 2))

(define (m3-import? type) (nth (find-type type identity types) 3))
;; does the type require extra M3 imports?

(define non-printing-options (append '(not-updatable)))

(define (options-string db lst) 
  ;; SQL column options
  (define (stringify-default def)
    (cond ((string? def) def)
    (else (stringify def))))

  (if (null? lst) 
      ""
      (string-append
       (cond ((list? (car lst))
        (if (eq? (caar lst) 'default) 
      (string-append " default " (stringify-default (cadar lst)))))
       ((eq? (car lst) 'not-null) " not null ")
       ((eq? (car lst) 'primary-key) " primary key ")
       ((memq (car lst) non-printing-options) "")
       (else (error "Unknown option " (car lst))))
       (options-string db (cdr lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    CODE GENERATION....
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (put-header db name) (dis dnl "create table " name dnl "(" dnl port))
(define (put-trailer db)  (dis ");" dnl dnl port))

(define (put-field-list db field)
  (let ((name (car field))
  (type (cadr field))
  (options (cddr field)))

    (dis "  " name " " (type->sql-string type) " " (options-string db options)
   port)))

(define (put-field db . x) (put-field-list db x))

(define (make-drop-table-sql tbl db)
  (let ((name (car tbl)))
    (dis "drop table " name " cascade;" dnl port)))

(define (make-write-seq p)
  (dis "create sequence write_seq;" dnl p))

(define (drop-write-seq p)
  (dis "drop sequence write_seq cascade;" dnl p))



(define (make-table-sql tbl db)
  (let ((name (car tbl))
  (fields (get-fields (complete-tbl tbl))))
    
    (put-header db name)
    (map2 (lambda (f) (begin (put-field-list db f)
           (dis "," dnl port)))

    (lambda (f) (put-field-list db f))
    
    fields)
    (dis dnl port)
    (put-trailer db)
    #t
))

(define (putm3-field-list fld port)
    (let ((name (car fld))
    (type (cadr fld)))

      (dis "  " name " : " (type->m3-typename type) ";" dnl port)
      (dis "  " name "_isNull : BOOLEAN := TRUE;" dnl port)

))

(define (field-cant-be-null? attr-lst)
  (or (memq 'not-null attr-lst)
      (memq 'primary-key attr-lst)))

(define (field-updatable? attr-lst)
  (not (memq 'not-updatable attr-lst)))

(define (put-getter-procedure fld ip mp)
  (let ((name (car fld))
        (not-null (field-cant-be-null? (cddr fld)))
        (m3type (type->m3-typename (cadr fld))))

    ;;(dis "put-getter-procedure " let " : " fld dnl dnl '())

    (map
     (lambda(port)
       (dis "PROCEDURE Get_" name "(READONLY t : T) : " m3type (if not-null "" " RAISES { DBTable.IsNull }")
      port))
     (list ip mp))

    (dis ";" dnl ip)

    (dis " =" dnl 
         (if not-null (string-append 
   "  <*FATAL DBTable.IsNull*>" dnl) "" )
   "  BEGIN" dnl
   "    IF t." name "_isNull THEN" dnl
         "      RAISE DBTable.IsNull(TableName & \"." name "\")" dnl
   "    ELSE" dnl
         "      RETURN t. " name dnl
   "    END" dnl
         "  END Get_" name ";" dnl dnl
   mp)
))
    
(define (put-setter-procedure fld ip mp)
  (let ((name (car fld))
        (m3type (type->m3-typename (cadr fld))))

    (if (field-updatable? (cddr fld))
        (begin
          (map
           (lambda(port)
             (dis "PROCEDURE Set_" name "(VAR t : T; val : " m3type ")"
                  port))
           (list ip mp))
          
          (dis ";" dnl ip)
          
          (dis " =" dnl 
               "  BEGIN" dnl
               "    t." name "_isNull := FALSE;" dnl
               "    t." name " := val" dnl
               "  END Set_" name ";" dnl dnl
               mp)
          ))))
    
(define (put-clearer-procedure fld ip mp)
  (let ((name (car fld))
        (not-null (field-cant-be-null? (cddr fld)))
        (m3type (type->m3-typename (cadr fld))))
    (if (or not-null (not (field-updatable? (cddr fld)))) #f
        (begin (map
                (lambda(port)
                  (dis "PROCEDURE Clear_" name "(VAR t : T)"
                       port))
                (list ip mp))
               
               (dis ";" dnl ip)
               
               (dis " =" dnl 
                    "  BEGIN" dnl
                    "    t." name "_isNull := TRUE;" dnl
                    "  END Clear_" name ";" dnl dnl
                    mp)
        ))))
    
(define (put-have-procedure fld ip mp)
  (let ((name (car fld))
        (not-null (field-cant-be-null? (cddr fld)))
        (m3type (type->m3-typename (cadr fld))))
    (map
     (lambda(port)
       (dis "PROCEDURE Have_" name "(READONLY t : T; VAR " name " : " m3type ") : BOOLEAN "
      port))
     (list ip mp))

    (dis ";" dnl ip)

    (dis " =" dnl 
   "  BEGIN" dnl
   "    IF t." name "_isNull THEN" dnl

   (if not-null
   "      <*ASSERT FALSE*>"
   "      RETURN FALSE"
   ) dnl

   "    ELSE" dnl
   "      " name " := t." name ";" dnl
   "      RETURN TRUE" dnl
   "    END" dnl
   "  END Have_" name ";" dnl dnl
   mp)
))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    VARIOUS MODULA-3 ROUTINES FOLLOW
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ass-sql fld)
  ;; the string corresponding to a SQL assignment (coded in M3)
  (let* ( (name (car fld))
          (m3name (string-append "record." name))
          (type-name (cadr fld)) )
    
    (string-append 
     "    IF NOT " m3name "_isNull THEN" dnl
     "      query.addhi(\"" name "=\"&" (format-sql-from-m3 m3name type-name) ")"
     dnl
     "    END;" dnl)))


(define (dis-update-or-insert-m3 tbl mp)
  (let ((tbl-name (car tbl)))
    (dis
     "  <*FATAL FloatMode.Trap, Lex.Error*>" dnl
     "  VAR query := \"select count(*) from " tbl-name " where \"&" dnl
     "                        restriction;" dnl
     "      attempts := 5;" dnl
     "  BEGIN" dnl
     "    db.sync();" dnl
     "    LOOP" dnl
     "      TRY" dnl
     "        WITH cnt = db.sExec(query, abortConnectionOnFail := attempts <= 1).getInt(\"count\") DO" dnl
     "          IF cnt > 0 THEN" dnl
     "            WITH id = db.sExec(\"select " tbl-name "_id from " tbl-name " where \"& restriction, abortConnectionOnFail := attempts <= 1).getInt(\"" tbl-name "_id\") DO" dnl
     "              VAR r := record; BEGIN " dnl
     "                r." tbl-name "_id := id;" dnl
     "                Update(db,r,ex)" dnl
     "               END" dnl
     "            END" dnl
     "          ELSE" dnl
     "            Insert(db,record,ex)" dnl
     "          END" dnl
     "        END(*WITH*);" dnl
     "        EXIT" dnl
     "      EXCEPT" dnl
     "        DBerr.Error(txt) =>" dnl
     "          Debug.Out(\"caught DBerr.Error, \" & txt & \" attempts = \""dnl
     "             & Fmt.Int(attempts));" dnl
     "          DEC(attempts);" dnl
     "          IF attempts = 0 THEN" dnl 
     "            ex.exception(query,txt);" dnl 
     "            EXIT" dnl
     "          ELSE" dnl
     "            Thread.Pause(NEW(Random.Default)." dnl
     "                  init().longreal(1.0d0,3.0d0))" dnl
     "          END" dnl
     "      END" dnl
     "    END" dnl
     "  END" dnl
     mp)
    #t))

(define (dis-update-m3 tbl mp)
  (let ((tbl-name (car tbl)) 
        (fields (get-fields (complete-tbl tbl))))
    (dis "  VAR query := NEW(TextSeq.T).init(); BEGIN " dnl mp)
    (map (lambda(fld) (dis (ass-sql fld) mp)) fields) 
    (dis dnl mp)

    (dis "    WITH q1 = \"update " tbl-name " set \" & TextUtils.FormatInfix(query,\",\") & \" where " 
   tbl-name "_id=\"&Fmt.Int(record." tbl-name "_id)&\";\", " dnl mp)
    (dis "         q2 = \"delete from clean where tabl='" tbl-name "' and rowid=\"&Fmt.Int(record." tbl-name "_id)&\";\"" dnl mp)
    (dis "      DO" dnl mp)

    (dis "      db.aExec(q1 & q2, ex, MakeResCallback(db));" dnl mp)
    (dis "    END " mp)
    (dis "  END " mp)
    #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (format-sql-from-m3 m3name type-name)
  (let* ((type (find-type type-name identity types))
   (conv (get-conversion type))
   (m3->sql (cadr conv)))
    (cond
      ;;
      ;; there are several legal ways of representing the 
      ;; conversion.  could either be a procedure or else
      ;; a Pair, representing the m3 interface and unqualified name
      ;; within that interface, of a standard M3 routine.
      ;;
      ;; (see the types table above)
      ;;
      ((procedure? m3->sql)       (m3->sql       m3name))
      ((procedure? (cdr m3->sql)) ((cdr m3->sql) m3name))
      (else
       (string-append (car m3->sql) "." (cdr m3->sql) "("
          m3name ")")))))

(define (ins-sql fld)
  ;; the string corresponding to a SQL assignment (coded in M3)
  (let* ((name (car fld))
   (m3name (string-append "record." name))
   (type-name (cadr fld)))
    (string-append 
     "    IF NOT " m3name "_isNull THEN" dnl
     "      fields.addhi(\" " name "\");" dnl
     "      values.addhi(" (format-sql-from-m3 m3name type-name) ")" dnl
     "    END;" dnl
     )))

(define (dis-nullset-m3 tbl mp)
  (let ((tbl-name (car tbl)) 
  (fields (get-fields (complete-tbl tbl))))
    (dis "  VAR" dnl mp)
    (dis "    res := FieldSet {};" dnl mp)
    (dis "  BEGIN" dnl mp)
    (map (lambda (fld) 
     (let ((fld-name (car fld)))
       (dis "    IF record."fld-name"_isNull THEN" dnl mp)
       (dis "      res := res + FieldSet { Field." fld-name "}" dnl mp)
       (dis "    END;" dnl mp)))
   fields)
    (dis "    RETURN res" dnl mp)
    (dis "  END " mp)
    ))

(define (dis-insert-m3 tbl mp)
  (let ((tbl-name (car tbl)) 
  (fields (get-fields (complete-tbl tbl))))
    (dis "  VAR fields, values := NEW(TextSeq.T).init();" dnl
         "  BEGIN" dnl mp)

    (map (lambda(fld) (dis (ins-sql fld) mp))
   fields) 

    (dis dnl mp)
    (dis "    db.aExec(\"insert into " tbl-name " (\" & TextUtils.FormatInfix(fields,\",\") & \")\"&"
   dnl mp)
    (dis "                 \" values (\"& TextUtils.FormatInfix(values,\",\")&\")\", ex, MakeResCallback(db))" dnl mp)
    (dis "  END " mp)
    #t))


(define (dis-batch-insert-m3 tbl mp)
  (let ((tbl-name (car tbl)) 
  (fields (get-fields (complete-tbl tbl))))
    (dis "  VAR maxFields, minFields : FieldSet;"           dnl mp)
    (dis "  BEGIN"                                          dnl mp)
    
    ;; anything to insert?
    
    (dis "    IF NUMBER(record) = 0 THEN RETURN END;"       dnl mp)

    ;; deleteFirst is used for fault-tolerant installations that
    ;; may re-use insert ids.  Because the caches are empty on a restart 
    ;; we may need to delete entries first
    ;; note that we don't regularly want to do this due to the overhead
    ;; of making the query first.

    (dis "    IF deleteFirst THEN"                                  dnl mp)
    (dis "      VAR wx := Wx.New(); BEGIN"                          dnl mp)
    (dis "        Wx.PutText(wx, \"delete from " tbl-name " where false \"); " dnl mp)
    (dis "        FOR i := FIRST(record) TO LAST(record) DO"        dnl mp)
    (dis "          Wx.PutText(wx, \" or (" tbl-name "_id = \");"   dnl mp)
    (dis "          Wx.PutInt (wx, record[i]." tbl-name "_id);"     dnl mp)
    (dis "          Wx.PutText(wx, \")\")"                          dnl mp)
    (dis "        END;"                                             dnl mp)
    (dis "        db.aExec(Wx.ToText(wx), ex, MakeResCallback(db))" dnl mp)
    (dis "      END"                                                dnl mp)
    (dis "    END;"                                                 dnl mp)

    ;; check to see if all inserts modify the SAME fields.
    ;; if so we use UNION SELECT to perform the inserts in a single
    ;; query (below).  Else we fall back to doing the inserts one at a time
    ;; sequentially.

    (dis "    maxFields := AllFields-NullSet(record[0]);"   dnl mp)
    (dis "    minFields := AllFields-NullSet(record[0]);"   dnl mp)
    (dis "    FOR i := 1 TO LAST(record) DO"                dnl mp)
    (dis "      WITH nri = AllFields-NullSet(record[i]) DO"           dnl mp)
    (dis "        maxFields := maxFields + nri; minFields := minFields * nri" dnl mp)
    (dis "      END"                                        dnl mp)
    (dis "    END;"                                         dnl mp)
    (dis "    IF maxFields # minFields THEN (*fallback*)"   dnl mp)
    (dis "      FOR i := FIRST(record) TO LAST(record) DO"  dnl mp)
    (dis "        Insert(db, record[i], ex)"                dnl mp)
    (dis "      END;"                                       dnl mp)
    (dis "      RETURN"                                     dnl mp)
    (dis "    END;"                                         dnl mp)
 
    ;; fast version follows (UNION SELECT)

    (dis "    VAR wx := Wx.New(); fields := NEW(TextSeq.T).init(); BEGIN" dnl mp)
    (dis "      FOR f := FIRST(Field) TO LAST(Field) DO" dnl mp)
    (dis "        IF f IN maxFields THEN fields.addhi(FieldNames[f]) END" dnl mp)
    (dis "      END;" dnl mp)

    (dis "      Wx.PutText(wx, \"insert into " tbl-name " (\" & TextUtils.FormatInfix(fields,\",\") & \") SELECT \"); " dnl mp)

    (dis "      FOR i := FIRST(record) TO LAST(record) DO" dnl mp)
    (dis "        VAR values := NEW(TextSeq.T).init(); BEGIN" dnl mp)

    (map (lambda (fld)
     (let* ((field-name (car fld))
      (m3name (string-append "record[i]." field-name))
      (type-name (cadr fld)))

             (dis "          IF Field."field-name" IN maxFields THEN" dnl 
      "            values.addhi(" (format-sql-from-m3 m3name type-name) ")" dnl
                  "           END;" dnl mp)))
   fields)
           
    (dis "          Wx.PutText(wx, TextUtils.FormatInfix(values, \",\"));" dnl mp)
    (dis "          IF i # LAST(record) THEN" dnl mp)
    (dis "            Wx.PutText(wx, \" UNION SELECT \")" dnl mp)
    (dis "          END" dnl mp)
    (dis "        END" dnl mp)
    (dis "      END(*FOR*);" dnl mp)
    (dis "      db.aExec(Wx.ToText(wx), ex, MakeResCallback(db))" dnl mp)
    (dis "    END" dnl mp)
    (dis "  END " mp)
    #t))

(define (dis-format-m3 tbl mp)
  (let ((tbl-name (car tbl)) 
  (fields (get-fields (complete-tbl tbl))))
    (dis "  VAR fields, values := NEW(TextSeq.T).init();" dnl
         "  BEGIN" dnl mp)

    (map (lambda(fld) (dis (ins-sql fld) mp))
   fields) 

    (dis dnl mp)
    (dis "    RETURN \"(\" & TextUtils.FormatInfix(fields,\",\") & \")\"&"
   dnl mp)
    (dis "                 \" values (\"& TextUtils.FormatInfix(values,\",\")&\")\"" dnl mp)
    (dis "  END " mp)
    #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ass-m3-old fld)
  (let* ((name (car fld))
   (type (find-type (cadr fld) identity types))
   (conv (get-conversion type))
   (m3conv (caddr conv))
   (query-op (car conv)))
    (string-append "      " 
       (if (null? m3conv) 
           ""
           (string-append (car m3conv) "." (cdr m3conv)))
       "(row.get(\"" name "\"))")))


(define (ass-q fld)
  (let* ((name (car fld))
   (type (find-type (cadr fld) identity types))
   (conv (get-conversion type))
   (query-op (car conv)))
    (string-append (query-op name) " as " name)))

(define (ass-m3 fld)
  ;; the string corresponding to an assignment to an M3 var. (starting from
  ;; the output of a SQL query)
  (let* ((name (car fld))
         (type (find-type (cadr fld) identity types))
         (not-null (memq 'not-null (cddr fld)))
         (conv (get-conversion type))
         (m3conv (caddr conv))
         (query-op (car conv)))

    (string-append
     "    res." name "_isNull := row.getIsNull(\"" name "\");" dnl
     "    IF NOT res." name "_isNull THEN" dnl
     "      res." name " := "
     (if (null? m3conv) 
         ""
         (string-append (car m3conv) "." (cdr m3conv)))
     "   (row.get(\"" name "\")) " dnl 
     "    END;" dnl
     )))
        
     
(define (dis-parse-m3 tbl mp)
  ;; print the entire routine for parsing a DB row into an M3 RECORD
  (let ((tbl-name (car tbl)) 
        (fields (get-fields (complete-tbl tbl))))

    (dis "  BEGIN " dnl mp)
    (map (lambda (fld) (dis (ass-m3 fld) mp))
         fields)
    
    (dis "  END " mp)
    
    #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    CLEAN/DIRTY MANAGEMENT OF TABLE ROWS
;;
;;    Summer/Fall 2009 we moved from a single dirty bit per row
;;    to an auxiliary table, "clean", that for now has to be provided
;;    by the specifier of the DB.  Eventually we should move this in
;;    so it is in this code rather than having to be specified by
;;    the user.
;;   
;;    The purpose of the clean table is to remember which rows have
;;    already been seen by clients and so do not need to be reloaded
;;    as long as they are cached.  Using a clean table allows multiple
;;    clients to cache the same table while yet allowing writers to
;;    dirty rows selectively.
;;
;;    To dirty a specific row:
;;
;;      DELETE FROM clean WHERE table=<table name> AND rowid=<row>;
;;
;;    For performance, then signal clients via an UpdateMonitor.  (Or
;;    make them poll sufficiently frequently.)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dis-clean-m3 tbl mp)
  (let ((tbl-name (car tbl)))
    (dis "  PROCEDURE Exec(q : TEXT) RAISES { DBerr.Error } =" dnl
         "    BEGIN" dnl
         "      IF sync THEN" dnl
   "        <*ASSERT res = NIL AND ex = NIL *>" dnl
         "        EVAL db.sExec(q, abortConnectionOnFail := FALSE)" dnl
         "      ELSE" dnl
   "        NARROW(db,DesynchronizedDB.T).aExec(q, ex, res)" dnl
         "      END" dnl
         "    END Exec;" dnl
         dnl mp)

    (dis "  BEGIN " dnl mp)
    (dis "    <*ASSERT clientTag # NIL*>" dnl mp)

    (dis "    IF row # AllRows THEN " dnl mp)

    (dis "      Exec(\"insert into clean (tabl,rowid,client) (select '" tbl-name "',\" & Fmt.Int(row) & \",'\" & clientTag & \"' from " tbl-name " where not exists (select * from clean where tabl='" tbl-name "' and clean.rowid=\" & Fmt.Int(row) &  \"_id and client='\"& clientTag &\"'));;\")" dnl mp)

    (dis "    ELSE " dnl mp)

    (dis "      Exec(\"insert into clean (tabl,rowid,client) (select '" tbl-name "'," tbl-name "_id,'\" & clientTag & \"' from " tbl-name " where not exists (select * from clean where tabl='" tbl-name "' and clean.rowid=" tbl-name "_id and client='\"& clientTag &\"'));;\")" dnl mp)

    (dis "    END " dnl mp)

    (dis "  END " mp)

    #t))
    
(define (dis-dirty-m3 tbl mp)
  (let ((tbl-name (car tbl)))
    (dis "  PROCEDURE Exec(q : TEXT) RAISES { DBerr.Error } =" dnl
         "    BEGIN" dnl
         "      IF sync THEN" dnl
         "        EVAL db.sExec(q, abortConnectionOnFail := FALSE)" dnl
         "      ELSE" dnl
         "        NARROW(db,DesynchronizedDB.T).aExec(q, ex)" dnl
         "      END" dnl
         "    END Exec;" dnl
         dnl mp)

    (dis "  BEGIN " dnl mp)

    (dis "    IF row # AllRows THEN " dnl mp)

    (dis "      <*ASSERT restriction = NIL *>" dnl mp)

    (dis "      Exec(\"delete from clean where tabl='" tbl-name "' and rowid=\"&Fmt.Int(row)&\";\")" dnl mp)
   

    (dis "    ELSIF restriction # NIL THEN " dnl mp)

    (dis "      Exec(\"delete from clean where tabl='" tbl-name "' and exists (select * from " tbl-name " where clean.rowid = " tbl-name "." tbl-name "_id and \" & restriction & \");\")" dnl mp)

    (dis "    ELSE " dnl mp)

    (dis "      Exec(\"delete from clean where tabl='" tbl-name "';\")" dnl mp)

    (dis "    END " dnl mp)

    (dis "  END " mp)

    #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

(define (dis-query-m3 tbl mp)
  ;; print the entire routine that generates the 'standard query string'
  (let ((tbl-name (car tbl)) 
  (fields (get-fields (complete-tbl tbl))))
    (dis "  BEGIN " dnl mp)
    (dis "    RETURN \"" mp)
    (map2 (lambda (fld) (dis (ass-q fld) "," mp))
    (lambda (fld) (dis (ass-q fld) ""  mp)) 
    fields)
    (dis " \"" dnl mp)
    (dis "  END " mp)
    #t
))

(define (dis-getid-m3 tbl mp)
  ;; having a procedure to get the ID lets us write generic
  ;; M3 code, without having to know all the field names.
  (let (  (tbl-name (car tbl))  )
    (dis "  BEGIN " dnl mp)
    (dis "    RETURN  t." tbl-name "_id" dnl mp)
    (dis "  END " mp)
    #t
))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dis-imports imports ip)
  (let ((uniq-imports (uniq eqv? imports)))
      (if (not (null? uniq-imports))
    (begin 
      (dis "<*NOWARN*>IMPORT " ip)
      (map2 (lambda (intf) (dis " " intf "," ip))
      (lambda (intf) (dis " " intf ";" dnl dnl ip))
      uniq-imports))
    )
      )
)

(define (dis-intf-imports fields ip)
  (let ((imports
   (append (list "DBerr" "Database" "DatabaseTable" "DesynchronizedDB"
                 "UpdateMonitor" "Thread" "Random" "Debug"
           "Scan" "Fmt" "Lex" "FloatMode" "PGSQLScan" "TextSeq" "Wx"
           "TextUtils" "DBTable")
     (map type->m3-intfname 
          (filter m3-import? (map cadr fields))))))
    (dis-imports imports ip) 
    #t
    ))

(define dis-modu-imports dis-intf-imports)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; shared header defn's
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define dirtyheader 
  (list
   "SetDirty" 
   "(db : DesynchronizedDB.Execer; ex : DesynchronizedDB.ExCallback; row : [AllRows .. LAST(CARDINAL)] := AllRows; sync := FALSE; restriction : TEXT := NIL) RAISES { DBerr.Error }"
   dis-dirty-m3
   ))

(define cleanheader
  (list
   "SetClean"
   "(db : DesynchronizedDB.Execer; ex : DesynchronizedDB.ExCallback; clientTag : TEXT; row : [AllRows .. LAST(CARDINAL)] := AllRows; sync := FALSE; res : DesynchronizedDB.ResCallback := NIL) RAISES { DBerr.Error }"
   dis-clean-m3
   ))

(define parseheader 
    (list "<*NOWARN*>Parse"
    "(row : DatabaseTable.T; VAR res : T) RAISES { Lex.Error, FloatMode.Trap, DBerr.Error }"
    dis-parse-m3
    ))

(define queryheader 
    (list "QueryHeader" "() : TEXT" dis-query-m3))

(define format 
    (list "Format"
    "(READONLY record : T) : TEXT"
    dis-format-m3
    ))

(define getid
    (list "GetRecordId" "(READONLY t : T) : CARDINAL" dis-getid-m3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    PUT IT ALL TOGETHER.  GENERATE ALL CODE FOR A SINGLE TABLE!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-server-table-m3 tbl db m3mp)

  ;; generate all the Modula-3 code for accessing a table

  (let* ((name (car tbl))
   (dbname (db-name db))
   (m3name (string-append "DBTable_" dbname "_" name))
   (ip (open-output-file (string-append m3name ".i3")))
   (mp (open-output-file (string-append m3name ".m3")))
   (fields (get-fields (complete-tbl tbl))))

    (dis "derived_interface(\"" m3name "\",VISIBLE)" dnl m3mp)
    (dis "derived_implementation(\"" m3name "\")" dnl m3mp)
    (dis "TableMonitor(\"" m3name "\",\"" m3name "\")" dnl dnl m3mp)
    (dis "Table(\"Int" m3name "\",\"Integer\",\"" m3name "\")" dnl dnl m3mp)

    (dis "(* AUTOMATICALLY GENERATED DO NOT EDIT *)" dnl ip)
    (dis "INTERFACE " m3name ";" dnl dnl ip)

    (dis "(* AUTOMATICALLY GENERATED DO NOT EDIT *)" dnl mp)
    (dis "MODULE " m3name ";" dnl dnl mp)

    (dis-intf-imports fields ip)
    (dis-modu-imports fields mp)

    (dis "TYPE T = RECORD" dnl ip)
    (map (lambda (fld) (putm3-field-list fld ip)) fields)
    (dis "END;" dnl ip)
    (dis dnl ip)

    (dis "TYPE Field = { " dnl ip)
    (dis (infixize (map (lambda(fld) (car fld)) fields) ",") dnl ip)
    (dis "};" dnl ip)
    (dis dnl ip)
    
    (dis "TYPE FieldSet = SET OF Field;" dnl ip)
    (dis dnl ip)

    (dis "CONST AllFields = SET OF Field { FIRST(Field) .. LAST(Field) };" dnl ip)
    (dis dnl ip)

    (dis "CONST FieldNames = ARRAY Field OF TEXT { " dnl ip)
    (dis (infixize (map (lambda(fld) (string-append "\"" (car fld) "\"")) fields) ",") dnl ip)
    (dis "};" dnl ip)
    (dis dnl ip)
    

    ;; the various field-wise procedures
    (map (lambda(proc)
           (map (lambda (fld) (proc fld ip mp)) fields))
         (list put-getter-procedure
               put-setter-procedure
               put-clearer-procedure
               put-have-procedure))

    (let 
  ;;
  ;; define some Modula-3 procedures -- several moved to global space now.
  ;; Update  a row
  ;; Parse   a row
  ;; Insert  a row
  ;; QueryHeader   provide the data spec for the query to Parse
  ;; SetDirty  clear/set dirty bit
  ;;
  ((upheader
    (list "Update" 
    "(db : DesynchronizedDB.T; READONLY record : T; ex : DesynchronizedDB.ExCallback)"
    dis-update-m3
    ))
   (nullset
    (list "NullSet"
    "(READONLY record : T) : FieldSet"
    dis-nullset-m3))

   (insert
    (list "Insert"
    "(db : DesynchronizedDB.T; READONLY record : T; ex : DesynchronizedDB.ExCallback)"
    dis-insert-m3
    ))

   (batch-insert
    (list "BatchInsert"
    "(db : DesynchronizedDB.T; READONLY record : ARRAY OF T; ex : DesynchronizedDB.ExCallback; deleteFirst := FALSE)"
    dis-batch-insert-m3
    ))

   (upinsert
    (list "UpdateOrInsert" 
          "(db : DesynchronizedDB.T; READONLY record : T; restriction : TEXT; ex : DesynchronizedDB.ExCallback)"
          dis-update-or-insert-m3
          ))
   
   )

      (dis dnl "CONST AllRows = -1; " dnl ip)

      (map 
       (lambda(h)
   (let (  (pname (car h))
     (proto (cadr h))
     (dis-code  (caddr h))  )
     (dis "PROCEDURE " pname proto ";" dnl dnl ip)
     (dis "PROCEDURE " pname proto "=" dnl mp)
     (dis-code tbl mp)
     (dis pname ";" dnl dnl dnl mp)))
       (list upheader 
             insert
       format
             upinsert
       parseheader
       dirtyheader
       cleanheader
       queryheader
             getid
       nullset
       batch-insert)
       )
      )

    ;; finish .i3 file
    (dis "CONST Brand = \"" m3name "\";" dnl dnl ip)
    (dis "CONST TableName = \"" name "\";" dnl dnl ip)
    (dis "END " m3name "." dnl ip)     

    ;; finish .m3 file

    ;; exec callback, call update monitor...

    (dis "TYPE" dnl
         "  ResCallback = DesynchronizedDB.ResCallback OBJECT" dnl
         "    um   : UpdateMonitor.T;" dnl
         "  OVERRIDES" dnl
         "    result := RCResult" dnl
         "  END;" dnl 
         dnl
         "PROCEDURE MakeResCallback(db : DesynchronizedDB.T) : ResCallback =" dnl
         "  BEGIN" dnl
         "    WITH um = NARROW(db.getAttribute(UpdateMonitor.Brand),UpdateMonitor.T) DO" dnl
         "      IF um = NIL THEN RETURN NIL ELSE RETURN NEW(ResCallback, um := um) END" dnl
         "    END" dnl
         "  END MakeResCallback;" dnl
         dnl
         "PROCEDURE RCResult(rc : ResCallback;" dnl
         "                   <*UNUSED*>dt : DatabaseTable.T) =" dnl
         "  BEGIN" dnl
         "    rc.um.locallyUpdated(\"" name "\")" dnl
         "  END RCResult;" dnl
         dnl
         mp)

    (dis "BEGIN END " m3name "." dnl mp)

    (close-output-port ip) (close-output-port mp)
    #t
    )
  )


(define (make-client-table-m3 tbl db m3mp)

  ;; generate all the Modula-3 code for accessing a table

  (let* ((name (car tbl))
   (dbname (db-name db))
   (m3name (string-append "DBTable_" dbname "_" name))
   (ip (open-output-file (string-append m3name ".i3")))
   (mp (open-output-file (string-append m3name ".m3")))
   (fields (get-fields (complete-tbl tbl))))

    (dis "derived_interface(\"" m3name "\",VISIBLE)" dnl m3mp)
    (dis "derived_implementation(\"" m3name "\")" dnl m3mp)
    (dis "TableMonitor(\"" m3name "\",\"" m3name "\")" dnl dnl m3mp)
    (dis "Table(\"Int" m3name "\",\"Integer\",\"" m3name "\")" dnl dnl m3mp)

    (dis "(* AUTOMATICALLY GENERATED DO NOT EDIT *)" dnl ip)
    (dis "INTERFACE " m3name ";" dnl dnl ip)

    (dis "(* AUTOMATICALLY GENERATED DO NOT EDIT *)" dnl mp)
    (dis "MODULE " m3name ";" dnl dnl mp)

    (dis-intf-imports fields ip)
    (dis-modu-imports fields mp)

    (dis "TYPE T = RECORD" dnl ip)
    (map (lambda (fld) (putm3-field-list fld ip)) fields)
    (dis "END;" dnl ip)
    (dis dnl ip)

    ;; the various field-wise procedures
    (map (lambda(proc)
           (map (lambda (fld) (proc fld ip mp)) fields))
         (list put-getter-procedure
               ;put-setter-procedure
               ;put-clearer-procedure
               put-have-procedure))

    (let 
  ;;
  ;; define some Modula-3 procedures -- moved out of here to global space
  ;;
  (
   
   )
      (dis dnl "CONST AllRows = -1; " dnl ip)

      (map 
       (lambda(h)
   (let (  (pname (car h))
     (proto (cadr h))
     (dis-code  (caddr h))  )
     (dis "PROCEDURE " pname proto ";" dnl dnl ip)
     (dis "PROCEDURE " pname proto "=" dnl mp)
     (dis-code tbl mp)
     (dis pname ";" dnl dnl dnl mp)))
       (list ;;upheader 
             parseheader 
             ;;insert 
             format
             queryheader dirtyheader cleanheader getid)
       )
      )

    (dis "CONST Brand = \"" m3name "\";" dnl dnl ip)
    (dis "CONST TableName = \"" name "\";" dnl dnl ip)
    (dis "END " m3name "." dnl ip)     (dis "BEGIN END " m3name "." dnl mp)

    (close-output-port ip) (close-output-port mp)
    #t
    )
  )

(define (make-table-m3 tbl db m3mp)
  (cond ((eq? 'client (cadr tbl)) (make-client-table-m3 tbl db m3mp))
        ((eq? 'server (cadr tbl)) (make-server-table-m3 tbl db m3mp))
        (else (error (string-append "Unknown owner : " (cadr tbl))))))
    
(define (complete-tbl tbl)
  ;;
  ;; "complete" the table, adding standard columns
  ;; id col.
  ;; created time
  ;; updated time
  ;; NO dirty bit -- replaced by clean table
  ;; active bit 
  ;;
  (let ((name (car tbl))
        (owner (cadr tbl))
        (old-data (cddr tbl)))
        
    (append 
     (list name owner)
     (list
      (make-field (string-append name "_id") 'serial 'primary-key )
      (make-field "created" 'timestamp 'not-null 'not-updatable (list 'default "now()"))
      (make-field "updated" 'timestamp (list 'default "now()"))
      (make-index (list "updated"))
      )
      (if (eq? owner 'client)
          (list 
           (make-index (list "active"))
           (make-field "active" 'boolean 'not-null (list 'default "false")))
          '())
     old-data)))

(define (is-id? field) 
  (and (list? (cadr field)) 
       (eq? 'id (caadr field))))

(define (display-constraint tab-name c)
  (let ((ftab-name (cadadr c))
  (col-name (car c)))
    (dis dnl
   "alter table " tab-name 
   " add constraint " tab-name "_" ftab-name "_" col-name "_fk"
   " foreign key (" col-name ") references " ftab-name "(" ftab-name 
   "_id);" dnl port)))

(define (display-constraints tab)
  (let ((tab-name (car tab)))
    (map (lambda (c) (display-constraint tab-name c)) 
         (filter is-id? (get-fields tab)))
    #t
    ))
    
(define (index-code idx tab-name)
  (let ((fields (car idx))
        (opts (cdr idx)))
    (let ((have-unique? (memq 'unique opts))
          (name         (string-append 
                         (infixize (cons tab-name fields) "_") 
                         "_idx" ))
          (colstr       (infixize fields ",")))
      (string-append
       "create "
       (if have-unique? "unique " "")
       "index "
       name
       " on " tab-name "(" colstr ");")
)))

(define (display-indexes tab)
  (let ((tab-name (car tab)))
    (map (lambda(i)(dis (index-code i tab-name) dnl port)) (get-indexes (complete-tbl tab)))
))
  
(define (find-constraints db)
  (map display-constraints (get-tables db)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    FINALLY, WRITE ALL THE CODE FOR THE DATABASE...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-database-sql db p)
  (begin 
    (set! port p)
    (map (lambda (tbl) (make-drop-table-sql tbl db)) (get-tables db))
    (drop-write-seq p)
    (dis dnl dnl dnl p)
    (make-write-seq p)
    (map (lambda (tbl) (make-table-sql tbl db)) (get-tables db))
    (map display-indexes (get-tables db))
    (map display-constraints (get-tables db))))


(define (write-database-sql db)
  (let ((p (open-output-file (string-append (car db) ".sql"))))
    (make-database-sql db p)
    (close-output-port p)))

(define (write-database-m3 db)
  (let ((p (open-output-file scodegen-m3makefile)))
    (map (lambda (t) (make-table-m3 t db p)) (get-tables db))
    (close-output-port p)))

