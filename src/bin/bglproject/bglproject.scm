(module bglproject.main
   (main main))

(define *project-name* "")
(define *executable* #t)
(define *create-project* #f)
;; default project type. valid values are 'library and 'bin
(define *project-type* 'library)
(define *working-dir* "./")
(define *author-name* (getenv "USER"))
(define *templates* '())

(define (get-template name)
   (and-let* ((res (assoc name *templates*)))
      (cdr res)))

(define-macro (initiate-templates)
   (define (read-into-string filename)
      (with-input-from-file filename
         (lambda () (read-string))))
   (define (read-templates)
      (map (lambda (f) (cons (basename f)
                     (read-into-string f)))
         (directory->path-list "./templates")))
   `(set! *templates* ',(read-templates)))

(initiate-templates)

(define (parse-args args)
   (args-parse (cdr args)
      (section "Help")
      ((("--help")
        (help "--help" "This help message"))
       (args-parse-usage #f))
      (section "Project Creation")
      (("new" ?project-name
          (help "new <project-name>" "create a new project"))
       (set! *project-name* project-name)
       (set! *create-project* #t))
      ((("--bin")
        (help "--bin" "create a bin project"))
       (set! *project-type* 'bin))
      (("--author" ?author
          (help "--author" "set the authors name"))
       (set! *author-name* author))
      (else
       (print "illegal argument '" else "'. Usage:")
       (args-parse-usage #f)
       (exit -1))))


(define (fold-left proc s lst)
   (if (pair? lst)
       (fold-left proc (proc s (car lst)) (cdr lst))
       s))

(define (hydrate-template t environment)
   (fold-left (lambda (s i)
                 (pregexp-replace* (format "{{\\s*~a\\s*}}" (car i)) s (cdr i)))
      t environment))

(define (make-project-directory working-dir project-name)
   (make-directories (make-file-name working-dir project-name)))

(define (make-file-from-templates working-dir templates environment)
   (for-each (lambda (t) (with-output-to-file
                          (make-file-path working-dir t)
                       (lambda ()
                          (display (hydrate-template (get-template t) environment)))))
      templates))


(define (make-environment)
   `(("project-name" . ,*project-name*)
     ("year" . ,(number->string (date-year (current-date))))
     ("author-name" . ,*author-name*)))

(define (main args)
   (parse-args args)
   (unless (and *create-project* (not (string=? "" *project-name*)))
      (parse-args '("dummy" "--help"))
      (exit -1))
   (let ((environment (make-environment)))
      (make-project-directory *working-dir* *project-name*) 
      (make-file-from-templates (make-file-path *working-dir* *project-name*) '("Makefile" "configure" "configure.scm") environment)
      (chmod (make-file-path *working-dir* *project-name* "configure") 'read 'execute)
      (if (eq? *project-type* 'library)
          (begin
             (make-directories (make-file-path *working-dir* *project-name* "src/lib/" *project-name*))
             (make-file-from-templates (make-file-path *working-dir* *project-name* "src/lib/" *project-name*) '("lib.init.in" "make_lib.scm" "lib.scm") environment))
          (begin
             (make-directories (make-file-path *working-dir* *project-name* "src/bin/" *project-name*))
             (make-file-from-templates (make-file-path *working-dir* *project-name* "src/bin/" *project-name*) '("main.scm") environment)))))
   