#lang at-exp racket
;;;
;;; Run this file to configure `pyffi`.
;;;

;; The Python module `sysconfig` provides access to Python's configuration information.
;; If we can load the shared library `libpython3` then we can use `sysconfig` to
;; get all the information needed.

;; However, we need a way to figure out where `libpython3` is, before
;; we can load it.

;; The most reliable way is to run `python3` in a terminal and let
;; Python tell us where the shared library is.
;; In order to make it easy for users, this file will find `python3`,
;; run it, and extract the needed configuration information.

;; We are using `(find-executable-path "python3")` to find the path.
;; In the terminal where the environment variable PATH is set, this
;; works fine. However, if run in DrRacket (at least on macOS) the
;; environment variable PATH won't be set and we risk picking up a
;; different version.

;; As I am writing this, DrRacket will find the system installed
;; version of Python in /usr/bin/python3 which is version 3.8.9.

;; In the terminal we find version 3.10:
;;   /Library/Frameworks/Python.framework/Versions/3.10/bin/python3
;; Update: after installing 3.14:
;;   /Library/Frameworks/Python.framework/Versions/3.14/bin/python3

;; That is:

;;   Run this configuration tool in an environment (terminal)
;;   where `python3` starts the Python command want to use.

;; After determining the location of the shared library
;; the location is written to the file "pyffi/path-to-libpython3.conf"
;; in the users preferences with `put-preferences`.


;; Notes:
;;  We run the command `python3 -m sysconfig` which writes
;;  the configuration information in groups of key/value pairs.
;;  The parser below parses the entire output (maybe we need
;;  more information at a later date).

(require raco/command-name)

(define system-configuration        #f)
(define system-configuration-string #f)


(define (get-configuration [given-path-to-python #f])
  (define path-to-python
    (or given-path-to-python
        (or (find-executable-path "python3")
            (find-executable-path "python")
            (find-executable-path "python3.14")
            (find-executable-path "python3.13")
            (find-executable-path "python3.12")
            (find-executable-path "python3.11")
            (find-executable-path "python3.10"))))

  (displayln "Configuration tool for `pyffi`.")
  (displayln "-------------------------------") 
  (displayln "This tool attempts to find the shared library `libpython3` ")
  (displayln "with the help of the `python3` executable.")
  (newline)
  (cond
    [path-to-python
     (displayln "The executable")
     (display   "    ")
     (displayln path-to-python)
     (displayln "will be used to find the location of the shared library.")]
    [else
     (displayln "The executable `python3` was not found in the current path.")
     (displayln "Double check that `python3` starts Python your terminal.")
     (displayln "Then run this configuration tool in the same terminal.")
     (exit 1)])
  (newline)

  (define success 'not-available-yet)
  (define str-path (if (path? path-to-python)
                       (path->string path-to-python)
                       path-to-python))
  (define command (string-append str-path " -m sysconfig"))
  (set! system-configuration-string
    (with-output-to-string
      (λ() (set! success (system command)))))
  (unless success
    (displayln "An error occurred while running the command:")
    (display   "    ")
    (displayln command)
    (displayln "Configuration of `pyffi` failed.")
    (exit 2))

  ; (displayln system-configuration-string)

  (define (string->lines x)
    (string-split x #rx"[\r\n]+"))
  (define (blank? x)
    (equal? (string-trim x) ""))
  (define (trim x)
    (string-trim (string-trim x) "\""))

  (define (string->key/value x)
    (and (string-contains? x ":")
         (match (regexp-match "([^:]*):(.*)" x)
           [(list full before after)
            (list (trim before) (trim after))])))

  ; parse the output of `python3 -m sysconfig` into an association list
  (define (parse info)
    (define lines (string->lines info))
    (let loop ([lines lines] [groups '()])
      (match lines
        ['()
         (reverse groups)]
        [(list* line lines)
         (if (blank? line)
             (loop (rest lines groups))
             (match (string->key/value line)
               [(list key value)
                (if (blank? value) ; new group?
                    (let ()
                      (define-values (group rest-lines) (parse-group line lines))
                      (loop rest-lines (cons group groups)))
                    (loop lines (cons (list key value) groups)))]))])))

  ; each group is parsed into a sub-association list
  (define (parse-group first-line lines)
    (define group-lines (takef lines (λ (x) (regexp-match "([^=]*)=(.*)" x))))
    (define rest-lines  (drop lines (length group-lines)))

    (define group
      (list (string-trim (trim first-line) ":")
            (for/list ([line group-lines])
              (match (regexp-match "([^=]*)=(.*)" line)
                [(list full before after)
                 (list (string-trim before)
                       (trim after))]))))
    (values group rest-lines))

  (set! system-configuration (parse system-configuration-string)))

;; Now the information is available in `system-configuration`.


;;; PATHS
(define (python-paths)
  (assoc  "Paths" system-configuration))

(define (python-data)
  (define result (assoc "data" (second (python-paths))))
  (and result (second result)))

;;; VARIABLES

(define (python-variables)
  (assoc  "Variables" system-configuration))

(define (python-libdir)
  (define result (assoc "LIBDIR" (second (python-variables))))
  (and result (second result)))


(define (python-bindir)
  (define result (assoc "BINDIR" (second (python-variables))))
  (and result (second result)))

(define (get-old-libdir)
  (get-preference 'pyffi:libdir (λ () #f)))

(define (set-new-libdir new-libdir-path)
  (define old (get-preference 'pyffi:libdir (λ () #f)))
  (unless (equal? old new-libdir-path)
    (put-preferences (list 'pyffi:libdir)
                     (list new-libdir-path)))
  (when old
    (displayln "The previous value of LIBDIR was:")
    (display   "    ")
    (displayln old))
  (displayln "The preference for LIBDIR is now set to:")
  (display   "    ")
  (displayln new-libdir-path))


(define (get-old-data)
  (get-preference 'pyffi:data (λ () #f)))

(define (set-new-data new-data)
  (define old (get-preference 'pyffi:data (λ () #f)))
  (unless (equal? old new-data)
    (put-preferences (list 'pyffi:data)
                     (list new-data)))
  (when old
    (displayln "The previous value of DATA was:")
    (display   "    ")
    (displayln old))
  (displayln "The preference for DATA is now set to:")
  (display   "    ")
  (displayln new-data))


(define (handle-libdir path-to-python)
  (cond
    [(python-libdir) => set-new-libdir]
    [(and (equal? (system-type 'os) 'windows)
          (python-bindir)) => set-new-libdir]
    [else
     (parameterize ([current-output-port (current-error-port)])
       (displayln "The LIBDIR key wasn't found.")
       (newline)
       (displayln "The sysconfiguration produced by the Python module `sysconfig` was:")
       (newline)
       (displayln system-configuration-string))]))

(define (run-python-query path-to-python code)
  (define str-path (if (path? path-to-python)
                       (path->string path-to-python)
                       (or path-to-python "python3")))
  (define cmd (string-append str-path " -c " (format "~s" code)))
  (define out (with-output-to-string (λ () (system cmd))))
  (string-trim out))

(define (handle-data path-to-python)
  (cond
    [(python-data) => set-new-data]
    [else
     (parameterize ([current-output-port (current-error-port)])
       (displayln "The DATA key wasn't found.")
       (newline)
       (displayln "The sysconfiguration produced by the Python module `sysconfig` was:")
       (newline)
       (displayln system-configuration-string))]))

(define (set-new-home new-home)
  (put-preferences (list 'pyffi:home) (list new-home))
  (displayln "The preference for PYTHONHOME is now set to:")
  (display   "    ")
  (displayln new-home))

(define (set-new-pyver new-pyver)
  (put-preferences (list 'pyffi:pyver) (list new-pyver))
  (displayln "The preference for Python version is now set to:")
  (display   "    ")
  (displayln new-pyver))

(define (set-new-venv new-venv)
  (put-preferences (list 'pyffi:venv) (list new-venv))
  (displayln "The preference for venv path is now set to:")
  (display   "    ")
  (displayln new-venv))

(define (set-new-platlibdir new-platlibdir)
  (put-preferences (list 'pyffi:platlibdir) (list new-platlibdir))
  (displayln "The preference for platlibdir is now set to:")
  (display   "    ")
  (displayln new-platlibdir))

(define (handle-home-and-ver path-to-python)
  (define data       (python-data))
  (define prefix     (run-python-query path-to-python "import sys; print(sys.base_prefix)"))
  (define pyver      (run-python-query path-to-python "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')"))
  (define platlibdir (run-python-query path-to-python "import sysconfig; print(sysconfig.get_config_var('platlibdir'))"))
  (when (and prefix (not (equal? prefix "")))     (set-new-home prefix))
  (when (and pyver  (not (equal? pyver "")))      (set-new-pyver pyver))
  (when (and platlibdir (not (equal? platlibdir ""))) (set-new-platlibdir platlibdir))
  (when (and data prefix (not (equal? data prefix)))
    (set-new-venv data)))

(define (configure [path-to-python #f])
  (get-configuration path-to-python)
  (handle-libdir path-to-python)
  (newline)
  (handle-data   path-to-python)
  (newline)
  (handle-home-and-ver path-to-python))

(define (show)
  ;; Print every preference this package reads, with a short legend.
  ;; Keep this in sync with the preferences written by `configure` in
  ;; `handle-libdir`, `handle-data`, and `handle-home-and-ver` above.
  (define (show-pref key label)
    (display "    ")
    (display label)
    (display " = ")
    (write (get-preference key (λ () #f)))
    (newline))

  (displayln "Current configuration for 'pyffi'.")
  (newline)
  (show-pref 'pyffi:libdir     "libdir    ")
  (show-pref 'pyffi:data       "data      ")
  (show-pref 'pyffi:home       "home      ")
  (show-pref 'pyffi:pyver      "pyver     ")
  (show-pref 'pyffi:platlibdir "platlibdir")
  (show-pref 'pyffi:venv       "venv      ")

  (newline)
  (displayln "Meaning:")
  (newline)
  (displayln "    libdir:     location of the shared library 'libpython'")
  (displayln "    data:       sysconfig 'data' path — usually a venv root,")
  (displayln "                or the install prefix when no venv is in use")
  (displayln "    home:       PYTHONHOME — Python's base prefix (where the")
  (displayln "                stdlib lives); differs from data when running")
  (displayln "                from a venv")
  (displayln "    pyver:      Python major.minor, e.g. 3.12")
  (displayln "    platlibdir: sysconfig 'platlibdir' (usually 'lib' or 'lib64')")
  (displayln "    venv:       venv root, if any — used to append")
  (displayln "                <venv>/lib/python<pyver>/site-packages to sys.path"))

(define usage
  @~a{
      Usage: raco pyffi <subcommand> <arg...>

        raco pyffi configure
            configure 'pyffi' using auto-detected python executable

        raco pyffi configure <path-to-python>
            configure 'pyffi' using  <path-to-python>

        raco pyffi show
            show the current 'pyffi' configuration

        raco pyffi diagnostics
            show the Python paths and variables})

(define (display-usage)
  (displayln usage))

(define (diagnostics)
  (define path-to-python #f)
  (get-configuration path-to-python)

  (displayln "Python Paths")
  (displayln "------------")
  (pretty-print (python-paths))
  
  (displayln "Python Variables")
  (displayln "----------------")
  (pretty-print (python-variables)))


(define (run)
  (command-line
   #:program    (short-program+command-name)
   #:usage-help
   "
       raco pyffi configure
           configure 'pyffi' using auto-detected python executable

       raco pyffi configure <path-to-python>
           configure 'pyffi' using  <path-to-python>

       raco pyffi show
           show the current 'pyffi' configuration"
   #:args args
   (match args
     [(list "configure")                (configure)]
     [(list "configure" path-to-python) (configure path-to-python)]
     [(list "show")                     (show)]
     [(list "diagnostics")              (diagnostics)]
     [else                              (display-usage)
                                        (exit 3)])))

(run)
