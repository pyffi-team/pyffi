#lang scribble/manual
@;(require racket/gui/base)
@; The following command will build the manual and open it in a browser.
@; raco84 scribble +m --dest html --redirect-main http://docs.racket-lang.org manual-pyffi.scrbl && open html/manual-pyffi.html

@;;; The package and first time users may not have Python 3.10 installed.
@;;; Since the manual has examples that invokes Python, we need to record the results
@;;; of the examples in a log file "examples.log". This is done my setting the mode
@;;; the mode to 'record. On the package server the mode is 'replay.
@;;; Remember to commit the log file and rerecord the examples when changes are made.

@;(define mode 'record)
@(define mode 'replay)

@(require racket/sandbox racket/format racket/file racket/runtime-path racket/string racket/list) 
@(require scribble/core scribble/html-properties (only-in xml cdata))
@(require scribble/example)
@(require (for-syntax racket/base syntax/parse))
@(require (for-label pyffi))


@; Used to reference other manuals.
@(define reference.scrbl '(lib "scribblings/reference/reference.scrbl"))

@; Long urls
@(define (wikipedia name . preflow)
   (define url (string-append "https://en.wikipedia.org/wiki/" name))
   @margin-note{@hyperlink[url (list* @bold{Wikipedia: } " " preflow)]})

@(define (wikipedia/section url . preflow)
   @margin-note{@hyperlink[url (list* @bold{Wikipedia: } " " preflow)]})

@(define license-link
   @hyperlink["https://creativecommons.org/licenses/by-nc-sa/4.0/"
              "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License."])

@(define pyffi-github
   @hyperlink["https://github.com/soegaard/pyffi/"
              "github.com/soegaard/pyffi/"])

@(define (python-docs query text)
   @hyperlink[(~a "https://docs.python.org/3/search.html?q=" query )]{@|text|})


@(require scribble/example)

@(require racket/sandbox
          scribble/example)

@(require racket/gui/dynamic racket/port)
@(define (make-log-based-eval-factory mod-paths
                                      log-file mode
                                      #:pretty-print? [pretty-print? #f])
   (parameterize ([sandbox-namespace-specs
      (cons (λ () 
              (let ([ns
                     ;; This namespace-creation choice needs to be consistent
                     ;; with the sandbox (i.e., with `make-base-eval')
                     (if gui?
                         ((gui-dynamic-require 'make-gui-empty-namespace))
                         (make-base-empty-namespace))])
                (parameterize ([current-namespace ns])
                  (for ([mod-path (in-list mod-paths)])
                    (dynamic-require mod-path #f))
                  (when pretty-print? (dynamic-require 'racket/pretty #f)))
                ns))
            (append mod-paths (if pretty-print? '(racket/pretty) '())))])
     (lambda ()
       (let ([ev (make-log-based-eval log-file mode)])
         (when pretty-print?
           (call-in-sandbox-context ev
             (lambda ()
               (current-print (dynamic-require 'racket/pretty 'pretty-print-handler)))))
         ev))))

@(require (only-in pyffi/structs obj?))

@(require racket/match rackunit)
@(require/expose racket/sandbox (evaluator-message?))
@(define (wrap ev)
   (lambda (x)
     (call-with-values
      (λ ()
        ; (displayln (list 'input: x))
        (if (evaluator-message? x)
            (ev x)
            (match x
              [(cons (or 'require 'define 'import 'struct) _)
               (ev x)]
              [else
               (ev `(asis (with-output-to-string 
                            (λ () (let ([vvv ,x])
                                    (if (void? vvv) (void) (print vvv)))))))])))
      (λ vs
        ; (displayln (list 'output: vs))
        (apply values vs)))))

@(define-runtime-path examples.log "examples.log")
@(define factory (make-log-based-eval-factory (list) examples.log mode))
@;(define factory (make-log-based-eval-factory (list) examples.log 'replay))
@;(define factory (make-base-eval-factory (list 'pyffi)))
@(define (make-pyffi-eval)
   (let ([e (factory)])
     (e '(require pyffi))
     (e '(require pyffi/structs))
     (e '(require pyffi/python-exception))
     (e '(require pyffi/python-callback))
     (e '(require racket/port))
     (e '(initialize))
     (e '(post-initialize))
     (wrap e)))
@(define pe (make-pyffi-eval))

@title[#:tag "pyffi"]{pyffi - Use Python from Racket}

@defmodule[pyffi]

This library @tt{pyffi} allows you to use Python from Racket.

@author[@author+email["Jens Axel Søgaard" "jensaxel@soegaard.net"]]
@author[@author+email["Eric Parsonage"    "eric@eparsonage.com"]]

@local-table-of-contents[]


@section[#:tag "introduction"]{Introduction}

The library @tt{pyffi} makes it possible to use Python libraries from Racket.

The bridge between Racket and Python is for now one-way only: a Racket
program can call Python function and invoke Python methods, but it's not possible (yet)
to pass Racket functions to the Python side.

Python libraries implemented in Python ought to work out of the box.

Python libraries implemented as C extensions might work - if the C
extension supports introspection via the Python module `inspect`. For
C extensions without introspection you can drop down to a low-level
FFI which works in the same style as the Racket C FFI.

Bindings for @tt{Numpy}, the popular package for numerical calculations, are provided.


@section[#:tag "release-information"]{Release Information}

This is the first release of the library, so be prepared to deal with
some rough edges here and there.

Report bugs to the @tt{pyffi} GitHub repository: @|pyffi-github|.

If you run into any questions, use the forum
@hyperlink["https://racket.discourse.group/"]{Racket Discourse}.

Alternatively, feel free to ask questions on the chat server 
@hyperlink["https://discord.com/invite/6Zq8sH5"]{Racket Discord}.


@section[#:tag "supported-platforms"]{Supported Platforms}

For now @tt{pyffi} supports Python 3.10 and newer on macOS and Linux/Unix.
Windows is currently not supported.


@section[#:tag "installation"]{Installation}

The plan is simple: install Python 3.1x then tell @tt{pyffi} where
Python is installed.

Optional: install @hyperlink["https://numpy.org/"]{NumPy} if you want
to use @racket[pyffi/numpy].


@subsection{Python 3.1x}

In order to use @tt{pyffi} you need Python 3.10 or newer.

The official distribution of Python is here:
@hyperlink["https://www.python.org/downloads/"]{https://www.python.org/downloads/}

If you prefer to use an alternative source of distribution
(read your favorite package manager), that's fine too - as long as it
includes both the standard interpreter as well as the shared library @tt{libpython}.

@subsection{One-time Configuration}

The last installation step consists of telling @tt{pyffi} where your
shared library @tt{libpython} is placed.

The easiest way of doing this is to run the script @tt{configure-pyffi}.

@itemlist[
          @item{Open a terminal.}
          @item{Check that @tt{python3} or @tt{python} invokes your Python interpreter.@linebreak[]}
          @item{Run: @tt{raco pyffi configure}}]

If the command @tt{python3} is not in your path, then you can write this instead:

@itemlist[
          @item{@tt{raco pyffi configure <path-to-your-python-command>}}]

This will find the location of your shared library, print it, and,
store it in your Racket preferences under the key @tt{pyffi:libdir}.

You are now ready to use @tt{pyffi}.


@subsection{Optional: @tt{NumPy} for scientific computing}

The Python package NumPy has the slogan:

@nested[#:style 'inset]{The fundamental package for scientific computing with Python.}

If you want to use @tt{pyffi/numpy} then you need to install @tt{NumPy}
in your Python environment first.

There are many ways of installing @tt{NumPy}, but the simplest is to use
the following in a terminal:

@itemlist[
          @item{@tt{python3 -m pip install numpy}}]



@section{An introduction to @tt{pyffi}}

The @tt{pyffi} library makes it possible to use Python libraries from a Racket program.

A Racket program can start a Python process by requiring @racket[pyffi] and calling
@racket[initialize]. After the initialization @racket[run] and @racket[run*] can
be used to evaluate expressions and statements in the Python process.

@examples[#:label #f #:eval pe
          (require pyffi)
          (initialize)
          (post-initialize)
          (run "1+2")]

Here Racket starts an embed Python process.
The Python "1+2" is parsed, compiled and evaluated by Python.
The resulting Python value 3 is then converted to the Racket value 3.

@subsection{Atomic values: numbers, Booleans and @tt{None}}

Atomic Python values (numbers, Booleans and None) are automatically converted
to their corresponding Racket values.

@examples[#:label #f #:eval pe
          (require pyffi)
          (initialize)
          (post-initialize)
          (run "12")
          (run "34.")
          (run "5+6j")
          (run "False")
          (run "True")
          (list (run "None"))]

@subsection{Compound Values: strings, tuples, lists, and, dictionaries}

Compound (non-atomic) Python values such as strings, tuples, lists and dicts are not converted
to Racket values. Instead they are wrapped in a struct named @racket[obj].
Due to a custom printer handler these wrapped values print nicely.

@examples[#:label #f #:eval pe
          (run "'Hello World'")
          (run "(1,2,3)")
          (run "[1,2,3]")
          (run "{'a': 1, 'b': 2}")]

The values display nicely too:

@examples[#:label #f #:eval pe
          (displayln (run "'Hello World'"))
          (displayln (run "(1,2,3)"))
          (displayln (run "[1,2,3]"))
          (displayln (run "{'a': 1, 'b': 2}"))]


Printing and displaying a Python object use the
@python-docs["object.__repr__"]{__repr__} and @python-docs["object.__str__"]{__str__}
methods of the object respectively.

The idea is that Racket gains four new data types: @tt{pystring}, @tt{pytuple}, 
@tt{pylist} and @tt{pydict}. 
                                                   
To convert a compound value use @racket[pystring->string], @racket[pytuple->vector],
@racket[pylist->list] or @racket[pydict->hash].

@examples[#:label #f #:eval pe
          (pystring->string (run "'Hello World'"))
          (pytuple->vector (run "(1,2,3)"))
          (pylist->list (run "[1,2,3]"))
          (pydict->hash (run "{'a': 1, 'b': 2}"))]

Similarly, you can convert Racket values to Python ones.

@examples[#:label #f #:eval pe
          (string->pystring "Hello World")
          (vector->pytuple #(1 2 3))
          (list->pylist '(1 2 3))
          (hash->pydict (hash "a" 1 "b" 2))]

It's important to note that creating Python values using
@racket[string->pystring], @racket[vector->pytuple], @racket[list->pylist]
and @racket[hash->pydict] is much more efficient than using @racket[run].
The overhead of @racket[run] is due to the parsing and compiling of its input string.
In contrast @racket[string->pystring] and friends use the C API to create the
Python values directly.

The data types also have constructors:

@examples[#:label #f #:eval pe
          (pystring #\H #\e #\l #\l #\o)
          (pytuple 1 2 3)
          (pylist 1 2 3)
          (pydict "a" 1 "b" 2)]

The new types @tt{pystring}, @tt{pytuple}, @tt{pylist} and @tt{pydict} can be used with @racket[for].

@examples[#:label #f #:eval pe
          (for/list ([x (in-pystring (string->pystring "Hello"))]) x)
          (for/list ([x (in-pytuple (vector->pytuple #(1 2 3)))]) x)
          (for/list ([x (in-pylist (list->pylist '(1 2 3)))]) x)
          (for/list ([(k v) (in-pydict (hash->pydict (hash "a" 1 "b" 2)))]) (list k v))]


@subsection{Builtin functions and modules}

@margin-note{Use @racket[run] for expressions and @racket[run*] for statements.}
The previous sections showed how to evaluate expressions using the Python interpreter.
Now we will look at statements.

@examples[#:label #f #:eval pe
          (run* "x = 1+2")]

Here the statement @tt{x = 1+2} is parsed, compiled and executed.
The result of the expression @tt{1+2} is stored in the global variable @tt{x}.

To retrieve the value of the Python variable @tt{x} we could use @racket[run]:
@examples[#:label #f #:eval pe
          (run "x")]

But due to the overhead of @racket[run] it is better to make a direct
variable reference.

@examples[#:label #f #:eval pe
          main.x]

Here @tt{main} is the name we have given to the module used for the 
global namespace of the Python interpreter. The dotted identifier @tt{main.x}
thus references the variable @tt{x} in the global namespace.

@margin-note{The import is done with @linebreak[] @tt{import builtins}}
In a standard Python interpreter nothing is imported when the interpreter
is started, but with @racket[pyffi] the module @python-docs["builtins"]{builtins}
is imported at startup. 

Since Python modules are first class values, we can see their printed 
representations:

@examples[#:label #f #:eval pe
          main
          builtins]

@margin-note{Table of @hyperlink["https://docs.python.org/3/library/functions.html"]{Built-in functions}}
The module @tt{builtins} is also how we access the built-in Python functions.

@examples[#:label #f #:eval pe
          (builtins.abs -7)
          (builtins.list "Hello")
          (builtins.range 2 5)
          (builtins.list (builtins.range 2 5))]

If you find the name @tt{builtins} too long, then you can give it a new, shorter name.

@examples[#:label #f #:eval pe
          (define b builtins)
          (b.abs -7)]

If you access the @tt{abs} functions directly, you get a callable object:

@examples[#:label #f #:eval pe
          b.abs]

A callable object can be used just like a normal Racket function:

@examples[#:label #f #:eval pe
          (map b.abs '(1 -2 3 -4))]

To use functions from the Python standard library, you need to import it before
you can use it. The standard library @python-docs["sys"]{sys} provide
a lot of system information. Let's use it to find the version of the Python interpreter.

@examples[#:label #f #:eval pe
          (import sys)
          sys.version_info]

The list of modules in
@hyperlink["https://docs.python.org/3/library/index.html"]{The Python Standard Library}
is long, so let's just try one more.

We want to print a text calendar for the current month.

@margin-note{Documentation for @hyperlink["https://docs.python.org/3/library/calendar.html"]{@tt{calendar}}.}
@examples[#:label #f #:eval pe
          (import calendar)
          (calendar.TextCalendar)
          (displayln ((calendar.TextCalendar) .formatmonth 2022 7))]

The expression @tt{(calendar.TextCalendar)} instantiates a @tt{TextCalendar} object.
The syntax @tt{(object .method arg ...)} is used to invoke the method @tt{formatmonth}
with the arguments 2022 and 7 (for July).

The documentation for @tt{formatmonth} shows its signature:

@centered{@tt{formatmonth(theyear, themonth, w=0, l=0)}}

The two first arguments @tt{theyear} and @tt{themonth} are positional arguments
and the two last arguments @tt{w} and @tt{l} are keyword arguments both has
0 has as default value.

The keyword argument @tt{w} specifies the width of the date columns.
We can get full names of the week days with a width of 9.

@examples[#:label #f #:eval pe
          (displayln ((calendar.TextCalendar) .formatmonth 2022 7 #:w 9))]


@subsection{Objects, Callable objects, Functions, Methods and Properties}

All values in Python are represented as @emph{objects}. This differs from Racket,
where most data types (e.g. numbers and strings) aren't objects.

In the data model used by Python, all objects have an identity, a type and a value.
In practice the identity of a Python object is represented by its address in memory
[the CPython implementation never moves objects].

To represent a Python object in Racket it is wrapped in an @racket[obj] struct.
The structure contains the type name as a string and a pointer to the object.
The wrapper sets the structure property @racket[gen:custom-write] to
display, write and print the wrapped objects nicely.

The result of @tt{repr()} is used to write an object. @linebreak[]
The result of @tt{str()} is used to display an object.

@examples[#:label #f #:eval pe
          (define s (string->pystring "foo"))
          (repr s)
          (writeln s)
          (str s)
          (displayln s)]

Functions and in general @emph{callable objects} support the well-known
syntax @tt{f(a,b,c)}. Such callable objects are wrapped in an @racket[callable-obj]
struct, which has @racket[obj] as a super type. The @racket[callable-obj]
use the struct property @racket[prop:procedure] to make the wrapper applicable.

@examples[#:label #f #:eval pe
          (run* "def f(x): return x+1")
          (define f main.f)
          f
          (f 41)]

Function calls with keywords work as expected. A Python keyword
is simply prefixed with @tt{#:} to turn it into a Racket keyword,
as this example shows:

@examples[#:label #f #:eval pe
          (run* "def hello(name, title='Mr'): return 'Hello ' + title + ' ' + name")
          (displayln (main.hello "Foo"))
          (displayln (main.hello #:title "Mrs" "Bar"))]

In order to illustrate methods, let's look at the @tt{Calendar} class in
the @tt{calendar} module.

@examples[#:label #f #:eval pe
          (import calendar)
          calendar.Calendar]

Calling the class gives us an instance object. We pass 0 to make Monday the first
week day.

@examples[#:label #f #:eval pe
          (calendar.Calendar #:firstweekday 0)]

One of the methods of a calendar object is @tt{monthdatescalendar}.

@examples[#:label #f #:eval pe
          (define cal (calendar.Calendar #:firstweekday 0))
          cal.monthdatescalendar]

The syntax @tt{obj.method} gives us a @emph{bound method},
which we can call. Bound methods are wrapped in @racket[method-obj]
to make them applicable.

@margin-note{The use of @racket[pyfirst] is to reduce the amount of output.}
@examples[#:label #f #:eval pe
          (define year  2022)
          (define month    9)
          (pyfirst (pyfirst (cal.monthdatescalendar year month)))]

However, we can also invoke the @tt{monthdatescalendar} method directly
with the help of the syntax @tt{(obj .method argument ...)}.

@examples[#:label #f #:eval pe
          (pyfirst (pyfirst (cal .monthdatescalendar year month)))]

Method invocations can be chained. That is, if a method call returns
an object, we can invoke a method on it. The fist element
of a list can be retrieved by the @tt{pop} method, so we can replace
the two calls to @racket[pyfirst] with two invocations of @tt{.pop}.

@examples[#:label #f #:eval pe
          (cal .monthdatescalendar year month  .pop 0 .pop 0)]

Besides methods an object can have properties (attributes).
The syntax is @tt{obj.attribute}. Most Python objects
carry a little documentation in the oddly named @tt{__doc__} attribute.

@examples[#:label #f #:eval pe
          (displayln cal.__doc__)]


@subsection{Exceptions}

A Python exception raised during a @racket[pyffi]-mediated call is
converted to a Racket exception that carries the live Python
exception object and its class.  The conversion is lossless: every
Python attribute (@tt{args}, @tt{__cause__}, @tt{__context__},
@tt{__traceback__}, @tt{__notes__}, custom attributes the raiser
attached, every method) remains reachable from Racket through the
usual @racket[py-attr] machinery, and the original exception can
be re-raised back into Python with full identity preserved.

The hierarchy mirrors Python's split between @tt{Exception} and
@tt{BaseException}:

@itemlist[
  @item{Python @tt{Exception} subclasses (@tt{ValueError},
        @tt{KeyError}, @tt{RuntimeError}, user-defined application
        errors, …) become @racket[exn:fail:pyffi:python] — a
        subtype of @racket[exn:fail].  The standard idiom
        @racket[(with-handlers ([exn:fail? handler]) ...)] catches
        them.}
  @item{Python @tt{KeyboardInterrupt} becomes
        @racket[exn:break:pyffi:python] — a subtype of
        @racket[exn:break].  Caught by @racket[exn:break?] but
        @emph{not} by @racket[exn:fail?], matching how an
        ordinary Racket break behaves.}
  @item{Python @tt{SystemExit} is honoured directly with
        @racket[exit], mirroring how Python embeddings handle
        @tt{sys.exit}.}
  @item{Python @tt{StopIteration} continues to be returned as the
        symbol @racket['StopIteration] for iterator drivers, so
        normal iteration paths do not pay exception-machinery
        cost.}
]

The two new structs share four observable fields and a
predicate/property surface that lets callers handle either
flavour uniformly.

@examples[#:label #f #:eval pe
          (eval:error (run "1/0"))]

@subsubsection{Catching by Python class}

Catch any Python exception:

@examples[#:label #f #:eval pe
          (with-handlers ([python-exception?
                           (λ (e)
                             (list (python-exception-type-name e)
                                   (exn-message e)))])
            (run "int('not a number')"))]

The standard Racket idiom also works because
@racket[exn:fail:pyffi:python] is a subtype of @racket[exn:fail]:

@examples[#:label #f #:eval pe
          (with-handlers ([exn:fail? (λ (_) 'caught)])
            (run "{}['missing']"))]

Dispatch on a specific Python class without parsing the message:

@examples[#:label #f #:eval pe
          (with-handlers ([(λ (e)
                             (and (python-exception? e)
                                  (string=? (python-exception-type-name e)
                                            "ValueError")))
                           (λ (_) 'value-error)]
                          [python-exception? (λ (_) 'other)])
            (run "int('abc')"))]

@subsubsection{Reaching the live Python object}

@racket[python-exception-value] returns the live Python exception
instance as an @racket[obj].  Custom attributes the Python raiser
attached (such as @tt{e.errno} or @tt{e.filename}) are reachable
exactly as in Python:

@examples[#:label #f #:eval pe
          (run* "
class CustomError(Exception):
    def __init__(self, msg, errno):
        super().__init__(msg)
        self.errno = errno
")
          (with-handlers ([python-exception?
                           (λ (e)
                             (define v (obj-the-obj
                                        (python-exception-value e)))
                             (PyLong_AsLong
                              (PyObject_GetAttrString v "errno")))])
            (run* "raise CustomError('boom', 42)"))]

The same access pattern reaches @tt{__cause__}, @tt{__context__},
the live traceback object via @tt{__traceback__} (walkable
frame-by-frame), @tt{__notes__}, and every method.

@subsubsection{Round-tripping back into Python}

@racket[reraise-into-python] re-installs a previously caught
Python exception in the current thread's Python error indicator,
so the next Python call surfaces it exactly as if Racket had not
intercepted.  Identity, traceback, and chained
@tt{__cause__}/@tt{__context__} are all preserved.

@racket[raise-into-python] installs a fresh Python exception of an
arbitrary class for surfacing Racket-side failures to Python code
as a typed Python exception.

@subsubsection{Exception API}

@defstruct[(exn:fail:pyffi:python exn:fail:pyffi)
           ([class    obj?]
            [value    obj?]
            [type-name string?]
            [traceback (or/c #f (listof obj?))])
           #:transparent]{
A Python @tt{Exception}-subclass error raised inside a
@racket[pyffi]-mediated call.  Subtype of @racket[exn:fail], so
@racket[exn:fail?] catches it.

@racket[class] is the live Python class object (equivalent to
@tt{type(e)} in Python).  @racket[value] is the normalised live
Python exception instance (equivalent to the @tt{e} bound by
@tt{except ... as e:}).  @racket[type-name] is the Python class
name as a string, pre-cached so dispatch on the class name avoids
an FFI hop.  @racket[traceback] is the formatted traceback as a
list of strings; the live traceback object is reachable via
@tt{(PyObject_GetAttrString (obj-the-obj value) "__traceback__")}.
}

@defstruct[(exn:break:pyffi:python exn:break)
           ([class    obj?]
            [value    obj?]
            [type-name string?]
            [traceback (or/c #f (listof obj?))])
           #:transparent]{
A Python @tt{KeyboardInterrupt} that reached the Racket boundary.
Subtype of @racket[exn:break], so @racket[exn:break?] catches it
and a broad @racket[exn:fail?] handler does @emph{not}.

The @racket[continuation] field inherited from @racket[exn:break]
is an escape continuation captured at the point of detection.
Invoking it returns control past the failed Python call — the
closest Racket analogue to "ignore the break and continue".

This struct is constructed only by @racket[pyffi]'s internals;
consumers test the predicate but do not call the constructor
directly.
}

@defproc[(python-exception? [v any/c]) boolean?]{
Returns @racket[#t] for instances of either
@racket[exn:fail:pyffi:python] or @racket[exn:break:pyffi:python];
@racket[#f] otherwise.  This is the catch-all predicate for "did
this come from Python?"
}

@deftogether[(
  @defproc[(python-exception-class    [e python-exception?]) obj?]
  @defproc[(python-exception-value    [e python-exception?]) obj?]
  @defproc[(python-exception-type-name [e python-exception?]) string?]
  @defproc[(python-exception-traceback [e python-exception?])
           (or/c #f (listof obj?))]
)]{
Uniform accessors that work across both
@racket[exn:fail:pyffi:python] and @racket[exn:break:pyffi:python].
See the field descriptions on @racket[exn:fail:pyffi:python] for
their meanings.
}

@defmodule[pyffi/python-exception]

The @racketmodname[pyffi/python-exception] module provides the
inverse direction: re-raising into Python with full fidelity.

@defproc[(reraise-into-python [e python-exception?]) void?]{
Re-installs the Python exception @racket[e] in the current
thread's Python error indicator using @tt{PyErr_Restore}.  The
exception fires when control next returns to Python (typically
the next @racket[pyffi] call after this returns).  Identity,
traceback, @tt{__cause__}, @tt{__context__}, @tt{__notes__} and
every other instance attribute are preserved across the round
trip.

This is the lossless inverse of the conversion that produced
@racket[e]: a Python @tt{except} clause downstream of the re-raise
sees the same object identity (@tt{is}-check passes) and can
introspect every attribute as if @racket[pyffi] had never
intercepted.
}

@defproc[(reraise-into-python/from [e python-exception?]
                                   [cause (or/c obj? cpointer?)])
         void?]{
Like @racket[reraise-into-python] but additionally sets
@tt{__cause__} on the Python exception to @racket[cause], mirroring
Python's @tt{raise X from Y}.
}

@defproc[(raise-into-python [class (or/c obj? cpointer?)]
                            [#:value value (or/c #f obj? cpointer?) #f]
                            [#:from  cause (or/c #f obj? cpointer?) #f])
         void?]{
Installs a fresh Python exception of class @racket[class] in the
current thread's Python error indicator.  Used to surface
Racket-side failures to Python code as a domain-specific exception
type — for example, a Racket-side validation error becoming a
Python @tt{ValueError} that downstream Python code knows how to
catch.

When @racket[value] is supplied, it is used as the exception
instance directly; otherwise the class is constructed with no
arguments via @tt{class()}.

When @racket[cause] is supplied, sets @tt{__cause__} on the new
exception, mirroring @tt{raise New(...) from cause}.
}


@subsection{Calling Racket from Python}

The bridge from Racket to Python is symmetric.  Where
@racket[run] / @racket[run*] / the @racket[obj] @tt{prop:procedure}
machinery let Racket call Python, the
@racketmodname[pyffi/python-callback] module wraps a Racket
procedure as a Python callable that any Python code can invoke.

This is what makes Python's higher-order builtins
(@tt{map}, @tt{filter}, @tt{sorted(key=…)}, @tt{reduce}, …),
library hooks (pandas @tt{apply}, JSON @tt{default=}, scikit-learn
custom metrics, …) and callback-driven APIs (asyncio, GUI
toolkits, signal handlers, plugin architectures) usable from
Racket: Python sees an ordinary callable; calling it routes
through a single C trampoline that invokes the Racket procedure
with converted arguments and translates the result back.

@examples[#:label #f #:eval pe
          (define keep-positive? (racket-procedure->python positive?))
          (run* "
def _apply(pred, xs):
    return list(filter(pred, xs))
")
          ((run "_apply") keep-positive? (run "[1, -2, 3, -4, 5]"))]

The wrapping is shallow on the way in (Python str/int/float/bool
arrive as Racket strings/numbers/booleans; compound types stay as
@racket[obj]) and shallow on the way out (Racket strings, numbers,
booleans, lists, vectors, void/None convert directly; an @racket[obj]
returned passes through with refcount preserved).  Exceptions
raised by the Racket procedure surface to Python: a
@racket[python-exception?] re-enters Python losslessly via
@racket[reraise-into-python]; any other @racket[exn:fail?] becomes
a Python @tt{RuntimeError} carrying the Racket exception message.

@defmodule[pyffi/python-callback]

@defproc[(racket-procedure->python [proc procedure?]
                                   [#:name name string? "racket-procedure"]
                                   [#:doc  doc  (or/c string? #f) #f])
         obj?]{
Wrap @racket[proc] as a Python callable.  The returned @racket[obj]
wraps a Python function whose @tt{__name__} is @racket[name] and
whose @tt{__doc__} is @racket[doc].  When Python calls the
function, the positional arguments are converted to Racket values
(via the deep @tt{python->racket} converter), @racket[proc] is
invoked, and its result is converted back to Python.

Each @racket[racket-procedure->python] call interns the Racket
procedure in a per-module hash so it remains live for as long as
the Python wrapper exists; when Python finalises the wrapper, the
hash entry is released and the Racket procedure becomes eligible
for garbage collection.

Also exported under the alias @racket[py-lambda].
}

@deftogether[(@defproc[(py-lambda [proc procedure?]
                                  [#:name name string? "racket-procedure"]
                                  [#:doc  doc  (or/c string? #f) #f])
                       obj?])]{
Alias for @racket[racket-procedure->python].
}


@section{Initialization of the Python Interpreter}

@subsection{The Big Picture}

In order to use the functions in @racket[pyffi] you need to start a Python interpreter.
The call @racket[(initialize)] does just that. A standard approach would be to
make the call in your "main" module of your program.

That the Python interpreter isn't available until the main module has been instantiated
leads to a few complications. The problem is the interpreter instance is not available
when the modules required by "main" is instantiated.

As an example: The module @racket[pyffi/numpy] contains bindings for NumPy.
If the main module looks like this:

@nested[#:style 'inset]{
  @verbatim{#lang racket
            (require pyffi pyffi/numpy)
            (initialize)}}

Then @racket[pyffi/numpy] is instantiated before the interpreter is started.
This means @racket[pyffi/numpy] can't inspect the Python module @tt{numpy}
to get the function signatures it needs.

To solve this problem @racket[pyffi/numpy] registers a number of initialization
thunks to be run after the interpreter has started. The function
@racket[post-initialize] runs these initialization thunks.

To sum up, the typical main module for a program that uses @tt{pyffi} starts:

@nested[#:style 'inset]{
  @verbatim{#lang racket
            (require pyffi pyffi/numpy)
            (initialize)
            (post-initialize)}}

@subsection{Reference}


@defproc[(initialize) void?]{
Starts a Python interpreter using @tt{libpython}.

The precise steps taken are:

@itemlist[
  @item{The locations of @tt{libpython} and the folder containing
        the standard library is fetched from the preferences 
        (keys @racket['pyffi:libdir] and @racket['pyffi:data]).}
  @item{Calls @python-docs["Py_Initialize"]{Py_Initialize}
        which starts a Python interpreter. Initializes the
        table of of loaded modules and creates the modules @tt{builtins},
        @tt{__main__} and @tt{sys}.}
  @item{Imports @tt{__main__}, @tt{builtins}, @tt{operator}, @tt{traceback} and @tt{inspect}.}
  @item{Creates instances of @tt{True}, @tt{False, and, @tt{None}.}}]
}

@defproc[(post-initialize) void?]{
Run initialization thunks that needs a running Python instance.
}

@defproc[(add-initialization-thunk [thunk thunk?]) void?]{
Add a thunk to be run by @racket[post-initialize].
}

@defproc[(diagnostics) void?]{
Print important Python paths.
}


@section{Evaluation}

@defproc[(run [string-to-evaluate string?]) obj?]{
Evaluate the string @racket[string-to-evaluate] in the running Python interpreter.
The string must contain a Python expression. 
The expression is evaluated in the "main" module.

The resulting Python object is converted to a Racket value via @racket[pr].

If an exception is triggered on the Python side, a Racket exception is 
raised containing the error message and the traceback.

@examples[#:label #f #:eval pe
          (run "[1+10, '2'+'20', (3,30), {4: 40}]")
          (eval:error (run "x = 'this is not an expression'"))]
}

@defproc[(run* [string-to-evaluate string?]) void?]{
Evaluate the string @racket[string-to-evaluate] in the running Python interpreter.
The string must contain a Python statement.
The statement is executed in the "main" module.

If an exception is triggered on the Python side, a Racket exception is 
raised containing the error message and the traceback.

@examples[#:label #f #:eval pe
          (run* "x = 1+10")
          main.x]
}


@section{Datatypes}

In Python all values are stored as objects. An object has an identity, a type and a value.

The identity of an object is given by the object's address in memory.
The @tt{is} operator in Python corresponds to @racket[eq?] in Racket.

Objects whose value can change are @emph{mutable}.
If the value of an object can't change the object is called @emph{immutable}.
Note: In Racket an immutable vector can contain (mutable) boxes.
Even though the vector is immutable the contents of the boxes can change.
What doesn't change is the identity of the boxes in the vector.
In the same way an immutable Python container might contain mutable objects.

In Python numbers, strings and tuples are immutable.
Python dictionaries and lists are mutable.

Python objects aren't explicitly destroyed. When an object becomes unreachable,
the memory of an object is reclaimed by the garbage collector. The current
Python interpreter uses @emph{reference counting} to keep track of an
objects reachability.

When a Python object is returned from Python to Racket as a @emph{new reference},
the Python system won't deallocate the object until Racket says it's okay to do so.
Since we don't want manually to keep track of the lifetime of Python objects,
the low-level C-bindings register all new references will a will executor.
When the Racket garbage collector detects a value is unreachable, the garbage
collector will execute any wills associated with the value. The will executor
we are using, simply decrements the reference count of the Python object.

If you stick to the high level functions in @racket[pyffi] you don't need
to worry about reference counting. However it might be relevant if you
need to use the low-level C-API.

@;;;
@;;; PYTHON LISTS
@;;; 

@subsection{Python Lists - @tt{pylist}}

Despite the name a Python list is not a singly linked list, but an array.
Objects with the type "list" will be called @racket[pylist] to tell
them apart from standard Racket lists.

The operations @racket[pylist], @racket[list->pylist] and @racket[vector->pylist]
can be used to construct pylists from Racket values.

In in @racket[for]-loops, use @racket[in-pylist] to iterate through the elements. 


@defproc[(pylist? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a pylist (an @racket[obj] with type @racket["list"]).
                               
@examples[#:label #f #:eval pe
          (pylist 1 2 3)
          (pylist? (pylist 1 2 3))
          (pylist? (pylist))
          (pylist? '(1 2 3))]
}

@defproc[(pylist [v any/c] ...) pylist?]{
Returns a newly allocated pylist containing the @racket[v]s as its elements.
                               
@examples[#:label #f #:eval pe
          (pylist 1 2 3 4)
          (pylist)
          (pylist (pylist 1 2 3 4) 5 (pylist 6 7))]
}

@defproc[(pylist-ref [xs pylist?] [i exact-nonnegative-integer?])  any/c]{
Returns the element with index @racket[i] in the pylist @racket[xs].
The first element has index 0, and the last elements is one less than @racket[(pylist-length xs)].

This function takes constant time.                               

@examples[#:label #f #:eval pe
          (pylist-ref (pylist "a" "b" "c" "d") 1)]
}

@defproc[(pyfirst [xs pylist?])  any/c]{
Returns the first element of the pylist @racket[xs].

@examples[#:label #f #:eval pe
          (pyfirst (pylist "a" "b" "c" "d"))]
}

@defproc[(pysecond [xs pylist?])  any/c]{
Returns the second element of the pylist @racket[xs].

@examples[#:label #f #:eval pe
          (pysecond (pylist "a" "b" "c" "d"))]
}


@defproc[(pylist-set! [xs pylist?] [i exact-nonnegative-integer?] [v any/c])  any/c]{
Replace the element with index @racket[i] of the pylist @racket[xs] with the value @racket[v].

This function takes constant time.


@examples[#:label #f #:eval pe
          (define xs (pylist 0 1 2 3 4))
          (pylist-set! xs 1 #t)
          xs]
}

@defproc[(list->pylist [xs list?]) pylist?]{
Returns a pylist with the same length and (possibly converted) elements as @racket[xs]. @linebreak[]
The elements are converted with @racket[racket->python].
                               
@examples[#:label #f #:eval pe
          (list->pylist '(1 2 3 4))
          (list->pylist '(1 "foo" #(3 4)))
          (list->pylist '(1 (2 3) #(4 (5 6))))]
}

@defproc[(vector->pylist [xs vector?]) pylist?]{
Returns a pylist with the same length and (possibly converted) elements as @racket[xs]. @linebreak[]
The elements are converted with @racket[racket->python].
                               
@examples[#:label #f #:eval pe
          (vector->pylist '#(1 2 3 4))
          (vector->pylist '#(1 "foo" #(3 4)))
          (vector->pylist '#(1 (2 3) #(4 (5 6))))]
}

@defproc[(pylist-length [xs pylist?]) integer?]{
Returns the length (size) of a pylist (i.e. the number of elements in the pylist).
                               
@examples[#:label #f #:eval pe
          (pylist-length (pylist 1 2 3))]
}



@defproc[(pylist->list [xs pylist?]) list?]{
Returns a list with the same length and elements as @racket[xs].

This function takes time proportional to the size of @racket[xs].

@examples[#:label #f #:eval pe
          (pylist->list (pylist 1 2 3 #t #f "a"))]
}

@defproc[(pylist->vector [xs pylist?]) vector?]{
Returns a vector with the same length and elements as @racket[xs].

This function takes time proportional to the size of @racket[xs].

@examples[#:label #f #:eval pe
          (pylist->vector (pylist 1 2 3 #t #f "a"))]
}

@defproc[(pylist->pytuple [xs pylist?])  pytuple?]{
Returns a @racket[pytuple] with the same length and elements as @racket[xs].

This function takes time proportional to the size of @racket[xs].

@examples[#:label #f #:eval pe
          (pylist->pytuple (pylist 1 2 3 #t #f "a"))]
}


@defproc[(in-pylist [xs pylist?]) stream?]{
Returns a sequence (that is also a stream) that is equivalent to using 
@racket[xs] directly as a sequence.
                                            
@examples[#:label #f #:eval pe
          (define xs (pylist 0 1 2 3))
          (for/list ([x (in-pylist xs)])
            x)]
}




@defproc[(pylist-insert! [xs pylist?] [i exact-nonnegative-integer?] [v any/c]) void?]{
Inserts the value @racket[v] in the pylist @racket[xs] at index @racket[i].

Worst case this function takes time proportional to the size of @racket[xs].

@examples[#:label #f #:eval pe
          (define xs (pylist 0 1 2 3))
          (pylist-insert! xs 2 #t)
          xs]
}

@defproc[(pylist-append-item! [xs pylist?] [v any/c]) void?]{
Add the element @racket[v] to the end of the pylist @racket[xs].
The length of the pylist becomes 1 greater.

@examples[#:label #f #:eval pe
          (define xs (pylist 10 11 12 13))
          (pylist-length xs)
          (pylist-append-item! xs 14)
          xs
          (pylist-length xs)]
}


@defproc[(pylist-reverse! [xs pylist?]) void?]{
Reverse the order in which the elements in the pylist @racket[xs] occur.

@examples[#:label #f #:eval pe
          (define xs (pylist 1 2 3 4))
          (pylist-reverse! xs)
          xs]
}

@defproc[(pylist-sort! [xs pylist?]) void?]{
Rearrange the order in which the elements in the pylist @racket[xs] occur.
After calling @racket[pylist-sort!] the elements will be in order with respect
to the Python comparison operator @tt{<}.

@examples[#:label #f #:eval pe
          (define xs (pylist 3 2 4 1))
          (pylist-sort! xs)
          xs
          (define ys (pylist 3 #t 2 4 #f #f 1))
          (pylist-sort! ys)
          ys]
}



@defproc[(pylist-get-slice [xs pylist?]
                           [low-index exact-nonnegative-integer?]
                           [high-index exact-nonnegative-integer?])
         pylist?]{
Returns a new pylist with the elements of @racket[xs]
from index @racket[low-index] inclusive
to   index @racket[high-index] exclusive.

In Python notation: @tt{list[low:high]}.

@examples[#:label #f #:eval pe
          (define xs (pylist 1 2 3 #t #f "a"))
          (pylist-get-slice xs 1 3)]
}


@;(
@;; @defproc[(pylist-new [k exact-nonnegative-integer?]) pylist?]{
@;; Returns a newly constructed pylist of length #racket[k].
@;; If @racket[k] is greater than zero, the elements of the pylist is set to @tt{NULL}.
@;; Use @racket[pylist-set!] to set the elements of the list to non-@tt{NULL} before
@;; using other pylist functions.
                               
@;; @examples[#:label #f #:eval pe
@;;           (pylist-new 3)
@;;           (pylist-new 0)]
@;; })



@;;;
@;;; PYTHON TUPLES
@;;; 

@subsection{Python Tuples - @tt{pytuple}}

Python tuples correspond to immutable Racket vectors.

Even though there is no datastructure in Racket called "tuple", 
Python tuples will have the name "pytuple" to match the names of
@racket[pylist] and @racket[pydict].

The operations @racket[pytuple], @racket[list->pytuple] and @racket[vector->pytuple]
can be used to construct pytuples from Racket values.

In in @racket[for]-loops, use @racket[in-pytuple] to iterate through the elements. 

@defproc[(pytuple? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a pytuple (an @racket[obj] with type @racket["tuple"]).
                               
@examples[#:label #f #:eval pe
          (pytuple 1 2 3)
          (pytuple? (pytuple 1 2 3))
          (pytuple? (pytuple))
          (pytuple? '(1 2 3))]
}

@defproc[(pytuple [v any/c] ...) pytuple?]{
Returns a newly allocated pytuple containing the @racket[v]s as its elements.
                               
@examples[#:label #f #:eval pe
          (pytuple 1 2 3 4)
          (pytuple)
          (pytuple (pytuple 1 2 3 4) 5 (pytuple 6 7))]
}

@defproc[(pytuple-ref [xs pytuple?] [i exact-nonnegative-integer?])  any/c]{
Returns the element with index @racket[i] in the pytuple @racket[xs].
The first element has index 0, and the last elements is one less than @racket[(pytuple-length xs)].

This function takes constant time.                               

@examples[#:label #f #:eval pe
          (pytuple-ref (pytuple "a" "b" "c" "d") 1)]
}


@defproc[(list->pytuple [xs list?]) pytuple?]{
Returns a pytuple with the same length and (possibly converted) elements as @racket[xs]. @linebreak[]
The elements are converted with @racket[racket->python].
                               
@examples[#:label #f #:eval pe
          (list->pytuple '(1 2 3 4))
          (list->pytuple '(1 "foo" #(3 4)))
          (list->pytuple '(1 (2 3) #(4 (5 6))))]
}

@defproc[(vector->pytuple [xs vector?]) pytuple?]{
Returns a pytuple with the same length and (possibly converted) elements as @racket[xs]. @linebreak[]
The elements are converted with @racket[racket->python].
                               
@examples[#:label #f #:eval pe
          (vector->pytuple '#(1 2 3 4))
          (vector->pytuple '#(1 "foo" #(3 4)))
          (vector->pytuple '#(1 (2 3) #(4 (5 6))))]
}

@defproc[(pytuple-length [xs pytuple?]) integer?]{
Returns the length (size) of a pytuple (i.e. the number of elements in the pytuple).
                               
@examples[#:label #f #:eval pe
          (pytuple-length (pytuple 1 2 3))]
}


@defproc[(pytuple->list [xs pytuple?]) list?]{
Returns a list with the same length and elements as @racket[xs].

This function takes time proportional to the size of @racket[xs].

@examples[#:label #f #:eval pe
          (pytuple->list (pytuple 1 2 3 #t #f "a"))]
}

@defproc[(pytuple->vector [xs pytuple?]) vector?]{
Returns a vector with the same length and elements as @racket[xs].

This function takes time proportional to the size of @racket[xs].

@examples[#:label #f #:eval pe
          (pytuple->vector (pytuple 1 2 3 #t #f "a"))]
}

@defproc[(pytuple->immutable-vector [xs pytuple?]) vector?]{
Returns an immutable vector with the same length and elements as @racket[xs].

This function takes time proportional to the size of @racket[xs].

@examples[#:label #f #:eval pe
          (define xs (pytuple->immutable-vector (pytuple 1 2 3 #t #f "a")))
          xs
          (immutable? xs)]
}

@defproc[(pytuple->pylist [xs pytuple?])  pylist?]{
Returns a @racket[pylist] with the same length and elements as @racket[xs].

This function takes time proportional to the size of @racket[xs].

@examples[#:label #f #:eval pe
          (pytuple->pylist (pytuple 1 2 3 #t #f "a"))]
}


@defproc[(in-pytuple [xs pylist?]) stream?]{
Returns a sequence (that is also a stream) that is equivalent to using 
@racket[xs] directly as a sequence.
                                            
@examples[#:label #f #:eval pe
          (define xs (pytuple 0 1 2 3))
          (for/list ([x (in-pytuple xs)])
            x)]
}


@defproc[(pytuple-get-slice [xs pytuple?]
                            [low-index exact-nonnegative-integer?]
                            [high-index exact-nonnegative-integer?])
         pytuple?]{
Returns a new pytuple with the elements of @racket[xs]
from index @racket[low-index] inclusive
to   index @racket[high-index] exclusive.

In Python notation: @tt{list[low:high]}.

@examples[#:label #f #:eval pe
          (define xs (pytuple 1 2 3 #t #f "a"))
          (pytuple-get-slice xs 1 3)]
}

@;;;
@;;; PYTHON DICTIONARIES
@;;; 

@subsection{Python Dictionaries - @tt{pydict}}

Dictionaries in Python are associative arrays indexed by @emph{keys}.
Given a key one can lookup a value. Think of dictionaries as
sets of key/value pairs. 

Any immutable value can be used as a key, so strings, numbers and tuples
can always be used as keys. 

The corresponding data structure in Racket is the hash table.


Use the operations @racket[pydict->hash] and @racket[hash->pydict]
to convert back and forth between pydicts and hash tables.

@defproc[(pydict? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a pydict (an @racket[obj] with type @racket["dict"]).
                               
@examples[#:label #f #:eval pe
          (pydict? (hash->pydict (hash "a" 1  "b" 2)))]
}



@defproc[(hash->pydict [x hash?] [#:convert convert procedure? rp]) pydict?]{
Returns a newly allocated pydict with the same key/value-pairs as the hash table @racket[x].
The function @racket[convert] is used to convert Racket values to Python ones.
The default value for the keyword argument @racket[convert] is @racket[rp].
                               
@examples[#:label #f #:eval pe
          (hash->pydict (hash "a" 1  "b" 2))
          (hash->pydict (hash 1 "x"  2 "y"))
          (hash->pydict (hash #(1 2) "tuple used as key"))]                        
}

@defproc[(pydict->hash [x pydict?]
                       [#:convert-key   convert-key   procedure? pr/key]
                       [#:convert-value convert-value procedure? pr])
         hash?]{
Returns a newly allocated hash table with the same key/value-pairs as the pydict @racket[x].

The function @racket[convert-key]   is used to convert the keys from Python values to Racket ones.@linebreak[]
The function @racket[convert-value] is used to convert the values.

The default value for the key conversion is @racket[pr/key].@linebreak[]
The default value for the value conversion is @racket[pr].

                               
@examples[#:label #f #:eval pe          
          (pydict->hash (hash->pydict (hash "a" 1  "b" 2)))
          (pydict->hash (hash->pydict (hash 1 "x"  2 "y")))
          (pydict->hash (hash->pydict (hash #(1 2) "tuple used as key")))]
}


@defproc[(pydict [#:convert convert procedure? rp] 
                 [key any/c] [val any/c] ... ...) pydict?]{
Creates a pydict with each given key mapped to the following val;
each key must have a val, so the total number of arguments to hash must be even.

The key to val mappings are added to the table in the order that they
appear in the argument list, so later mappings can hide earlier
mappings if the keys are equal.


The default value for the key and value conversion is @racket[rp].

                               
@examples[#:label #f #:eval pe          
          (pydict "a" 1  "b" 2)
          (pydict 1 "x"  2 "y")
          (pydict #(1 2) "tuple used as key")]
}



@defproc[(pydict-ref [d pydict?] [key any/c]
                     [failure-result failure-result/c 	
                                     (lambda ()
                                       (raise (make-exn:fail:contract ....)))])
         any/c]{

Returns the value for key in hash. If no value is found for key, then
failure-result determines the result:

If failure-result is a procedure, it is called (through a tail call)
with no arguments to produce the result.

Otherwise, failure-result is returned as the result.
                               
@examples[#:label #f #:eval pe
          (define d (pydict "a" 1  "b" 2))
          d
          (pydict-ref d "a")
          (pydict-ref d "b")
          (eval:error (pydict-ref d "c"))]
}


@defproc[(pydict-set! [d pydict?] [key any/c] [val any/c]) void?]{
Maps @racket[key] to @racket[val] in @racket[d], overwriting any existing mapping for key.
                                                                  
                               
@examples[#:label #f #:eval pe
          (define d (pydict "a" 1))
          (pydict-set! d "a" 11)
          (pydict-set! d "b" 22)
          d]
}


@defproc[(pydict-remove! [d pydict?] [key any/c]) void?]{
Removes any existing mapping for @racket[key] in the pydict @racket[d].
                                                                  
                               
@examples[#:label #f #:eval pe
          (define d (pydict "a" 1 "b" 2))
          (pydict-remove! d "b")
          (pydict-remove! d "c")
          d]
}


@defproc[(pydict-clear! [d pydict?]) void?]{
Remove all key/value pairs from the dictionary.
                               
@examples[#:label #f #:eval pe
          (define d (pydict "a" 1  "b" 2))
          d
          (pydict-clear! d)
          d]
}


@defproc[(pydict-contains? [d pydict?] [key any/c]) boolean?]{
Returns @racket[#t] if the pydict @racket[x] contains the key @racket[key].

                               
@examples[#:label #f #:eval pe
          (define d (pydict "a" 1  "b" 2))
          (pydict-contains? d "a")
          (pydict-contains? d "x")]
          
}


@defproc[(pydict-copy [d pydict?]) pydict?]{
Return a new dict with the same key/value pairings as the pydict @racket[d].

                               
@examples[#:label #f #:eval pe
          (define d1 (pydict "a" 1  "b" 2))
          (define d2 (pydict-copy d1))
          (list d1 d2)
          (pydict-set! d1 "a" 11)
          (list d1 d2)]
}


@defproc[(pydict-keys [d pydict?]) pylist?]{
Return a pylist with all keys in the pydict @racket[d].
                               
@examples[#:label #f #:eval pe
          (define d (pydict "a" 1  "b" 2))
          (pydict-keys d)]
}

@defproc[(pydict-values [d pydict?]) pylist?]{
Return a pylist with all values in the pydict @racket[d].
                               
@examples[#:label #f #:eval pe
          (define d (pydict "a" 1  "b" 2 "c" 1))
          (pydict-values d)]
}


@defproc[(pydict-count [d pydict?]) integer?]{
Returns the number of keys mapped by the pydict @racket[hash].
                               
@examples[#:label #f #:eval pe
          (define d (pydict "a" 1  "b" 2 "c" 1))
          (pydict-count d)]
}



@defproc[(pydict-merge! [d1 pydict?] [d2 pydict?] [override? boolean? #t]) void?]{
Computes the union of @racket[d1] with the @racket[d2] by mutable update,
adding each element of @racket[d2] to @racket[d1] in turn. 

If a key @racket[k] is present in both pydicts and is mapped to
values @racket[v1] and @racket[v2] in @racket[d1] and @racket[d2] respectively,
then @racket[k] is mapped to @racket[v2] if @racket[override?] is true,
and mapped to @racket[v1] otherwise.
                               
@examples[#:label #f #:eval pe
          (define d1 (pydict "a" 1  "b" 2 ))
          (define d2 (pydict "b" 22 "c" 33))
          (pydict-merge! d1 d2)
          d1]

@examples[#:label #f #:eval pe
          (define d1 (pydict "a" 1  "b" 2 ))
          (define d2 (pydict "b" 22 "c" 33))
          (pydict-merge! d1 d2 #f)
          d1]
}

@defproc[(in-pydict [d pydict?]) stream?]{
Returns a sequence (that is also a stream) equivalent to  
@racket[d] directly as a sequence.
                                            
@examples[#:label #f #:eval pe
          (define d (pydict "a" 1 "b" 2))
          (for/list ([(key value) (in-pydict d)])
            (list key value))]
}


@;;;
@;;; PYTHON STRINGS (UNICODE OBJECTS)
@;;; 

@subsection{Python Strings - @tt{pystring}}

Strings in Python are represented as objects with type @tt{str}.
Python strings are immutable sequences of Unicode code points.

There is no character type in Python, so various Python libraries
use strings of length 1 to represent characters.


@defproc[(pystring? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a pystring (an @racket[obj] with type @racket["str"]).
                               
@examples[#:label #f #:eval pe
          (string->pystring "foo")
          (pystring? (string->pystring "foo"))]
}

@defproc[(pystring [char char?] ...) pystring?]{ 
Returns a new pystring whose length is the number of provided
@racket[char]s, and whose positions are initialized with the given
@racket[char]s.


@examples[#:label #f #:eval pe
          (pystring #\f #\o #\o)]
}


@defproc[(string->pystring [x string?]) pystring?]{ 
Return a newly allocated pystring with the same characters as the string @racket[x].

@examples[#:label #f #:eval pe
          (string->pystring "foo")]
}


@defproc[(pystring->string [x pystring?]) string?]{ 
Return a newly allocated string with the same characters as the pystring @racket[x].

@examples[#:label #f #:eval pe
          (pystring->string (pystring #\f #\o #\o))]
}


@defproc[(pystring-length [x pystring?]) integer?]{ 
Returns the length of @racket[x] (i.e. the number of characters in string).

@examples[#:label #f #:eval pe
          (pystring-length (pystring #\f #\o #\o))]
}


@defproc[(pystring-ref [x pystring?] [k exact-nonnegative-integer?]) char?]{ 
Returns the character at position @racket[k] in the pystring @racket[x].
The first position in the string corresponds to @racket[0], so the
position @racket[k] must be less than the length of the string,
otherwise the exception @racket[exn:fail:contract] is raised.

@examples[#:label #f #:eval pe
          (define foo (pystring #\f #\o #\o))
          (pystring-ref foo 0)
          (eval:error (pystring-ref foo 10))]          
}


@defproc[(subpystring [x pystring?]
                      [start exact-nonnegative-integer?]
                      [end exact-nonnegative-integer? (pystring-length x)])
         pystring?]{ 

Returns a pystring that is @racket[(- end start)] characters
long, and that contains the same characters as @racket[x] from
@racket[start] inclusive to @racket[end] exclusive.

The first position in a string corresponds to @racket[0], so the @racket[start]
and @racket[end] arguments must be less than or equal to the length of
@racket[x], and @racket[end] must be greater than or equal to
@racket[start], otherwise the exception @racket[exn:fail:contract] is raised.
                    
@examples[#:label #f #:eval pe
          (subpystring (string->pystring "foobarbaz") 3 6)]
}

@defproc[(in-pystring [x pystring]) stream?]{
Returns a sequence (that is also a stream) that is equivalent to using 
@racket[x] directly as a sequence.
                                            
@examples[#:label #f #:eval pe
          (define x (string->pystring "foo"))
          (for/list ([c (in-pystring x)])
            c)]
}

@;;;
@;;; PYTHON GENERATORS
@;;;

@subsection{Python Generators}

Python generators correspond to Racket generators from @racket[racket/generator].
Think of a generator as a function that can produce a series of values.

If the body of Python function contains a @tt{yield} statement, calling the
function returns a generator object. Use @tt{next} repeatedly on the generator
to generate the series of values.

The @tt{yield expr} statement both "pauses" the computation of the generator and returns
the result of evaluating the expression. The computation is resumed by @tt{next}.

This example shows a generator that produces the natural numbers.

@verbatim{ (run*  @~a|{@~a{def f():
                             x=0
                             while 1:
                               x=x+1
                             yield x}
           (let ([g (main.f)])
             (list (next g) (next g) (next g))) }|)}

This produces the list @racket[(list 1 2 3)].


Usually the most convenient way of using such a generator is to use @racket[in-pygenerator].

@defproc[(in-pygenerator [pygen pygenerator?]) stream?]{
Returns a sequence (that is also a stream) that produces 
the elements from @tt{pygen}.

@verbatim{ (let ([g (main.f)])
             (for ([_ 3] 
                   [x (in-pygenerator g)])
               x))}}

Generators are automatically wrapped in an @racket[generator-obj] struct,
which has @racket[obj] as super type. The wrapping allows us to make the
@racket[in-generator] implicit in @racket[for] loops.

@verbatim{ (let ([g (main.f)])
             (for ([_ 3] [x g])
               x))}







@;;;
@;;; PYTHON OBJECTS
@;;;


@;;; ;{
@;; @subsection{Under the hood}

@;; The Python interpreter is available as shared library @tt{libpython}
@;; The shared library provides the (almost) all of the functions described in
@;; @hyperlink["https://docs.python.org/3/c-api/index.html"]{Python/C API Reference Manual}
@;; }

@index-section[]
