#lang racket/base

;;; Python exception interop helpers
;;;
;;; This module provides the lossless round-trip surface for Python
;;; exceptions caught at the Racket boundary:
;;;
;;;   - `reraise-into-python`        — re-install a previously caught
;;;                                    Python exception in the current
;;;                                    thread's Python error indicator,
;;;                                    preserving identity, traceback,
;;;                                    chained `__cause__`/`__context__`,
;;;                                    notes, and all other instance
;;;                                    attributes.  The exception fires
;;;                                    when control next returns to
;;;                                    Python (the next pyffi call).
;;;
;;;   - `reraise-into-python/from`   — like `reraise-into-python`, but
;;;                                    additionally sets `__cause__` to
;;;                                    a supplied Python exception
;;;                                    object, mirroring Python's
;;;                                    `raise X from Y`.
;;;
;;;   - `raise-into-python`          — install a fresh Python exception
;;;                                    of the given class with optional
;;;                                    value and `__cause__`.  For
;;;                                    Racket-side errors that the
;;;                                    caller wants Python to see as a
;;;                                    domain-specific exception type.
;;;
;;; The `handle-python-exception` macro that turns Python failures into
;;; the typed Racket exns lives in python-types.rkt; this module is the
;;; complementary direction.

(require "structs.rkt"
         "python-c-api.rkt")

(provide reraise-into-python
         reraise-into-python/from
         raise-into-python)

;; Helper: extract the underlying Python pointer from an `obj` wrapper,
;; or pass through a raw pointer.
(define (coerce-py-pointer x)
  (cond
    [(obj? x) (obj-the-obj x)]
    [else     x]))

;; Bump a Python pointer's refcount, tolerating #f / NULL.
(define (incref! p)
  (when p (Py_IncRef p)))

;; Re-install a previously caught Python exception in the current
;; thread's Python error indicator.  No effect on Racket flow; the
;; exception fires the next time control returns to Python (typically
;; the next pyffi call after this returns).
;;
;; `e` must be a `python-exception?` (either flavour).  The live class,
;; value and traceback are read off the struct and handed to
;; `PyErr_Restore`, which steals references — we Py_IncRef each so
;; the obj wrappers can still safely Py_DecRef on finalisation.
(define (reraise-into-python e)
  (unless (python-exception? e)
    (raise-argument-error 'reraise-into-python "python-exception?" e))
  (define cls   (coerce-py-pointer (python-exception-class e)))
  (define value (coerce-py-pointer (python-exception-value e)))
  ;; The live traceback object stays on value.__traceback__ across the
  ;; round trip — read it directly so we restore the original frame
  ;; info instead of synthesising a new one.
  (define tb    (and value (PyObject_GetAttrString value "__traceback__")))
  (when (equal? tb the-None) (set! tb #f))
  (incref! cls)
  (incref! value)
  (incref! tb)
  (PyErr_Restore cls value tb))

;; Like `reraise-into-python` but additionally sets `__cause__` on the
;; exception to `cause` (an `obj` or raw Python pointer wrapping a
;; Python exception instance).  Mirrors Python's `raise X from Y`.
;;
;; `__context__` is left as whatever was already on the value; if you
;; want to clear it use Python's `e.__suppress_context__ = True` via
;; `py-attr` before calling.
(define (reraise-into-python/from e cause)
  (unless (python-exception? e)
    (raise-argument-error 'reraise-into-python/from "python-exception?" e 0))
  (define cause-ptr (coerce-py-pointer cause))
  (unless cause-ptr
    (raise-argument-error 'reraise-into-python/from "Python exception object" cause 1))
  (define value (coerce-py-pointer (python-exception-value e)))
  ;; PyException_SetCause steals a reference to `cause-ptr`.
  (incref! cause-ptr)
  (PyException_SetCause value cause-ptr)
  (reraise-into-python e))

;; Install a fresh Python exception of class `cls` (an `obj` or raw
;; pointer wrapping a Python exception class) into the error
;; indicator.  Used to surface a Racket-side failure to Python as a
;; domain-specific exception type — for example a Racket-side
;; validation error becoming a `ValueError` to downstream Python code.
;;
;; `value` is optional: when supplied, used as the exception
;; instance directly; when omitted, the class is constructed with no
;; arguments via `cls()`.
;;
;; `cause` is optional: when supplied, sets `__cause__` on the new
;; exception, mirroring `raise New(...) from cause`.
(define (raise-into-python cls
                            #:value [value #f]
                            #:from  [cause #f])
  (define cls-ptr (coerce-py-pointer cls))
  (unless cls-ptr
    (raise-argument-error 'raise-into-python "Python exception class" cls))
  ;; Build the exception value.  If the caller supplied one, use it
  ;; directly; otherwise instantiate with no args (`cls()`).
  (define value-ptr
    (cond
      [value (coerce-py-pointer value)]
      [else (PyObject_CallNoArgs cls-ptr)]))
  (when cause
    (define cause-ptr (coerce-py-pointer cause))
    (incref! cause-ptr)
    (PyException_SetCause value-ptr cause-ptr))
  (incref! cls-ptr)
  (incref! value-ptr)
  (PyErr_Restore cls-ptr value-ptr #f))
