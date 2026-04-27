#lang racket
(require pyffi)

;; Setup Python
(initialize)
(finish-initialization)


;; pyautogui is an optional Python dependency that also requires
;; a graphical display.  Skip silently when it's not importable
;; instead of failing the test suite for environments that don't
;; have it.
(define pyautogui-available?
  (with-handlers ([exn:fail? (λ (e) #f)])
    (run* "import pyautogui")
    #t))

(when pyautogui-available?
  (run* "import pyautogui")
  (run "pyautogui.position()")
  (run "pyautogui.write('echo \"Hello World\" \\n')"))
