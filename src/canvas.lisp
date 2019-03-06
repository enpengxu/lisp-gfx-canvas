;;; C-demo
(require 'cffi)
(defpackage :lisp-to-c-user
  (:use :cl :cffi))
 
(in-package :lisp-to-c-user)

(define-foreign-library liblispcanvas
  (:darwin "liblispcanvas.dylib")
  (:unix "/home/leo/github/lisp-gfx-canvas/debug/liblispcanvas.so")
  (t (:default "/home/leo/github/lisp-gfx-canvas/debug/liblispcanvas.so")))
 
(load-foreign-library 'liblispcanvas)
(use-foreign-library liblispcanvas)

 (defcfun "canvas_init" :int
  (w :int)
  (h :int))
;;(defcfun "canvas_fini" :void)
 
