;;; C-demo
(require 'cffi)
(defpackage :lisp-to-c-user
  (:use :cl :cffi))
 
(in-package :lisp-to-c-user)

(define-foreign-library liblispcanvas
  (:darwin "liblispcanvas.dylib")
  (:unix "~/github/lisp-gfx-canvas/debug/liblispcanvas.so")
  (t (:default "~/github/lisp-gfx-canvas/debug/liblispcanvas.so")))
 
(load-foreign-library 'liblispcanvas)
(use-foreign-library liblispcanvas)

(defcfun "canvas_init" :int
  (w :int)
  (h :int))
;;(defcfun "canvas_fini" :void)

(defcfun "canvas_fini" :void
  (w :int))


(defcfun "foo" :int
  (w :int)
  (h :int))

(foo 200 20)
(canvas-init 500 600)
