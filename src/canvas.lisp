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
;;;(defcfun "canvas_fini" :void)
(defcfun "canvas_fini" :void)

;;;void canvas_active(int canvas);
(defcfun "canvas_active" :void
  (canvas :int))

;;int canvas_draw_begin(int primitive);
(defcfun "canvas_draw_begin" :int
  (primitive :int))

;;int canvas_point_color(float r, float g, float b);
(defcfun "canvas_point_color" :int
  (r :float)
  (g :float)
  (b :float))

;;int canvas_point_size(float s);
(defcfun "canvas_point_size" :int
  (s :float))

;;int canvas_draw_point(float x, float y);
(defcfun "canvas_draw_point" :int
  (x :float)
  (y :float))

;;int canvas_draw_end();
(defcfun "canvas_draw_end" :int)

;;int canvas_remove_points(int num);
(defcfun "canvas_remove_points" :int)

;; (foo 200 20)
(canvas-init 800 600)
(canvas-active 0)

(canvas-point-size 10.0)
(canvas-point-color 1.0 0.0 0.0)

(canvas-draw-begin 0)
(canvas-draw-point 0.0  0.0)
(canvas-draw-end)
