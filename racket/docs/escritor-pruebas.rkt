;; -*- mode: racket; coding: utf-8 -*-

;;; escritor-pruebas.rkt --- 

;; Copyright (C) 2016 Eduardo Acuña Yeomans <eduardo.acye@gmail.com>

;; Author: Eduardo Acuña Yeomans <eduardo.acye@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Este código es parte de mi tesis de licenciatura.
;;

;;; Code:

#lang racket/base

(require rackunit
         "estructuras.rkt"
         "lector.rkt"
         "escritor.rkt")

(display "Probando escritura de expresiones a texto plano sin abuso de notación...")

(test-case "Variable sencilla"
  (test-equal? "Variable sencilla -- comparación textual"
               (expresión->texto-plano (leer (open-input-string "foo")))
               "foo"))

(test-case "Abstracción sencilla"
  (test-equal? "Abstracción sencilla -- comparación textual"
               (expresión->texto-plano (leer (open-input-string "λfoo.foo")))
               "(λfoo.foo)"))

(test-case "Aplicación sencilla"
  (test-equal? "Aplicación sencilla -- comparación textual"
               (expresión->texto-plano (leer (open-input-string "foo bar (baz baz)")))
               "((foo bar) (baz baz))"))

(test-case "Hueco"
  (test-equal? "Hueco -- comparación textual"
               (expresión->texto-plano (leer (open-input-string "   [       ]")))
               "[ ]"))

(test-case "Metainstrucción"
  (test-equal? "Metainstrucción -- comparación textual"
               (expresión->texto-plano (leer (open-input-string "foo[λbar.baz,baz bar]")))
               "foo[(λbar.baz), (baz bar)]"))

(display "ok\n")

(display "Probando escritura de expresiones a texto plano con abuso de notación...")

(test-case "Variable sencilla"
  (test-equal? "Variable-sencilla -- comparación textual"
               (expresión->abuso-texto-plano (leer (open-input-string "foo")))
               "foo"))

(test-case "Abstracción sencilla"
  (test-equal? "Abstracción sencilla -- comparación textual"
               (expresión->abuso-texto-plano (leer (open-input-string "λfoo.foo")))
               "λfoo.foo"))

(test-case "Aplicación sencilla"
  (test-equal? "Aplicación sencilla -- comparación textual"
               (expresión->abuso-texto-plano (leer (open-input-string "foo bar (baz baz)")))
               "foo bar (baz baz)"))

(test-case "Aplicación compleja"
  (test-equal? "Aplicación compleja -- comparación textual"
               (expresión->abuso-texto-plano (leer (open-input-string "foo (bar baz) λquux.quux quux")))
               "foo (bar baz) λquux.quux quux"))

(test-case "Hueco"
  (test-equal? "Hueco -- comparación textual"
               (expresión->abuso-texto-plano (leer (open-input-string "   [       ]")))
               "[ ]"))

(test-case "Metainstrucción"
  (test-equal? "Metainstrucción -- comparación textual"
               (expresión->abuso-texto-plano (leer (open-input-string "foo[λbar.baz,baz bar]")))
               "foo[λbar.baz, baz bar]"))

(display "ok\n")
