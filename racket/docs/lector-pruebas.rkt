;; -*- mode: racket; coding: utf-8 -*-

;;; lector-pruebas.rkt --- 

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
         "lector.rkt")

(display "Probando lectura de expresiones...")

(test-case "Variable sencilla"
  (test-equal? "Variable sencilla -- comparación estructural"
               (leer (open-input-string "foo"))
               (variable "foo"))
  (test-true "Variable sencilla -- ¿es término?"
             (término? (leer (open-input-string "foo"))))
  (test-true "Variable sencilla -- ¿es expresión?"
             (expresión? (leer (open-input-string "foo")))))

(test-case "Aplicación con abuso de notación"
  (test-equal? "Aplicación con abuso de notación -- comparación estructural"
               (leer (open-input-string "foo bar"))
               (aplicación (variable "foo")
                           (variable "bar")))
  (test-true "Aplicación con abuso de notación -- ¿es término?"
             (término? (leer (open-input-string "foo bar"))))
  (test-true "Aplicación con abuso de notación -- ¿es expresión?"
             (expresión? (leer (open-input-string "foo bar")))))

(test-case "Aplicación sin abuso de notación"
  (test-equal? "Aplicación sin abuso de notación -- comparación estructural"
               (leer (open-input-string "(foo bar)"))
               (aplicación (variable "foo")
                           (variable "bar")))
  (test-true "Aplicación sin abuso de notación -- ¿es término?"
             (término? (leer (open-input-string "(foo bar)"))))
  (test-true "Aplicación sin abuso de notación -- ¿es expresión?"
             (expresión? (leer (open-input-string "(foo bar)")))))

(test-case "Aplicación múltiple con abuso de notación"
  (test-equal? "Aplicación múltiple con abuso de notación -- comparación estructural"
               (leer (open-input-string "foo bar baz"))
               (aplicación
                (aplicación (variable "foo")
                            (variable "bar"))
                (variable "baz")))
  (test-true "Aplicación múltiple con abuso de notación -- ¿es término?"
             (término? (leer (open-input-string "foo bar baz"))))
  (test-true "Aplicación múltiple con abuso de notación -- ¿es expresión?"
             (expresión? (leer (open-input-string "foo bar baz")))))

(test-case "Aplicación múltiple sin abuso de notación"
  (test-equal? "Aplicación múltiple sin abuso de notación -- comparación estructural"
               (leer (open-input-string "((foo bar) baz)"))
               (aplicación
                (aplicación (variable "foo")
                            (variable "bar"))
                (variable "baz")))
  (test-true "Aplicación múltiple sin abuso de notación -- ¿es término?"
             (término? (leer (open-input-string "((foo bar) baz)"))))
  (test-true "Aplicación múltiple sin abuso de notación -- ¿es expresión?"
             (expresión? (leer (open-input-string "((foo bar) baz)")))))

(test-case "Aplicación múltiple alternativa con abuso de notación"
  (test-equal? "Aplicación múltiple alternativa con abuso de notación -- comparación estructural"
               (leer (open-input-string "foo (bar baz)"))
               (aplicación
                (variable "foo")
                (aplicación (variable "bar")
                            (variable "baz"))))
  (test-true "Aplicación múltiple alternativa con abuso de notación -- ¿es término?"
             (término? (leer (open-input-string "foo (bar baz)"))))
  (test-true "Aplicación múltiple alternativa con abuso de notación -- ¿es expresión?"
             (expresión? (leer (open-input-string "foo (bar baz)")))))

(test-case "Aplicación múltiple alternativa sin abuso de notación"
  (test-equal? "Aplicación múltiple alternativa sin abuso de notación -- comparación estructural"
               (leer (open-input-string "(foo (bar baz))"))
               (aplicación
                (variable "foo")
                (aplicación (variable "bar")
                            (variable "baz"))))
  (test-true "Aplicación múltiple alternativa sin abuso de notación -- ¿es término?"
             (término? (leer (open-input-string "(foo (bar baz))"))))
  (test-true "Aplicación múltiple alternativa sin abuso de notación -- ¿es expresión?"
             (expresión? (leer (open-input-string "(foo (bar baz))")))))

(test-case "Abstracción sencilla con abuso de notación"
  (test-equal? "Abstracción sencilla con abuso de notación -- comparación estructural"
               (leer (open-input-string "λfoo.foo"))
               (abstracción (variable "foo")
                            (variable "foo")))
  (test-true "Abstracción sencilla con abuso de notación -- ¿es término?"
             (término? (leer (open-input-string "λfoo.foo"))))
  (test-true "Abstracción sencilla con abuso de notación -- ¿es expresión?"
             (expresión? (leer (open-input-string "λfoo.foo")))))

(test-case "Abstracción sencilla sin abuso de notación"
  (test-equal? "Abstracción sencilla sin abuso de notación -- comparación estructural"
               (leer (open-input-string "(λbar.bar)"))
               (abstracción (variable "bar")
                            (variable "bar")))
  (test-true "Abstracción sencilla sin abuso de notación -- ¿es término?"
             (término? (leer (open-input-string "(λbar.bar)"))))
  (test-true "Abstracción sencilla sin abuso de notación -- ¿es expresión?"
             (expresión? (leer (open-input-string "(λbar.bar)")))))

(test-case "Abstracción curry con abuso de notación"
  (test-equal? "Abstracción curry con abuso de notación -- comparación estructural"
               (leer (open-input-string "λfoo bar.foo"))
               (abstracción
                (variable "foo")
                (abstracción
                 (variable "bar")
                 (variable "foo"))))
  (test-true "Abstracción curry con abuso de notación -- ¿es término?"
             (término? (leer (open-input-string "λfoo bar.foo"))))
  (test-true "Abstracción curry con abuso de notación -- ¿es expresión?"
             (expresión? (leer (open-input-string "λfoo bar.foo")))))

(test-case "Abstracción curry con abuso de notación leve"
  (test-equal? "Abstracción curry con abuso de notación leve -- comparación estructural"
               (leer (open-input-string "(λfoo bar.foo)"))
               (abstracción
                (variable "foo")
                (abstracción
                 (variable "bar")
                 (variable "foo"))))
  (test-true "Abstracción curry con abuso de notación leve -- ¿es término?"
             (término? (leer (open-input-string "(λfoo bar.foo)"))))
  (test-true "Abstracción curry con abuso de notación leve -- ¿es expresión?"
             (expresión? (leer (open-input-string "(λfoo bar.foo)")))))

(test-case "Abstracción curry con abuso de notación más leve"
  (test-equal? "Abstracción curry con abuso de notación más leve -- comparación estructural"
               (leer (open-input-string "(λfoo.λbar.foo)"))
               (abstracción
                (variable "foo")
                (abstracción
                 (variable "bar")
                 (variable "foo"))))
  (test-true "Abstracción curry con abuso de notación más leve -- ¿es término?"
             (término? (leer (open-input-string "(λfoo.λbar.foo)"))))
  (test-true "Abstracción curry con abuso de notación más leve -- ¿es expresión?"
             (expresión? (leer (open-input-string "(λfoo.λbar.foo)")))))

(test-case "Abstracción curry con abuso de notación super leve"
  (test-equal? "Abstracción curry con abuso de notación super leve -- comparación estructural"
               (leer (open-input-string "(λfoo.(λbar.foo))"))
               (abstracción
                (variable "foo")
                (abstracción
                 (variable "bar")
                 (variable "foo"))))
  (test-true "Abstracción curry con abuso de notación super leve -- ¿es término?"
             (término? (leer (open-input-string "(λfoo.(λbar.foo))"))))
  (test-true "Abstracción curry con abuso de notación super leve -- ¿es expresión?"
             (expresión? (leer (open-input-string "(λfoo.(λbar.foo))")))))

(test-case "Abstracción con aplicación"
  (test-equal? "Abstracción con aplicación -- comparación estructural"
               (leer (open-input-string "λfoo.bar baz"))
               (abstracción
                (variable "foo")
                (aplicación
                 (variable "bar")
                 (variable "baz"))))
  (test-true "Abstracción con aplicación -- ¿es término?"
             (término? (leer (open-input-string "λfoo.bar baz"))))
  (test-true "Abstracción con aplicación -- ¿es expresión?"
             (expresión? (leer (open-input-string "λfoo.bar baz")))))

(test-case "Abstracción curry con aplicación"
  (test-equal? "Abstracción curry con aplicación -- comparación estructural"
               (leer (open-input-string "λfoo bar.bar baz"))
               (abstracción
                (variable "foo")
                (abstracción
                 (variable "bar")
                 (aplicación
                  (variable "bar")
                  (variable "baz")))))
  (test-true "Abstracción curry con aplicación -- ¿es término?"
             (término? (leer (open-input-string "λfoo bar.bar baz"))))
  (test-true "Abstracción curry con aplicación -- ¿es expresión?"
             (expresión? (leer (open-input-string "λfoo bar.bar baz")))))

(test-case "Aplicación de abstracciones con abuso de notación"
  (test-equal? "Aplicación de abstracciones con abuso de notación -- comparación estructural"
               (leer (open-input-string "(λfoo.foo)λbar.bar"))
               (aplicación (abstracción (variable "foo")
                                        (variable "foo"))
                           (abstracción (variable "bar")
                                        (variable "bar"))))
  (test-true "Aplicación de abstracciones con abuso de notación -- ¿es término?"
             (término? (leer (open-input-string "(λfoo.foo)λbar.bar"))))
  (test-true "Aplicación de abstracciones con abuso de notación -- ¿es expresión?"
             (expresión? (leer (open-input-string "(λfoo.foo)λbar.bar")))))

(test-case "Metainstrucción simple"
  (test-equal? "Metainstrucción simple -- comparación estructural"
               (leer (open-input-string "foo[bar,baz]"))
               (metainstrucción "foo"
                                (list (variable "bar")
                                      (variable "baz"))))
  (test-false "Metainstrucción simple -- ¿es término?"
              (término? (leer (open-input-string "foo[bar,baz]"))))
  (test-true "Metainstrucción simple -- ¿es expresión?"
             (expresión? (leer (open-input-string "foo[bar,baz]")))))

(test-case "Metainstrucción compleja"
  (test-equal? "Metainstrucción compleja -- comparación estructural"
               (leer (open-input-string "  foo[λbar.bar bar, λbaz.baz baz] quux "))
               (aplicación
                (metainstrucción "foo"
                                 (list (abstracción (variable "bar")
                                                    (aplicación
                                                     (variable "bar")
                                                     (variable "bar")))
                                       (abstracción (variable "baz")
                                                    (aplicación
                                                     (variable "baz")
                                                     (variable "baz")))))
                (variable "quux")))
  (test-false "Metainstrucción compleja -- ¿es término?"
              (término? (leer (open-input-string "  foo[λbar.bar bar, λbaz.baz baz] quux "))))
  (test-true "Metainstrucción compleja -- ¿es expresión?"
             (expresión? (leer (open-input-string "  foo[λbar.bar bar, λbaz.baz baz] quux ")))))

(test-case "Hueco simple"
  (test-equal? "Hueco simple -- comparación estructural"
               (leer (open-input-string "  []  "))
               (hueco))
  (test-false "Hueco simple -- ¿es término?"
              (término? (leer (open-input-string "  []  "))))
  (test-true "Hueco simple -- ¿es expresión?"
             (expresión? (leer (open-input-string "  []  ")))))

(test-case "Hueco complejo"
  (test-equal? "Hueco complejo -- comparación estructural"
               (leer (open-input-string "λx.x [] x"))
               (abstracción
                (variable "x")
                (aplicación
                 (aplicación (variable "x")
                             (hueco))
                 (variable "x"))))
  (test-false "Hueco complejo -- ¿es término?"
              (término? (leer (open-input-string "λx.x [] x"))))
  (test-true "Hueco complejo -- ¿es expresión?"
             (expresión? (leer (open-input-string "λx.x [] x")))))

(display "ok\n")

