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

(test-case
    "Prueba de lectura de términos λ y metainstrucciones"
  (check-equal? (leer (open-input-string "foo"))
                (variable "foo")
                "Variable sencilla")

  (check-equal? (leer (open-input-string "foo bar"))
                (aplicación (variable "foo")
                            (variable "bar"))
                "Aplicación con abuso")

  (check-equal? (leer (open-input-string "(foo bar)"))
                (aplicación (variable "foo")
                            (variable "bar"))
                "Aplicación sin abuso")

  (check-equal? (leer (open-input-string "foo bar baz"))
                (aplicación
                 (aplicación (variable "foo")
                             (variable "bar"))
                 (variable "baz"))
                "Aplicación múltiple con abuso")

  (check-equal? (leer (open-input-string "((foo bar) baz)"))
                (aplicación
                 (aplicación (variable "foo")
                             (variable "bar"))
                 (variable "baz"))
                "Aplicación múltiple sin abuso")

  (check-equal? (leer (open-input-string "foo (bar baz)"))
                (aplicación
                 (variable "foo")
                 (aplicación (variable "bar")
                             (variable "baz")))
                "Aplicación múltiple alternativa con abuso")

  (check-equal? (leer (open-input-string "(foo (bar baz))"))
                (aplicación
                 (variable "foo")
                 (aplicación (variable "bar")
                             (variable "baz")))
                "Aplicación múltiple alternativa sin abuso")

  (check-equal? (leer (open-input-string "λfoo.foo"))
                (abstracción (variable "foo")
                             (variable "foo"))
                "Abstracción sencilla con abuso")

  (check-equal? (leer (open-input-string "(λbar.bar)"))
                (abstracción (variable "bar")
                             (variable "bar"))
                "Abstracción sencilla sin abuso")

  (check-equal? (leer (open-input-string "λfoo bar.foo"))
                (abstracción
                 (variable "foo")
                 (abstracción
                  (variable "bar")
                  (variable "foo")))
                "Abstracción curry con abuso")

  (check-equal? (leer (open-input-string "(λfoo bar.foo)"))
                (abstracción
                 (variable "foo")
                 (abstracción
                  (variable "bar")
                  (variable "foo")))
                "Abstracción curry con abuso menor")

  (check-equal? (leer (open-input-string "(λfoo.λbar.foo)"))
                (abstracción
                 (variable "foo")
                 (abstracción
                  (variable "bar")
                  (variable "foo")))
                "Abstracción curry con abuso menor 2")

  (check-equal? (leer (open-input-string "(λfoo.(λbar.foo))"))
                (abstracción
                 (variable "foo")
                 (abstracción
                  (variable "bar")
                  (variable "foo")))
                "Abstracción curry con abuso menor 3")

  (check-equal? (leer (open-input-string "λfoo.bar baz"))
                (abstracción
                 (variable "foo")
                 (aplicación
                  (variable "bar")
                  (variable "baz")))
                "Abstracción con aplicación")

  (check-equal? (leer (open-input-string "λfoo bar.bar baz"))
                (abstracción
                 (variable "foo")
                 (abstracción
                  (variable "bar")
                  (aplicación
                   (variable "bar")
                   (variable "baz"))))
                "Abstracción con aplicación 2")

  (check-equal? (leer (open-input-string "(λfoo.foo)λbar.bar"))
                (aplicación (abstracción (variable "foo")
                                         (variable "foo"))
                            (abstracción (variable "bar")
                                         (variable "bar")))
                "Aplicación de abstracciones con abuso")

  (check-equal? (leer (open-input-string "foo[bar,baz]"))
                (metainstrucción "foo"
                                 (list (variable "bar")
                                       (variable "baz")))
                "Metainstrucción simple")

  (check-equal? (leer (open-input-string "  foo[λbar.bar bar, λbaz.baz baz] quux "))
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
                 (variable "quux"))
                "Metainstrucción compleja")

  (display "Aparentemente todo funciona bien... ¡Aparentemente!\n"))
