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
  (check-equal? (λ-leer (open-input-string "foo"))
                (λ-variable "foo")
                "Variable sencilla")

  (check-equal? (λ-leer (open-input-string "foo bar"))
                (λ-aplicación (λ-variable "foo")
                              (λ-variable "bar"))
                "Aplicación con abuso")

  (check-equal? (λ-leer (open-input-string "(foo bar)"))
                (λ-aplicación (λ-variable "foo")
                              (λ-variable "bar"))
                "Aplicación sin abuso")

  (check-equal? (λ-leer (open-input-string "foo bar baz"))
                (λ-aplicación
                 (λ-aplicación (λ-variable "foo")
                               (λ-variable "bar"))
                 (λ-variable "baz"))
                "Aplicación múltiple con abuso")

  (check-equal? (λ-leer (open-input-string "((foo bar) baz)"))
                (λ-aplicación
                 (λ-aplicación (λ-variable "foo")
                               (λ-variable "bar"))
                 (λ-variable "baz"))
                "Aplicación múltiple sin abuso")

  (check-equal? (λ-leer (open-input-string "foo (bar baz)"))
                (λ-aplicación
                 (λ-variable "foo")
                 (λ-aplicación (λ-variable "bar")
                               (λ-variable "baz")))
                "Aplicación múltiple alternativa con abuso")

  (check-equal? (λ-leer (open-input-string "(foo (bar baz))"))
                (λ-aplicación
                 (λ-variable "foo")
                 (λ-aplicación (λ-variable "bar")
                               (λ-variable "baz")))
                "Aplicación múltiple alternativa sin abuso")

  (check-equal? (λ-leer (open-input-string "λfoo.foo"))
                (λ-abstracción (λ-variable "foo")
                               (λ-variable "foo"))
                "Abstracción sencilla con abuso")

  (check-equal? (λ-leer (open-input-string "(λbar.bar)"))
                (λ-abstracción (λ-variable "bar")
                               (λ-variable "bar"))
                "Abstracción sencilla sin abuso")

  (check-equal? (λ-leer (open-input-string "λfoo bar.foo"))
                (λ-abstracción
                 (λ-variable "foo")
                 (λ-abstracción
                  (λ-variable "bar")
                  (λ-variable "foo")))
                "Abstracción curry con abuso")

  (check-equal? (λ-leer (open-input-string "(λfoo bar.foo)"))
                (λ-abstracción
                 (λ-variable "foo")
                 (λ-abstracción
                  (λ-variable "bar")
                  (λ-variable "foo")))
                "Abstracción curry con abuso menor")

  (check-equal? (λ-leer (open-input-string "(λfoo.λbar.foo)"))
                (λ-abstracción
                 (λ-variable "foo")
                 (λ-abstracción
                  (λ-variable "bar")
                  (λ-variable "foo")))
                "Abstracción curry con abuso menor 2")

  (check-equal? (λ-leer (open-input-string "(λfoo.(λbar.foo))"))
                (λ-abstracción
                 (λ-variable "foo")
                 (λ-abstracción
                  (λ-variable "bar")
                  (λ-variable "foo")))
                "Abstracción curry con abuso menor 3")

  (check-equal? (λ-leer (open-input-string "λfoo.bar baz"))
                (λ-abstracción
                 (λ-variable "foo")
                 (λ-aplicación
                  (λ-variable "bar")
                  (λ-variable "baz")))
                "Abstracción con aplicación")

  (check-equal? (λ-leer (open-input-string "λfoo bar.bar baz"))
                (λ-abstracción
                 (λ-variable "foo")
                 (λ-abstracción
                  (λ-variable "bar")
                  (λ-aplicación
                   (λ-variable "bar")
                   (λ-variable "baz"))))
                "Abstracción con aplicación 2")

  (check-equal? (λ-leer (open-input-string "(λfoo.foo)λbar.bar"))
                (λ-aplicación (λ-abstracción (λ-variable "foo")
                                             (λ-variable "foo"))
                              (λ-abstracción (λ-variable "bar")
                                             (λ-variable "bar")))
                "Aplicación de abstracciones con abuso")

  (check-equal? (λ-leer (open-input-string "foo[bar,baz]"))
                (λ-metainstrucción "foo"
                                   (list (λ-variable "bar")
                                         (λ-variable "baz")))
                "Metainstrucción simple")

  (check-equal? (λ-leer (open-input-string "  foo[λbar.bar bar, λbaz.baz baz] quux "))
                (λ-aplicación
                 (λ-metainstrucción "foo"
                                    (list (λ-abstracción (λ-variable "bar")
                                                         (λ-aplicación
                                                          (λ-variable "bar")
                                                          (λ-variable "bar")))
                                          (λ-abstracción (λ-variable "baz")
                                                         (λ-aplicación
                                                          (λ-variable "baz")
                                                          (λ-variable "baz")))))
                 (λ-variable "quux"))
                "Metainstrucción compleja")

  (display "Aparentemente todo funciona bien... ¡Aparentemente!\n"))
