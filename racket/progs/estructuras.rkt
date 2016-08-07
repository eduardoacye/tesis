;; -*- mode: racket; coding: utf-8 -*-

;;; estructuras.rkt --- 

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

(provide λ-variable λ-variable? λ-variable-nombre
         λ-aplicación λ-aplicación? λ-aplicación-operador λ-aplicación-operando
         λ-abstracción λ-abstracción? λ-abstracción-argumento λ-abstracción-cuerpo
         λ-metainstrucción λ-metainstrucción? λ-metainstrucción-nombre λ-metainstrucción-argumentos
         λ-término? λ-expresión?)

(struct λ-variable (nombre)
  #:transparent)

(struct λ-aplicación (operador operando)
  #:transparent)

(struct λ-abstracción (argumento cuerpo)
  #:transparent)

(struct λ-metainstrucción (nombre argumentos)
        #:transparent)

(define (λ-término? x)
  (or (λ-variable? x)
      (λ-abstracción? x)
      (λ-aplicación? x)))

(define (λ-expresión? x)
  (or (λ-término? x)
      (λ-metainstrucción? x)))
