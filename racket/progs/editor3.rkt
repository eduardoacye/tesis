;;; -*- mode: racket; coding: utf-8 -*-

#lang racket/gui

(require framework
         pict
         pict/tree-layout
         pict/color
         "../docs/estructuras.rkt"
         "../docs/lector.rkt"
         "../docs/escritor.rkt"
         "../docs/evaluador.rkt")

(define *expresión-actual* (hueco))
(define *cursor* *expresión-actual*)

