#lang racket/gui

(require "estructuras.rkt"
         "lector.rkt"
         "escritor.rkt"
         "evaluador.rkt"
         framework
         pict
         pict/snip)

(define ventana (new frame%
                     [label "REPL"]
                     [width 800]  [min-width 200]
                     [height 600] [min-height 200]))

(define panel-principal (new panel:vertical-dragable% [parent ventana]))

(define lienzo-historial (new editor-canvas% [parent panel-principal]))

(define editor-historial%
  (class text%
    (define bloqueado? #t)
    (define/augment (can-insert? s l) (not bloqueado?))
    (define/augment (can-delete? s l) #f)
    (define/public (insertar snip-entrada snip-salida)
      (set! bloqueado? #f)
      (send this insert #\newline (send this last-position))
      (send this insert snip-entrada (send this last-position))
      (send this insert (new pict-snip% [pict (arrow 10 0)]) (send this last-position))
      (send this insert snip-salida (send this last-position))
      (send this insert #\newline (send this last-position))
      (set! bloqueado? #t))
    (define/public (mostrar-ayuda)
      (set! bloqueado? #f)
      (send this insert
            "\nINFORMACIÓN DE AYUDA\n\nLa siguiente tabla contiene los atajos del teclado para el cuadro de texto de abajo\n`c:' es la tecla Ctrl y `m:' es la tecla Alt\n\n"
            (send this last-position))
      (send this insert
            (new pict-snip% [pict (table 2 (map (lambda (x) (text (format "~a" x))) lista-de-atajos) cc-superimpose cc-superimpose 10 10)])
            (send this last-position))
      (send this insert
            "\nEl contenido de los cuadros que aparecen después de una evaluación puede ser\ncopiado al cuadro de texto de abajo haciéndole clic\n\nPara cambiar el modo de escritura entre `formal' (F) y `breve' (B) presiona el botón\ncon la letra correspondiente.\n\n"
            (send this last-position))
      (set! bloqueado? #t))
    (super-new)
    (send this hide-caret #t)
    (set! bloqueado? #f)
    (send this insert "Bienvenido al mundo del cálculo λ.\n\nEn el cuadro de texto de abajo puedes escribir expresiones, para\nevaluarlas solo tienes que presionar el botón etiquetado con la λ!!!\n\nPara un mensaje de ayuda presiona el botón con el signo de interrogación.\n\n")
    (set! bloqueado? #t)))

(define editor-historial (new editor-historial%))

(send lienzo-historial set-editor editor-historial)

(define expresion-snip%
  (class editor-snip%
    (init-field [cadena ""])
    (define/override (on-event dc x y editorx editory event)
      (if (eq? 'left-down (send event get-event-type))
          (send campo-entrada set-value cadena)
          (send campo-entrada focus)))
    (super-new)
    (define txt (new text%))
    (send txt change-style (make-object style-delta% 'change-size 10))
    (send txt change-style (make-object style-delta% 'change-family 'modern))
    (send txt insert cadena)
    (send this set-editor txt)))

(define panel-entrada (new horizontal-panel%
                           [parent panel-principal]
                           [alignment '(left top)]))

(define campo-entrada (new text-field%
                           [label " > "]
                           [parent panel-entrada]
                           [style '(multiple)]
                           [font (make-object font% 10 'modern)]))

(define atajos-teclado (new keymap%))

(define lista-de-atajos null)

(define-syntax define-atajo
  (syntax-rules ()
    [(_ (nombre . argumentos) teclas cuerpo ...)
     (let ([nombre* (symbol->string 'nombre)])
       (send atajos-teclado add-function nombre*
             (lambda argumentos
               cuerpo ...))
       (send atajos-teclado map-function teclas nombre*)
       (set! lista-de-atajos (append (list teclas nombre*) lista-de-atajos)))]))

(define-atajo (inserta-lambda x e) "m:l" (send x insert "λ"))
(define-atajo (inserta-alpha x e) "m:a" (send x insert "α"))
(define-atajo (inserta-beta x e) "m:b" (send x insert "β"))
(define-atajo (selecciona-todo x e) "c:a" (send x do-edit-operation 'select-all))
(define-atajo (deshacer x e) "c:z" (send x do-edit-operation 'undo))
(define-atajo (rehacer x e) "c:y" (send x do-edit-operation 'redo))
(define-atajo (copiar x e) "c:c" (send x do-edit-operation 'copy))
(define-atajo (pegar x e) "c:v" (send x do-edit-operation 'paste))
(define-atajo (cortar x e) "c:x" (send x do-edit-operation 'cut))
(define-atajo (enviar-entrada x e) "c:enter" (enviar-texto))
(define-atajo (inserta-tab x e) "tab" (send x insert "  "))

(send (send campo-entrada get-editor) set-keymap atajos-teclado)

(define panel-botones (new vertical-panel%
                           [parent panel-entrada]
                           [stretchable-width #f]))

(define boton-entrada (new button%
                           [label "λ"]
                           [parent panel-botones]
                           [callback
                            (lambda (b e)
                              (when (eq? 'button (send e get-event-type))
                                (enviar-texto)))]))

(define boton-ayuda (new button%
                         [label "?"]
                         [parent panel-botones]
                         [callback
                          (lambda (b e)
                            (when (eq? 'button (send e get-event-type))
                              (send editor-historial mostrar-ayuda)))]))

(define modo-escritura 'formal)

(define boton-escritura (new button%
                             [label "B"]
                             [parent panel-botones]
                             [callback
                              (lambda (b e)
                                (when (eq? 'button (send e get-event-type))
                                  (if (eq? modo-escritura 'formal)
                                      (begin
                                        (send boton-escritura set-label "F")
                                        (set! modo-escritura 'breve))
                                      (begin
                                        (send boton-escritura set-label "B")
                                        (set! modo-escritura 'formal)))))]))

(define (enviar-texto)
  (define entrada (send campo-entrada get-value))
  
  (define parseado (with-handlers ([exn:fail? (lambda (exn)
                                                (list 'error (exn-message exn)))])
                     (leer (open-input-string entrada))))
  (define evaluado (if (and (list? parseado) (eq? 'error (car parseado)))
                       parseado
                       (with-handlers ([exn:fail? (lambda (exn)
                                                    (list 'error (exn-message exn)))])
                         (evaluar-expresión parseado))))
  (define salida (cond [(expresión? evaluado)
                        ((if (eq? modo-escritura 'formal)
                             expresión->texto-plano
                             expresión->abuso-texto-plano)
                         evaluado)]
                       [(and (list? evaluado) (eq? 'error (car evaluado)))
                        (cadr evaluado)]
                       [else
                        (format "~v" evaluado)]))
  (send editor-historial insertar
        (new expresion-snip% [cadena entrada])
        (new expresion-snip% [cadena salida]))
  (send campo-entrada set-value "")
  (send campo-entrada focus))

(send ventana show #t)
