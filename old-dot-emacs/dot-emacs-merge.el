;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File name: ` ~/.emacs '
;;; ---------------------
;;;
;;; If you need your own personal ~/.emacs
;;; please make a copy of this file
;;; an placein your changes and/or extension.
;;;
;;; Copyright (c) 1997-2002 SuSE Gmbh Nuernberg, Germany.
;;;
;;; Author: Werner Fink, <feedback@suse.de> 1997,98,99,2002
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;================================================
  ;; Modificaciones generales 
  ;;===============================================
  ;; con la siguiente linea a;adimos el directorio y todos los subdirectorios.
  (progn (cd "/home/eojojos/Varios/emacs") (normal-top-level-add-subdirs-to-load-path))

  ;; Ponemos el tema de color que queremos
  (require 'color-theme)
  (eval-after-load "color-theme"
                 '(progn
                    (color-theme-initialize)
                    ;(color-theme-gruber-darker)))
                    (color-theme-zenburn)))

  ;; esto es para que se active el resaltado de codigo para todos los modos en los que este disponible
  (setq global-font-lock-mode t)
  ;; Set maximum level of font decoration
  (setq font-lock-maximum-decoration
        '(
            (c-mode . 3) 
            (c++-mode . 3)
            (python-mode . 3) 
         )
  )
  ;;Ilumina la linea actual
  (global-hl-line-mode 1)
  ;;Para activar el realzado de las coincidencias encontradas hasta el momento en las búsquedas, 
  ;;debemos poner las siguientes sentencias:
  (setq search-highlight t)
  (setq query-replace-highlight t)

  ;; Dont make backup files
  (setq make-backup-files nil) ;disable backup
  (setq backup-inhibited t)
  (setq auto-save-list-file-prefix "~/.emacs-saves/.saves-")
  (setq auto-save-default nil)


  ;; con esto deberia poner los numeros de linea 
  (global-linum-mode 1) 
  (setq linum-format "%d ") ; esto deberia poner un espacio detras del numero

  ;; Esto se supone que es para que resalte los pares de parentesis y esas cosas
  (require 'paren)
  (show-paren-mode 1)

  ;; para que la "campana" sea visual
  (setq visible-bell 1)
  ;; no añade lineas vacías al final
  (setq next-line-add-newlines nil)

  ;; Para que se muestre la hora en la barra de estado en formato 24h
  (setq display-time-day-and-date t)
  (setq display-time-interval 30)
  (setq display-time-24hr-format t)
  ;; Para que se muestre el tiempo en la barra de estado ( o modo)
  ;; OJO -> Primero hay que modificar las variables que afecten al modo y luego activarlo!!!
  (display-time-mode t)

  ;; keybindings para hacer que C-w borre la palabra anterior.
  ;; De esta manera no tenemos que tocar el delete casi nunca.
  ;; C-w esta por defecto puesto para kill-region, por eso remapeamos esta tb.
  ;; Nota: en los shell C-w borra la palabra anterior tb, asi que no esta tan mal
  (global-set-key "\C-w" 'backward-kill-word)
  (global-set-key "\C-x\C-k" 'kill-region)
  (global-set-key "\C-c\C-k" 'kill-region)
  ;;Modificaciones para que C-a te lleve al primer caracter no blanco de la linea
  ;;y que cuando lo presiones dos veces si que te lleve al primer caracter de la linea.

  ;; navigation
  
   (defun heretic/real-line-beginning()
     (let ((c (char-after (line-beginning-position)))
           (n (line-beginning-position)))
       (while (or (eql c ?\ )
                  (eql c ?\t))
              (incf n)
              (setq c (char-after n)))
       n))
  (defun heretic/goto-alternate-line-beginning()
    (interactive)
    (let ((n (heretic/real-line-beginning)))
      (if (eql n (point))
        (goto-char (line-beginning-position))
        (goto-char n))))
  ;; Mapeamos C-a a la nueva funcion
  (global-set-key  "\C-a" 'heretic/goto-alternate-line-beginning)

  ;; Esto es para que te lleve al parentesis o la llave que matche con esta 
  (global-set-key "%" 'match-paren)
  (defun match-paren (arg)
    "Go to the matching paren if on a paren; otherwise insert %."
    (interactive "p")
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
          (t (self-insert-command (or arg 1)))))

;;Autofill mode
;;Cuando escribimos un fichero fuente o cuando escribimos un fichero de texto suele ser interesante que el editor decida 
;;cuando no caben más palabras en en la línea actual y pase a la siguiente. Esta funcionalidad en emacs se conoce como 
;;"auto-fill". Con la siguiente sentencia indicamos que la columna donde deben terminar las líneas es la 80 y que debe 
;;activar el modo "auto-fill" para todos los modos derivados del modo de texto y en los modos para C y C++:
(setq default-fill-column 80)
;; con esto pondriamos el modo activo para todos
;; (setq auto-fill-mode 1)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'c++-mode-hook 'turn-on-auto-fill)
(add-hook 'c-mode-hook 'turn-on-auto-fill)
(add-hook 'python-mode-hook 'turn-on-auto-fill)

;;Por defecto en emacs, si vamos al final del fichero y pulsamos el cursor para ir hacia abajo emacs introduce un 
;;salto de linea, pero a veces este comportamiento no es el deseado. Con la siguiente sentencia podemos evitarlo:
;; no añade lineas vacías al final
(setq next-line-add-newlines nil)

;; para que la indentacion estandar sea de 2 espacios
(setq standard-indent 2)
;; Espacios en vez de tabuladores
(setq-default indent-tabs-mode nil) ;; nota:setq-default solo modifica el valor de la variable en los buffers que no tienen un valor para ella.


;; Autopair configuration
(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers
(setq autopair-autowrap t) ;;Esto es para que ponga el delimitador alrededor de una palabra que tengamos seleccionada.
(setq autopair-blink t) 

;; Para que funcione con el triple quote de python
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))
;; Nuevos caracteres para hacer autopair en c++ ( solo en codigo )
(add-hook 'c-mode-common-hook
          #'(lambda ()
              (push '(?< . ?>)
                    (getf autopair-extra-pairs :code))))

  ;; Autocompletado hippie e ido
  (global-set-key "\M-_ " 'hippie-expand)
  ;; ido makes competing buffers and finding files easier
  ;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
  (require 'ido) 
  (ido-mode 'both) ;; for buffers and files
  (setq 
    ido-save-directory-list-file "~/.emacs.d/ido.last"

    ido-ignore-buffers ;; ignore these guys
    '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"

      "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
    ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
    ido-case-fold  t                 ; be case-insensitive

    ido-enable-last-directory-history t ; remember last used dirs
    ido-max-work-directory-list 30   ; should be enough
    ido-max-work-file-list      50   ; remember many
    ido-use-filename-at-point nil    ; don't use filename at point (annoying)
    ido-use-url-at-point nil         ; don't use url at point (annoying)

    ido-enable-flex-matching nil     ; don't try to be too smart
    ido-max-prospects 8              ; don't spam my minibuffer
    ido-confirm-unique-completion t) ; wait for RET, even with unique completion

  ;; when using ido, the confirmation is rather annoying...
  (setq confirm-nonexistent-file-or-buffer nil)



  ;; Para poder simular las busquedas de palabras bajo el cursor. Resalta la busqueda actual
  (require 'highlight-symbol)
;; TODO: asignarle teclas a las funciones siguientes.

;Use C-f3 to toggle highlighting of the symbol at point throughout the current buffer. 
;Use highlight-symbol-mode to keep the symbol at point always highlighted.
;The functions highlight-symbol-next, highlight-symbol-prev, highlight-symbol-next-in-defun and
; highlight-symbol-prev-in-defun allow for cycling through the locations of any symbol at point. 
;When highlight-symbol-on-navigation-p is set, highlighting is triggered regardless of highlight-symbol-idle-delay.

  ;; Esta es una funcion para poder usar astyle dentro de emacs
(defun astyle-this-buffer (pmin pmax)
    (interactive "r")
    (shell-command-on-region pmin pmax
                             "astyle" ;; add options here...
                             (current-buffer) t 
                             (get-buffer-create "*Astyle Errors*") t)
  )

  ;;;;============================================
  ;;;; Visual bookmarks
  ;;;;============================================
  (add-to-list 'load-path "/home/eojojos/emacs-dir/visualBookmarks")

  ;; Persistence
  ;; make bookmarks persistent as default
  (setq-default bm-buffer-persistence t)

  ;; Make sure the repository is loaded as early as possible
  (setq bm-restore-repository-on-load t)
  (require 'bm)
  ;; Configuration TODO: Asignar teclas a estas funciones
  ;(global-set-key (kbd "<C-f2>") 'bm-toggle)
  ;(global-set-key (kbd "<f2>")   'bm-next)
  ;(global-set-key (kbd "<S-f2>") 'bm-previous)
               
  ;; Loading the repository from file when on start up.
  (add-hook' after-init-hook 'bm-repository-load)

  ;; Restoring bookmarks when on file find.
  (add-hook 'find-file-hooks 'bm-buffer-restore)

  ;; Saving bookmark data on killing a buffer
  (add-hook 'kill-buffer-hook 'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook '(lambda nil
                                (bm-buffer-save-all)
                                (bm-repository-save)))

  ;; Update bookmark repository when saving the file.
  (add-hook 'after-save-hook 'bm-buffer-save)

  ;; Restore bookmarks when buffer is reverted.
  (add-hook 'after-revert-hook 'bm-buffer-restore)

  (require 'cc-mode)
  

  ;; yasnippet configuration
  (add-to-list 'load-path "/home/eojojos/Varios/emacs/yasnippet/yasnippet.el")
  (require 'yasnippet)
  (setq yas/root-directory "/home/eojojos/Varios/emacs/yasnippet/snippets")
  (yas/load-directory yas/root-directory)


;;=============================================
  ;; auto-complete mode
  ;;=============================================
  ;; Configuracion que recomienda el proceso de instalacion de la herramienta.
  (add-to-list 'load-path "/home/eojojos/.emacs.d/auto-complete/")
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
  (ac-config-default) ; la funcion ac-common-setup define las ac-sources, asi que la sobreescrimos.
  ;(add-to-list 'ac-sources 'ac-source-semantic)

  ;; Add ac-source-dictionary to ac-sources of all buffer
;; TODO: estas son las ac-sources que tengo en el fichero de configuracion de c++, 
;; seguramente tenga que cambiarlas en cada uno de los modos. 

  (defun ac-common-setup ()
     (setq ac-sources '(ac-source-filename)) ; Esto es importante para el omni-completado ...
     ; El orden importa
     (setq ac-sources (append ac-sources '(ac-source-semantic
                                           ac-source-yasnippet
                                           ac-source-functions
                                           ac-source-variables
                                           ac-source-symbols
                                           ac-source-features
                                           ac-source-abbrev
                                           ac-source-words-in-same-mode-buffers
                                           ac-source-dictionary))
      )
  
   )

  ;;Esto hace que con el tabulador salte el autocompletado.
  ;(ac-set-trigger-key "TAB")

  ;; configuramos Ctrl-n y Ctrl-p para que se comporten para seleccionar los candidatos pero solo cuando este el menu.
  (setq ac-use-menu-map t)
  ;; Default settings
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)

  ; Se supone que esto es para que muestre la ayuda, pero no se si funciona porque no me muestra nada de nada.
  (setq ac-use-quick-help t)
  ; Delay en segundos para mostrar la ayuda en el autocompletado.
  (setq ac-quick-help-delay 1)

  ;Algunas teclas para mostrar la ayuda.
  ;(define-key ac-mode-map (kbd "C-c h") 'ac-last-quick-help)
  (define-key ac-mode-map (kbd "C-c H") 'ac-last-help)

  ;; A;adimos semantic para las fuentes en el modo c y c++
  ;;(add-hook 'c++-mode (lambda () (add-to-list 'ac-sources 'ac-source-semantic)))
  ;;(add-hook 'c-mode (lambda () (add-to-list 'ac-sources 'ac-source-semantic)))
  ;;(add-hook 'cc-mode (lambda () (add-to-list 'ac-sources 'ac-source-semantic)))


  


  ;; Case configuration
  ;; ;; Just ignore case
  ;; (setq ac-ignore-case t)
  ;; Ignore case if completion target string doesn't include upper characters
  (setq ac-ignore-case 'smart)
  ;; ;; Distinguish case
  ;; (setq ac-ignore-case nil)

  (add-hook 'c-mode-common-hook '(lambda ()
                                   ;; ac-omni-completion-sources is made buffer local so
                                   ;; you need to add it to a mode hook to activate on 
                                   ;; whatever buffer you want to use it with.  This
                                   ;; example uses C mode (as you probably surmised).
                                   ;; auto-complete.el expects ac-omni-completion-sources to be
                                   ;; a list of cons cells where each cell's car is a regex
                                   ;; that describes the syntactical bits you want AutoComplete
                                   ;; to be aware of. The cdr of each cell is the source that will
                                   ;; supply the completion data.  The following tells autocomplete
                                   ;; to begin completion when you type in a . or a ->
                                   (add-to-list 'ac-omni-completion-sources
                                                (cons "\\." '(ac-source-semantic)))
                                   (add-to-list 'ac-omni-completion-sources
                                                (cons "->" '(ac-source-semantic)))
                                   (add-to-list 'ac-omni-completion-sources
                                                (cons "\\:" '(ac-source-semantic)))
                                   ;; ac-sources was also made buffer local in new versions of
                                   ;; autocomplete.  In my case, I want AutoComplete to use 
                                   ;; semantic and yasnippet (order matters, if reversed snippets
                                   ;; will appear before semantic tag completions).
                                   (setq ac-sources '(ac-source-semantic ac-source-yasnippet))
                                   )
  )
                                   ;;
  ;; Esto es porque tendremos configurado el flymake para no liarla.
  (ac-flyspell-workaround)


;; Makes final line always be a return
(setq require-final-newline t)

;; deleting files goes to OS's trash folder
(setq delete-by-moving-to-trash t) ; "t" for true, "nil" for false
;; Make copy and paste to work with other programs
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; uniquify!
;;;(es una herramienta para hacer que los nombres de los buffers cuyos nombers
;;;de fichero son iguales, sean mas descriptivos.
;;;Ej: Tenemos abiertos /NSdiametercom/Makefile y /NSPrelay/Makefile
;;;Normalmente tendriamos los buffers Makefile y Makefile<2>
;;;con esto deberiamos tener los buffers Makefile|NSdiametercom y Makefile|NSPrelay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


;; recentf stuff
;; Esto es una utilidad que compone una lista de los ficheros abiertos recientemente.
;;
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-exclude (append recentf-exclude '("/usr*")))
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(setq bookmark-save-flag 1)



;; Esto nos permite navegar por internet mediante emacs. Util para poder ver la documentacion de python
;; sin necesidad de abrir otro navegador. ( ademas si accedes mediante ssh no se abre ... )
(require 'w3m-load)
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)


;;----------------------------------------------          
;; Funcion para modificaciones tipicas de python
;;----------------------------------------------
(defun my-python-mode-hook ()
  ;;; bind RET to py-newline-and-indent
  (define-key python-mode-map (kbd "C-m") 'newline-and-indent)
  ;; La indentacion es importante en Python
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  ;; Esto en teoria es solo para python-mode.el
  (setq-default py-indent-offset 4)


  ;; Modificamos las fuentes del auto-complete-mode
  (add-to-list 'ac-omni-completion-sources
               (cons "\\." '(ac-source-rope)))
                                        
  (setq ac-sources '(ac-source-rope ac-source-yasnippet))
)
(add-hook 'python-mode-hook 'my-python-mode-hook )


;;----------------------------------------------          
;; Funcion para modificaciones tipicas de C y C++ ( vamos de todos los modos C ) 
;;----------------------------------------------
(defun my-c-mode-common-hook ()
    
 ;;; DE nuevo no consigo que esto funcione correctamente. 
;;;; Definimos el modo de indentacion 
;  (c-add-style "jlom-c-indent-style"
;    '("bsd"                      ;; based on bsd mode
;       (c-hanging-colons-alist     . ((member-init-intro after) ;; que ponga una nueva linea despues 
;                                                                ;;de los : de la lista de inicializacion.
;                                      (member-init-cont after)
;                                      (inher-intro after)
;                                      (case-label after)
;                                      (label after)
;                                      (access-label after)))
;       (c-set-offset 'inher-cont c-lineup-multi-inher)
;       (c-set-offset 'arglist-intro c-lineup-arglist-intro-after-paren)
;       (inher-cont . c-lineup-multi-inher)
;       (setq arglist-close 'c-lineup-close-paren) ;; para que aline el parentesis de cierre con el de apertura en las llamadas a funcion
;       (arglist-intro . c-lineup-arglist-intro-after-paren) ;; para que el primer parametro de la llamada lo aline con la llamada.
;       (arglist-cont . c-lineup-arglist-cont) ;; Para que alinee los argumentos con el que quedo en la primera linea.
;       ;(arglist-cont-nonempty . c-lineup-arglist-operators c-lineup-arglist)
;       )
;    )
;;
;;  ;;Ahora lo a;adimos a la lista y lo seteamos para el buffer
;;  (c-add-style "jlom-style" jlom-c-indent-style t)

  ;; --------
  ;; primero definimos el estilo que usaremos como base.
  ;; --------
  (c-set-style "bsd")
  ;; ------
  ;; Ahora ponemos las modificaciones que creamos oportunas
  ;; ------
  ;; coloca en la misma columna todas las lineas de herencia de una clase
  ;(c-set-offset 'inher-cont 'c-lineup-multi-inher) 
  ;(c-set-offset 'topmost-intro-cont 'c-lineup-multi-inher) ;;este es el simbolo que me aparece en las lineas de herencia.
  ;;; indenta correctamente el primer parametro de la llamada a una funcion si esta en otra linea
  ;(c-set-offset 'arglist-intro 'c-lineup-arglist-intro-after-paren)
  ;;; indenta los siguientes parametros de la llamada a una funcion, poniendolos en la 
  ;;; misma columna que el ultimo que se indento
  ;(c-set-offset 'arglist-cont 'c-lineup-arglist)
  ;;; indenta el parentesis de cierre de la llamada a una funcion poniendolo en la misma columna que el de apertura
  ;(c-set-offset 'arglist-close 'c-lineup-close-paren)
  ;;; Alinea los operadores >> y << de los streams.
  ;(c-set-offset 'stream-op 'c-lineup-streamop)
  ;;; Para indentar las macros de manera que los \ del final queden en la misma columna
  ;(c-set-offset 'cpp-define-intro 'c-lineup-cpp-define)

  ;; Esta variable tiene una lista donde se ponen los distintos ':'  que obligan a insertar un retorno de 
  ;; carro y cuando ( before and/or after - tambien se puede dejar sin nada lo que quiere decir que no pone ninguno 
  ;; ni antes ni despues )
;;  (setq c-hanging-colons-alist     . ((member-init-intro after) ;; que ponga una nueva linea despues 
;;                                                           ;;de los : de la lista de inicializacion.
;;                                 (member-init-cont before after)  ;; idem pero para todos los demas
;;                                 (inher-intro after)
;;                                 (case-label after)
;;                                 (label after)
;;                                 (access-label after)
;;                                )
;;   )

  ;;
  ;;---------- fin de las modificaciones de estilo



  ;; key bindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, idl-mode-map, and pike-mode-map inherit from it.
  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
  ;; Asignamos el retorno de carro al newline-and-indent para que las nuevas lineas las inserte indentadas directamente
  ;; (define-key c-mode-map (kbd "C-m") 'newline-and-indent)
  ;; (define-key c++-mode-map (kbd "C-m") 'newline-and-indent)
  
  ;; Indentacion:
  (setq c-basic-offset 2) ;; numero de columnas que se indenta el codigo
  ;; Definimos el formato estandar del codigo. Para todos los modos relativos de C que no son pocos.
  


  ;; Utilizamos espacios para indentar
  (setq indent-tabs-mode nil)
  ;; No sangra si estamos en literales
  (setq c-tab-always-indent "other")
  
  ;; we like auto-newline and hungry-delete ( borra todos los espacios/tabuladores que esten juntos)
  (c-toggle-auto-hungry-state 1)
)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)



;;========================================================================
;; Autoloads para las distintas inicializaciones de los modos
;;========================================================================

;;Inicializaciones para C y CPP
;; (eval-after-load "cc-mode"
;;   '(progn
;;      ;; Do whatever you need to do here, it will only get executed after python-mode.el has loaded
;;      (require 'my-own-cpp-initialization)
;;    )
;; )
;;(autoload 'c-mode  "dot-emacs-initializations-for-cpp" "" t)

;; ;; Inicializaciones para python
;; (eval-after-load "python-mode"
;;   '(progn
;;      ;; Do whatever you need to do here, it will only get executed after python-mode.el has loaded
;;      (require 'my-own-python-initialization)
;;    )
;; )
;;(autoload 'python-mode  "dot-emacs-initializations-for-python" "" t)

;;=================================================================================================================
;;=================================================================================================================
;; Como aun no he conseguido que esto funcione voy a tirar por la calle de enmedio e inicializo las cosas aqui
;;=================================================================================================================
;;=================================================================================================================

;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; CPP
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "bsd")) ;; para todos los demas elegimos el modo bsd que es el que mas se parece ...
  )
  ;;=============================================
  ;;  CEDET
  ;;=============================================
  ;; con esto cargamos el cedet
  (load-file "/home/eojojos/Varios/emacs/cedet/common/cedet.el") 

  ;;habilitamos el modo semantico
  ;(semantic-mode 1)
  ;; Elegimos el modo con mas informacion para el Semantic
  ;(select only one)
  ;;(semantic-load-enable-minimum-features)
  ;;(semantic-load-enable-code-helpers)
  ;;(semantic-load-enable-gaudy-code-helpers)
  ;;(semantic-load-enable-all-exuberent-ctags-support)
  (semantic-load-enable-excessive-code-helpers)

  ;; Habilitamos el minor-mode para que parsee los includes ( creo )
  (require 'semantic-decorate-include)

  ;; Habilitamos el paquete semantic-ia para poder tener informacion adicional sobre los objetos
  (require 'semantic-ia)

  ;; Habilitamos el paquete semantic-gcc para que cedet pregunte a gcc por los system headers y los parsee.
  (require 'semantic-gcc)

  ;; Esta es otra herramienta que ofrece otras habilidades ...
  (require 'eassist)

  ;; Esto sirve para delimitar el ambito de la busqueda de simbolos
  (setq-mode-local c-mode semanticdb-find-default-throttle
                    '(project unloaded system recursive))
  (setq-mode-local c++-mode semanticdb-find-default-throttle
                    '(project unloaded system recursive))
  (setq-mode-local erlang-mode semanticdb-find-default-throttle
                    '(project unloaded system recursive))         

  ;; Esto no tengo del todo claro para que puede ser .
  ;; (custom-set-variables
  ;;   '(semantic-idle-scheduler-idle-time 3)
  ;;   '(semantic-self-insert-show-completion-function (lambda nil (semantic-ia-complete-symbol-menu (point))))
  ;;   '(global-semantic-tag-folding-mode t nil (semantic-util-modes)))
  ;; (global-semantic-folding-mode 1)
  ;; Si queremos podemos poner directorios de include adicionales de la siguiente manera.
  ;; (semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)

  ;; ----------------------------
  ;; Opciones varias de Semantic
  ;; ----------------------------
  ;; Opciones varias
  ;;(global-semantic-idle-scheduler-mode 1) ;The idle scheduler with automatically reparse buffers in idle time.
  (global-semantic-idle-completions-mode nil) ;Display a tooltip with a list of possible completions near the cursor.
  ;;(global-semantic-idle-summary-mode 1) ;Display a tag summary of the lexical token under the cursor.
  ;;;(semantic-idle-scheduler-idle-time 1)
  ;;(global-semantic-folding-mode 1)  ;; Habilitamos el folding del codigo segun semantic

  (setq semantic-idle-scheduler-idle-time 1) ; que empiece parsear cuando lleve 1 segundo parado

  ; Definicion de las teclas para cedet
  (global-set-key "\C-cg" 'semantic-ia-fast-jump)
  (global-set-key "\C-cd" 'semantic-analyze-proto-impl-toggle)
  (global-set-key "\C-ci" 'semantic-decoration-include-visit)
  (global-set-key "\C-cs" 'semantic-symref)
  (global-set-key "\C-ct" 'semantic-mrub-switch-tags)
  (global-set-key "\C-cl" 'goto-line)
  (global-set-key "\C-ca" 'eassist-switch-h-cpp)
  (global-set-key "\C-cb" 'semantic-mrub-switch-tags)
  (global-set-key "\C-ch" 'highlight-symbol-at-point)


  ;;Habilitar semanticDB
  (require 'semanticdb)
  (global-semanticdb-minor-mode 1)


  ;; Esto se supone que debe darme soporte para cscope
  (require 'xcscope)

  ;; EDE customization
  
  (require 'semantic-lex-spp)
  (global-ede-mode 1)
  ;; cpp-tests project definition
  ;(setq RG-project
        (ede-cpp-root-project "RG"
                              :file "/home/eojojos/Work/RatingGroups/source/Imakefile"
                              :include-path '("/"
                                             )
                              ;
                              ;:system-include-path '("/home/ott/exp/include"
                              ;                       boost-base-directory)
                              ;:local-variables (list
                              ;                   (cons 'compile-command 'alexott/gen-cmake-debug-compile-string)
                              ;                 )
        )
  ;)

  ;;=========================================
  ;;  GDB settings
  ;;=========================================
 
  ;; Si esta variable no es nil deberian mostrarse las ventanas al ejecutar gdb
  (setq gdb-many-windows 1)
  ;; Si esta variable es nil los buffers de comandos no apareceran
  ;; (setq gdb-use-separate-io-buffer nil)

  ;; Es comando gdb-restore-windows restaura las ventanas por defecto
  ;; El comando gdb-many-windows permite cambiar en el el layout normal de gdb y el de ventanas
  ;; Si queremos terminar la sesion de depuracion podemos matar el buffer de comandos de gdb con C-x k
  ;; Esto terminara con todos los demas buffers asociados. No es necesario matar si queremos seguir despues de 
  ;; hacer alguna modificacion. Si no lo matamos mantendra los breakpoints y otros datos.

  ;; si la variable gdb-find-source-frame no es nil y gdb se para en un sitio de donde no tiene codigo
  ;; la ventana de codigo mostrara el frame mas cercano del que si que tenga codigo

  ;; Si la variable gdb-delete-out-of-scope no es nil, gdb mantendra los watch aunque la variable salga de scope.


;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; PYTHON
;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'python)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;; Initialize Pymacs                                                                                           
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)



(require 'ipython)
(add-to-list 'interpreter-mode-alist '("ipython" . python-mode))

;; PAra que era esto exactamente ?
;;(autoload 'python-mode "python-mode" "Python Mode." t)

;; Configuracion para pythonlookup.
;; Es una herramienta que permite ir a la documentacion sobre algo que tengamos en el editor 
;; y mostrarla en el navegador.
(autoload 'pylookup-lookup "pylookup")
(autoload 'pylookup-update "pylookup")
(setq pylookup-program "/home/eojojos/Varios/emacs/pythonlookup/pylookup.py")
(setq pylookup-db-file "/home/eojojos/Varios/emacs/pythonlookup/pylookup.db")
(global-set-key "\C-ch" 'pylookup-lookup)




;; PYdbgr settings 
;;
(setq pydbgr-many-windows t)
