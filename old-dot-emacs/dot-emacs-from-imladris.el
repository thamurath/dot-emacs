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
;;;
;;; Test of Emacs derivates
;;; -----------------------
(if (string-match "XEmacs\\|Lucid" emacs-version)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; XEmacs
  ;;; ------
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (progn
     (if (file-readable-p "~/.xemacs/init.el")
        (load "~/.xemacs/init.el" nil t))
  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; GNU-Emacs
  ;;; ---------
  ;;; load ~/.gnu-emacs or, if not exists /etc/skel/.gnu-emacs
  ;;; For a description and the settings see /etc/skel/.gnu-emacs
  ;;;   ... for your private ~/.gnu-emacs your are on your one.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (if (file-readable-p "~/.gnu-emacs")
      (load "~/.gnu-emacs" nil t)
    (if (file-readable-p "/etc/skel/.gnu-emacs")
	(load "/etc/skel/.gnu-emacs" nil t)))

  ;; Custum Settings
  ;; ===============
  ;; To avoid any trouble with the customization system of GNU emacs
  ;; we set the default file ~/.gnu-emacs-custom
  (setq custom-file "~/.gnu-emacs-custom")
  (load "~/.gnu-emacs-custom" t t)
;;;
  ;;==============================================
  ;; Cargando paquetes adicionales.
  ;;==============================================
  

  ;;================================================
  ;; Modificaciones generales 
  ;;===============================================
    
  ;; Esto se supone que es para que resalte los pares de parentesis y esas cosas
  (require 'paren)
  (show-paren-mode 1)

  ;; Esto es para que te lleve al parentesis o la llave que matche con esta
  (global-set-key "%" 'match-paren)
  (defun match-paren (arg)
    "Go to the matching paren if on a paren; otherwise insert %."
    (interactive "p")
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
          (t (self-insert-command (or arg 1)))))
  
  ;; esto es para que se active el resaltado de codigo
  (setq global-font-lock-mode t)

  ;; con la siguiente linea a;adimos el directorio y todos los subdirectorios.
  ;;(progn (cd "~/emacs-dir") (normal-top-level-add-subdirs-to-load-path))
  (add-to-list 'load-path "~/emacs-dir/color-theme-6.6.0")
  (require 'color-theme)
  (eval-after-load "color-theme"
                 '(progn
                    (color-theme-initialize)
                    (color-theme-gruber-darker)))


  ;; con esto deberia poner los numeros de linea 
  (global-linum-mode 1) 
  (setq linum-format "%d ") ; esto deberia poner un espacio detras del numero
  ;; keybinding para hacer M-x sin necesidad de tocal el alt 
  ;; ( que parece que da problemas en algunos sitios )
  (global-set-key "\C-x\C-m" 'execute-extended-command)
  (global-set-key "\C-c\C-m" 'execute-extended-command)

  ;; keybindings para hacer que C-w borre la palabra anterior.
  ;; De esta manera no tenemos que tocar el delete casi nunca.
  ;; C-w esta por defecto puesto para kill-region, por eso remapeamos esta tb.
  ;; Nota: en los shell C-w borra la palabra anterior tb, asi que no esta tan mal
  (global-set-key "\C-w" 'backward-kill-word)
  (global-set-key "\C-x\C-k" 'kill-region)
  (global-set-key "\C-c\C-k" 'kill-region)

  ;; Eliminacion de la barra de menu, la barra de herramientas y el scroll bar
  ;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  ;(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  ;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

  ;; para que la "campana" sea visual
  (setq visible-bell 1)
  ;; Para que se resalte la ultima busqueda
  (setq search-highlight t)
  (setq query-replace-highlight t)
  ;; Para que cuando sobrepasemos los 'n' caracteres pase a la siguiente linea
  (setq default-fill-column 80)
  ;; Emacs a;ade lineas al final del fichero si le damos para abajo.
  ;; Con esto conseguimos que no añade lineas vacías al final
  (setq next-line-add-newlines nil)
  ;; Esto deberia activar un modo por el cual se resalta text que coincida con una
  ;; expresion regular que le demos. 
  ;; Ver http://www.gnu.org/software/emacs/manual/html_node/emacs/Highlight-Interactively.html#Highlight-Interactively
  (setq global-hi-lock-mode 1)
  ;; Para que se muestre la hora en la barra de estado en formato 24h
  (setq display-time-day-and-date t)
  (setq display-time-interval 30)
  (setq display-time-24hr-format t)
  ;; Para que se muestre el tiempo en la barra de estado ( o modo)
  ;; OJO -> Primero hay que modificar las variables que afecten al modo y luego activarlo!!!
  (display-time-mode t)
  ;; Pretty diff mode
  (autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
  (autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)
  (autoload 'ediff-files-remote "ediff" "Intelligent Emacs interface to diff")
  ;;algunas de mis variables para este modo
  (setq ediff-patch-options "-u")
  

  ;; Autocompletado hippie e ido
  (global-set-key "\M- " 'hippie-expand)
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

  ;; Para poder simular las busquedas de palabras bajo el cursor
  (add-to-list 'load-path "~/emacs-dir/highlight-symbol")

  (require 'highlight-symbol)
  ;; highlight symbol at point and jump to next automatically
  (load-library "highlight-symbol")
  ;;(defun hl-symbol-and-jump ()
  ;;  (interactive)
  ;;  (let ((symbol (highlight-symbol-get-symbol)))
  ;;    (unless symbol (error "No symbol at point"))
  ;;    (unless hi-lock-mode (hi-lock-mode 1))
  ;;    (if (member symbol highlight-symbol-list)
  ;;      (highlight-symbol-next)
  ;;      (highlight-symbol-at-point)
  ;;      (highlight-symbol-next))))
  ;;(defun hl-symbol-cleanup ()
  ;;  (interactive)
  ;;  (mapc 'hi-lock-unface-buffer highlight-symbol-list)
  ;;  (setq highlight-symbol-list ()))
  ;;(global-set-key (kbd "C-x *") 'hl-symbol-and-jump)
  ;;(global-set-key (kbd "C-*") 'hl-symbol-cleanup)



  (defun my-isearch-word-at-point ()
    (interactive)
    (call-interactively 'isearch-forward-regexp))

  (defun my-isearch-yank-word-hook ()
    (when (equal this-command 'my-isearch-word-at-point)
      (let ((string (concat "\\<"
                            (buffer-substring-no-properties
                              (progn (skip-syntax-backward "w_") (point))
                              (progn (skip-syntax-forward "w_") (point)))
                            "\\>")))
        (if (and isearch-case-fold-search
                 (eq 'not-yanks search-upper-case))
          (setq string (downcase string)))
        (setq isearch-string string
              isearch-message
              (concat isearch-message
                      (mapconcat 'isearch-text-char-description
                                 string ""))
              isearch-yank-flag t)
        (isearch-search-and-update))))

  (add-hook 'isearch-mode-hook 'my-isearch-yank-word-hook)

  ;;================================================
  ;; Algunas cosas comunes al modo c y c++
  ;;================================================
  (require 'cc-mode) ; modo para C/C++

  ;; Esta es una funcion para poder usar astyle dentro de emacs

  (defun astyle-this-buffer (pmin pmax)
    (interactive "r")
    (shell-command-on-region pmin pmax
                             "astyle" ;; add options here...
                             (current-buffer) t 
                             (get-buffer-create "*Astyle Errors*") t)
  )
  ;;=================================================
  ;; Algunas modificaciones relativas al modo C
  ;;=================================================

  (add-hook 'c-mode-hook 'turn-on-auto-fill) ;; activamos el auto-fill para modo c
  ;; Hace que cuando damos a retorno la nueva linea este indentada
  (add-hook 'c-mode-hook
            '(lambda ()
               (define-key c-mode-map "\C-m" 'newline-and-indent)
               )
            )
  ;;=================================================
  ;; Algunas modificaciones relativas al modo C++
  ;;=================================================

  (add-hook 'c++-mode-hook 'turn-on-auto-fill) ;; activamos el auto-fill para modo c++
  ;; Hace que cuando damos a retorno la nueva linea este indentada
  (add-hook 'c++-mode-hook
            '(lambda ()
               (define-key c++-mode-map "\C-m" 'newline-and-indent)
               )
            )
  ;; Esto se supone que debe darme soporte para cscope
  (add-to-list 'load-path "~/emacs-dir/xcscope")

  (require 'xcscope)
  ;;=================================================
  ;;  CC-MODE
  ;;=================================================
  (setq c-basic-offset 2) ;; numero de columnas que se indenta el codigo
  ;; Definimos el formato estandar del codigo.
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "bsd"))
  )
  ;; Utilizamos espacios para indentar
  (setq indent-tabs-mode nil)

    
  ;;--------------------------------
  ;; Font lock
  ;; ------------------------------
  
  ;; Set maximum level of font decoration
  (setq font-lock-maximum-decoration
        '(
            (c-mode . 3) 
            (c++-mode . 3)
         )
  )
  ;; Add some expresions to font lock 
  (add-hook 'c-mode-hook
            ' (lambda ()
              (font-lock-add-keywords nil
                                      '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)))))
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
  
  ;;;;=============================================
  ;;;;  CEDET
  ;;;;=============================================

  ;;;; con esto cargamos el cedet
  (add-to-list 'load-path "~/emacs-dir/cedet")

  (load-file "~/emacs-dir/cedet/common/cedet.el")

  ;; con esto activariamos el modo para usar proyectos.
  ;; (global-ede-mode t)

  ;; Semantica
  ;; Semantic's customization
  ;; Depending on your requirements, you can use one of the commands, described below, to load corresponding set of features (these commands are listed in increasing order, and each command include features of previous commands):
  ;;
  ;; semantic-load-enable-minimum-features enables only minimum of necessary features keep syntactic information for current buffer up-to date, storing of syntactic information for later use (Semanticdb), and loading of corresponding information with Semanticdb and Ebrowse;
  ;; semantic-load-enable-code-helpers enables senator-minor-mode for navigation in buffer, semantic-mru-bookmark-mode for storing positions of visited tags, and semantic-idle-summary-mode, that shows information about tag under point;
  ;; semantic-load-enable-gaudy-code-helpers enables semantic-stickyfunc-name that displays name of current function in topmost line of buffer, semantic-decoration-mode to decorate tags, using different faces, and semantic-idle-completion-mode for automatic generation of possible names completions, if user stops his work for some time;
  ;; semantic-load-enable-excessive-code-helpers enables which-func-mode, that shows name of current function in status line;
  ;; semantic-load-enable-semantic-debugging-helpers enables several modes, that are useful when you debugging Semantic displaying of parsing errors, its state, etc.
  ;; So, you need to add call to one of these commands right after command, that performs loading of Cedet. For example:
  ;;
  (semantic-load-enable-excessive-code-helpers)

  ;; Este paquete es necesario para que el completado de nombres tenga en cuenta la semantica
  ;;To use additional features for names completion, and displaying of information for tags &
  ;;classes, you also need to load the semantic-ia package. This could be performed with 
  ;;following command:
  (require 'semantic-ia)

  ;; Con el siguiente comando habilitamos el completado semantico para los system header files
  (require 'semantic-gcc)  

  ;; Con el siguiente comando podemos a;adir nuestros propios ficheros header al completado
  ;; semantico
  ;(semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)

  ;; Estos son algunos keybindings para el completado semantico
  (defun my-cedet-hook ()
    ;(local-set-key [(control return)] 'semantic-ia-complete-symbol)
    (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
    ;(local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
    (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
    ;(local-set-key "\C-c?" 'semantic-ia-complete-tip )
    (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
    (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
  (add-hook 'c-mode-common-hook 'my-cedet-hook)

  ; Para que intente autocompletar cuando pongamos . o >
  (defun my-c-mode-cedet-hook ()
    (local-set-key "." 'semantic-complete-self-insert)
    (local-set-key ":" 'semantic-complete-self-insert)
    (local-set-key ">" 'semantic-complete-self-insert)

    (define-key c++-mode-map "\"" 'electric-pair)
    (define-key c++-mode-map "\'" 'electric-pair)
    (define-key c++-mode-map "(" 'electric-pair)
    (define-key c++-mode-map "[" 'electric-pair)
    (define-key c++-mode-map "{" 'electric-pair)
    )
  (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

  ;;=========================================================
  ;; ECB (Emacs Code Browser
  ;;=========================================================

  ;; primero cargamos los ficheros necesarios
  ;; En teoria esto deberia estar hecho porque tengo a;adido todos los subdirectorios de Varios/emacs

  ;; CArgamos
  (add-to-list 'load-path "~/emacs-dir/ecb-2.40")
  (require 'ecb)
  (require 'ecb-autoloads)


  ;;; Electric Pairs

(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

;;configuracion de autocomplete
(add-to-list 'load-path "~/.emacs.d/autocomplete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/autocomplete//ac-dict")
(ac-config-default)


;; Configuracion de rope-ropemode-ropemacs and yanispet para python
(add-to-list 'load-path "~/.emacs.d/vendor")
(progn (cd "~/.emacs.d/vendor")
       (normal-top-level-add-subdirs-to-load-path))



;;Esto es directamente copiado de la web enigmacurry.com
(require 'python)
(require 'auto-complete)
(require 'yasnippet)

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Initialize Pymacs                                                                                           
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;; Initialize Rope                                                                                             
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

;; Initialize Yasnippet                                                                                        
;Don't map TAB to yasnippet                                                                                    
;In fact, set it to something we'll never use because                                                          
;we'll only ever trigger it indirectly.                                                                        
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                         
;;; Auto-completion                                                                                            
;;;  Integrates:                                                                                               
;;;   1) Rope                                                                                                  
;;;   2) Yasnippet                                                                                             
;;;   all with AutoComplete.el                                                                                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                         
(defun prefix-list-elements (list prefix)
  (let (value)
    (nreverse
     (dolist (element list value)
      (setq value (cons (format "%s%s" prefix element) value))))))
(defvar ac-source-rope
  '((candidates
     . (lambda ()
         (prefix-list-elements (rope-completions) ac-target))))
  "Source for Rope")
(defun ac-python-find ()
  "Python `ac-find-function'."
  (require 'thingatpt)
  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
    (if (null symbol)
        (if (string= "." (buffer-substring (- (point) 1) (point)))
            (point)
          nil)
      symbol)))
(defun ac-python-candidate ()
  "Python `ac-candidates-function'"
  (let (candidates)
    (dolist (source ac-sources)
      (if (symbolp source)
          (setq source (symbol-value source)))
      (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
             (requires (cdr-safe (assq 'requires source)))
             cand)
        (if (or (null requires)
                (>= (length ac-target) requires))
            (setq cand
                  (delq nil
                        (mapcar (lambda (candidate)
                                  (propertize candidate 'source source))
                                (funcall (cdr (assq 'candidates source)))))))
        (if (and (> ac-limit 1)
                 (> (length cand) ac-limit))
            (setcdr (nthcdr (1- ac-limit) cand) nil))
        (setq candidates (append candidates cand))))
    (delete-dups candidates)))
(add-hook 'python-mode-hook
          (lambda ()
                 (auto-complete-mode 1)
                 (set (make-local-variable 'ac-sources)
                      (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
                 (set (make-local-variable 'ac-find-function) 'ac-python-find)
                 (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
                 (set (make-local-variable 'ac-auto-start) nil)))

;;Ryan's python specific tab completion                                                                        
(defun ryan-python-tab ()
  ; Try the following: 
  ; 1) Do a yasnippet expansion                                               ; 2) Do a Rope code completion                                              ; 3) Do an indent                                                                                            
  (interactive)
  (if (eql (ac-start) 0)
      (indent-for-tab-command)))

(defadvice ac-start (before advice-turn-on-auto-start activate)
  (set (make-local-variable 'ac-auto-start) t))
(defadvice ac-cleanup (after advice-turn-off-auto-start activate)
  (set (make-local-variable 'ac-auto-start) nil))

(define-key python-mode-map "\t" 'ryan-python-tab)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End Auto Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
)

;;;
