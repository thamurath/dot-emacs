  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Fichero con las inicializaciones necesarias para el modo cc
  ;; que solo han de hacerse una vez
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Requerimos el modo C/C++ para poder hacer modificaciones sobre el . TODO: esto puede que tenga que ponerlo en el general
  (require 'cc-mode) ; modo para C/C++

  ;;Flymake
  (require 'flymake)

  ;;  TODO: No tengo nada claro que esto funcione. de hecho creo recordar que 
  ;; para que funcione tendria que modificar los makefiles para incluir un nuevo target que 
  ;; le permite al flymake poder conocer las dependencias entre los modulos y demas cosas.
  
  ;; para configurar mediante make
  (setq flymake-allowed-file-name-masks
        (cons '(".+\\.cc$"
                flymake-simple-make-init
                flymake-simple-cleanup
                flymake-get-real-file-name)
              flymake-allowed-file-name-masks)
  )

  ;;=============================================
  ;;  CEDET
  ;;=============================================
  ;; con esto cargamos el cedet
  ;; (load-file "/home/eojojos/Varios/emacs/cedet/common/cedet.el") 

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
(provide 'my-own-cpp-initialization)
