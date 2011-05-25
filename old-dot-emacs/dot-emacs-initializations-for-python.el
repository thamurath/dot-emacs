


;; Para utilizar ropemacs
(add-to-list 'load-path "/home/eojojos/Varios/emacs/python-rope")
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

;; (require 'python)
;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("ipython" . python-mode))


;;; Initialize Pymacs                                                                                           
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)


;; Ipython es un shell de python mas avanzado y con cosas mas chulas.
;; OJO Necesita el python-mode.
;; Ahora mismo tengo cargados tanto python.el como python-mode.el y no se si se daran de leches.
(require 'ipython)


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



(provide 'my-own-python-initialization)
