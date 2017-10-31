(require 'package)
(setq package-archives '(
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")
     ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

; autoinstalacion del paquete use-package si no estuviera ya instalado
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

; autoinstalo numerar las ventanas y crea bindings con M-[1-9]
(use-package window-numbering
  :ensure t
  :config (window-numbering-mode)) ;config en use-package hace que cuando se haya cargado el paquete, ejecutara config.

; autoinstalo el maximizar ventana
(use-package zygospore
  :ensure t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows))) ;bind en use-package, hace que el paquete no se carge en memoria hasta que se llame por primera vez

; autoactivo el resaltar linea
(use-package hl-line
  :init (setq global-hl-line-sticky-flag nil) ;esta variable hace que solo se resalte en el buffer activo
  :config (global-hl-line-mode)
  :bind (("s-l" . resalt_line_and_recenter)) ; atajo para activar/desactivar el resalto
  :demand ;obligo a cargar este paquete directamente, sin esperar a la primera llamada al binding
  :init
  (defun resalt_line_and_recenter()
    "Mark the current line and re-center in the buffer."
    (interactive)
    (if global-hl-line-mode
	(setq global-hl-line-mode nil)
      	(setq global-hl-line-mode t)
      )
    (recenter)
  )
)

;autoinstalo magit
(use-package magit
  :disabled
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (("C-<" . mc/mark-next-like-this)
         ("C->" . mc/mark-previous-like-this)
         ("C-M-<" . mc/skip-to-next-like-this)
         ("C-M->" . mc/skip-to-previous-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-M-0" . mc/mark-all-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)))

(use-package all-the-icons
  ;; Este paquete instala los iconos para el neotree
  :disabled
  :ensure t
  :commands neotree-toggle)

(use-package neotree
  :ensure t
  :bind (("M-º" . neotree-toggle))
  :init
  (setq neo-theme (if (display-graphic-p) 'arrow 'arrow) ;; configuro el theme normal tanto para window mode como terminal, si usas 'icons 'arrow hay que habilitar el paquete all-the-icons
        neo-window-position 'left
        neo-smart-open t
        neo-window-width 30
        neo-window-fixed-size t
        neo-auto-indent-point t)
  (defun neotree-project-toggle ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (if project-dir
          (if (neotree-toggle)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  )

(use-package buffer-move
  :ensure t
  :bind (("<C-s-up>" . buf-move-up)
	 ("<C-s-down>" . buf-move-down)
	 ("<C-s-left>" . buf-move-left)
	 ("<C-s-right>" . buf-move-right))
)

(use-package elpy
  ; necesitamos tener instaladas en el sistema unas dependencias en el entorno virtual que configuro abajo, para que esta configuracion de elpy funcione correctamente
  ; pip install --user elpy jedi pylint importmagic autopep8 yapf epc
  :ensure t
  :demand
  :init (fset 'set_python_breakpoint "\C-p\C-e\C-mimport ipdb; ipdb.set_trace()\C-f")
  (setq elpy-rpc-backend "jedi")
  (pyvenv-activate "~/.virtualenvs/emacs/")
  (add-hook 'python-mode-hook 'company-mode)
  :config (elpy-enable) ; cuando termine de cargar elpy, lo ativo
  :bind (("M-SPC" . company-complete)
	 ("s-b" . set_python_breakpoint)
  	 ("s-d" . elpy-doc)
	 ("s-g" . elpy-goto-definition)
	 ("s-o" . elpy-goto-definition-other-window))
)

(use-package flycheck
  :ensure t
  :init (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  :config (global-flycheck-mode)
  :bind ("s-e" . flycheck-list-errors))

(use-package helm-projectile
  :ensure t
  :bind* (
	  ;; ("C-c p D" . projectile-dired)
          ;; ("C-c p v" . projectile-vc)
          ;; ("C-c p k" . projectile-kill-buffers)

          ;; ("C-c p p" . helm-projectile-switch-project)

          ;; ("C-c p f" . helm-projectile-find-file)
          ("C-x C-p" . helm-projectile-find-file)
	  ("M-b" . helm-buffers-list)
	  ("s-i" . helm-semantic-or-imenu)

          ;; ("C-c p g" . helm-projectile-find-file-dwin)
          ;; ("C-c p d" . helm-projectile-find-dir)
          ;; ("C-c p C-r" . helm-projectile-recentf)
          ;; ("C-c p b" . helm-projectile-switch-to-buffer)
          ;; ("C-c p s s" . helm-projectile-ag)
          ;; ("C-c p s g" . helm-projectile-grep)
	  )
  :diminish projectile-mode
  :init
  (setq-default projectile-enable-caching t
                projectile-indexing-method 'alien
                projectile-completion-system 'helm
                projectile-mode-line '(:eval (format " {%s}" (projectile-project-name))))

  :config
  (projectile-global-mode)
  (helm-projectile-on))

; macro seleccion de palabra
(fset 'select-word
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217835 67108896 134217969] 0 "%d")) arg)))
; macro seleccion de linea
(fset 'select-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([9 67108896 16] 0 "%d")) arg)))

; alias de funciones y atajos globales
(defalias 'keys 'describe-bindings)
(defalias 'keys-description 'describe-key)
(defalias 'truncar 'toggle-truncate-lines)
; Alt-3 y Alt-Shft-3 para comentar/descomentar -> Sobrescrito para tener la almohadilla
;; (global-set-key (kbd "s-·") 'comment-line)
; atajos de busqueda, se ejecutan con M-x <alias>
(defalias 'se 'rgrep)
; atajo global togle truncar lineas
(global-set-key (kbd "M-t") 'truncar)
; atajo aumentar/disminuir tamano letra
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
; atajos de movimientos por el buffer
(global-set-key (kbd "C-ñ") 'forward-char)
(global-set-key (kbd "C-k") 'backward-char)
(global-set-key (kbd "C-l") 'next-line)
(global-set-key (kbd "C-o") 'previous-line)
(global-set-key (kbd "M-ñ") 'forward-word)
(global-set-key (kbd "M-k") 'backward-word)
(global-set-key (kbd "M-l") 'forward-sentence)
(global-set-key (kbd "M-o") 'backward-sentence)
(global-set-key (kbd "C-p") 'move-end-of-line)
(global-set-key (kbd "C-i") 'move-beginning-of-line)
(global-set-key (kbd "M-p") 'end-of-buffer)
(global-set-key (kbd "M-i") 'beginning-of-buffer)
; copy&paste por el buffer
; atajo global deshacer; Redo es C-g C-z
(global-set-key (kbd "C-q") 'undo)
(global-set-key (kbd "C-w") 'kill-region)
(global-set-key (kbd "C-e") 'kill-ring-save)
(global-set-key (kbd "C-r") 'yank)
; borrado en el buffer
(global-set-key (kbd "M-d") 'kill-line)
; atajos de seleccion
(global-set-key (kbd "C-,") 'select-word)
(global-set-key (kbd "C-.") 'select-line)
; atajos caracteres especiales
(global-set-key (kbd "s-º") "\\")
(global-set-key (kbd "s-1") "|")
(global-set-key (kbd "s-2") "@")
(global-set-key (kbd "s-3") "#")
(global-set-key (kbd "C-;") 'comment-line)


; configuraciones personales
; activo el auto-cerrar llaves, parentesis, corchetes, etc...
(electric-pair-mode 1)
; activo el resaltar la llave, parentesis, etc, de apertura
(show-paren-mode 1)
; muestro el numero de columna
(column-number-mode 1)
; cargo el tema
(load-theme 'wombat)
; quito las barras
(scroll-bar-mode -1)
; configuro el scroll con el raton
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2)))
(setq mouse-wheel-progressive-speed nil)
; setteo el size de la fuente por defecto
(set-face-attribute 'default nil :height 165)
;configuro las teclas en el Mac para no perder la # y la @
;(setq mac-command-modifier 'super)
;(setq mac-option-modifier 'meta)
; abro maximizado
(toggle-frame-fullscreen)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (buffer-move multiple-cursors all-the-icons neotree helm-projectile flycheck elpy zygospore window-numbering use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
