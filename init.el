(require 'package)
(setq package-archives '(
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")
     ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

; remapeo del Ctrl-X a Ctrl-Q y Alt-X a Alt-Q para el Dvorak
(defun setup-input-decode-map ()
  (define-key input-decode-map (kbd "C-u") (kbd "C-x"))
  (define-key input-decode-map (kbd "M-u") (kbd "M-x"))
)
(setup-input-decode-map)

; desmapeo el Ctrl-C
(global-set-key "\C-c" nil)

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
  :bind (("C-§" . mc/mark-next-like-this)
         ("C-±" . mc/mark-previous-like-this)
         ("C-M-§" . mc/skip-to-next-like-this)
         ("C-M-±" . mc/skip-to-previous-like-this)
         ;; ("C-S-c C-S-c" . mc/edit-lines)
         ;; ("C-M-0" . mc/mark-all-like-this)
         ;; ("C-c C-<" . mc/mark-all-like-this)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)))

(use-package all-the-icons
  ;; Este paquete instala los iconos para el neotree
  :disabled
  :ensure t
  :commands neotree-toggle)

(use-package neotree
  :ensure t
  :bind (("M-\`" . neotree-toggle))
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

; Con Ctrl-Cmd y los cursores muevo los buffers entre ventanas
(use-package buffer-move
  :ensure t
  :bind (("<C-s-up>" . buf-move-up)
	 ("<C-s-down>" . buf-move-down)
	 ("<C-s-left>" . buf-move-left)
	 ("<C-s-right>" . buf-move-right))
  )

; el modo mayor de Python usa Ctrl-C como prefijo, aqui lo libero para poder usarlo como yo quiero
(defun unbinds-after-load-python ()
  (define-key python-mode-map (kbd "C-c") nil))
(eval-after-load "python" '(unbinds-after-load-python))

; el paquete helm-projectile me permite buscar archivos rapidamente en el proyecto
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

; macro breakpoint en python
(fset 'alex_set_python_breakpoint "\C-r\nimport ipdb; ipdb.set_trace(context=21)\n")
(global-set-key (kbd "s-b") 'alex_set_python_breakpoint)
; macro breakpoint y cargar json en variable
(fset 'alex_set_python_mocked_var "\C-r\nimport ipdb; ipdb.set_trace(context=21)\nimport json\nfile = open('/mnt/docker/mocked_content.txt', 'r')\nmocked_response = json.loads(file.read())\n")
;; (global-set-key (kbd "s-v") 'alex_set_python_mocked_var)

; macros para mover lineas de texto
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))
(global-set-key (kbd "<M-up>") 'move-line-up)

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))
(global-set-key (kbd "<M-down>") 'move-line-down)

(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text count))
(global-set-key (kbd "<M-right>") 'shift-right)

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))
(global-set-key (kbd "<M-left>") 'shift-left)

; alias de funciones y atajos globales
(defalias 'keys 'describe-bindings)
(defalias 'keys-description 'describe-key)
(defalias 'truncar 'toggle-truncate-lines)

; atajos de busqueda, se ejecutan con M-x <alias>
(defalias 'se 'rgrep)

; atajo para editar este archivo
(defun alex_edit_initel ()
  (interactive)
  (find-file (expand-file-name "~/personal/configurations/init.el"))
)

; seleccionar palabra actual
(defun alex_select_current_word ()
  (interactive)
  (forward-word)
  (push-mark (point) t t)
  (backward-word)
  )
; atajos de seleccion
(global-set-key (kbd "C-x m") 'alex_select_current_word)
; macro seleccion de linea
(defun alex_select_current_line ()
  (interactive)
  (end-of-line)
  (push-mark (point) t t)
  (beginning-of-line)
  )
; y su atajo de seleccion de linea
(global-set-key (kbd "C-x w") 'alex_select_current_line)

; buscar clase python en el proyecto dbss
(defun alex_search_py_class ()
  (interactive)
  (setq searchterm (buffer-substring (mark) (point)))
  (rgrep (concat "class " searchterm) "*.py" "~/trabajo/dbss")
  )

; atajo global togle truncar lineas
(global-set-key (kbd "s-t") 'truncar)

; atajo aumentar/disminuir tamano letra
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s-z") 'text-scale-decrease)

; atajos de movimientos por el buffer
(global-set-key (kbd "C-n") 'forward-char)
(global-set-key (kbd "C-h") 'backward-char)
(global-set-key (kbd "C-t") 'next-line)
(global-set-key (kbd "C-c") 'previous-line)
(global-set-key (kbd "M-n") 'forward-word)
(global-set-key (kbd "M-h") 'backward-word)
(global-set-key (kbd "M-t") 'forward-sentence)
(global-set-key (kbd "M-c") 'backward-sentence)
(global-set-key (kbd "C-r") 'move-end-of-line)
(global-set-key (kbd "C-g") 'move-beginning-of-line)
(global-set-key (kbd "M-r") 'end-of-buffer)
(global-set-key (kbd "M-g") 'beginning-of-buffer)

; copy&paste por el buffer
; atajo global deshacer; Redo es C-g C-z
(global-set-key (kbd "C-'") 'undo)
(global-set-key (kbd "C-,") 'kill-region)
(global-set-key (kbd "C-.") 'kill-ring-save)
(global-set-key (kbd "C-p") 'yank)

; borrado en el buffer
(global-set-key (kbd "M-d") 'kill-line)

; atajos caracteres especiales
;; (global-set-key (kbd "s-§") "\\")
;; (global-set-key (kbd "s-±") "LIBRE")
(global-set-key (kbd "C-/") 'comment-line)

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

; abro maximizado
(toggle-frame-fullscreen)

; setteo a usar espacios en lugar de tabs
(setq-default indent-tabs-mode nil)

; setteo que recarge los buffers de los archivos que han sido modificados fuera del Emacs
(global-auto-revert-mode t)

; setteo que al guardar limpie los espacios en blanco
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; autoactivo el autocompletar para los lenguajes que me interesan
(global-auto-complete-mode t)
(setq ac-modes '(emacs-lisp-mode sh-mode python-mode groovy-mode))

