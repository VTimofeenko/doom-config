;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Vladimmir Timofeenko"
      user-mail-address "id@vtimofeenko.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Week starts on Monday
(setq org-agenda-start-on-weekday 1)
(setq calendar-week-start-day 1)

;; Log the "Done" time in the task
(setq org-log-done 'time)

;; Configure the terminal cursor to change
(use-package! evil-terminal-cursor-changer
              :hook (tty-setup . evil-terminal-cursor-changer-activate))

;; Make the line numbers more visible
(custom-set-faces!
  '(line-number-current-line :foreground "#9A70A4")
  '(line-number :foreground "#A1A19A")
  )


;; More intuitive open link shortcut
(map! :leader
      (
       :prefix-map ("l" . "link")
       :desc "Open link at cursor" "o" #'org-open-at-point
       )
      )

;; Jump back-forth between visible headers
(map! :leader
      (:desc "Next visible heading" "]" #'outline-next-visible-heading)
      )
(map! :leader
      (:desc "Previous visible heading" "[" #'outline-previous-visible-heading)
      )

;; Open png using xdg-open
;; (use-package! openwith
;;               :after-call pre-command-hook
;;               :config
;;               (openwith-mode t)
;;               (add-to-list 'openwith-associations '("\\.png\\'" "xdg-open" (file)))
;;               )


;; Custom template to capture a meeting
(after! org
        (add-to-list 'org-capture-templates
                     '(
                       "m" "Meeting" entry
                       (file+headline "~/org/inbox.org" "Meetings")
                       "* MEET %T %?\n\nMet with%i"
                       :prepend t :kill-buffer t
                       )
                     )
        (setq org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
                                  (sequence "MEET(m)")
                                  (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
                                  (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))
        (setq org-todo-keywords-for-agenda '("TODO" "PROJ" "LOOP" "STRT" "WAIT" "HOLD" "IDEA" "DONE" "KILL" "[ ]" "[-]" "[?]" "[X]" "OKAY" "YES" "NO" "MEET"))
        (set org-todo-keyword-faces '(("[-]" . +org-todo-active)
                                      ("STRT" . +org-todo-active)
                                      ("[?]" . +org-todo-onhold)
                                      ("WAIT" . +org-todo-onhold)
                                      ("HOLD" . +org-todo-onhold)
                                      ("PROJ" . +org-todo-project)
                                      ("NO" . +org-todo-cancel)
                                      ("KILL" . +org-todo-cancel)))
        )

;; Diary setup
(setq diary-file "~/org/diary")
(setq org-agenda-include-diary t)
;; Ignore tasks that have SCHEDULED in future
(setq org-agenda-todo-ignore-scheduled 'future)

;; Keep the log within the heading
(setq org-log-into-drawer "LOGBOOK")

;; Spelling personal file location
(setq ispell-personal-dictionary "~/.cache/aspell.pws")

;; Image capturing
;; Original from zzamboni
(defun zz/org-download-paste-clipboard (&optional use-default-filename)
  (interactive "P")
  (require 'org-download)
  (let ((file
         (if (not use-default-filename)
             (read-string (format "Filename [%s]: "
                                  org-download-screenshot-basename)
                          nil nil org-download-screenshot-basename)
           nil)))
    (org-download-clipboard file)))

(after! org
  (setq org-download-method 'directory)
  (setq org-download-image-dir "images")
  (setq org-download-heading-lvl nil)
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")
  (setq org-image-actual-width 300)
  (map! :leader
        :prefix-map ("v" . "paste")
        (:desc "Paste image from clipboard" "i" #'zz/org-download-paste-clipboard))
  )

;; mac specific settings
(when (eq system-type 'darwin)
  ;; https://github.com/nobiot/org-transclusion/issues/52
  (advice-remove 'org-link-search '+org--recenter-after-follow-link-a)
  (use-package! org-transclusion
                :after org
                :init))

;; Change archive location
(setq org-archive-location ".archive/%s_archive::")

;; Add lines of context
(setq scroll-margin 5)

;; Automatically format .nix files on save
(add-hook 'nix-mode-hook 'nixpkgs-fmt-on-save-mode)
