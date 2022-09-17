;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Vladimir Timofeenko"
      user-mail-address "id@vtimofeenko.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12))
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
                 "* MEET %?\n%T\nMet with%i"
                 :prepend t :kill-buffer t
                 )
               )
  ;; Should shadow the default todo template
  (add-to-list 'org-capture-templates
               '(
                 "t" "Todo" entry
                 (file+headline "~/org/inbox.org" "Tasks")
                 "* TODO %?\n%i\n%a"
                 :prepend t :kill-buffer t
                 )
               )
  (add-to-list 'org-capture-templates
               '(
                 "n" "Note" entry
                 (file+headline "~/org/inbox.org" "Notes")
                 "* %?\n"
                 :prepend t :kill-buffer t))
  (add-to-list 'org-capture-templates
               '(
                 "l" "New Project" entry
                 (file+headline "~/org/inbox.org" "Projects")
                 (function vt/arc42-capture-template)
                 :prepend t :kill-buffer t))
  (setq org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
                            ;; Capture with "MEET" keyword, if I revisited it later and refiled stuff - move to "MEET_P"
                            (sequence "MEET(m)" "|" "MEET_P")
                            (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
                            (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))
  (setq org-todo-keywords-for-agenda '("TODO" "PROJ" "LOOP" "STRT" "WAIT" "HOLD" "IDEA" "DONE" "KILL" "[ ]" "[-]" "[?]" "[X]" "OKAY" "YES" "NO" "MEET" "MEET_P"))
  (setq org-todo-keyword-faces '(("[-]" . +org-todo-active)
                                 ("STRT" . +org-todo-active)
                                 ("[?]" . +org-todo-onhold)
                                 ("WAIT" . +org-todo-onhold)
                                 ("HOLD" . +org-todo-onhold)
                                 ("PROJ" . +org-todo-project)
                                 ("NO" . +org-todo-cancel)
                                 ("MEET" . +org-todo-active)
                                 ("KILL" . +org-todo-cancel)))
  )

;; Diary setup
(setq diary-file "~/org/diary")
(setq org-agenda-include-diary t)
;; Ignore tasks that have SCHEDULED in future
(setq org-agenda-todo-ignore-scheduled 'future)

;; Keep the log within the heading
(setq org-log-into-drawer "LOGBOOK")

;; Add all files under ~/org to agenda
(setq org-agenda-files (directory-files-recursively "~/org" "\\.org$"))

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


;; Add custom surround for nix multiline variables
(after! evil-surround
  (let ((pairs '((?m "''\n" . "\n''"))))
    (prependq! evil-surround-pairs-alist pairs)
    (prependq! evil-embrace-evil-surround-keys (mapcar #'car pairs))))

;; habit
(after! org
  (add-to-list 'org-modules 'org-habit)
  (set 'org-habit-show-all-today t))

(defun vt/insert-timestamp-now()
  "Insert the current time stamp"
  (interactive)
  (org-insert-time-stamp (current-time) t t))

(after! org
  (map! :localleader
        :prefix-map ("d"."datetime")
        (:desc "Insert current time at cursor" "i" #'vt/insert-timestamp-now)))

(setq datetime-timezone #'US/Pacific)

;; Arc42-based project description
;; See: https://www.innoq.com/en/blog/brief-introduction-to-arc42/
;; Config inspired by Academic-doom-emacs-config:
;; https://github.com/sunnyhasija/Academic-Doom-Emacs-Config
(defun mkComment (str)
  "Make an org-mode comment from string `str'"
  (concat "# " str))
(defun vt/arc42-capture-template ()
  "Returns `org-capture' template string for new project.
See `org-capture-templates' for more information."
  (let* ((date (format-time-string (org-time-stamp-format  :inactive) (org-current-time)))
         (title (read-from-minibuffer "Project Title: ")) ;Prompt to enter the post title
         )
    (mapconcat #'identity
               `(
                 ,(concat "* PROJ " title " [%]")
                 ":PROPERTIES:"
                 ,(concat ":CREATED_DATE: " date) ;Enter current date and time
                 ":END:"
                 ""
                 "** Introduction and goals"
                 ,(mkComment "Top three(max five) quality requirements")
                 "*** [#A] Why do anything?"
                 ""
                 "*** [#A] Why now?"
                 ""
                 "*** Stakeholders"
                 "| Who? | Expectation |"
                 "|------+-------------|"
                 "|      |             |"
                 "** Constraints"
                 ,(mkComment "Anything that constrains teams in design and implementation decisions, or decision about related processes. Constraints can sometimes go beyond individual systems and might valid for whole organizations and companies (e.g. company-wide technology choices or government regulations). Time and money are common constraints in many organizations.")
                 "** Context and scope"
                 ,(mkComment "The context delimits your system from its (external) communication partners (neighboring systems and users). It specifies or documents the external interfaces. You should always document the context from a business or domain perspective. If infrastructure or specific hardware plays an important role, you might also show a technical perspective.")
                 "** Solution strategy"
                 ,(mkComment "Summary of the fundamental decisions and solution strategies that shape the architecture: can include technology, top-level decomposition, approaches to achieve top quality goals and relevant organizational decisions.")
                 "** Building block view"
                 ,(mkComment "Usually, boxes and arrows, showing the high-level code structure of the system. To phrase it a little more formal: The building block view explains the static structure of the system and contains, abstractions of source-code. It refines the context view, where the complete system is depicted as a black box.")
                 ,(mkComment "Level 1 contains the major subsystems, components or parts of the system. In some cases, this coarse overview provides enough structural overview.")
                 ,(mkComment "Level 2 refines one or more elements from level 1: Create a separate white box (plus explanations) for every level-1 building block you would like to explain in detail.")
                 "** Runtime view"
                 ,(mkComment "The runtime view explains the behavior or processing of one or several building blocks. It serves as companion to the static building block view from section 5 above. The runtime view might explain important use cases or features, interactions at critical external interfaces, error and exception behavior")
                 "** Deployment view"
                 ,(mkComment "Software needs hardware to execute on, that’s where the deployment view comes into play: It shows the technical infrastructure with environments, computers, processors, networks and network topologies. In addition, it maps the software building blocks to those infrastructure elements.")
                 "** Cross-cutting concepts"
                 ,(mkComment "Overall, principal regulations and solution approaches relevant in multiple parts (→ cross-cutting) of the system. Concepts are often related to multiple building blocks. Include different topics like domain models, architecture patterns and -styles, rules for using specific technology and implementation rules.")
                 "** Architecture decisions"
                 ,(mkComment "Keep a collection of architecturally significant decisions that are important, expensive, critical, large scale or risky including rationales, that are not recorded elsewhere.")
                 ,(mkComment "Please use your judgment to decide whether an architectural decision should be documented here or whether you better document it locally (e.g. within the building block view or any cross-cutting concept). You may have already captured the most important decisions of your architecture in the solution strategy.")
                 ,(mkComment "See https://docs.arc42.org/section-9/ for ideas")
                 "** Quality requirements"
                 ,(mkComment "Quality requirements, often described as scenarios. You may use a quality tree to provide a high-level overview. The most important quality goals should have been already described in section 1.2. (quality).")
                 ,(mkComment "Quality should ideally map to a user-facing scenario. 'User clicks on a widget - gets result in <100ms'")
                 "** Risks and technical debt"
                 ,(mkComment "Known technical risks or technical debt. What potential problems exist within or around the system? What does the development team feel miserable about.")
                 ,(mkComment "In case somebody reviewed or audited your system, their findings might be included here.")
                 "** Implementation plan [%]"
                 ,(mkComment "Implementation tasks go here")
                 "** Glossary"
                 ,(mkComment "Important domain and technical terms that stakeholders use when discussing the system. Please include only specific terms - and avoid explaining REST, HTTPS or other words that have common explanations.")
                 ,(mkComment "In addition, the glossary can serve as the translation reference if you work in a multi-language (international) environment.")
                 "%?\n")          ;Place the cursor here
               "\n")))

;; A very simple semantic commits implementation
;; Queries the user for the issue type and inserts it
(define-derived-mode vt-git-commit-mode text-mode "Git commit"
  (save-match-data
    (when (save-excursion (re-search-forward "\\`[\n[:space:]]*#" nil :noerror))
      (let (
            (committype (completing-read "Choose semantic commit type: "
                                          '("fix" "feat" "chore" "doc") nil t)))
        (save-excursion
            (insert (format "%s: \n" committype)))))))

(setq git-commit-major-mode 'vt-git-commit-mode)
