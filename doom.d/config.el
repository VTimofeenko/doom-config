;;; $DOOMDIR/config.el --- Doom-emacs configuration file
;;; Commentary:
;;; Code:

(setq user-full-name "Vladimir Timofeenko"
      user-mail-address "id@vtimofeenko.com")
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 12))
(setq doom-theme 'doom-one)
(setq display-line-numbers-type 'relative)
;; Configure the terminal cursor to change
(use-package! evil-terminal-cursor-changer
  :hook (tty-setup . evil-terminal-cursor-changer-activate))
;; Make the line numbers more visible
(custom-set-faces!
  '(line-number-current-line :foreground "#9A70A4")
  '(line-number :foreground "#A1A19A")
  )
;; Make the relative numbers disregard folds
(setq display-line-numbers-type 'visual)
(setq ispell-personal-dictionary "~/.cache/aspell.pws")
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
(when (eq system-type 'darwin)
  (require 'ejc-sql)
  (setq nrepl-sync-request-timeout 60)
  (setq clomacs-httpd-default-port 8090) ; Use a port other than 8080.
  (after! org
    (add-to-list 'org-structure-template-alist
                 '("sql" . "src sql :exports both :eval no-export\n"))))
;; Add lines of context
(setq scroll-margin 5)
;; Automatically format .nix files on save
(add-hook 'nix-mode-hook 'nixpkgs-fmt-on-save-mode)
;; Add custom surround for nix multiline variables
(after! evil-surround
  (let ((pairs '((?m "''\n" . "\n''"))))
    (prependq! evil-surround-pairs-alist pairs)
    (prependq! evil-embrace-evil-surround-keys (mapcar #'car pairs))))
;; override the priorities icons
(after! org-fancy-priorities
  (setq org-fancy-priorities-list '("↑" "←" "↓")))
(setq datetime-timezone #'US/Pacific)
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
(after! org
  (setq calendar-week-start-day 1)
  (setq org-log-done 'time)
  (setq org-log-into-drawer "LOGBOOK")
  ;; More intuitive link opening
  (map! :leader
        (
         :prefix-map ("l" . "link")
         :desc "Open link at cursor" "o" #'org-open-at-point
         )
        )

  (setq org-archive-location ".archive/%s_archive::")
  ;; Jump back-forth between visible headers
  (map! :leader
        (:desc "Next visible heading" "]" #'outline-next-visible-heading)
        )
  (map! :leader
        (:desc "Previous visible heading" "[" #'outline-previous-visible-heading)
        )
  (setq org-download-method 'directory)
  (setq org-download-image-dir "images")
  (setq org-download-heading-lvl nil)
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")
  (setq org-image-actual-width 300)
  (map! :leader
        :prefix-map ("v" . "paste")
        (:desc "Paste image from clipboard" "i" #'zz/org-download-paste-clipboard))
  (add-to-list 'org-modules 'org-habit)
  (set 'org-habit-show-all-today t)
(setq org-capture-templates
      `(("t" "Task" entry (file "inbox.org")
         ,(string-join '("* TODO %?"
                         ":PROPERTIES:"
                         ":CREATED: %U"
                         ":END:")
                       "\n"))
       ("n" "Note" entry (file "inbox.org")
         ,(string-join '("* %?"
                         ":PROPERTIES:"
                         ":CREATED: %U"
                         ":END:")
                       "\n"))
        ("m" "Meeting" entry (file "inbox.org")
         ,(string-join '("* %? :meeting:"
                         "<%<%Y-%m-%d %a %H:00>>"
                         ""
                         "/Met with: /")
                       "\n"))
        ("a" "Appointment" entry (file "inbox.org")
         ,(string-join '("* %? :appointment:"
                         ":PROPERTIES:"
                         ":CREATED: %U"
                         ":END:")
                       "\n"))
        ))
(setq org-todo-keywords
      '((sequence "TODO(t)" "STRT(s)" "HOLD(h)" "|" "DONE(d)" "CNCL(c)")))
(setq org-todo-keyword-faces '(("STRT" . +org-todo-active)
                               ("HOLD" . +org-todo-onhold)
                               ("CNCL" . +org-todo-cancel)))
(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ;; Only show entries with the tag "inbox" -- just in case some entry outside inbox.org still has that file
         ((tags "inbox"
                ((org-agenda-prefix-format "  %?-12t% s")
                 ;; The list of items is already filtered by this tag, no point in showing that it exists
                 (org-agenda-hide-tags-regexp "inbox")
                 ;; The header of this section should be "Inbox: clarify and organize"
                 (org-agenda-overriding-header "\nInbox: clarify and organize\n")))))))
(add-to-list 'org-capture-templates
             `("t" "Task" entry (file "inbox.org")
               ,(string-join '("* TODO %?"
                               ":PROPERTIES:"
                               ":CREATED: %U"
                               ":END:"
                               "/Context:/ %a")
                             "\n"
                             )))
(add-to-list 'org-capture-templates
             `("n" "Note" entry (file "inbox.org")
               ,(string-join '("* %?"
                               ":PROPERTIES:"
                               ":CREATED: %U"
                               ":END:"
                               "/Context:/ %a")
                             "\n")))
(setq org-refile-contexts
      '((((("inbox.org") . (:regexp . "Projects"))) ;; example
         ((lambda () (string= (org-find-top-headline) "Inbox")))
         )
        ;; 6: Notes without a project go to notes.org
        (((("inbox.org") . (:regexp . "Notes")))
         ;;((lambda () (string= (org-element-property :my_type (org-element-at-point)) "NOTE")))
         ((lambda () ('regexp ":my_type:")))
         )
        ))
(setq org-agenda-files (list "inbox.org" "agenda.org"
                             "notes.org" "projects.org"))
(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ;; Only show entries with the tag "inbox" -- just in case some entry outside inbox.org still has that file
         ((tags "inbox"
                ((org-agenda-prefix-format "  %?-12t% s")
                 ;; The header of this section should be "Inbox: clarify and organize"
                 (org-agenda-overriding-header "\nInbox: clarify and organize\n")))
          ;; Show tasks that can be started and their estimates, do not show inbox
          (todo "TODO"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline 'scheduled))
                 (org-agenda-files (list "agenda.org" "notes.org" "projects.org"))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-max-entries 5)
                 (org-agenda-overriding-header "\nTasks: Can be done\n")))
          ;; Show agenda around today
          (agenda nil
                  ((org-scheduled-past-days 0)
                   (org-deadline-warning-days 0)))
          ;; Show tasks on hold
          (todo "HOLD"
                ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks: on hold\n")))
          ;; Show tasks that are in progress
          (todo "STRT"
                ((org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks: in progress\n")))

          ;; Show tasks that I completed today
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n"))))
         (
          ;; The list of items is already filtered by this tag, no point in showing that it exists
          (org-agenda-hide-tags-regexp "inbox")))
        ("G" "All tasks that can be done"
         ((todo "TODO"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline 'scheduled))
                 (org-agenda-files (list "agenda.org" "notes.org" "projects.org")) (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks: Can be done\n")))
          (agenda nil
                  ((org-scheduled-past-days 0)
                   (org-deadline-warning-days 0)))))))
(setq org-agenda-time-grid
  '((daily today require-timed remove-match)
    (800 1000 1200 1400 1600 1800 2000)
    "......"
    "----------------"))
;; taken from stackexchange
;; https://emacs.stackexchange.com/questions/59357/custom-agenda-view-based-on-effort-estimates
(defun fs/org-get-effort-estimate ()
  "Return effort estimate when point is at a given org headline.
If no effort estimate is specified, return nil."
  (let ((limits (org-get-property-block)))
    (save-excursion
      (when (and limits                            ; when non-nil
                 (re-search-forward ":Effort:[ ]*" ; has effort estimate
                                    (cdr limits)
                                    t))
        (buffer-substring-no-properties (point)
                                        (re-search-forward "[0-9:]*"
                                                           (cdr limits)))))))
(defun fs/org-search-for-quickpicks ()
  "Display entries that have effort estimates inferior to 15.
ARG is taken as a number."
  (let ((efforts (mapcar 'org-duration-from-minutes (number-sequence 1 15 1)))
        (next-entry (save-excursion (or (outline-next-heading) (point-max)))))
    (unless (member (fs/org-get-effort-estimate) efforts)
      next-entry)))
(defun vt/org-search-for-long-tasks ()
  "Display entries that have effort estimates longer than 1h "
  (let ((efforts (mapcar 'org-duration-from-minutes (number-sequence 120 600 1)))
        (next-entry (save-excursion (or (outline-next-heading) (point-max)))))
    (unless (member (fs/org-get-effort-estimate) efforts)
      next-entry)))

(add-to-list 'org-agenda-custom-commands
             '("E" "Efforts view"
               ((alltodo ""
                         ((org-agenda-skip-function 'fs/org-search-for-quickpicks)
                          (org-agenda-overriding-header "Quick tasks")))
                (alltodo ""
                         ((org-agenda-skip-function 'vt/org-search-for-long-tasks)
                          ;; For longer tasks - show how long they are
                          (org-agenda-prefix-format "[%e] ")
                          (org-agenda-overriding-header "Long tasks"))))))
(add-to-list 'org-structure-template-alist
             '("elisp" . "src elisp\n"))
(add-to-list 'org-structure-template-alist
             '("lua" . "src lua\n"))
(add-to-list 'org-structure-template-alist
             '("nix" . "src nix\n"))
(defun abs--quick-capture ()
  ;; redefine the function that splits the frame upon org-capture
  (defun abs--org-capture-place-template-dont-delete-windows (oldfun args)
    (cl-letf (((symbol-function 'org-switch-to-buffer-other-window) 'switch-to-buffer))
      (apply oldfun args)))

  ;; run-once hook to close window after capture
  (defun abs--delete-frame-after-capture ()
    (delete-frame)
    (remove-hook 'org-capture-after-finalize-hook 'abs--delete-frame-after-capture)
    )

  ;; set frame title
  (set-frame-name "emacs org capture")
  (add-hook 'org-capture-after-finalize-hook 'abs--delete-frame-after-capture)
  (abs--org-capture-place-template-dont-delete-windows 'org-capture nil))
(defun my-org-show-current-heading-tidily()
  "Show current entry, keep other entries folded"
  (interactive)
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-fold-show-entry) (outline-show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-at-heading-p))
      (org-up-heading-safe)
      (outline-hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-fold-show-entry)
    (outline-show-children)))

(after! org
  (map! :leader
        (:prefix-map ("l" . "link")
         :desc "Show only the current heading, fold all others"
         "c"
         'my-org-show-current-heading-tidily)))
(after! org
  (setq org-export-with-superscripts '{})
  (setq org-use-sub-scripts '{})
  (setq org-export-with-section-numbers 'nil))
;; Changes the TODO state based on statistics cookie
(defun org-todo-if-needed (state)
  "Change header state to STATE unless the current item is in STATE already."
  (unless (or
           (string-equal (org-get-todo-state) state)
           (string-equal (org-get-todo-state) nil)) ;; do not change item if it's not in a state
    (org-todo state)))

(defun ct/org-summary-todo-cookie (n-done n-not-done)
  "Switch header state to DONE when all subentries are DONE, to TODO when none are DONE, and to DOING otherwise"
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo-if-needed (cond ((= n-done 0)
                               "TODO")
                              ((= n-not-done 0)
                               "DONE")
                              (t
                               "STRT")))))
(add-hook 'org-after-todo-statistics-hook #'ct/org-summary-todo-cookie)

(defun ct/org-summary-checkbox-cookie ()
  "Switch header state to DONE when all checkboxes are ticked, to TODO when none are ticked, and to DOING otherwise"
  (let (beg end)
    (unless (not (org-get-todo-state))
      (save-excursion
        (org-back-to-heading t)
        (setq beg (point))
        (end-of-line)
        (setq end (point))
        (goto-char beg)
        ;; Regex group 1: %-based cookie
        ;; Regex group 2 and 3: x/y cookie
        (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                               end t)
            (if (match-end 1)
                ;; [xx%] cookie support
                (cond ((equal (match-string 1) "100%")
                       (org-todo-if-needed "DONE"))
                      ((equal (match-string 1) "0%")
                       (org-todo-if-needed "TODO"))
                      (t
                       (org-todo-if-needed "STRT")))
              ;; [x/y] cookie support
              (if (> (match-end 2) (match-beginning 2)) ; = if not empty
                  (cond ((equal (match-string 2) (match-string 3))
                         (org-todo-if-needed "DONE"))
                        ((or (equal (string-trim (match-string 2)) "")
                             (equal (match-string 2) "0"))
                         (org-todo-if-needed "TODO"))
                        (t
                         (org-todo-if-needed "STRT")))
                (org-todo-if-needed "DOING"))))))))
(add-hook 'org-checkbox-statistics-hook #'ct/org-summary-checkbox-cookie)
;; Reset the child checkboxes when a todo task is repeated
(add-hook 'org-todo-repeat-hook #'org-reset-checkbox-state-subtree)
(add-to-list 'org-agenda-custom-commands
             '("h" "home maintenance"
               ((agenda ""
                        ((org-agenda-span 7)
                         (org-agenda-start-on-weekday 1)
                         (org-agenda-time-grid nil)
                         (org-agenda-start-day "+0d") ;; Without this line the custom view seems to be stuck on the previous week
                         (org-agenda-repeating-timestamp-show-all t)
                         (org-agenda-prefix-format "%-12c:   ")
                         (org-agenda-hide-tags-regexp "home_maintenance") ;; [2]
                         (org-agenda-sorting-strategy '((agenda priority-down category-up time-up)
                                                        (todo priority-down category-keep)
                                                        (tags priority-down category-keep)
                                                        (search category-keep)))

                         (org-agenda-todo-keyword-format "") ;; [3]
                         (org-agenda-tag-filter-preset '("+home_maintenance")) ;; [1]
                         )))
               nil
               ("~/code/infra/services/dashy/home_maint.html")))
)
(map! :leader :desc "Search project" "/" #'consult-ripgrep)
(setq speedbar-use-images nil)
(use-package! lsp-mode
  :ensure t)

(use-package! lsp-nix
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

(use-package! nix-mode
  :hook (nix-mode . lsp-deferred)
  :ensure t)
