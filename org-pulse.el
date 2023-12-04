;;; org-pulse.el --- utilities for advanced org-mode scheduling

;; Author: Stormrider stormrider.io@proton.me
;; Version: 0.1
;; Keywords: org, scheduling

;;; Commentary:

;; org-pulse provides advanced utilities for managing scheduling in
;; org-mode.  functions are all interactive for convenience, but the
;; names are left long and descriptive, to encourage key-bindings.
;; the functions are, in alpha order:
;;
;; org-pulse-create-journal-directory: creates org-pulse-journal-dir
;; if nonexistent & adds it to org-agenda-files, if not already in
;; that list. run by other org-pulse functions, so typically would not
;; be run via M-x.
;;
;; org-pulse-create-or-open-todays-journal-file: runs
;; org-pulse-create-journal-directory to make sure things are set up,
;; then either opens today's journal file (if it already exists) or
;; creates it and populates it with a standard MOTD, and an additional
;; file if org-pulse-additional-file is set and the additional file
;; exists.

;;; Code:

(defun org-pulse-copy-all-todos-and-dones-to-buffer (target-buffer)
  "Copy all TODO and DONE headlines in the current file to the specified TARGET-BUFFER."
  (let ((items-to-copy '()))
    ;; Collect all TODO and DONE items
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (element)
        (when (member (org-element-property :todo-type element) '(todo done))
          (let ((begin (org-element-property :begin element))
                (end (org-element-property :end element)))
            (push (buffer-substring-no-properties begin end) items-to-copy)))))
    ;; Append collected items to target buffer
    (with-current-buffer target-buffer
      (goto-char (point-max))
      (dolist (item (reverse items-to-copy))
        (insert item "\n")))))

(defun org-pulse-create-new-journal-entry ()
  "Jump to today's journal file and insert a new level 1 org heading with current timestamp, leaving the cursor on the next line for quick entry."
  (interactive)
  (let* ((today-file (format-time-string "%Y%m%d.org"))
         (full-path (concat org-pulse-journal-dir today-file))
         (buffer-exists (find-buffer-visiting full-path)))
    (if buffer-exists
        (switch-to-buffer buffer-exists)
      (org-pulse-create-or-open-todays-journal-file))
    (goto-char (point-max))
    (insert "\n* " (format-time-string "%Y-%m-%d %H:%M:%S") "\n")
    (visual-line-mode 1)))

(defun org-pulse-create-new-todo-in-todays-journal ()
  "Insert TODO, SCHEDULED for today, and position point at the task description."
  (interactive)
  (let* ((today-file (format-time-string "%Y%m%d.org"))
         (full-path (concat org-pulse-journal-dir today-file))
         (buffer-exists (get-buffer today-file)))
    (if buffer-exists
        (switch-to-buffer buffer-exists)
      (org-pulse-create-or-open-todays-journal-file))
    (goto-char (point-max)) ; Go to the end of the buffer
    (insert "\n** TODO \nSCHEDULED: <"
          (format-time-string "%Y-%m-%d %a")
	  ">")
    (previous-line)))

(defun org-pulse-create-journal-directory ()
  "create journal dir org-pulse-journal-dir if non-existent & add to org-agenda-files list if not already there"
  (interactive)
  (unless (file-exists-p org-pulse-journal-dir)
    (make-directory org-pulse-journal-dir))
  (unless (member org-pulse-journal-dir org-agenda-files)
    (add-to-list 'org-agenda-files org-pulse-journal-dir)))
    
(defun org-pulse-create-or-open-todays-journal-file ()
  "Create a journal file for today if it doesn't exist; open it if it does."
  (interactive)
  (org-pulse-create-journal-directory)
  (let ((today-file (format-time-string "%Y%m%d.org"))
        (additional-file org-pulse-additional-file))
    (let ((full-path (concat org-pulse-journal-dir today-file)))
      (find-file full-path)
      (when (= (buffer-size) 0)
	(insert "* MOTD\n")
	(insert (format-time-string "%a %b %d %I:%M:%S %p %Z %Y (%s)\n"))
	(insert (shell-command-to-string "ddate"))
	(insert (shell-command-to-string "fortune -s fortunes"))
	(when (and additional-file (file-exists-p additional-file))
          (insert-file-contents additional-file))))))

(defun org-pulse-move-todos-to-todays-journal (wildcard)
  "Move all TODOs with their SCHEDULED lines, PROPERTIES, and LOGBOOK drawers from files matching WILDCARD to today's journal file."
  (interactive "sEnter filename pattern (regexp): ")
  (let* ((today-file (format-time-string "%Y%m%d.org"))
         (today-path (concat org-pulse-journal-dir today-file))
         (regexp (concat "^" (replace-regexp-in-string "\\*" ".*" wildcard) "\\.org$"))
         (files (directory-files org-pulse-journal-dir t regexp)))
    ;; Process each file
    (dolist (file files)
      (unless (string= file today-path) ; Skip today's file
        (with-current-buffer (find-file-noselect file)
          (org-mode)
          (goto-char (point-min))
          (while (re-search-forward org-todo-line-regexp nil t)
            ;; Found a TODO item
            (let ((element (org-element-at-point)))
              (when (and (eq (car element) 'headline) (org-element-property :todo-type element))
                ;; Check for PROPERTIES and LOGBOOK
                (let ((content (org-element-property :contents-begin element))
                      (end (org-element-property :contents-end element)))
                  ;; Extract content including TODO line, PROPERTIES, and LOGBOOK
                  (when content
                    (let ((todo-text (buffer-substring-no-properties content end)))
                      ;; Add to today's file
                      (with-current-buffer (find-file-noselect today-path)
                        (goto-char (point-max))
                        (insert todo-text "\n"))
                      ;; Remove from current file
                      (delete-region content end)))))))
          (save-buffer))))

    ;; Save today's file
    (with-current-buffer (find-file-noselect today-path)
      (save-buffer))))

(defun org-pulse-front-active-todo-items (wildcard)
  "Move all TODOs and their SCHEDULED lines from files matching WILDCARD to today's journal file."
  (interactive "sEnter filename wildcard: ")
  (let* ((today-file (format-time-string "%Y%m%d.org"))
         (today-path (concat org-pulse-journal-dir today-file))
         (files (file-expand-wildcards wildcard))
         (todo-items '()))
    ;; Extract TODO items and their SCHEDULED lines from each file
    (dolist (file files)
      (when (not (string= file today-path)) ; Exclude today's file
        (with-temp-buffer
          (insert-file-contents file)
          (while (re-search-forward "^\*+ TODO .*\n\\(SCHEDULED: .*\n\\)?" nil t)
            (push (match-string 0) todo-items))
          (goto-char (point-min))
          ;; Remove TODO items and their SCHEDULED lines from the file
          (while (re-search-forward "^\*+ TODO .*\n\\(SCHEDULED: .*\n\\)?" nil t)
            (replace-match ""))
          (write-file file)))) ; Save changes to the original file

    ;; Insert collected TODO items into today's file
    (when todo-items
      (with-current-buffer (find-file-noselect today-path)
        (goto-char (point-max))
        (dolist (item (nreverse todo-items))
          (insert item "\n"))
        (save-buffer)))))

(defun org-pulse-jump-to-todays-journal ()
  "Open || create journal file and jump to bottom"
  (interactive)
  (org-pulse-create-or-open-todays-journal-file)
    (goto-char (point-max)))

(defun org-pulse-process-files (wildcard target-file)
  "Process all files matching WILDCARD and copy their TODO and DONE items to TARGET-FILE."
  (interactive "sEnter filename pattern: \nFEnter the target file path: ")
  (let* ((regex (concat "^" (replace-regexp-in-string "\\*" ".*" wildcard) "$"))
         (files (directory-files org-pulse-journal-dir t regex))
         (target-buffer (find-file-noselect target-file)))
    (dolist (file files)
      (when (and (not (file-directory-p file))
                 (string-match regex (file-name-nondirectory file)))
        (with-current-buffer (find-file-noselect file)
          (org-mode) ; Ensure Org mode is activated for parsing
          (org-pulse-copy-all-todos-and-dones-to-buffer target-buffer)
          (kill-buffer))))
    (with-current-buffer target-buffer
      (save-buffer))))

(defun org-pulse-update-all-clock-tables ()
  "Update all clock tables and related summaries in the current buffer."
  (interactive)
  (save-excursion
    ;; Update all clocktables
    (message "Starting to update clock tables...")
    (goto-char (point-min))
    (while (re-search-forward "#\\+BEGIN: clocktable" nil t)
      (message "Updating clock table at position %d" (point))
      (org-clock-report)
      (while (org-at-clocktable-p)  ; Ensure we move past the clock table
        (forward-line)))

    ;; Recalculate all tables
    (message "Recalculating tables...")
    (goto-char (point-min))
    (while (re-search-forward "^\\|" nil t)
      (when (org-at-table-p)
        (message "Recalculating table at position %d" (point))
        (org-table-recalculate 'all)
        (goto-char (org-table-end)))  ; Move to the end of the table
      (forward-line))  ; Ensure we move forward in the buffer

    ;; Save the buffer
    (when (buffer-file-name)
      (save-buffer))
    (message "Clock table update and recalculation complete!")))

(defun org-at-clocktable-p ()
  "Check if the point is at a clock table."
  (save-excursion
    (move-beginning-of-line nil)
    (looking-at-p "#\\+BEGIN: clocktable")))

(provide 'org-pulse)

;;; org-pulse.el ends here
