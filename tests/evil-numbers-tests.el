;;; evil-numbers-tests.el --- Testing -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; See: `evil-numbers-tests.sh' for launching this script.

;; TODO: tests that handle bugs: #20, #26, #27.
;; Bugs fixed in:
;; c37a4cf92a9cf8aa9f8bd752ea856a9d1bc6c84c

(require 'ert)

(setq evil-numbers-tests-basedir
      (concat (file-name-directory load-file-name) ".."))
(add-to-list 'load-path evil-numbers-tests-basedir)
(require 'evil-numbers)

;;; Code:

;; ---------------------------------------------------------------------------
;; Global State (Set Up Key Bindings)

;; VIM-style increment/decrement key bindings.
(global-set-key (kbd "C-a") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-x") 'evil-numbers/dec-at-pt)

;; Incremental variants (not standard VIM).
(global-set-key (kbd "C-M-a") 'evil-numbers/inc-at-pt-incremental)
(global-set-key (kbd "C-M-x") 'evil-numbers/dec-at-pt-incremental)


;; ---------------------------------------------------------------------------
;; Internal Functions/Macros

(defmacro simulate-input (&rest keys)
  "Helper macro to simulate input using KEYS."
  (declare (indent 0))
  `(let ((keys-list (list ,@keys)))
     (dolist (keys keys-list)
       (execute-kbd-macro keys))))

(defun buffer-reset-text (initial-buffer-text)
  "Initialize buffer with INITIAL-BUFFER-TEXT."
  (buffer-disable-undo)
  (simulate-input
    (kbd "i"))
  (erase-buffer)
  ;; Don't move the cursor.
  (save-excursion (insert initial-buffer-text))
  (simulate-input
    (kbd "<escape>"))
  (buffer-enable-undo))

(defmacro with-evil-numbers-test (initial-buffer-text &rest body)
  "Run BODY with messages inhibited, setting buffer text to INITIAL-BUFFER-TEXT."
  (declare (indent 1))
  ;; Messages make test output noisy (mainly evil mode switching messages).
  ;; Set `inhibit-message' to nil to see messages when debugging.
  `(let ((inhibit-message t))
     (evil-mode 1)
     (buffer-reset-text ,initial-buffer-text)
     (prog1 (progn
              ,@body)
       (evil-mode 0))))


;; ---------------------------------------------------------------------------
;; Tests

(ert-deftest simple ()
  "Check a single number increments."
  (let ((text-expected "2|")
        (text-initial "1"))
    (with-evil-numbers-test text-initial
      ;; Increment the number.
      (simulate-input
        (kbd "C-a")
        "a|")
      (should (equal text-expected (buffer-string))))))

(ert-deftest simple-negative ()
  "Check a single number decrements."
  (let ((text-expected "-1|")
        (text-initial "0"))
    (with-evil-numbers-test text-initial
      ;; Decrement the number.
      (simulate-input
        (kbd "C-x")
        "a|")
      (should (equal text-expected (buffer-string))))))

;; See bug #18.
(ert-deftest simple-hex ()
  "Check that hexadecimal is detected at all positions."
  (let ((text-initial " 0xFFF "))
    ;; Test incrementing at different offsets,
    ;; this ensures scanning the hexadecimal is handled properly.
    (dotimes (i 6)
      (with-evil-numbers-test text-initial
        (dotimes (_ i)
          (simulate-input
            "l"))
        (simulate-input
          (kbd "C-a")
          "a|")
        (should (equal " 0x1000| " (buffer-string)))))
    (with-evil-numbers-test text-initial
      (simulate-input
        (kbd "llllll")
        (kbd "C-a")
        "a|"
        (kbd "<escape>"))
      (should (equal " 0xFFF |" (buffer-string))))))

;; See bug #17.
(ert-deftest simple-hex-positive-to-negative ()
  "Change positive hex to negative."
  (let ((text-expected " -0x1| ")
        (text-initial " 0x1 "))
    (dotimes (i 4)
      (with-evil-numbers-test text-initial
        (dotimes (_ i)
          (simulate-input
            "l"))
        (simulate-input
          (kbd "C-x")
          (kbd "C-x"))
        (simulate-input
          "a|"
          (kbd "<escape>"))
        (should (equal text-expected (buffer-string)))))))

(ert-deftest simple-hex-negative-to-positive ()
  "Change negative hex to positive."
  (let ((text-expected " 0x1| ")
        (text-initial " -0x1 "))
    (dotimes (i 5)
      (with-evil-numbers-test text-initial
        (dotimes (_ i)
          (simulate-input
            "l"))
        (simulate-input
          (kbd "C-a")
          (kbd "C-a"))
        (simulate-input
          "a|"
          (kbd "<escape>"))
        (should (equal text-expected (buffer-string)))))))

(ert-deftest simple-nop-non-number ()
  "Do nothing, the value under the cursor is not a number."
  (let ((text-expected "X|")
        (text-initial "X"))
    (with-evil-numbers-test text-initial
      (simulate-input
        (kbd "C-a")
        "a|")
      (should (equal text-expected (buffer-string))))))

(ert-deftest simple-nop-non-number-signed ()
  "Do nothing, the value under the cursor is not a number, but it has a sign."
  (let ((text-expected "-|X")
        (text-initial "-X"))
    (with-evil-numbers-test text-initial
      (simulate-input
        (kbd "C-a")
        "a|")
      (should (equal text-expected (buffer-string))))))

;; See bug #25.
(ert-deftest simple-nop-non-number-with-newline-before ()
  "Do nothing, ensure the newline isn't stepped over."
  (let ((text-expected "|\n0")
        (text-initial "\n0"))
    (with-evil-numbers-test text-initial
      (simulate-input
        (kbd "<end>")
        (kbd "C-a")
        "a|")
      (should (equal text-expected (buffer-string))))))

(ert-deftest simple-nop-non-number-with-newline-after ()
  "Do nothing, ensure the newline isn't stepped over."
  (let ((text-expected "0\n|")
        (text-initial "0\n"))
    (with-evil-numbers-test text-initial
      (simulate-input
        "j"
        (kbd "C-a")
        "a|")
      (should (equal text-expected (buffer-string))))))

(ert-deftest simple-nop-cursor-after-decimal ()
  "Do nothing, the cursor is after the number so it shouldn't be modified."
  (let ((text-expected "1 |\n")
        (text-initial "1 \n"))
    (with-evil-numbers-test text-initial
      (simulate-input
        (kbd "<end>")
        (kbd "C-a")
        "a|")
      (should (equal text-expected (buffer-string))))))

(ert-deftest simple-nop-cursor-after-hex ()
  "Do nothing, the cursor is after the number so it shouldn't be modified."
  (let ((text-expected "0xBEEF |\n")
        (text-initial "0xBEEF \n"))
    (with-evil-numbers-test text-initial
      (simulate-input
        (kbd "<end>")
        (kbd "C-a")
        "a|")
      (should (equal text-expected (buffer-string))))))

(ert-deftest simple-separator-chars ()
  "Check separator characters are handled when incrementing."
  (let ((text-expected "1_11_111|")
        (text-initial "1_11_110"))
    ;; Test at different offsets to ensure
    ;; there are no bugs similar to #18 occurring.
    (dotimes (i 8)
      (with-evil-numbers-test text-initial
        (setq-local evil-numbers-separator-chars "_")
        (dotimes (_ i)
          (simulate-input
            "l"))
        (simulate-input
          (kbd "C-a")
          "a|")
        (should (equal text-expected (buffer-string)))))))

(ert-deftest simple-separator-chars-disabled ()
  "Check separator characters are ignored when disabled."
  (let ((text-expected "2|_11_111")
        (text-initial "1_11_111"))
    (with-evil-numbers-test text-initial
      (setq-local evil-numbers-separator-chars nil)
      (simulate-input
        (kbd "C-a")
        "a|")
      (should (equal text-expected (buffer-string))))))

(ert-deftest selected-block-column-first ()
  "Block selection test."
  (let ((text-expected
         ;; format-next-line: off
         (concat "1| 0 0\n"
                 "1 0 0\n"
                 "1 0 0\n"))
        (text-initial
         ;; format-next-line: off
         (concat "0 0 0\n"
                 "0 0 0\n"
                 "0 0 0\n")))
    (with-evil-numbers-test text-initial
      (simulate-input
        ;; Block select the column.
        (kbd "C-v")
        "jj"
        ;; Increment.
        (kbd "C-a")
        ;; Show cursor location.
        "a|")
      (should (equal text-expected (buffer-string))))))

(ert-deftest selected-block-column-second ()
  "Block selection test."
  (let ((text-expected
         ;; format-next-line: off
         (concat "0 1| 0\n"
                 "0 1 0\n"
                 "0 1 0\n"))
        (text-initial
         ;; format-next-line: off
         (concat "0 0 0\n"
                 "0 0 0\n"
                 "0 0 0\n")))
    (with-evil-numbers-test text-initial
      (simulate-input
        (kbd "w")
        ;; Block select the column.
        (kbd "C-v")
        "jj"
        ;; Increment.
        (kbd "C-a")
        ;; Show cursor location.
        "a|")
      (should (equal text-expected (buffer-string))))))

(ert-deftest selected-block-column-third ()
  "Block selection test."
  (let ((text-expected
         ;; format-next-line: off
         (concat "0 0 1|\n"
                 "0 0 1\n"
                 "0 0 1\n"))
        (text-initial
         ;; format-next-line: off
         (concat "0 0 0\n"
                 "0 0 0\n"
                 "0 0 0\n")))
    (with-evil-numbers-test text-initial
      (simulate-input
        (kbd "ww")
        ;; Block select the column.
        (kbd "C-v")
        "jj"
        ;; Increment.
        (kbd "C-a")
        ;; Show cursor location.
        "a|")
      (should (equal text-expected (buffer-string))))))

(ert-deftest selected-block-column-first-incremental ()
  "Incremental block selection test."
  (let ((text-expected
         ;; format-next-line: off
         (concat "1| 0 0\n"
                 "2 0 0\n"
                 "3 0 0\n"))
        (text-initial
         ;; format-next-line: off
         (concat "0 0 0\n"
                 "0 0 0\n"
                 "0 0 0\n")))
    (with-evil-numbers-test text-initial
      (simulate-input
        ;; Block select the column.
        (kbd "C-v")
        "jj"
        ;; Increment.
        (kbd "C-M-a")
        ;; Show cursor location.
        "a|")
      (should (equal text-expected (buffer-string))))))

;; See bug #24.
(ert-deftest simple-hex-case-preserved ()
  "Check that hexadecimal case is preserved when incrementing/decrementing."
  ;; Lowercase hex should stay lowercase.
  (let ((text-expected " 0xf1| ")
        (text-initial " 0xf0 "))
    (with-evil-numbers-test text-initial
      (simulate-input
        (kbd "C-a")
        "a|")
      (should (equal text-expected (buffer-string)))))
  ;; Uppercase hex should stay uppercase.
  (let ((text-expected " 0xF1| ")
        (text-initial " 0xF0 "))
    (with-evil-numbers-test text-initial
      (simulate-input
        (kbd "C-a")
        "a|")
      (should (equal text-expected (buffer-string)))))
  ;; Numeric-only hex (no alpha) should become lowercase (default).
  (let ((text-expected " 0xa| ")
        (text-initial " 0x9 "))
    (with-evil-numbers-test text-initial
      (simulate-input
        (kbd "C-a")
        "a|")
      (should (equal text-expected (buffer-string)))))
  ;; Test decrement preserves case too.
  (let ((text-expected " 0xef| ")
        (text-initial " 0xf0 "))
    (with-evil-numbers-test text-initial
      (simulate-input
        (kbd "C-x")
        "a|")
      (should (equal text-expected (buffer-string))))))

;; See bug #21.
(ert-deftest selected-line-cursor-at-end ()
  "Line selection increments numbers before the cursor."
  (let ((text-expected "2 2 2|\n")
        (text-initial "1 1 1\n"))
    (with-evil-numbers-test text-initial
      (simulate-input
        ;; Move to end of line (cursor after last number).
        (kbd "<end>")
        ;; Line select.
        (kbd "V")
        ;; Increment.
        (kbd "C-a")
        ;; Show cursor location.
        "a|")
      (should (equal text-expected (buffer-string))))))

(ert-deftest selected-lines-incremental ()
  "Incremental line selection test."
  (let ((text-expected
         ;; format-next-line: off
         (concat "1| 2 3\n"
                 "4 5 6\n"
                 "7 8 9\n"))
        (text-initial
         ;; format-next-line: off
         (concat "0 0 0\n"
                 "0 0 0\n"
                 "0 0 0\n")))
    (with-evil-numbers-test text-initial
      (simulate-input
        ;; Line select the rows.
        (kbd "V")
        "jj"
        ;; Increment.
        (kbd "C-M-a")
        ;; Show cursor location.
        "a|")
      (should (equal text-expected (buffer-string))))))

(provide 'evil-numbers-tests)
;;; evil-numbers-tests.el ends here
