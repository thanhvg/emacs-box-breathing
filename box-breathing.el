;;; box-breathing.el --- A simple box breathing exercise in Emacs

;; Author: Thanh Vuong
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: breathing, relaxation, mindfulness

;;; Commentary:

;; This package provides a simple way to practice box breathing
;; in Emacs. It visually represents the four phases of box
;; breathing: inhale, hold, exhale, and hold.

;;; Code:

(require 'cl-lib)

(defvar box-breathing-steps '(("Inhale" . 4)
                              ("Hold" . 4)
                              ("Exhale" . 4)
                              ("Hold" . 4))
  "Steps for the box breathing exercise.")

(defvar box-breathing-overlay nil
  "Overlay used to draw the breathing square.")

(defconst box-breathing-buffer "*box-breathing*"
  "TODO"
  )

(defvar box-breathing-grid-size 5
  "TODO")

(defvar box-breathing--inhale '((0 . 1)
                                (0 . 2)
                                (0 . 3)
                                (0 . 4)))


(defvar box-breathing--hold-inhale '((1 . 4)
                                     (2 . 4)
                                     (3 . 4)
                                     (4 . 4)))

(defvar box-breathing--exhale '((4 . 3)
                                (4 . 2)
                                (4 . 1)
                                (4 . 0)))

(defvar box-breathing--hold-exhale '((3 . 0)
                                     (2 . 0)
                                     (1 . 0)
                                     (0 . 0)))

(defvar box-breathing--loop-0
  '((box-breathing--inhale . "inhale")
    (box-breathing--hold-inhale . "hold")
    (box-breathing--exhale . "exhale")
    (box-breathing--hold-exhale . "hold")))


(defvar box-breathing--loop
  (append box-breathing--inhale
          box-breathing--hold-inhale 
    box-breathing--exhale 
    box-breathing--hold-exhale))


(defun box-breathing--draw-grid ()
  (erase-buffer)
  (dotimes (_ box-breathing-grid-size)
    (dotimes (_ box-breathing-grid-size)
      (insert ". "))
    (insert "\n")))

(defun box-breathing--draw-pointer (i j)
  (goto-char (point-min))
  (forward-line i)
  (forward-char (* 2 j))
  (delete-region (point) (1+ (point)))
  (insert "o")
  (backward-char 1))


(defun box-breathing-exercise ()
  "Start the box breathing exercise."
  (interactive)
  (switch-to-buffer box-breathing-buffer)
  (buffer-disable-undo (current-buffer))
  (let* ((i 0)
        (j 0)
        (current-pointer (car box-breathing--loop))
        (next-phase-loop (cdr box-breathing--loop))
        )

    (box-breathing--draw-grid)
    (box-breathing--draw-pointer (car current-pointer) (cdr current-pointer))
    (while (sit-for 1)
      (if next-phase-loop
          (progn
            (setq current-pointer (car next-phase-loop))
            (setq next-phase-loop (cdr next-phase-loop)))
        (setq current-pointer (car box-breathing--loop))
        (setq next-phase-loop (cdr box-breathing--loop)))

      (box-breathing--draw-grid)
      (box-breathing--draw-pointer (car current-pointer) (cdr current-pointer)))))


(provide 'box-breathing)
