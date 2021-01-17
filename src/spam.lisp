;;;; spam.lisp

(in-package #:carm)

(defvar *url-regex*
  (ppcre:create-scanner "(https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|www\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9]+\.[^\s]{2,}|www\.[a-zA-Z0-9]+\.[^\s]{2,})"))
(defvar *viagra-regex*
  (ppcre:create-scanner "[^a-zA-Z]viagra[^a-zA-Z]"))
(defvar *russian-regex*
  (ppcre:create-scanner "\\p{Cyrillic}"))

(defun spam-filter (s)
  (or (ppcre:scan *url-regex* s)
      (ppcre:scan *viagra-regex* s)
      (ppcre:scan *russian-regex* s)))
