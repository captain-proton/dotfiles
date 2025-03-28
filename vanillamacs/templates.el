;;; templates.el --- Provide temp.el templates -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Nils Verheyen
;;
;; Author: Nils Verheyen <nils@ungerichtet.de>
;; Maintainer: Nils Verheyen <nils@ungerichtet.de>
;; Created: Juni 24, 2022
;; Modified: Juni 24, 2022
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/captain-proton/dotfiles
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

fundamental-mode ;; Available everywhere

(today (format-time-string "%d.%m.%Y"))

yaml-ts-mode

(an-file "- name: " p n> "  ansible.builtin.file:" n> "  name: \"" p "\"" n> "mode: \"" p "\"")
(an-lineinfile "- name: " p n> "  ansible.builtin.lineinfile:" n> "  path: \"" p "\"" n> "line: \"" p "\"")
(an-template "- name: " p n> "  ansible.builtin.template:" n> "  src: \"" p "\"" n> "dest: \"" p "\"" n> "mode: \"" p "\"")
;;; templates.el ends here
