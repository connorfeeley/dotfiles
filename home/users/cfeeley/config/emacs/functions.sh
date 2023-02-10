#!/usr/bin/env bash

ediff() {
  emacs -nw --eval "(ediff-files \"$1\" \"$2\")"
}

eman() {
  emacs -nw --eval "(switch-to-buffer (man \"$1\"))"
}

ekill() {
  emacsclient --eval '(kill-emacs)'
}
