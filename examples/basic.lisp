#!/usr/bin/env -S sbcl --script
(require :uiop)

(format t "~A~%" (uiop:command-line-arguments))
