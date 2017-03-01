#lang sicp

﻿;> (car ''avracedavra)
;'quote

; This returns 'quote because ''avracedavra is really (quote (quote
; avracedavra)). When the interpreter hits the first quote, it return the
; enclosed expression as a list of quoted elements. This means that we are left
; with (cons 'quote (cons (cons 'avracedavra '()) '())). Car returns the first
; element of the list, which is 'quote.
