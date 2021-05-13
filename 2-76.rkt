#lang sicp

; Changes needed to add new types and operations for:

; Generic operations with explicit dispatch:
; This looks very close to how typeclasses work in haskell. We have a dictionary of
; methods that we dispatch on based on the type. When adding a new data object we need
; to add a new install-package/instance where we gather up all the methods associated
; with that type and use 'put to add them to the central dispatch. When adding a new
; method, we need to add it to all the install-packages. In this way, it is pretty
; similar to the message-passing style.

; Data-directed style:
; For every new data object, we need to change all the methods to be able to dispatch
; on it. If we are just adding a method, we only need to implement the method and make
; sure we cover all the data objects in the dispatch. On the other hand, when adding a
; new data object, we need to change all the methods to dispatch on the new object.
; This is probably best for when we have to often add new methods, and not data objects.

; Message-passing style:
; This is similar to how java works with objects that have methods you send "messages"
; to. To add a new method here, we have to add the method to all the data objects. When
; adding a new data object, we only have to create the data object and make sure to
; implement all the methods required. This is most useful for when you often add new
; objects and rarely change the number of methods.