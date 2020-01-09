rpnben
======

Normalize boolean expressions (given in postfix notation) and express them as a sum of products.

To compile:

ocamlbuild main.native

Reserved characters are `+&*|-~01`. All other characters are treated as variables.

Examples:

(A) --> `A`
(not A) --> `A~`
(A or B) --> `BA|`
(A or B) and (C or D) --> `DC|BA|&`
(not A and not B) --> `B~A~&`
(A xor B) --> `BA^`
