# How to start the application

1. Build the project
```
lisp-interpreter$ make
```
2. Run interpreter
```
lisp-interpreter$ ./hal <lisp-filename> <'-i' (optional interactive mode)>
```

# Example of usage

```

lisp-interpreter$ cat fact.scm
(define (fact x)
  (cond ((eq? x 1) 1)
    (#t (* x (fact (- x 1))))))
```

```
lisp-interpreter$ hal fact.scm -i
> (fact 10)
10946
```

#

* Made as part of the FP course at EPITECH
* Made with [Hugo Prevost](https://github.com/DipStax)
