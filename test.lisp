(defpackage :4store-test
  (:use :cl :4store :fiveam))

(in-package :4store-test)

(parse-literal "2")
(parse-literal "\"2\"^^<http://www.w3.org/2001/XMLSchema#integer>")
(parse-literal "\"hello\"")
(parse-literal "<http://bonanza.cl.no>")
