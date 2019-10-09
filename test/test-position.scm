(use gauche.test)

(use srfi-19)

(use util.match)

(add-load-path "./lib/")

(test-start "Query")
(load "./lib/query")
(import query)

(test-module 'query)

;;;;;;;;;;;;;;;;;

(test-start "Position")
(load "./lib/position")
(import position)

(test-module 'position)
