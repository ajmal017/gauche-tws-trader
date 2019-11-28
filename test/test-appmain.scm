(use gauche.test)
(use srfi-19)
(use util.match)

(add-load-path "./lib/")
(use violet)

(test-start "appmain")
(load "./lib/appmain")
(import appmain)

(test-module 'appmain)
