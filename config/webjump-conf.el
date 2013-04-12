;; webjump-conf.el

(eval-after-load 'webjump
  '(progn
     (setq webjump-sites
           (append '(("Urban Dictionary" .
                      [simple-query
                       "www.urbandictionary.com"
                       "http://www.urbandictionary.com/define.php?term="
                       ""])
                     ("stackoverflow" .
                      [simple-query
                       "www.stackoverflow.com"
                       "http://stackoverflow.com/search?q="
                       ""])
                     ("askubuntu" .
                      [simple-query
                       "www.askubuntu.com"
                       "http://askubuntu.com/search?q="
                       ""])
                     ("superuser" .
                      [simple-query
                       "www.superuser.com"
                       "http://superuser.com/search?q="
                       ""])
                     ("tex.stackexchange" .
                      [simple-query
                       "tex.stackexchange.com"
                       "http://tex.stackexchange.com/search?q="
                       ""])
                     ("math.stackexchange" .
                      [simple-query
                       "math.stackexchange.com"
                       "http://math.stackexchange.com/search?q="
                       ""])
                     ("leo" .
                      [simple-query
                       "dict.leo.org"
                       "http://dict.leo.org/ende?search="
                       ""])
                     ("Java API" .
                      [simple-query
                       "www.google.com"
                       "http://www.google.ca/search?hl=en&as_sitesearch=http://java.sun.com/javase/6/docs/api/&q="
                       ""])
                     ("ClojureDocs" .
                      [simple-query
                       "clojuredocs.org"
                       "http://clojuredocs.org/search?q="
                       ""])
                     ("Clojars" .
                      [simple-query
                       "clojars.org"
                       "https://clojars.org/search?q="
                       ""]))
                   webjump-sample-sites))))
