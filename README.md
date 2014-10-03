# instagenerate

An implementation of the core functionality of instaparse written in core.logic. While this version
is obviously not nearly as performant, with this model we can solve other problems like generating
all possible strings for a parser, or getting the string that will parse to a certain parse tree.

## Usage

The key here is that there is now a contraint, "instaparseo", which given a parser,
constrains a string to a parse tree.

	=> (take 3 (run* [input output]
	                 (instaparseo (insta/parser "S = 'ab' C
	                                             C = 'c'+")
	                              input output)))
	([("ab" "c") (:S "ab" (:C "c"))]
	 [("ab" "c" "c") (:S "ab" (:C "c" "c"))]
	 [("ab" "c" "c" "c") (:S "ab" (:C "c" "c" "c"))])

Note that in this model, the "input" is actually a list of strings, and each string is a
chunk of characters that is parsed in each "string" combinator. This turns out
to not be a problem because we typically would use this model to _generate_ strings rather than
consume them, and once they are generated it's trivial to just append them together with
`(apply str)`.

With this model it's simple to ask what strings can be generated with a given parser (essentially
we just ignore the output):

	=> (run* [input]
	         (fresh [output]
	                (instaparseo (insta/parser "S = 'a' | 'b'") input output)))
	(("a") ("b"))

As Zack Maril suggested, one may want to generate input strings given a parse tree, though the parse
tree may have some holes to fill in:

	=> (def grammar
         (insta/parser
           "S = name <' '> business
            name = 'Steve' | 'Mary' | 'Bob'
            business = 'Bakery' | 'Shop'"))
       (map (partial apply str)
            (run* [input]
                  (fresh [dont-care1 dont-care2]
                    (instaparseo grammar input
                                 (list :S (lcons :name dont-care1)
                                          ; lisp-style cons pairs, i.e. (:name & dont-care1)
                                          (lcons :business dont-care2))))))
    ("Steve Bakery"
	 "Steve Shop"
	 "Mary Bakery"
	 "Bob Bakery"
	 "Mary Shop"
	 "Bob Shop")



## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
