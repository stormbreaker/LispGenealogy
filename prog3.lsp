#| Class: CSC 461 - Programming Languages
 | Professor: Dr. John Weiss
 | Fall 2015
 | Benjamin Kaiser
 | Dates Modified:
 | 	11-27-15: Began
 | 	12-3-15: Wrote most of the fucntions
 | 	12-5-15: Wrote the rest of the functions and began commenting
 |	12-6-15: Finished commenting
 | Description:  This program reads a file into a *database* variable.  It then
 | contains functionality for querying the database via the functions which are
 | written.  The include: parents, mothers, fathers, children, sons,
 | daughters, siblings, sisters, brothers, grandparents, grandfathers, grandmothers,  
 | grandchildren, grandsons, granddaughters, uncles, aunts, nieces, nephews, cousins, male
 | cousins, femalecousins, ancestors, male-ancestors, female-ancestors, descendants, 
 | male-descendants, and female-descendants.
|#

#|This structure was created by Dr. Weiss to assist in creating the database.
 | It contains the name, gender, parents, and children of an individual
 |#
(defstruct person name gender parents children)
;Initialize the database to empty
(setf *database* nil)

#| Name: readfile
 | Author: Dr. Weiss, modified by Benjamin Kaiser
 | Description:  This function takes an input name and opens it.  It then reads into 
 | the global *database* variable which contains a list of the person structures.  
 | If the file cannot be opened, it returns the appropriate error message
 |#
(defun readfile (&optional filename)
	(let (temp)
		(when (null filename) (return-from readfile (format t "Usage: -repl fileio.lsp filename")))
	    (setf fin (open filename :if-does-not-exist nil))   ; open file, returning NIL on error
		(when (null fin) (return-from readfile (format t "Error: cannot open file ~a" filename)))
		(do ((data (read fin nil) (read fin nil)))          ; read entire file, returning 	NIL at EOF
			((null data) (close fin))				; exit when file is read
			(setf temp (make-person :name (first data) :gender (second data) :parents (third data) :children (fourth data)))			
			(push temp *database*)
		)
	)
)

#|
 | Name: malefilter
 | Author: Benjamin Kaiser
 | Description:  This function takes in the name of a person and if it is a female,
 | lists it.  If it is male, it ignores it.  In this way, I am filtering out the
 | males.  It is meant to be used with a mapcar function.  
 |#
(defun malefilter (name)
	(let (x temp)	
		(dolist (temp *database*)
			(if (equal name (person-name temp)) (setf x temp))
		)
		(if (equal 'female (person-gender x)) (list (person-name x)) nil)
	)
)

#|
 | Name: femalefilter
 | Author: Benjamin Kaiser
 | Description:  This function takes in the name of a person and if it is a male,
 | lists it.  If it is female, it ignores it.  In this way, I am filtering out the
 | females.  It is meant to be used with a mapcar function.
|#
(defun femalefilter (name)
	(let (x temp)	
		(dolist (temp *database*)
			(if (equal name (person-name temp)) (setf x temp))
		)
		(if (equal 'male (person-gender x)) (list (person-name x)) nil)
	)
)

#|
 | Name: findChildren
 | Author: Benjamin Kaiser
 | Description:  This function is a helper function to the queries.  
 | It takes in a list of names and returns a list of the children
 | of everyone in this list.  
 |#
(defun findChildren (listnames)
	(let (y x temp)
		(dolist (x listnames)
			(setf y (append y (children x)))
		)
		y
	)
)

#|
 | Name: findParents
 | Author: Benjamin Kaiser
 | Description:  This function is a helper function to the queries.
 | It takes in a list of names and returns a list of the parents
 | of everyone in this list.
 |#

(defun findParents (listnames)
	(let (y x temp)
		(dolist (x listnames)
			(setf y (append y (parents x)))
		)
		y
	)
)

#|
 | Name: findSiblings
 | Author: Benjamin Kaiser
 | Description:  This function is a helper function to the queries.
 | It takes in a list of names and returns a list of the siblings
 | of everyone in this list.
 |#
(defun findSiblings (listnames)
	(let (y x temp)
		(dolist (x listnames)
			(setf y (append y (siblings x)))
		)
		y
	)
)

#|
 | Name: findSpouse
 | Author: Benjamin Kaiser
 | Description:  This function is a helper function to the queries.
 | It takes in a name and returns a list of the spouse of this person
 | of everyone in this list.
 |#
(defun findSpouse (name)
	(let (x y temp)
		(setf x (children name))
		(setf y (findParents x))
		(setf temp (delete name y))
		temp
	)
)

#|
 | Name: children
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of the children of this person
 |#
(defun children (name)
	(let (x temp)
		(dolist (temp *database*)
			(if	(equal name (person-name temp)) (setf x (person-children temp)))
		)
		x
	)
)

#|
 | Name: parents
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of the parents of this person
 |#
(defun parents (name)
	(let (x temp)
		(dolist (temp *database*)
			(if	(equal name (person-name temp)) (setf x (person-parents temp)))
		)
		x
	)
)

#| Name: mothers
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of the parents of this person.
 | However, it also calls the male filter so that
 | in the end, the only people returned are female parents
 |#
(defun mothers (name)
	(let (x y temp temp1 temp2)
		;(setf y (mapcar #'parents name))
		(setf temp (parents name))
		(setf y (mapcar #'malefilter temp))
		(setf y (apply #'append y))
	)
)

#|
 | Name: fathers
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of the parents of this person.
 | However, it also calls the female filter so that
 | in the end, the only people returned are male parents
 |#
(defun fathers (name)
	(let (x y temp temp1 temp2)
		(setf temp (parents name))
		(setf y (mapcar #'femalefilter temp))
		(setf y (apply #'append y))
	)
)

#|
 | Name: sons
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of the children of this person.
 | However, it also calls the female filter so that
 | in the end, the only people returned are male children
 |#
(defun sons (name)
	(let (x y temp temp1 temp2)
		(setf temp (children name))
		(setf y (mapcar #'femalefilter temp))
		(setf y (apply #'append y))
	)
)


#| Name: daughters
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of the children of this person.
 | However, it also calls the male filter so that
 | in the end, the only people returned are female children
 |#
(defun daughters (name)
	(let (x y temp temp1 temp2)
		(setf temp (children name))
		(setf y (mapcar #'malefilter temp))
		(setf y (apply #'append y))
	)
)

#| Name: siblings
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of the siblings of this person.
 |#
(defun siblings (name)
	(let (x temp temp1)
		(setf x (parents name))
		(dolist (temp1 x)
			(if (eq nil (children temp1)) nil (setf temp (append temp (children temp1))))
		)
		(setf x (delete name temp))
		(setf temp (remove-duplicates x))
	)
)

#| Name: sisters
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of the siblings of this person.
 | However, it also calls the male filter so that
 | in the end, the only people returned are female siblings
 |#
(defun sisters (name)
	(let (x y temp temp1)
		(setf x (parents name))
		(dolist (temp1 x)
			(setf temp (append temp (children temp1)))
		)
		(setf x (delete name temp))
		(setf temp (remove-duplicates x))
		(setf y (mapcar #'malefilter temp))
		(setf y (apply #'append y))
	)
)

#| Name: brothers
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of the siblings of this person.
 | However, it also calls the female filter so that
 | in the end, the only people returned are male siblings
 |#
(defun brothers (name)
	(let (x y temp temp1)
		(setf x (parents name))
		(dolist (temp1 x)
			(setf temp (append temp (children temp1)))
		)
		(setf x (delete name temp))
		(setf temp (remove-duplicates x))
		(setf y (mapcar #'femalefilter temp))
		(setf y (apply #'append y))
	)
)

#| Name: grandparents
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of all the grandparents of this person.  It does this by
 | calling parents and then by calling parents on each parent
 |#
(defun grandparents (name)
	(let (x y temp temp1)
		(setf x (parents name))
		(dolist (temp1 x)
			(setf temp (append temp (parents temp1)))
		)
		temp
	)
)

#| Name: grandfathers
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of all the grandparents of this person.  It does this by
 | calling parents and then by calling parents on each parent
 | However this also calls the female filter so that only male
 | grandparents are returned.  
 |#
(defun grandfathers (name)
	(let (x y temp temp1)
		(setf x (parents name))
		(dolist (temp1 x)
			(setf temp (append temp (parents temp1)))
		)
		(setf y (mapcar #'femalefilter temp))
		(setf y (apply #'append y))
	)
)

#| Name: grandmothers
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of all the grandparents of this person.  It does this by
 | calling parents and then by calling parents on each parent
 | However this also calls the male filter so that only female
 | grandparents are returned.  
 |#
(defun grandmothers (name)
	(let (x y temp temp1)
		(setf x (parents name))
		(dolist (temp1 x)
			(setf temp (append temp (parents temp1)))
		)
		(setf y (mapcar #'malefilter temp))
		(setf y (apply #'append y))
	)
)

#| Name: grandchildren
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of all the grandchildren of this person.  It does this by
 | calling children and then by calling children on each child
 |#
(defun grandchildren (name)
	(let (x y temp temp1)
		(setf x (children name))
		(dolist (temp1 x)
			(setf temp (append temp (children temp1)))
		)
		temp
	)
)

#| Name: grandsons
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of all the grandchildren of this person.  It does this by
 | calling children and then by calling children on each child
 | However, this also calls the female filter so that only
 | male grandchildren are returned.  
 |#
(defun grandsons (name)
	(let (x y temp temp1)
		(setf x (children name))
		(dolist (temp1 x)
			(setf temp (append temp (children temp1)))
		)
		(setf y (mapcar #'femalefilter temp))
		(setf y (apply #'append y))
	)
)

#| Name: granddaughters
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of all the grandchildren of this person.  It does this by
 | calling children and then by calling children on each child
 | However, this also calls the male filter so that only
 | female grandchildren are returned.  
 |#
(defun granddaughters (name)
	(let (x y temp temp1)
		(setf x (children name))
		(dolist (temp1 x)
			(setf temp (append temp (children temp1)))
		)
		(setf y (mapcar #'malefilter temp))
		(setf y (apply #'append y))
	)
)

#| Name: uncles
 | Author: Benjamin Kaiser
 | Description:  This function takes in a name and returns
 | a list of all the uncles of the person.  It does this by
 | finding the parents of the person, finding the siblings of
 | each parent and then finding the children of each sibling.
 | Each parent of the children is then found.  A female filter
 | is applied and the male parents are returned
 |#
(defun uncles (name)
	(let (x y temp temp1)
		(setf x (parents name))
		(setf y (findSiblings x))
		(setf x (findChildren y))
		(setf y (findParents x))
		(setf y (mapcar #'femalefilter y))
		(setf y (apply #'append y))
		(setf temp (remove-duplicates y))
		temp
	)
)

#| Name: aunts
 | Author: Benjamin Kaiser
 | Description:  This function takes in a name and returns
 | a list of all the aunts of the person.  It does this by
 | finding the parents of the person, finding the siblings of
 | each parent and then finding the children of each sibling.
 | Each parent of the children is then found.  A male filter
 | is applied and the female parents are returned
 |#
(defun aunts (name)
	(let (x y temp temp1)
		(setf x (parents name))
		(setf y (findSiblings x))
		(setf x (findChildren y))
		(setf y (findParents x))
		(setf y (mapcar #'malefilter y))
		(setf y (apply #'append y))
		(setf temp (remove-duplicates y))
		temp
	)
)

#| Name: nieces
 | Author: Benjamin Kaiser
 | Description:  This function takes in a name and returns
 | a list of all the nieces of the person.  It does this by
 | finding the siblings of the person, finding the children of
 | each sibling and then finding the spouse(s) and their siblings.
 | Each child of these siblings is also found.  A male filter
 | is applied and the female children are returned. 
 |#
(defun nieces (name)
	(let (x y temp temp1)
		(setf y (siblings name))
		(setf x (findChildren y))
		(setf temp1 (findSpouse name))
		;(break)
		(setf y (findSiblings temp1))
		(setf x (append x (findChildren y)))
		(setf x (mapcar #'malefilter x))
		(setf x (apply #'append x))
		(setf temp (remove-duplicates x))
		temp
	)
)

#| Name: nephews
 | Author: Benjamin Kaiser
 | Description:  This function takes in a name and returns
 | a list of all the nephews of the person.  It does this by
 | finding the siblings of the person, finding the children of
 | each sibling and then finding the spouse(s) and their siblings.
 | Each child of these siblings is also found.  A female filter
 | is applied and the male children are returned. 
 |#
(defun nephews (name)
	(let (x y temp temp1)
		(setf y (siblings name))
		(setf x (findChildren y))
		(setf temp1 (findSpouse name))
		;(break)
		(setf y (findSiblings temp1))
		(setf x (append x (findChildren y)))
		(setf x (mapcar #'femalefilter x))
		(setf x (apply #'append x))
		(setf temp (remove-duplicates x))
		temp
	)
)
#| Name: cousins
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of the cousins of a person.  It does this by calling
 | aunts and then uncles, appending the lists together,
 | finding the children of each aunt or uncle and then removing duplicates.  
 |#
(defun cousins (name)
	(let (x y temp)
		(setf x (aunts name))
		(setf x (append x (uncles name)))
		(setf y (findChildren x))
		(setf temp (remove-duplicates y))
		temp
	)
)

#| Name: male-cousins
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of the male cousins of a person.  It does this by calling
 | aunts and then uncles, appending the lists together,
 | finding the children of each aunt or uncle and then removing duplicates. 
 | It then applies a female filter to get only males.   
 |#
(defun male-cousins (name)
	(let (x y temp)
		(setf x (aunts name))
		(setf x (append x (uncles name)))
		(setf y (findChildren x))
		(setf y (mapcar #'femalefilter y))
		(setf y (apply #'append y))
		(setf temp (remove-duplicates y))
		temp
	)
)

#| Name: female-cousins
 | Author: Benjamin Kaiser
 | Description: This function takes in a name and returns
 | a list of the male cousins of a person.  It does this by calling
 | aunts and then uncles, appending the lists together,
 | finding the children of each aunt or uncle and then removing duplicates. 
 | It then applies a male filter to get only females.   
 |#
(defun female-cousins (name)
	(let (x y temp)
		(setf x (aunts name))
		(setf x (append x (uncles name)))
		(setf y (findChildren x))
		(setf y (mapcar #'malefilter y))
		(setf y (apply #'append y))
		(setf temp (remove-duplicates y))
		temp
	)
)

#| Name: ancestors
 | Author: Benjamin Kaiser
 | Description:  This function is the function that should be called
 | to query the databse for the ancestors of a specific function.  
 | It takes a name and calls a helper function to get the list of
 | ancestors back.  
 |#
(defun ancestors (name)
    (let (ancestorlist)
        (setf ancestorlist (parents name))
        (setf ancestorlist (findAncestors ancestorlist))
		ancestorlist
    )
)

#| Name: male-ancestors
 | Author: Benjamin Kaiser
 | Description:  This function is the function that should be called
 | to query the databse for the ancestors of a specific function.  
 | It takes a name and calls a helper function to get the list of
 | ancestors back.  It then calls a female filter to return only
 | male ancestors.
 |#
(defun male-ancestors (name)
	(let (temp x)
		(setf temp (ancestors name))
		(setf x (mapcar #'femalefilter temp))
		(setf x (apply #'append x))
		x
	)
)

#| Name: female-ancestors
 | Author: Benjamin Kaiser
 | Description:  This function is the function that should be called
 | to query the databse for the ancestors of a specific function.  
 | It takes a name and calls a helper function to get the list of
 | ancestors back.  It then calls a male filter to return only
 | female ancestors.
 |#
(defun female-ancestors (name)
	(let (temp x)
		(setf temp (ancestors name))
		(setf x (mapcar #'malefilter temp))
		(setf x (apply #'append x))
		x
	)
)

#| Name: descendants
 | Author: Benjamin Kaiser
 | Description:  This function is the function that should be called
 | to query the databse for the descendants of a specific function.  
 | It takes a name and calls a helper function to get the list of
 | descendants back.  
 |#
(defun descendants (name)
    (let (descendantlist)
        (setf descendantlist (children name))
        (setf descendantlist (findDescendants descendantlist))
		descendantlist
    )
)

#| Name: male-descendants
 | Author: Benjamin Kaiser
 | Description:  This function is the function that should be called
 | to query the databse for the descendants of a specific function.  
 | It takes a name and calls a helper function to get the list of
 | descendants back.  It then calls a female filter to return only
 | male ancestors.
 |#
(defun male-descendants (name)
	(let (temp x)
		(setf temp (descendants name))
		(setf x (mapcar #'femalefilter temp))
		(setf x (apply #'append x))
		x
	)
)

#| Name: female-descendants
 | Author: Benjamin Kaiser
 | Description:  This function is the function that should be called
 | to query the databse for the descendants of a specific function.  
 | It takes a name and calls a helper function to get the list of
 | descendants back.  It then calls a male filter to return only
 | female ancestors.
 |#
(defun female-descendants (name)
	(let (temp x)
		(setf temp (descendants name))
		(setf x (mapcar #'malefilter temp))
		(setf x (apply #'append x))
		x
	)
)

#| Name: findAncestors
 | Author: Benjamin Kaiser
 | Description:  This function is a helper function and does the main
 | processing for when finding ancestors.  It takes a list of names
 | and recursively finds the ancestors for each.  It then puts this in
 | an optional ancestorlist parameter which eventually gets returned.  
 | The base case is when the parents hit nil.  
 |#
(defun findAncestors (listnames &optional ancestorlist)
    (let (tempParents temp)
        (dolist (temp listnames)
             (setf ancestorlist (append ancestorlist (list temp)))
        )
        (setf tempParents (findParents listnames))
        (if (equal nil tempParents) (return-from findAncestors ancestorlist)
            (return-from findAncestors (findAncestors tempParents ancestorlist))
        )
    )
)

#| Name: findDescendants
 | Author: Benjamin Kaiser
 | Description:  This function is a helper function and does the main
 | processing for when finding descendants.  It takes a list of names
 | and recursively finds the descendants for each.  It then puts this in
 | an optional descendantlist parameter which eventually gets returned.  
 | The base case is when the children hit nil.  
 |#
(defun findDescendants (listnames &optional descendantlist)
	(let (tempChildren temp)
		(dolist (temp listnames)
			(setf descendantlist (append descendantlist (list temp)))
		)
		(setf tempChildren (findChildren listnames))
		(if (equal nil tempChildren) (return-from findDescendants descendantlist)
			(return-from findDescendants (findDescendants tempChildren descendantlist))
		)
	)
)

#| Name: "main"
 | Author: Benjamin Kaiser
 | These two lines read into the databse.  
 |#
(setf *database* nil)
(readfile (car *args*))

