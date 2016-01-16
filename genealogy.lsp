#|
Program:     Program Assignment 3: Genealogy in Lisp
Author:	     Carrie Veer
Date:	     December 06, 2015
Professor:   Dr. John Weiss
Course:	     CSC 461 - M001
Location:    McLaury 305
Description: This program reads in a family database file and puts all of the
                information into a global variable called *database*.  The
                following queries are functions which can be run in order to
                find the parents, mothers, fathers, children, sons, daughters,
                siblings, sistres, brothers, grandparents, grandfathers, 
                grandmothers, grandchildren, grandsons, granddaughters, uncles,
                aunts, nieces, nephews, cousins, male-cousins, female-cousins, 
                ancestors, male-ancestors, female-ancestors, descendants, 
                male-descendants, and female-descendants of a person in the
                database.  If the person is not in the database, an error
                message will be printed.  If the person is in the database
                and has relatives of the type queried, their names will be
                printed.  If they do not, NIL will be printed to the screen.
                If a file that does not exist or if no argument at all is
                passed in for the filename, error messages will be printed.
Usage:	      clisp -repl genealogy.lsp family.dat
                where family.dat is the name of the database file
Bugs:	      None
Todo:	      None
Timeline:
    Date:			Modification:
    Nov 20, 2015        Wrote the program.
    Dec 02, 2015	Finished commenting the program.
|#


;Initializes the global variable database to nil
(setf *database* nil)


#|
Author: Carrie Veer
Description: The add function takes the information in data and turns it into a
    structure of type person then adds it to the global list named database
Parameters: info - A list in the following format:
                    (name gender (parent's names) (kid's names))
|#
(defun add (info)
    ;Adds a person created out of the information in info to the database
    (let (new_person)
        (setf new_person (make-person :name (first info) :gender (second info) 
        :parents (third info) :children (fourth info)))
        (setf *database* (cons new_person *database*))
    )
)


#|
Author: Carrie Veer
Description: The ancestors function calls the find_ancestors function which 
    returns a list of all of the ancestors of the person whose name is held in 
    the name variable.  Since the list returned from find_ancestors includes 
    the name of the person, whose ancestors are being queried, name is removed
    from the list.
Parameters: name - The name of the person whose ancestors are being queried
|#
(defun ancestors (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from ancestors (format t "Usage: (ancestors name)")))
    ;Creates local variable my_ancestors
    (let (my_ancestors)
        ;Puts the atom name into the list my_ancestors
        (setf my_ancestors (cons name nil))
        ;Sets my_ancestors to the list of ancestors which contains name
        (setf my_ancestors (find_ancestors my_ancestors))
        ;Removes the original name from my_ancestors
        (if (eq nil my_ancestors) nil
            (setf my_ancestors (remove name my_ancestors :test #'equal))
        )
    )
)


#|
Author: Carrie Veer
Description: The aunts function calls the find_aunt_uncles function which 
    returns a list of the aunts and uncles of the person whose name is held in 
    the variable name.  The results are then filtered using the femalefilter 
    function.
Parameters: name - The name of the person whose aunts are being queried
|#
(defun aunts (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from aunts (format t "Usage: (aunts name)")))
    ;Creates local variable my_aunts
    (let (my_aunts)
        ;Sets my_aunts to the list of aunts and uncles of name
        (setf my_aunts (find_aunt_uncles name))
        ;Removes uncles from my_aunts if the list contains names
        (if (eq my_aunts nil) nil
            (setf my_aunts (femalefilter my_aunts))
        )
    )
)


#|
Author: Carrie Veer
Description: The brother function class the find_siblings function and then 
    filters the results using the malefilter function.
Parameters: name - The name of the person whose brothers are being queried
|#
(defun brothers (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from brothers (format t "Usage: (brothers name)")))
    ;Turns the atom name into a list
    (setf name (cons name nil))
    ;Creates local variable curr_siblings
    (let (my_brothers)
        ;Sets my_brothers to a list of name's siblings
        (setf my_brothers (find_siblings name))
        ;Removes sisters from my_brothers if the list contains names
        (if (eq nil my_brothers) nil
            (setf my_brothers (malefilter my_brothers))
        )
    )
)


#|
Author: Carrie Veer
Description: The children function calls the find_kids function returns a list
    of the person's, whose name was passed in, children.
Parameters: name - The name of the person whose children are being queried
|#
(defun children (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from children (format t "Usage: (children name)")))
    ;Turns the atom name into a list
    (setf name (cons name nil))
    ;Returns the children of name
    (find_kids name)
)


#|
Author: Carrie Veer
Description: The cousins funtion calls the find_cousins function and returns a
    list of the person's, whose name was passed in, cousins.
Parameters: name - The name of the person whose cousins are being queried
|#
(defun cousins (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from cousins (format t "Usage: (cousins name)")))
    ;Returns the cousins of name
    (find_cousins name)
)

#|
Author: Carrie Veer
Description: The daughters function calls the find_kids function and then
    filters the results using the femalefilter function
Parameters: name - The name of the person whose daughters are being queried
|#
(defun daughters (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from daughters (format t "Usage: (daughters name)")))
    ;Turns the atom name into a list
    (setf name (cons name nil))
    ;Creates local variable my_daughters
    (let (my_daughters)
        ;Sets my_daughters to a list of children of name
        (setf my_daughters (find_kids name))
        ;Removes sons from my_daughters if the list contains names
        (if (eq my_daughters nil) nil (setf my_daughters (femalefilter my_daughters)))
    )
)


#|
Author: Carrie Veer
Description: The descendants function calls the find_descendants function which
    returns a list of all of the descendants of the person whose name is held 
    in the name variable.  Since the list returned from find_descendants 
    includes the name of the person whose descendants are being queried, name 
    is removed from the list.
Parameters: name - The name of the person whose descendants are being queried
|#
(defun descendants (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from descendants (format t "Usage: (descendants name)")))
    ;Creates the local variable my_descendants
    (let (my_descendants)
        ;Puts name into the list my_descendants
        (setf my_descendants (cons name nil))
        ;Returns a list of descendants of name which includes name
        (setf my_descendants (find_descendants my_descendants))
        ;Removes name from my_descendants
        (if (eq nil my_descendants) nil
            (setf my_descendants (remove name my_descendants :test #'equal))
        )
    )
)


#|
Author: Carrie Veer
Description: The father function calls the find_parents function and then 
    filters the results using the malefilter function.
Parameters: name - The name of the person whose fathers are being queried
|#
(defun fathers (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from fathers (format t "Usage: (fathers name)")))
    ;Turns the atom name into a list
    (setf name (cons name nil))
    ;Creates the local variable my_fathers
    (let (my_fathers)
        ;Sets my_fathers to a list of parents
        (setf my_fathers (find_parents name))
        ;Removes mothers from my_fathers if the list contains names
        (if (eq my_fathers nil) nil (setf my_fathers (malefilter my_fathers)))
    )
)


#|
Author: Carrie Veer
Description: The femalefilter function filters a list of names by their gender.
    It does this by calling find_people which turns the names into their 
    respective person structures which contain the gender of each person.  Then
    all of the females' names are added to a different list which is returned 
    from the function.
Parameters: list_names - The list of names to filter
|#
(defun femalefilter (list_names)
    ;Creates the local variable gals
    (let (gals)
        ;Removes all males from the list
        (loop for item in (find_people list_names) while item do
            (if (eq 'male (person-gender item)) nil
                (setf gals (cons (person-name item) gals))
            )
        )
        ;Returns gals
        (return-from femalefilter gals)
    )
)


#|
Author: Carrie Veer
Description: The female-ancestors function calls the find_ancestors function 
    which returns a list of all of the ancestors of the person whose name is 
    held in the name variable.  Since the list returned from find_ancestors 
    includes the name of the person whose ancestors are being queried, name is 
    removed from the list.  Then the list is filtered using the femalefilter 
    function.
Parameters: name - The name of the person whose female ancestors are being 
                        queried
|#
(defun female-ancestors (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name)
        (return-from female-ancestors (format t "Usage: (female-ancestors name)"))
    )
    ;Creates local variable my_ancestors
    (let (my_ancestors)
        ;Puts the atom name in the list my_ancestors
        (setf my_ancestors (cons name nil))
        ;Sets my_ancestors to a list of ancestors of name and includes name
        (setf my_ancestors (find_ancestors my_ancestors))
        ;Removes name from my_ancestors if it contains names
        (if (eq nil my_ancestors) (return-from female-ancestors nil)
            (setf my_ancestors (remove name my_ancestors :test #'equal))
        )
        ;Removes male ancestors from my_ancestors if it contains names
        (if (eq nil my_ancestors) nil
            (setf my_ancestors (femalefilter my_ancestors))
        )
    )
)


#|
Author: Carrie Veer
Description: The female-cousins function calls the find_cousins function and 
    then filters the results using the femalefilter function.
Parameters: name - The name of the person whose female cousins are being 
                    queried
|#
(defun female-cousins (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name)
        (return-from female-cousins (format t "Usage: (female-cousins name)"))
    )
    ;Creates a local variable my_cousins
    (let (my_cousins)
        ;Sets my_cousins to a list of male and female cousins
        (setf my_cousins (find_cousins name))
        ;Removes male cousins from my_cousins if it contains names
        (if (eq my_cousins nil) nil
            (setf my_cousins (femalefilter my_cousins))
        )
    )
)


#|
Author: Carrie Veer
Description: The female-descendants function calls the find_descendants 
    function which returns a list of all of the descendants of the person whose 
    name is held in the name variable.  Since the list returned from 
    find_descendants includes the name of the person whose descendants are 
    being queried, name is removed from the list.  Finally, the results are 
    filtered using the femalefilter function.
Parameters: name - The name of the person whose female descendants are being 
                    queried
|#
(defun female-descendants (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name)
        (return-from female-descendants (format t "Usage: (female-descendants name)"))
    )
    ;Creates local variable my_descendants
    (let (my_descendants)
        ;Puts the atom name into the list my_descendants
        (setf my_descendants (cons name nil))
        ;Sets my_descendants to a list of male and female descendants
        (setf my_descendants (find_descendants my_descendants))
        ;Removes name from my_descendants if it contains names
        (if (eq nil my_descendants) (return-from female-descendants nil)
            (setf my_descendants (remove name my_descendants :test #'equal))
        )
        ;Removes male descendants from my_descendants if it contains names
        (if (eq nil my_descendants) nil
            (setf my_descendants (femalefilter my_descendants))
        )
    )
)


#|
Author: Carrie Veer
Description: The find_ancestors function recursively calls the find_parents 
    function and adds the results to a list which is then returned from the 
    function.  This list also contains the names that were in the original list
    which was passed in to the function.
Parameters: list_names - The list of names of the person whose ancestors need
                            to be found
            my_ancestors - An optional parameter: a list of ancestors that have
                                                        been found
|#
(defun find_ancestors (list_names &optional my_ancestors)
    ;Creates local variable my_parents
    (let (my_parents)
        ;Loops through the list passed in and adds all of the items to my_ancestors
        (loop for item in list_names while item do
             (setf my_ancestors (cons item my_ancestors))
        )
        ;Sets my_parents to the parents of list_names
        (setf my_parents (find_parents list_names))
        ;If parents were found the function is ended,
        ;otherwise a recursive call is made
        (if (eq nil my_parents) (return-from find_ancestors my_ancestors)
            (return-from find_ancestors (find_ancestors my_parents my_ancestors))
        )
    )
)


#|
Author: Carrie Veer
Description: The find_aunt_uncle function calls the find_parent_siblings 
    function and then calls the find_kids function on the list that 
    find_parent_siblings returns.  It then calls the find_parents function on
    the list the find_kids function returned.  Combining the results of 
    find_parent_siblings and find_parents together gives a list of aunts and 
    uncles for the person whose name was passed in.
Parameters: name - The name of the person whose aunts and uncles are being 
                    queried
|#
(defun find_aunt_uncles (name)
    ;Turns the atom name into a list
    (setf name (cons name nil))
    ;Creates local variables my_aunt_uncles and my_in-laws
    (let (my_aunt_uncles my_in-laws)
        ;Sets my_aunt_uncles to a list of the parents' siblings of name
        (setf my_aunt_uncles (find_parent_siblings name))
        ;Exits the function if my_aunt_uncles is empty otherwise it sets 
        ;my_in-laws to the children of the aunts and uncles found so far
        (if (eq nil my_aunt_uncles) (return-from find_aunt_uncles nil)
            (setf my_in-laws (find_kids my_aunt_uncles))
        )
        ;Returns my_aunt_uncles from the function if they do not have any
        ;children, otherwise my_in-laws is set to the parents of those children
        (if (eq nil my_in-laws) (return-from find_aunt_uncles my_aunt_uncles)
            (setf my_in-laws (find_parents my_in-laws))
        )
        ;Puts my_in-laws into my_aunt_uncles
        (nconc my_aunt_uncles my_in-laws)
        ;Removes the duplicates from my_aunt_uncles
        (setf my_aunt_uncles (remove-duplicates my_aunt_uncles))
    )
)


#|
Author: Carrie Veer
Description: The find_cousins function calls the find_parent_siblings function 
    and then calls the find_kids function on the results of the first function 
    call.  Then it removes any duplicate names.
Parameters: name - The name of the person whose cousins are being queried
|#
(defun find_cousins (name)
    ;Turns the atom name into a list
    (setf name (cons name nil))
    ;Creates local variable my_cousins
    (let (my_cousins)
        ;Sets my_cousins to a list of parent siblings
        (setf my_cousins (find_parent_siblings name))
        ;If my_cousins contains names, it is set to the list of their kids
        (if (eq my_cousins nil) nil
            (setf my_cousins (find_kids my_cousins))
        )
    )
)


#|
Author: Carrie Veer
Description: The find_descendants function recursively calls the find_kids 
    function and adds the results to a list which is then returned from the 
    function.  This list also contains the names that were in the original list
    which was passed in to the function.
Parameters: list_names - The list of names of the person whose descendants need
                            to be found
            my_ancestors - An optional parameter: a list of descendants that 
                                                    have been found
|#
(defun find_descendants (list_names &optional my_descendants)
    ;Creates local variable my_kids
    (let (my_kids)
        ;Adds list_names to my_descendants
        (loop for item in list_names while item do
             (setf my_descendants (cons item my_descendants))
        )
        ;Sets my_kids to the children of those in list_names
        (setf my_kids (find_kids list_names))
        ;Makes a recursive call if kids were found, otherwise it my_descendants
        ;is returned
        (if (eq nil my_kids) (return-from find_descendants my_descendants)
            (return-from find_descendants (find_descendants my_kids my_descendants))
        )
    )
)


#|
Author: Carrie Veer
Description: The find_grandchildren function calls the find_kids function and 
    then calls it again on the results of the first call.
Parameters: name - The name of the person whose grandchildren are being queried
|#
(defun find_grandchildren (name)
    ;Turns the atom name into a list
    (setf name (cons name nil))
    ;Creates local variable my_grandkids
    (let (my_grandkids)
        ;Sets my_grandkids to the kids of name
        (setf my_grandkids (find_kids name))
        ;If my_grandkids contains names, the children of those names
        ;replace the previous names in my_grandkids
        (if (eq nil my_grandkids) nil 
            (setf my_grandkids (find_kids my_grandkids))
        )
    )
)


#|
Author: Carrie Veer
Description: The find_grandparents function calls the find_parents function and
    then calls it again on the results of the first call.
Parameters: name - The name of the person whose grandparents are being queried
|#
(defun find_grandparents (name)
    ;Turns the atom name into a list
    (setf name (cons name nil))
    ;Creates a local variable my_grandparents
    (let (my_grandparents)
        ;Sets my_grandparents to the parents of name
        (setf my_grandparents (find_parents name))
        ;If my_grandparents contains names, it is set to the parents of the
        ;names currently in the list
        (if (eq my_grandparents nil) nil
            (setf my_grandparents (find_parents my_grandparents))
        )
    )
)


#|
Author: Carrie Veer
Description: The find_kids function accesses the children property in the 
    person structure, which is retrieved by using find_people function, and 
    then adds those names to a list which is then returned from the function.
Parameters: name - The name of the person whose children are being queried
|#
(defun find_kids (list_names)
    ;Creates a list of people structures
    (setf list_names (find_people list_names))
    ;Creates local variables my_kids and curr_kids
    (let (my_kids curr_kids)
        ;Loops through the people in list_names
        (loop for item in list_names while item do
            ;Sets curr_kids to the names of the children of item
            (setf curr_kids (person-children item))
            (if (eq curr_kids nil) nil
                ;Adds the curr_kids list to my_kids
                (loop for this in curr_kids while this do
                    (setf my_kids (cons this my_kids))
                )
            )
        )
        ;Removes duplicates from my_kids
        (setf my_kids (remove-duplicates my_kids))
    )
)

#|
Author: Carrie Veer
Description: The find_name function loops through the database looking for a 
    person structure whose name matches the one passed into the function.  If 
    one is found, that structure is returned; if a structure cannot be found, 
    the function returns nil.
Parameters: name - The name of the person whose person structure is being found
|#
(defun find_name (name)
    ;Loops through the structures in database
    (loop for item in *database* while item do
        ;Returns the person structure if found, nil otherwise
        (if (eq name (person-name item)) (return-from find_name item) nil)
    )
)

#|
Author: Carrie Veer
Description: The find_niblings function calls the find_siblings function and 
    then the find_kids function on the results of the find_siblings function.
Parameters: name - The name of the person whose niblings are being queried
|#
(defun find_niblings (name)
    ;Turns the atom name into a list
    (setf name (cons name nil))
    ;Creates the local variable my_niblings
    (let (my_niblings)
        ;Sets my_niblings to the siblings of name
        (setf my_niblings (find_siblings name))
        ;If my_niblings contains names, set my_niblings to the children
        ;of the names it contains
        (if (eq my_niblings nil) nil
            (setf my_niblings (find_kids my_niblings))
        )
    )
)


#|
Author: Carrie Veer
Description: The find_parents function accesses the parents property in the 
    person structure, which is retrieved using the find_people function, and 
    then adds those names to a list which is then returned from the function.
Parameters: list_names - The list of names of the people whose parents are 
                            being queried
|#
(defun find_parents (list_names)
    ;Creates local variables my_parents and curr_parents
    (let (my_parents curr_parents)
        ;Turns the list of names into a list of person structures
        (setf list_names (find_people list_names))
        ;If there are items in list_names, it loops through them and
        ;puts their parents in a list
        (if (eq list_names nil) (return-from find_parents nil)
            (loop for item in list_names while item do
                (setf curr_parents (person-parents item))
                (if (eq curr_parents nil) nil
                    (loop for item in curr_parents while item do
                        (setf my_parents (cons item my_parents))
                    )
                )
            )
        )
        ;Returns the list of my_parents
        (return-from find_parents my_parents)
    )
)


#|
Author: Carrie Veer
Description: The find_parent_siblings function calls the find_parents function
    and then the find_siblings function off of the results of the first 
    function call.
Parameters: list_names - The list of names of the people whose parents'  
                            siblings are being queried
|#
(defun find_parent_siblings (list_names)
    ;Creates local variable parent_siblings
    (let (parent_siblings)
        ;Sets parent_siblings to the parents of the names in list_names
        (setf parent_siblings (find_parents list_names))
        ;Sets parent_siblings to the siblings of the parents currently in
        ;parent_siblings if the list contains names
        (if (eq nil parent_siblings) nil
            (setf parent_siblings (find_siblings parent_siblings))
        )
    )
)


#|
Author: Carrie Veer
Description: The find_people function loops through the list of names and calls
    the find_name function to turn them into a list of person structures.  If a
    person is not found, an error message is printed to alert the user.
Parameters: list_names - The list of names of the people whose person 
                            structures are being found
|#
(defun find_people (list_names)
    ;Creates local variables curr_name and my_people
    (let (curr_name my_people)
        ;Sets curr_name to the person structure of name if it exists, and
        ;then adds it to my_people, otherwise it prints an error message
        (loop for item in list_names while item do
            (setf curr_name (find_name item))
            (if (eq nil curr_name)
                (format t "~a is not in the database~%" item)
                (setf my_people (cons curr_name my_people))
            )
        )
        ;Returns my_people
        (return-from find_people my_people)
    )
)

#|
Author: Carrie Veer
Description: The find_siblings function calls the find_parents function and 
    then the find_kids function on the result of the first function call.  It 
    puts these names into a list and removes the names which were originally 
    passed into the function from that list before returning it.
Parameters: list_names - The list of names of the people whose siblings are
                            being queried
|#
(defun find_siblings (list_names)
    ;Creates local variable kids
    (let (kids)
        ;Sets kids to the parents of the names in list_names
        (setf kids (find_parents list_names))
        ;If kids contains names, it is set to the children of
        ;the names it contains
        (if (eq kids nil) (return-from find_siblings nil)
            (setf kids (find_kids kids))
        )
        ;Removes the names in list_names from kids
        (loop for item in list_names while item do
            (if (eq nil kids) (return-from find_siblings nil)
                (setf kids (remove item kids))
            )
        )
        ;Returns kids
        (return-from find_siblings kids)
    )
)


#|
Author: Carrie Veer
Description: The grandchildren function calls the find_grandchildren function.
Parameters: name - The name of the person whose grandchildren are being queried
|#
(defun grandchildren (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from grandchildren (format t "Usage: (grandchildren name)")))
    ;Returns name's grandchildren
    (find_grandchildren name)
)


#|
Author: Carrie Veer
Description: The granddaughters function calls the find_grandchildren function 
    and then filters the results using the femalefilter function.
Parameters: name - The name of the person whose granddaughters are being queried
|#
(defun granddaughters (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name)
        (return-from granddaughters (format t "Usage: (granddaughters name)"))
    )
    ;Creates local variable my_granddaughters
    (let (my_granddaughters)
        ;Sets my_granddaughters to the grandchildren of name
        (setf my_granddaughters (find_grandchildren name))
        ;Removes grandsons from the my_granddaughters list
        (if (eq nil my_granddaughters) nil 
            (setf my_granddaughters (femalefilter my_granddaughters))
        )
    )
)


#|
Author: Carrie Veer
Description: The grandfathers function calls the find_grandparents function and
    then filters the results using the malefilter function.
Parameters: name - The name of the person whose grandfathers are being queried
|#
(defun grandfathers (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from grandfathers (format t "Usage: (grandfathers name)")))
    ;Creates the local variable my_grandfathers
    (let (my_grandfathers)
        ;Sets my_grandfathers to the grandparents of name
        (setf my_grandfathers (find_grandparents name))
        ;Removes the grandmothers from the my_grandfathers list
        (if (eq nil my_grandfathers) nil
            (setf my_grandfathers (malefilter my_grandfathers))
        )
    )
)


#|
Author: Carrie Veer
Description: The grandmothers function calls the find_grandparents function and
    then filters the results using the femalefilter function.
Parameters: name - The name of the person whose grandmothers are being queried
|#
(defun grandmothers (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from grandmothers (format t "Usage: (grandmothers name)")))
    ;Creates a local variable my_grandmothers
    (let (my_grandmothers)
        ;Sets my_grandmothers to the grandparents of name
        (setf my_grandmothers (find_grandparents name))
        ;Removes the grandfathers from the my_grandmothers list
        (if (eq nil my_grandmothers) nil
            (setf my_grandmothers (femalefilter my_grandmothers))
        )
    )
)


#|
Author: Carrie Veer
Description: The grandparents function calls the find_grandparents function.
Parameters: name - The name of the person whose grandparents are being queried
|#
(defun grandparents (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from grandparents (format t "Usage: (grandparents name)")))
    ;Returns the grandparents of name
    (find_grandparents name)
)


#|
Author: Carrie Veer
Description: The grandsons function calls the find_grandchildren function and
    then filters the results using the malefilter function.
Parameters: name - The name of the person whose grandsons are being queried
|#
(defun grandsons (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from grandsons (format t "Usage: (grandsons name)")))
    ;Creates local variable my_grandsons
    (let (my_grandsons)
        ;Sets my_grandsons to the grandchildren of name
        (setf my_grandsons (find_grandchildren name))
        ;Removes granddaughters from my_grandsons
        (if (eq nil my_grandsons) nil
            (setf my_grandsons (malefilter my_grandsons))
        )
    )
)


#|
Author: Carrie Veer
Description: The malefilter function filters a list of names by their gender.
    It does this by calling find_people which turns the names into their 
    respective person structures which contain the gender of each person.  
    Then all of the males' names are added to a different list which is 
    returned from the function.
Parameters: list_names - The list of names to be filtered
|#
(defun malefilter (list_names)
   ;Creates local variable guys
    (let (guys)
        ;Removes all females from the list
        (loop for item in (find_people list_names) while item do
            (if (eq 'female (person-gender item)) nil
                (setf guys (cons (person-name item) guys))
            )
        )
        ;Returns guys
        (return-from malefilter guys)
    )
)


#|
Author: Carrie Veer
Description: The male-ancestors function calls the find_ancestors function 
    which returns a list of all of the ancestors of the person whose name is 
    held in the name variable.  Since the list returned from find_ancestors 
    includes the name of the person whose ancestors are being queried, name is
    removed from the list.  Then the list is filtered using the malefilter 
    function.
Parameters: name - The name of the person whose male ancestors are being queried
|#
(defun male-ancestors (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name)
        (return-from male-ancestors (format t "Usage: (male-ancestors name)"))
    )
    ;Creates local variable my_ancestors
    (let (my_ancestors)
        ;Adds the atom name to the list my_ancestors
        (setf my_ancestors (cons name nil))
        ;Adds the ancestors of name to my_ancestors
        (setf my_ancestors (find_ancestors my_ancestors))
        ;Removes name from my_ancestors
        (if (eq nil my_ancestors) (return-from male-ancestors nil)
            (setf my_ancestors (remove name my_ancestors :test #'equal))
        )
        ;Removes the female ancestors from my_ancestors
        (if (eq nil my_ancestors) nil
            (setf my_ancestors (malefilter my_ancestors))
        )
    )
)


#|
Author: Carrie Veer
Description: The male-cousins function calls the find_cousins function and then
    filters the results using the malefilter function.
Parameters: name - The name of the person whose male cousins are being queried
|#
(defun male-cousins (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name)
        (return-from male-cousins (format t "Usage: (male-cousins name)"))
    )
    ;Creates local variable my_cousins
    (let (my_cousins)
        ;Sets my_cousins to name's cousins
        (setf my_cousins (find_cousins name))
        ;Removes the male cousins from my_cousins
        (if (eq my_cousins nil) nil
            (setf my_cousins (malefilter my_cousins))
        )
    )
)


#|
Author: Carrie Veer
Description: The male-descendants function calls the find_descendants function
    which returns a list of all of the descendants of the person whose name is 
    held in the name variable.  Since the list returned from find_descendants 
    includes the name of the person whose descendants are being queried, name 
    is removed from the list.  Finally, the results are filtered using the 
    malefilter function.
Parameters: name - The name of the person whose male descendants are being
                    queried
|#
(defun male-descendants (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name)
        (return-from male-descendants (format t "Usage: (male-descendants name)"))
    )
    ;Creates the local variable my_descendants
    (let (my_descendants)
        ;Adds the atom name to the list my_descendants
        (setf my_descendants (cons name nil))
        ;Adds the descendants of name to my_descendants
        (setf my_descendants (find_descendants my_descendants))
        ;Removes name from my_descendants
        (if (eq nil my_descendants) (return-from male-descendants nil)
            (setf my_descendants (remove name my_descendants :test #'equal))
        )
        ;Removes female descendants from my_descendants
        (if (eq nil my_descendants) nil
            (setf my_descendants (malefilter my_descendants))
        )
    )
)


#|
Author: Carrie Veer
Description: The mothers function calls the find_parents function and then 
    filters the results using the femalefilter function.
Parameters: name - The name of the person whose mothers are being queried
|#
(defun mothers (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from mothers (format t "Usage: mothers(name)")))
    ;Turns the atom name into a list
    (setf name (cons name nil))
    ;Creates local variable my_mothers
    (let (my_mothers)
        ;Sets my_mothers to the parents of name
        (setf my_mothers (find_parents name))
        ;Removes fathers from my_mothers
        (if (eq my_mothers nil) nil
            (setf my_mothers (femalefilter my_mothers))
        )
    )
)


#|
Author: Carrie Veer
Description: The nephews function calls the find_niblings function and then 
    filters the results using the malefilter function.
Parameters: name - The name of the person whose nephews are being queried
|#
(defun nephews (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from nephews (format t "Usage: (nephews name)")))
    ;Creates local variable my_nephews
    (let (my_nephews)
        ;Sets my_nephews to the list of neices and nephews of name
        (setf my_nephews (find_niblings name))
        ;Removes neices from my_nephews
        (if (eq nil my_nephews) nil
            (setf my_nephews (malefilter my_nephews))
        )
    )
)


#|
Author: Carrie Veer
Description: The nieces function calls the find_niblings function and then 
    filters the results using the femalefilter function.
Parameters: name - The name of the person whose nieces are being queried
|#
(defun nieces (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from nieces (format t "Usage: (nieces name)")))
    ;Creates local variable my_neices
    (let (my_neices)
        ;Sets my_neices to the list of neices and nephews of name
        (setf my_neices (find_niblings name))
        ;Removes nephews from my_neices
        (if (eq nil my_neices) nil
            (setf my_neices (femalefilter my_neices))
        )
    )
)


#|
Author: Carrie Veer
Description: The parents function calls the find_parents function.
Parameters: name - The name of the person whose parents are being queried
|#
(defun parents (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from parents (format t "Usage: (parents name)")))
    ;Turns the atom name into a list
    (setf name (cons name nil))
    ;Returns the parents of name
    (find_parents name)
)


#|
Author: Dr. John Weiss
Editor: Carrie Veer
Description: The read_file function opens the input file using with-open-file 
    and then reads in one line at a time into data.  Each time a line is read,
    the add function is called until the end of file is reached.
Parameters: filename - The name of the file which will be read
|#
(defun read_file (&optional filename)
    ;Overides the system error message when a name is not passed in
    (when (null filename)
        (return-from read_file (format t "Usage: clisp -repl genealogy.lsp filename"))
    )
    ;Opens the file and reads a line into data then calls add to add
    ;the person structure to *database*.  An error message is printed
    ;to screen when the input file cannot be opened
    (with-open-file (fin filename :if-does-not-exist nil)
        (when (null fin) (return-from read_file 
            (format t "Error: cannot open file ~a" filename)))
        (do ((data (read fin nil) (read fin nil)))
            ((null data))
            (add data)
        )
    )
)


#|
Author: Carrie Veer
Description: The siblings function calls the find_siblings function
Parameters: name - The name of the person whose siblings are being queried
|#
(defun siblings (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from siblings (format t "Usage: (siblings name)")))
    ;Turns the atom name into a list
    (setf name (cons name nil))
    ;Returns the siblings of name
    (find_siblings name)
)


#|
Author: Carrie Veer
Description: The sisters function calls the find_siblings function and then 
    uses the femalefilter function to filter the results.
Parameters: name - The name of the person whose sisters are being queried
|#
(defun sisters (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from sisters (format t "Usage: (sisters name)")))
    ;Turns the atom name into a list
    (setf name (cons name nil))
    ;Creates local variable curr_sisters
    (let (curr_sisters)
        ;Sets curr_sisters to the list of siblings of name
        (setf curr_sisters (find_siblings name))
        ;Removes the brothers from curr_sisters
        (if (eq nil curr_sisters) nil
            (setf curr_sisters (femalefilter curr_sisters))
        )
    )
)


#|
Author: Carrie Veer
Description: The sons functions calls the find_kids function and then filters 
    the results using the malefilter function.
Parameters: name - The name of the person whose sons are being queried
|#
(defun sons (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from sons (format t "Usage: (sons name)")))
    ;Turns the atom name into a list
    (setf name (cons name nil))
    ;Creates local variable my_sons
    (let (my_sons)
        ;Sets my_sons to the list of children of name
        (setf my_sons (find_kids name))
        ;Removes daughters from my_sons
        (if (eq my_sons nil) nil
            (setf my_sons (malefilter my_sons))
        )
    )
)


#|
Author: Carrie Veer
Description: The uncles function calls the find_aunt_uncles function and then 
    filters the results using the malefilter function.
Parameters: name - The name of the person whose uncles are being queried
|#
(defun uncles (&optional name)
    ;Overides the system error message when a name is not passed in
    (when (null name) (return-from uncles (format t "Usage: (uncles name)")))
    ;Creates local variable my_uncles
    (let (my_uncles)
        ;Sets my_uncles to the list of aunts and uncles of name
        (setf my_uncles (find_aunt_uncles name))
        ;Removes aunts from my_uncles
        (if (eq my_uncles nil) nil
            (setf my_uncles (malefilter list_people))
        )
    )
)


;Declares the structure of a person strucuture
(defstruct person name gender parents children)
;Calls the read_file function and passes in the first command line argument
(read_file (car *args*))
