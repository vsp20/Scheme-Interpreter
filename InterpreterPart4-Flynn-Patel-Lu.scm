; Tommy Lu/Vishal Patel/Jamie Flynn
; twl23/vsp20/gjf20
; If you are using racket instead of scheme, uncomment these two lines, comment the (load "functionParser.scm") and uncomment the (require "functionParser.scm")
; #lang racket
 (require "classParser.scm")
;(load "classParser.scm")
;(require racket/trace)

; An interpreter for the simple language that uses call/cc for the continuations.
;(define call/cc call-with-current-continuation)

; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value
 
; The main function.  Calls parser to get the parse tree and interprets the main with an environment that is created from global variables and functions.
(define interpret
  (lambda (file)
    (scheme->language
     (call/cc
      (lambda (return)
        (interpret-statement-list (body (func-lookup 'main (interpret-statement-list-raw (parser file) (newenvironment) (lambda (v env) (myerror "Uncaught exception thrown")))))
                                  (push-frame (interpret-statement-list-raw (parser file) (newenvironment) (lambda (v env) (myerror "Uncaught exception thrown"))))
                                  (lambda (v) (return v))
                                  (lambda (env) (myerror "Break used outside of loop"))
                                  (lambda (env) (myerror "Continue used outside of loop"))
                                  (lambda (v env) (myerror "Uncaught exception thrown"))))))))

; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list-raw
  (lambda (statement-list environment throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list-raw (cdr statement-list) (interpret-statement-raw (car statement-list) environment throw) throw))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement-raw
  (lambda (statement environment throw)
    (cond
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment))
      ((eq? 'function (statement-type statement)) (interpret-function statement environment))
      (else (myerror "Illegal Statement:" (statement-type statement))))))


; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw) return break continue throw))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return break continue throw))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment throw))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw))
      ((eq? 'continue (statement-type statement)) (continue environment))
      ((eq? 'break (statement-type statement)) (break environment))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw))
      ((eq? 'function (statement-type statement)) (interpret-function statement environment))
      ((eq? 'funcall (statement-type statement)) (interpret-funcall-state statement environment throw))
      (else (myerror "Unknown statement:" (statement-type statement))))))


; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return break continue throw)
    (return (eval-expression (get-expr statement) environment throw))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment throw)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment throw) environment)
        (insert (get-declare-var statement) 'novalue environment))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment throw)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment throw) environment)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.

(define interpret-if
  (lambda (statement environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) environment throw) (interpret-statement (get-then statement) environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw))
      (else environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment throw)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw)
    (pop-frame (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break environment))
                                         (lambda (env) (continue environment))
                                         (lambda (v env) (throw v environment))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment throw) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex environment)
                                                 return 
                                                 (lambda (env2) (break environment)) 
                                                 (lambda (env2) (continue environment)) 
                                                 (lambda (v env2) (throw v environment)))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

;Inserts function into current frame to be called
(define interpret-function
  (lambda (statement environment)
    (insert-func (func-name statement) (list (func-arg-list statement) (func-body statement)) environment)))


;Sets up the function by passing the actual parameters to formal and creating a function scope.
;-value returns values
;-state returns states which is the state that was before the function call, which is updated through boxes.
(define interpret-funcall-value
  (lambda (statement environment throw)
    (call/cc
      (lambda (return)
        (interpret-statement-list (body (func-lookup (func-name statement) environment))
                                  (add-binding
                                   (param-list (func-lookup (func-name statement) environment))
                                   (func-actual-param statement) environment
                                   (push-frame (get-func-environment (func-name statement) environment)) throw)
                                  (lambda (v) (return v)) 
                                  (lambda (env) (myerror "Break used outside of loop"))
                                  (lambda (env) (myerror "Continue used outside of loop"))
                                  throw)))))

(define interpret-funcall-state
  (lambda (statement environment throw)
    (call/cc
      (lambda (return)
        (if (null? (interpret-statement-list (body (func-lookup (func-name statement) environment))
                                  (add-binding
                                   (param-list (func-lookup (func-name statement) environment))
                                   (func-actual-param statement) environment
                                   (push-frame (get-func-environment (func-name statement) environment)) throw)
                                  (lambda (v) (return environment)) 
                                  (lambda (env) (myerror "Break used outside of loop"))
                                  (lambda (env) (myerror "Continue used outside of loop"))
                                  throw))
            environment
            environment)))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables. 
(define eval-expression
  (lambda (expr environment throw)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((and (list? expr) (eq? 'funcall (operator expr))) (interpret-funcall-value expr environment throw))
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr environment throw)))))

; Evaluate a binary (or unary) operator. I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.
(define eval-operator
  (lambda (expr environment throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment throw)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment throw) environment throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation. Also checks for functions
(define eval-binary-op2
  (lambda (expr op1value environment throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment throw)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

; these helper functions define function related statements
(define func-name cadr)
(define func-arg-list caddr)
(define func-body cadddr)
(define func-actual-param cddr)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is four lists
; the format is ( (variable names) (variable values) (function names) (function values) (class names) (class closures) ) where a function value is
;( (function parameter names - we need these to add the params into the new environment layer) (the function body statement list) ) 
(define newframe
  (lambda ()
    '(() () () () () ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

;------------------------------------------
; Variable portion of Environment functions
;------------------------------------------

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (unbox (get-value (indexof var (variables frame)) (store frame))))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (box(scheme->language val)) (store frame)) (functions frame) (func-info frame))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)) (functions frame) (func-info frame))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((and (eq? var (car varlist)) (void? (set-box! (car vallist) (scheme->language val)))) vallist)
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



;------------------------------------------
; Function portion of Environment functions
;------------------------------------------


;Returns function environment, that is the environment viewed by function
(define add-binding
  (lambda (formal actual environment functionenvironment throw)
    (cond
      ((and (null? formal) (null? actual)) functionenvironment)
      ((null? formal) myerror "Too many parameters for function")
      ((null? actual) myerror "Not enough parameters for function")
      (else (add-binding (cdr formal) (cdr actual) environment (insert (car formal) (eval-expression (car actual) environment throw) functionenvironment) throw)))))

; returns the environment specific to the given function

(define get-func-environment
  (lambda (func environment)
    (if (func-exists-in-list? func (functions (topframe environment)))
        environment
        (get-func-environment func (cdr environment)))))

; does a function exist in the environment?
(define func-exists?
  (lambda (func environment)
    (cond
      ((null? environment) #f)
      ((func-exists-in-list? func (functions (topframe environment))) #t)
      (else (func-exists? func (remainingframes environment))))))


; does a function exist in a list?
(define func-exists-in-list?
  (lambda (func l)
    (cond
      ((null? l) #f)
      ((eq? func (car l)) #t)
      (else (func-exists-in-list? func (cdr l))))))

; Looks up function info in the environment.
(define func-lookup
  (lambda (func environment)
    (lookup-function func environment)))
  
; A helper function that does the lookup.  Returns an error if the function does not have info associated with it
(define lookup-function
  (lambda (func environment)
    (let ((info (lookup-func-in-env func environment)))
      (if (null? info)
          (myerror "error: function without parameters or body:" func) ;this may be a source of error (possible debug point)
          info))))

; Return the info bound to a function in the environment
(define lookup-func-in-env
  (lambda (func environment)
    (cond
      ((null? environment) (myerror "error: undefined function" func))
      ((func-exists-in-list? func (functions (topframe environment))) (lookup-func-in-frame func (topframe environment)))
      (else (lookup-func-in-env func (cdr environment))))))

; Return the info bound to a function in the frame
(define lookup-func-in-frame
  (lambda (func frame)
    (cond
      ((not (func-exists-in-list? func (functions frame))) (myerror "error: undefined function" func))
      (else (get-info (indexof-func func (functions frame)) (func-info frame))))))

; Get the location of a function name in a list of names
(define indexof-func
  (lambda (func l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? func (car l)) 0)
      (else (+ 1 (indexof-func func (cdr l)))))))

; Get the function info stored at a given index in the list
(define get-info
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-info (- n 1) (cdr l))))))

; Adds a new function/function-info binding pair into the environment.  Gives an error if the function already exists in this frame.

(define insert-func
  (lambda (func info environment)
    (if (func-exists-in-list? func (functions (car environment)))
        (myerror "error: function is being re-declared:" func)
        (cons (add-func-to-frame func info (car environment)) (cdr environment)))))

; Add a new variable/value pair to the frame.
(define add-func-to-frame
  (lambda (func info frame)
    (list (variables frame) (store frame) (cons func (functions frame)) (cons info (func-info frame)) (envframe_class_name_list frame) (envframe_class_closure_list frame))))

; returns the list of functions from a frame
(define functions
  (lambda (frame)
    (caddr frame)))

; returns the function info from a frame
(define func-info
  (lambda (frame)
    (car (cdddr frame))))

; returns the function parameter list from the function info - info formatted: ((parameter list) (statement list for the function body))
(define param-list
  (lambda (info)
    (car info)))

; returns the body of the function, stored as the raw parse tree statement list 
(define body
  (lambda (info)
    (cadr info)))

;----------------MARK---------------------
; Class portion of Environment functions
;------------------------------------------

(define envframe_class_name_list ;caddddr
  (lambda (lis)
    (cadr (cdddr lis))))

(define envframe_class_closure_list ; cadddddr
  (lambda (lis)
    (caddr (cdddr lis))))

;need a get closure func given name and  -> list is empty? error, name match? return the closure, otherwise pass cdr's and recur
(define lookup_class_closure ;returns the closure of a class given the environment and the class name
  (lambda (env name)
    (cond
      ((null? env) (myerror "class is not in current environment"))
      ((is_in_frame? name (topframe env)) (lookup_class_in_frame name (topframe env)))
      (else (lookup_class_closure (popframe env) name)))))

;returns #t if the class name is in the frame
(define is_in_frame?
  (lambda (class frame)
    (cond
      ((null? frame) #f)
      (else (is_in_list? class (envframe_class_name_list frame))))))
    
;returns #t if the class name is in the list of class names
(define is_in_list?
  (lambda (class lis)
    (cond
      ((null? lis) #f)
      ((eq? (car lis) class) #t)
      (else (is_in_list? class (cdr lis))))))

;returns the class closure in the frame corresponding to the passed class name
(define lookup_class_in_frame
  (lambda (class frame)
    (cond
      (lookup_closure name () ()))))

;returns the closure in closlist associated with name
(define lookup_closure
  (lambda (name namelist closlist)
    (cond
      ((and (null? namelist) (null? closlist)) (myerror "class not in provided namelist" namelist))
      ((null? namelist) (myerror "mismatch number of names to closures: not enough names. Remaining closures: " closlist))
      ((null? closlist) (myerror "mismatch number of names to closures: not enough closures. Remaining names: " namelist))
      ((eq? name (car namelist)) (car closlist))
      (else (lookup_closure name (cdr namelist) (cdr closlist))))))

;add a class name/closure binding to the environment
(define add_class_binding
  (lambda (name closure env)
    (cond
      ((null? env) (myerror "Received null environment when adding class binding"))
      ((is_in_frame name (topframe env)) (myerror "class already in top frame of the environment")); allows for same name classes at different scope levels
      (else (cons (add_class_binding_to_frame name closure (topframe env)) (cdr env))))))

(define add_class_binding_to_frame ;takes a class name and closure and the environment frame - returns the frame with the class binding added
  (lambda (name closure frame)
    ((null? frame) (myerror "Recieved null frame when adding class binding"))
    (else (list ((variables frame) (store frame) (functions frame) (func-info frame) (cons name (envframe_class_name_list frame)() (cons closure (envframe_class_closure_list frame))))))))


;------------- MARK--------
;   Class Closure Methods
;--------------------------

;class closure format: ('parent_class (instance field names) (class method names) (class method closures))

;makes a class closure



;getter functions
;----------------

(define class_closure_parent_class car)
(define class_closure_instance_field_list cadr)
(define class_closure_method_names_list caddr)
(define class_closure_method_closures_list cadddr)

(define class_closure_add_fields ;takes a list of arguments and a class name - returns the list of argument appended with the instance_field_list of the super classes
  (lambda (arglist class)
    (cond
      ((null? class) '())
      (else (class_closure_add_fields (append list (class_closure_instance_field_list (lookup_class_closure class)) (class_closure_parent_class (lookup_class_closure class))))))))

(define class_closure_get_index ;takes a class closure and a field name and returns the index of the field (distance from end of list)
  (lambda (clos field)
    (get_index (class_closure_instance_field_list clos) field)))

(define get_index ;takes an instance field list and a field name and returns the index of the field (distance from end of list)
  (lambda (lis name)
    (cond
      ((null? lis) myerror"error: field not in field list")
      ((eq? (car lis) name) (count_to_end (cdr lis) 0))
      (else (get_index (cdr lis) name)))))

(define count_to_end ;takes a list and continueation and returns the list's length
  (lambda (lis continue)
    (cond
      ((null? lis) continue)
      (else (count_to_end (cdr lis) (+ 1 continue))))))

;------------- MARK--------
; Instance Closure Methods
;--------------------------

;the instance closure looks like: ('runtime_type (instance values))

;make an instance closure



;getter functions
;------------------
(define instance_closure_runtime_type car)
(define instance_closure_instance_values_list cadr)


;------------------------
;    System Helpers
;------------------------


; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

;check prints the parse tree to the console - for programmer informational purposes 
(define check
  (lambda (filename)
    (parser filename)))