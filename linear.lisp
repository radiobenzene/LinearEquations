"Program to solve linear equations"

"Parser function for linear equation"
(defun Parser(L)
        (cond
        ((eq L nil) nil)
        ((eq (car L) '+) (InternalParser '+ 1 nil 0 (cons '0 L)))
        ((eq (car L) '-) (InternalParser '+ 1 nil 0 (cons '0 L)))
        (T (InternalParser '+ 1 nil 0 L))
        )
)


"InternalParser Function for Parser function"
(defun InternalParser(sign number equalSign symbol L)
        (cond
        ((eq L nil) (cons (cons (getSignedNumber sign equalSign number) (cons symbol nil)) nil))
        ((numberp (car L)) (InternalParser sign (car L) equalSign symbol (cdr L)))
        ((eq (car L) '+) (cons (cons (getSignedNumber sign equalSign number) (cons symbol nil)) (InternalParser '+ 1 equalSign 0 (cdr L))))
        ((eq (car L) '-) (cons (cons (getSignedNumber sign equalSign number) (cons symbol nil)) (InternalParser '- 1 equalSign 0 (cdr L))))
        ((eq (car L) '=) (cons (cons (getSignedNumber sign equalSign number) (cons symbol nil)) (InternalParser '+ 1 T 0 (cdr L))))
        (T (InternalParser sign number equalSign 1 (cdr L))) ; Found variable Y
        )
)


"getSignedNumber : Checks for any sign in linear equation"
(defun getSignedNumber(sign equalSign number)
        (cond
        ((eq sign '-) (cond ((eq equalSign T) number) (T (- 0 number))))
        (T (cond ((eq equalSign T) (- 0 number)) (T number)))
        )
)


"Function to get Number with number 1"
(defun GetVariableOne (L)
        (caar L)
)

"Function to get Number with 0"
(defun GetVariableZero (L)
        (caadr L)
)

"Function to divide variables"
(defun IsolateVariable (L)
        (cond 
            ((eq (GetVariableZero L) 0)
                    (cond 
                    ((eq (GetVariableOne L) 0) (format t "Infinite Solutions"))
                    )
                )
            ((eq (GetVariableOne L) 0) (format t "No Solution"))
            (T(* -1 (/ (GetVariableZero L) (GetVariableOne L))))
        )
)

"Function to write the final answer"
(defun WriteAnswer (L)
        (format t "Y = ~A" (IsolateVariable L))
)

"Function to add all numbers with Zero"
(defun CollectZero (L)
        (cond
        ((eq L NIL) 0)
        ((eq (GetSecondElement L) 1) (CollectZero (cdr L)))
        ((eq (GetSecondElement L) 0) (+ (GetElement L) (CollectZero (cdr L))))
        (T (+ CollectZero (cdr L)))
        )
)

"Function to add numbers with One"
(defun CollectWithOne (L)
        (cond
        ((eq L NIL) 0)
        ((eq (GetSecondElement L) 0) (CollectWithOne (cdr L)))
        ((eq (GetSecondElement L) 1) (+ (GetElement L) (CollectWithOne (cdr L))))
        (T (+ CollectWithOne (cdr L)))
        )
)

"Function to get the second element"
(defun GetSecondElement (L)
        (cadar L)
)

"Function to get the first element"
(defun GetElement (L)
        (caar L)
)

"Function to make list with One"
(defun MakeListWithOne (L)
        (MakeList (CollectWithOne L) 1)
)

"Function to make list with Zero"
(defun MakeListWithZero (L)
        (MakeList (CollectZero L) 0)
)

"Function to combine both lists"
(defun CombineLists (L)
        (MakeList (MakeListWithOne L) (MakeListWithZero L))
)

"Function to MakeList"
(defun MakeList(x y) (cons x (cons y nil)))

"Function To Build Answer"
(defun BuildAnswer (L)
        (WriteAnswer(CombineLists (Parser L)))
)

"Main Function"
(defun LinearEquation (L)
        (BuildAnswer L)
)
