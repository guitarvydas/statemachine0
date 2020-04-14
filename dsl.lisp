(in-package :statemachine0)

(defparameter *asm* "
= rmSpaces
  [ ?SPACE | ?COMMENT | * . ]

= statemachine0
  ~rmSpaces
  {[ ?SYMBOL/machine 
     @machine
   | * >
  ]}
  @pipeline

= machineName
  SYMBOL

= machine
  SYMBOL/machine
  @machineName
  @optional-initially
  @states
  SYMBOL/end SYMBOL/machine


= optional-initially
  [ ?SYMBOL/initially
     SYMBOL/initially
     @statemachine0Statements
     SYMBOL/end SYMBOL/initially
  | *
  ]

= states
  {[ ?SYMBOL/state @state
   | * >
  ]}

= state
  SYMBOL/state
  @stateName
  ':'
  @events

= stateName
  SYMBOL

= eventName
  SYMBOL/in  %% in v0 state machines, all input events are called 'in' (hard-wired name)

= events
  {[ ?SYMBOL/on 
    SYMBOL/on
    @eventName 
    ':' 
    @statemachine0Statements
   | * >
  ]}

= statemachine0Statements
  {[ ?SYMBOL/send 
     @statemachine0SendStatement
   | ?SYMBOL/end >
   | ?SYMBOL @statemachine0Call
   | * >
  ]}

= statemachine0SendStatement
  SYMBOL/send
  @statemachine0Expr

= optionalParameters
  [ ?'('   '(' @parameters ')'
  | *
  ]

= parameters
  {[ ?'$'   '$' @statemachine0expr
   | ?')' >
   | ?SYMBOL @statemachine0expr
  ]}

= statemachine0Call
  SYMBOL
  @optionalParameters

= pipeline
    SYMBOL 
    @morePipes

= morePipes
  {[?'|' 
     '|'
     SYMBOL 
   | * >
  ]}

= statemachine0expr
  [ ?'$' @statemachine0DollarExpr 
  | ?'{' @statemachine0GeneralExpr
  | *
  ]

= statemachine0DollarExpr
  '$' exprPushDollarRef

= statemachine0GeneralExpr
  '{' 
  exprPushEmpty
  {[ ?'{' @statemachine0GeneralExpr
   | ?'}' >
   | * . exprAppendText
  ]}
  '}'
  exprEmit
  exprPop
")

(defclass statemachine0Parser (pasm:parser)
  ((exprStack :accessor exprStack)))

(defmethod initially ((self statemachine0Parser) token-list)
  (setf (exprStack self) nil)
  (call-next-method))

;; mechanisms
(defmethod exprPop ((self statemachine0Parser)) (pop (exprStack self)))
(defmethod exprPushEmpty ((self statemachine0Parser)) (push "" (exprStack self)))
(defmethod exprAppendText ((self statemachine0Parser))
  (let ((topExprString (pop (exprStack self))))
    (let ((new-text (scanner:token-text (pasm:accepted-token self))))
      (push (string-append topExprString new-text) (exprStack self)))))
(defmethod exprPushDollarRef ((self statemachine0Parser)) (push "(%get-data self)" (exprStack self)))
(defmethod exprEmit ((self statemachine0Parser)) (pasm:emit-string self (first (exprStack self))))
(defmethod errorNoExpr ((self statemachine0Parser))
  (ptrace self)
  (pasm:pasm-parse-error self "expecting an EXPR"))
;; end mechanisms

(defun string-append (s1 s2)
  (format nil "~a~a" s1 s2))
