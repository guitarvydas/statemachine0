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
    machinePushNew
  @optional-initially
  @states
  SYMBOL/end SYMBOL/machine
    machinePopClose

= optional-initially
  [ ?SYMBOL/initially
     SYMBOL/initially
     exprPushNewInitially
     @statemachine0Statements
     machineSetInitially
     exprPop
     SYMBOL/end SYMBOL/initially
  | *
  ]

= states
  {[ ?SYMBOL/state 
     exprPushNewState 
     @state 
     machineAppendState 
     exprPop
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
  codePushNewSend
  @statemachine0Expr
  codeAppendExpr

= optionalParameters
  [ ?'('   '(' @parameters ')'
  | *
  ]

= parameters
  {[ ?'$'   '$' @statemachine0expr codeAppendExpr
   | ?')' >
   | ?SYMBOL @statemachine0expr codeAppendExpr
  ]}

= statemachine0Call
  SYMBOL
  codePushNewCall
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

(defclass machine ()
  ((name :accessor name)
   (intially-code :accessor initially-code :initform nil)
   (states :accessor states :initform nil)))

(defclass statemachine0Parser (pasm:parser)
  ((exprStack :accessor exprStack)
   (machineStack :accessor machineStack)
   (codeStack :accessor codeStack)))

(defmethod initially ((self statemachine0Parser) token-list)
  (setf (exprStack self) nil)
  (setf (machineStack self) nil)
  (setf (codeStack self) nil)
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

(defmethod machinePushNew ((self statemachine0Parser))
  (let ((m (make-instance 'machine)))
    (setf (name m) (scanner:token-text (pasm:accepted-token self)))
    (push m (machineStack self))))

(defmethod machinePopClose ((self statemachine0Parser))
  (let ((m (pop (machineStack self))))
    (pasm:emit-string self "~&(defmachine ~a () ~a)~%" (name m) (pop (codeStack self)))))

(defmethod codePushNewSend ((self statemachine0Parser))
  (push '(send) (codeStack self)))

(defmethod codePushNewCall ((self statemachine0Parser))
  (push (list 'call (scanner:token-text (pasm:accepted-token self))) (codeStack self)))

(defmethod codeAppendExpr ((self statemachine0Parser))
  (setf (first (codeStack self)) (append (pop (codeStack self)) (list (pop (exprStack self))))))

;; end mechanisms

(defun string-append (s1 s2)
  (format nil "~a~a" s1 s2))
