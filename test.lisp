(in-package :statemachine0)

#| 

in this state language,

each state machine is an independent closure
each state machine has exactly one input
each state machine has exactly one output 
(later, we will develop machines with multiple inputs and multiple outputs)

a machine consists of an initially block followed by 1 or more state declarations

each state declaration is
id ':' {on-clauses}*

an on-clause is 
'on' 'in' ':' {statement}+
(I use the name "event" to mean an on-id (plus incoming data))
(in this v0 state language, there is exactly one input event called "in")

a block of statements can consist of
1) a 'send'
2) a function call
(nothing else!)

a send is
'send' {expr}+

a function call is
<symbol>[parameters]

parameters (optional) is
'(' {expr}+ ')'

an expr (expression) is

'{' <anything> '}'
or
'$'

(expressions are transpiled by the expr-dsl language transpiler)
(expressions can be nested) (but, braces must come in matching pairs '{' ... '{' ... '}' ... '}')
($ means the data associated with the input event)
(actuals are separated by whitespace, commas and semi-colons and spaces and newlines are whitespace)

a pipeline is specified as
pipeline
  id | id | id ...
end pipeline

(pipes may consist of unbounded queues, see other versions of the state language for a solution to the problem of exploding/infinite queues using protocols.  Unbounded queues work OK if you have a well-constrained problem and/or enough memory.)

|#

(defparameter *dsl-string0*
  "
machine producer
  initially
    send {t}
  end initially
end machine

machine consumer
  state idle: on in: emit($)
end machine

pipeline
  producer | consumer
end pipeline
")

(defun test ()
  (let ((p (make-instance 'statemachine0parser)))
    (let ((pasm:*pasm-tracing* nil))
      (let ((r (pasm:transpile p *asm* *dsl-string0* 'statemachine0)))
	(format *standard-output* "~&result0=~a~%" r)))))

