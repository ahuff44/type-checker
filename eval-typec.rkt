Assignment: Type Checker
Name: <insert your and your partner's names here>

Instructions: 
* Open this file in DrRacket (or your favorite plain-text editor) and add your answers 
  at the end of each line for each question. If you want to add more explanation or 
  justification, you may add one or more lines under the question.  Remember to add 
  your name as well.  Once complete, submit this to Learning Suite as a plain text file.
* For the questions asking about function correctness, indicate Yes (Y) or No (N)
  depending on whether the function meets ALL specifications.
* For each of the documentation questions, indicate Yes (Y) or No (N) based on whether
  the function has both contract and purpose statements.
* For each of the test case questions, indicate the line number of the corresponding
  test (or tests) using "L" and the number of the line.  For example, a test on
  line 61 of the file would be "L61".  If you don't have a particular test, put "N".
* If you need to add any more explanation of justification, just add it on a line
  underneath the respective question.
  
Function: type-of (and any supporting functions)

General:
 * Is the function correct?
 * Is the function documented correctly (i.e. contract and purpose statement)?
 * Does type-of allow through runtime errors?

Expression: num
 * Is there an example of type-of on a correct num expression?

Expression: true
 * Is there an example of type-of on a correct true expression?

Expression: false
 * Is there an example of type-of on a correct false expression?

Expression: +
 * Is there an example of type-of on a correct + expression?
 * Is there a test case for the lhs not being a number?
 * Is there a test case for the rhs not being a number?

Expression: -
 * Is there an example of type-of on a correct - expression?
 * Is there a test case for the lhs not being a number?
 * Is there a test case for the rhs not being a number?

Expression: *
 * Is there an example of type-of on a correct * expression?
 * Is there a test case for the lhs not being a number?
 * Is there a test case for the rhs not being a number?

Expression: iszero
 * Is there an example of type-of on a correct iszero expression?
 * Is there a test case for the input not being a number?

Expression: bif
 * Is there an example of type-of on a correct bif expression?
 * Is there a test case for a non-boolean condition error?
 * Is there a test case for a mismatch error?

Expression: id
 * Is there an example of type-of on a correct id expression?
 * Is there a test case for a unbound identifier?

Expression: with
 * Is there an example of type-of on a correct with expression?
 * Is there a test case for misuse of the identifier in the body?

Expression: fun
 * Is there an example of type-of on a correct fun expression?
 * Is there a test case for misuse of the formal parameter in the body?
 * Is there a test case for a return-type mismatch error?

Expression: app
 * Is there an example of type-of on a correct app expression?
 * Is there a test case for an operator that isn't a function?
 * Is there a test case for a wrong argument type?

Expression: nempty
 * Is there an example of type-of on a correct nempty expression?

Expression: ncons
 * Is there an example of type-of on a correct ncons expression?
 * Is there a test case for the first parameter not being a number?
 * Is there a test case for the second parameter not being an nlist?

Expression: nempty?
 * Is there an example of type-of on a correct nempty? expression?
 * Is there a test case for the input not being an nlist?

Expression: nfirst
 * Is there an example of type-of on a correct nfirst expression?
 * Is there a test case for the input not being an nlist?

Expression: nrest
 * Is there an example of type-of on a correct nrest expression?
 * Is there a test case for the input not being an nlist?


