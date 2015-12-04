Assignment: Type Checker
Name: Andrew Huff and Adrian Foong

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
 * Is the function correct? Yes
 * Is the function documented correctly (i.e. contract and purpose statement)? Yes
 * Does type-of allow through runtime errors? Yes

Expression: num
 * Is there an example of type-of on a correct num expression? L358

Expression: true
 * Is there an example of type-of on a correct true expression? L359

Expression: false
 * Is there an example of type-of on a correct false expression? L360

Expression: +
 * Is there an example of type-of on a correct + expression? L361
 * Is there a test case for the lhs not being a number? L414
 * Is there a test case for the rhs not being a number L? L413

Expression: -
 * Is there an example of type-of on a correct - expression? L363
 * Is there a test case for the lhs not being a number? L418
 * Is there a test case for the rhs not being a number? L417

Expression: *
 * Is there an example of type-of on a correct * expression? L365
 * Is there a test case for the lhs not being a number? L422
 * Is there a test case for the rhs not being a number? L421

Expression: iszero
 * Is there an example of type-of on a correct iszero expression? L367
 * Is there a test case for the input not being a number? L426

Expression: bif
 * Is there an example of type-of on a correct bif expression? L370
 * Is there a test case for a non-boolean condition error? L431
 * Is there a test case for a mismatch error? L436

Expression: id
 * Is there an example of type-of on a correct id expression? L372
 * Is there a test case for a unbound identifier? L411

Expression: with
 * Is there an example of type-of on a correct with expression? L373
 * Is there a test case for misuse of the identifier in the body? L455

Expression: fun
 * Is there an example of type-of on a correct fun expression? L375
 * Is there a test case for misuse of the formal parameter in the body? L465
 * Is there a test case for a return-type mismatch error? L467

Expression: app
 * Is there an example of type-of on a correct app expression? L382
 * Is there a test case for an operator that isn't a function? L502
 * Is there a test case for a wrong argument type? L508

Expression: nempty
 * Is there an example of type-of on a correct nempty expression? L389

Expression: ncons
 * Is there an example of type-of on a correct ncons expression? L390
 * Is there a test case for the first parameter not being a number? L529
 * Is there a test case for the second parameter not being an nlist? L534

Expression: nempty?
 * Is there an example of type-of on a correct nempty? expression? L392
 * Is there a test case for the input not being an nlist? L548

Expression: nfirst
 * Is there an example of type-of on a correct nfirst expression? L394
 * Is there a test case for the input not being an nlist? L539

Expression: nrest
 * Is there an example of type-of on a correct nrest expression? L396
 * Is there a test case for the input not being an nlist? L544
