A Tutorial on the Construction of a Parser for the WHILE Programming Language
=======================================================================================

The While language is a small imperative programming language. It is a very simple language and the programmes it constructs are formed from variable assignments, sequential composition, if statements, skip statements, braced statements and, while loops. Variable assignments involve a variable being given a value. In the while language, all values must be over a specified signature or evaluate to the same type. Sequential composition is where the order of evaluation is sequential and in the while language, this is denoted by the (;) symbol. Therefore the expression before the (;) symbol must be evaluated before the expression(s) after it. An if statement is where the control flow of a program is dependent on a Boolean value. If the Boolean value is true, the program will evaluate the next lines of code until the if statement finishes; otherwise it will execute the code inside the else block. A skip statement is a line where nothing is executed. A braced statement is a series of 1 or more statements that are grouped together by brace brackets. Finally a while statement is where a section of code will loop sequentially whilst a Boolean value remains true.

The grammar of the WHILE language
----------------------------------
A while language program consists of a single statement which may be:

A sequence of one statement followed by another separated by a semi-colon and a line break.

>  <statement> ::= <statement>; cr <statement>

A statement grouped by braces

>          | { <statement> }

Allows the assignments of values to variables

>          | LET <variable> '=' <expression>

A simple statement where nothing is executed.

>          | SKIP

Expression of the If statement

>          | IF <boolean> THEN <statement> ELSE <statement>

Expression of the While loop

>          | WHILE <boolean> DO <statement>

For simplicity's sake we will define the variables of the language to be only single capital letters.

> <variable> ::= A | B | .. | Z

In the while language, expressions must all be over the same signature. For this tutorial, the signature will be that of integer arithmetic and therefore expressions will only involve integers and variables over the operations +,-,*,/. To ensure the precedence of mathematical operations is maintained, expressions will be sub-divided into terms which are further sub-divided into facts.

The expression consists of an optional + or - sign in front of 1 term, followed by 0 or more terms preceded by a + or - sign. The optional sign at the beginning indicates the sign of the first number whilst the following + or - symbols denote addition and subtraction. By placing the addition and subtraction operations at the highest level, they will be consumed last by the parser.

> <expression> ::= [ + | - ] <term> (( + | -) <term>)*

The term simply consists of a single fact or many under either the division or multiplication operation. This makes multiplication and division the second last operations to occur.

> <term> ::= <fact> | <fact> ( (* | \) <fact>)*

The fact consists of either variables, numbers or expressions inside parentheses. This sets expressions inside parenthesis as having the highest precedence.

> <fact> ::= <variable> | <number> | '(' <expression> ')'

Numbers are parsed as a series of digits

> <number> ::= <digit> <digit>*

> <digit> ::= 0 | 1 | .. | 9

The boolean values are all inputs that can be evaluated to either true or false. Therefore they may involve: true or false, comparisons between two expressions, or boolean operations between boolean terms.

The boolean values may be prefaced with a NOT keyword but must contain at least 1 boolfact. This is followed by 0 or more boolean operation values.

> <boolean> ::= [ NOT ] <boolfact> (<boolop>)*

The boolfact variable is either the TRUE value, FALSE value, a boolean expression grouped by parentheses or a comparison between two algebraic expressions.

> <boolfact> ::= TRUE | FALSE | '(' <boolexpr> ')' | <expression> <comparator> <expression>

In this grammar, the boolean operations will involve only the binary operations AND and OR.

> <boolop> ::= AND <boolfact> | OR <boolfact>

Finally the comparison operations involve evaluating two expressions to either true or false. These include the less than (<), less than or equal to (<=), equal to (==), not equal to (<>), greater than (>) and the greater than or equal to (>=) comparisons.

> <comp> ::= < | <= | == | > | >= | <>

> import Data.List (intercalate)
> import Data.Char
> import Data.Maybe

Parser Combinators
--------------------

We use parser combinators from the Megaparsec library. Parser combinators are functions that return parsers. They can be used to construct more complicated parsers.

Parser a is simply a datatype that is equivalent to String -> [(String,a)], in other words a function that takes an input stream and produces a list of pairs of the stream with tokens consumed and values of type a.

Produce is a simple function which takes an input stream and a value we want to produce. It returns a parser which takes the input stream and doesn't consume any of it, but pairs it with the value we want to produce.

> produce :: a -> Parser a

Failure is a parser that instantly fails on any input.

> failure :: Parser a

Satisfy is the main building block on which all parsers are built. It takes a function which checks if the first character in an input satisfies a predicate. If the function is satisfied on char c, the produce c parser is returned, otherwise the failure parser is returned.

> satisfy :: (Char -> Bool) -> Parser Char

The fmap parser combinator <$> maps a single function onto an existing parser to create a parser for the result of that function. When given a function f' and a parser p, the input stream is parsed by p resulting in a list of (Strings,Parsed Values). The function f' is then applied to those parsed values.

> (<$>) :: Functor f => (a -> b) -> f a -> f b

> (<$) :: Functor f => a -> f b -> f a

There are two functions of the Applicative class, the pure function and the applied function <*>.

The pure function takes any value and returns the produce parser for that value.

> pure :: Applicative f => a -> f a

The applied parser combinator applies a parser for a function to a parser of a value and returns a parser for the result of applying the function to the value. When given a parser for a function pf and a parser for a value px, the input stream is first parsed by pf giving pairs of resultant tokens and functions. The tokens are parsed by px to give further tokens and x value pairs. The resultant output are pairs of the tokens produced by px and values returned by applying the function to the x value.

> (<*>) :: Applicative f => f (a -> b) -> f a -> f b

The missing < or > symbols are used where we don't use the parsed value from that side of the combinator. I.e string tok <* whitespace shows that we do not use the result of the whitespace parser.

> (<*) :: Applicative f => f a -> f b -> f a

> (*>) :: Applicative f => f a -> f b -> f b

There are two functions of the Alternative class, the empty function and the choice function <|>.

The empty function returns the failure parser which consumes no tokens from the input stream.

> empty :: Applicative f => f a

The choice parser combinator can be thought of as an expression of the | symbol in a grammar. This function takes two parsers for the same type of value, px and py. The function first attempts to run the px parser on the input. If it succeeds it returns the result of that parser. If it fails and consumes no input it runs the py parser on the same input and produces the result of that.

> (<|>) :: Alternative f => f a -> f a -> f a

Some is a function that takes a Parser for a single value, px, and produces a Parser for one or more of those values, pxs. As such pxs is a parser for the + symbol in BNF notation. This is defined by mapping the (:) function (which adds a value to the front of a list of values) over px using <$>. This returns a Parser for a function that takes a part of a list and returns a list with something added on, pf. This is applied to the result of many px, a function that takes a parser for a single value and returns a parser for 0 or more of those values.

> some :: Alternate f => f a -> f [a]

The many function is an expression of the * symbol in a grammar. It is used to parse 0 or more occurrences of an element. It takes a Parser of an element, px, and uses the <|> combinator to return either the parser returned by the some function given px or it uses produce [] to return the parser for the empty list.

> many :: Alternate f => f a -> f [a]

The sepBy1 function is a special form of the some function. This function produces a parser that parses 0 or more occurrences of a value separated by a separator value.

> sepBy1 :: Alternative f => f a -> f sep -> f [a]

The sepBy function is similar to the many function but parses 0 or more occurrences of an element x separated by a separator value sep. It does this by taking a parser for the value, px, and a parser for the separator, ps. Like many it uses the <|> combinator to return either the parser returned by the sepBy1 function given px and ps or it uses produce [] to return the parser for the empty list.

> sepBy :: Alternative f => f a -> f sep -> f [a]

The oneOf function takes a list of values and is satisfied if the token consumed is one of those values.

> oneOf :: [a] -> Parser a

The noneOf function does the opposite and is only satisfied if the token consumed is not in the list of values.

> noneOf :: [a] -> Parser a

Constructing the Parser
------------------------

Firstly we need access to the parser combinators outlined above. To do this the MegaParsec libraries and some other features must be imported.

> {-# LANGUAGE StandaloneDeriving #-}
> module WhileParser where
> import Text.Megaparsec
> import Text.Megaparsec.String

In order to construct the parser, a data type must be constructed for each variable in the above grammar.

First a datatype for the statement variable. A Statement can be constructed with:
 a list of 0 or more statements called a Sequence [SEQ], a statement wrapped with parentheses

\begin{code}
 data Statement = SEQ [Statement]
              -- A list of 0 or more Statements
               | Braces Statement
              -- A statement wrapped by braces
               | LET Char Expression
              -- A variable assignment constructed with a single character and an expression
               | SKIP
              -- A skip statement
               | IF BoolExpr Statement Statement
              -- An if statement constructed by a boolean value and two more statements
               | WHILE BoolExpr Statement
              -- A while loop constructed by a boolean value and a statement
\end{code}

The Parity data type corresponds to whether an expression is positive or negative and therefore can be positive or negative.

> data Parity = POS | NEG

The data type for the <expression> variable in the grammar. It shows an expression requires a Parity value, a Term and a list of Expressions to construct. This shows that an Expression must consist of at least one term.

> data Expression = Expression Parity Term [Expressions]

The data type for expressions, showing that each Expressions is a term being added or subtracted.

> data Expressions = (:+:) Term | (:-:) Term

The <term> variable requires a Fact followed by a list of Terms to construct. This ensures each Term contains at least one value.

> data Term = Term Fact [Terms]

The Terms data type requires a Fact and is either the multiplication or division of a value by that Fact.

> data Terms = (:*:) Fact | (:/:) Fact

The data type for the <fact> variable shows that it can be constructed with either a Var which simply takes a single character, a Number which takes an integer or a Parens which is an expression wrapped in parentheses.

> data Fact = Var Char | Number Int | Parens Expression

The data types for the Boolean values can be constructed by a TRUE value, a FALSE value, a "notted" Boolean expression, a Boolean operation which requires a Boolean operator and two boolean expressions or a comparison which requires a comparator and two expressions to compare.

The data types for the Boolean expressions require a Not value, a single BoolFact and 0 or more Boolean operations.

> data BoolExpr = BoolExpr Not BoolFact [BoolOp]

The Not data type can either be the Not value, representing the NOT unary boolean operation, or the Is value representing the absence of the NOT operation

> data Not = Not | Is

The BoolFact data type can be TRUE, FALSE, BoolParens (constructed with a Boolean expression and representing a boolean expression inside parentheses), or BoolComp (constructed with two Expression values and a Comp value representing a comparison between two expressions)

> data BoolFact = TRUE
>               | FALSE
>               | BoolParens BoolExpr
>               | BoolComp Expression Comp Expression

The BoolOp data type can either be And or Or and both are constructed with BoolFact values. They represent the AND and OR binary boolean operations

> data BoolOp = And BoolFact
>                | Or BoolFact

The comparison symbols can be applied to two expressions and consist of the comparison symbols.

> data Comp = (:<:) | (:<=:) | (:<>:) | (:==:) | (:>:) | (:>=:)

Now the parsers for each symbol must be defined:

Firstly a parser to consume carriage returns must be constructed. This can be a list of 0 or more line break values read in from the input stream so the parser is constructed as follows

> cr :: Parser [Char]
> cr = many (oneOf "\r\n")

The tok parser parses a token or a string which must be followed by a whitespace value. The absent > symbol shows that this results in a parser of type String as string is a parser of type String. This is used to parse specified words followed by whitespace.

> tok :: String -> Parser String
> tok t = string t <* whitespace

The whitespace parser consumes 0 or more spaces or tabs and produces a pure () parser which produces nothing. This is used to detect and strip whitespace without producing anything.

> whitespace :: Parser ()
> whitespace = many (oneOf " \t") *> pure ()

A parser for the while language must then be constructed. As a while language program is defined as being a statement, we can parse the entire language with a statement parser. First a parser is defined that will parse sequences of statements which can be 1 or more statements long and are separated by semi colons by using the sepBy1 function. It parses each single statement as it does so and places those parsers into a list. If the list only contains 1 statement parser then that parser is returned, otherwise a list of statement parsers for each of those statements are returned as a SEQ of that list.

> statementP :: Parser Statement
> statementP =
>   do list <- (sepBy1 oneStatementP (tok ";" <* cr))
>      -- If there's only one statement return it without using Seq.
>      return $ if length list == 1 then head list else SEQ list

oneStatementP is a parser that parses a single statement; either the assign statement, the skip statement, the if statement, the while statement, or a statement surrounded by braces.

> oneStatementP :: Parser Statement
> oneStatementP = assignP <|> skipP <|> ifP <|> whileP <|> bracesP

assignP is a parser that consumes the keyword LET followed by a variable followed by = and then an expressions. This is the assignment aspect of the while language. This is done by creating a parser p' that consumes LET, a variable, an = and an expression. Parser p' produces the result of the variable parse and the result of the expression parse. p' is then folded to become a parser for the LET value.

> assignP :: Parser Statement
> assignP = LET <$ tok "LET" <*> variableP <* tok "=" <*> expressionP

> bracesP :: Parser Statement
> bracesP = Braces <$ tok "{" <*> statementP <* tok "}"

skipP is a parser that consumes the keyword SKIP. This is the skip statement in the while language. It works by producing a parser that consumes the SKIP keyword and folds it into a parser for the SKIP value.

> skipP :: Parser Statement
> skipP = SKIP <$ tok "SKIP"

ifP parses the IF then ELSE statement in the while language. It is a parser that consumes the keyword IF followed by a boolean value, the keyword THEN, a statement, the keyword ELSE and then another statement. It produces the boolean parser applied to the two statement parses and folds them into a parser for the IF value.

> ifP :: Parser Statement
> ifP = IF <$ tok "IF" <*> boolExprP <* tok "THEN"
>       <*> oneStatementP <* tok "ELSE" <*> oneStatementP

whileP parses the WHILE loop in the while language. It is a parser that consumes the WHILE keyword, a boolean value, the DO keyword and then a statement and produces the parsers for the boolean value and the statement

> whileP :: Parser Statement
> whileP = WHILE <$ tok "WHILE" <*> boolExprP <* tok "DO" <*> statementP

varP parses a variable. It consumes any single character capital letter followed by a whitespace and produces a parser for that letter.

> variableP :: Parser Char
> variableP = oneOf ['A' .. 'Z'] <* whitespace

expressionP parses an expression. A positive or negative sign (if neither are present it assumes positive and produces a POS parser) parser is applied to a term parser which is then applied to a parser for 0 or more expression operation parsers. This resultant function is folded to become a parser for Expression values.

> expressionP :: Parser Expression
> expressionP = Expression <$> ( parityP <|> pure POS) <*> termP <*> many expressionsP

parityP parses either a positive sign or a negative sign. It tries to consume a + symbol and if it does it returns a parser for the POS parity value. If it fails it tries to consume a - symbol and if it does it returns a parser for the NEG parity value.

> parityP :: Parser Parity
> parityP = (POS <$ tok "+") <|> (NEG <$ tok "-")

termP parses a term. This is a parser that consumes a fact and then 0 or more terms. This result is then folded as a Term value parser.

> termP :: Parser Term
> termP = Term <$> factP <*> many termsP

expressionsP parses addition or subtraction operations in an expressions. If a + symbol is parsed, it is applied to the term parser and folded as a (:+:) value parser. If a - symbol is parsed, it is applied to the term parser and folded as a (:-:) value parser

> expressionsP :: Parser Expressions
> expressionsP = ((:+:) <$ tok "+" <*> termP) <|> ((:-:) <$ tok "-" <*> termP)

termsP parses multiplication or division operations in expressions. If a * symbol is parsed it is applied to the fact parser and folded as a parser for (:*:) values. If a / symbol is parsed, it is applied to the fact parser and folded as a (:/:) value parser.

> termsP :: Parser Terms
> termsP = ((:*:) <$ tok "*" <*> factP) <|> ((:/:) <$ tok "/" <*> factP)

factP is a parser for either variables, numbers or expressions in parentheses. First the variable parser is run and if it succeeds it is folded as a parser for Var values. If it fails the number parser runs and if that succeeds is folded as a parser for Number values. Finally if both parsers fail a ( value is parsed which is applied to the expression parser which is applied to a parser for the ) symbol. If this succeeds this is folded as a parser for Parens values.

> factP :: Parser Fact
> factP = (Var <$> variableP) <|> (Number <$> numberP)
>     <|> (Parens <$ tok "(" <*> expressionP <* tok ")")

numberP is the parser for integer values. some (oneOf ['0' .. '9']) Produces a parser p for a list of 1 or more digits as characters. This is then bound to the return . read function which converts the parser p to one that reads in those characters as digits, resulting in a parser for numbers

> numberP :: Parser Int
> numberP = (some (oneOf ['0' .. '9']) >>= return . read) <* whitespace

boolExprP is the function that parses boolean expressions. It first parses a Not value if the NOT keywork is found and otherwise it parses an Is value. This is applied to a parser for boolean facts and then to a parser for a list of 0 or more boolean operations. The resultant parser is folded to be a parser of BoolExpr values

> boolExprP :: Parser BoolExpr
> boolExprP = BoolExpr <$> ((Not <$ tok "NOT") <|> pure Is) <*> boolFactP <*> many boolOpP

boolFactP parses boolean facts. It either parses the TRUE keyword, the FALSE keyword, a ( symbol applied to a boolean expression parser which is then applied to a ) symbol parser, or a parser p that applies an algebraic expression parser to a comparison symbol parser and then applies that to another algebraic expression parser. This parser p is folded to become a parser for the BoolComp values.

> boolFactP :: Parser BoolFact
> boolFactP = (TRUE <$ tok "TRUE")
>         <|> (FALSE <$ tok "FALSE")
>         <|> (BoolParens <$ tok "(" <*> boolExprP <* tok ")")
>         <|> (BoolComp <$> expressionP <*> compP <*> expressionP)

boolOpP is a parser for the AND and OR operations. To parse And values, it parses the keyword AND, applies that to the boolean fact parser and then folds it over the And data constructor. To parse Or values, the OR keyword is parsed and then applied to the boolean fact parser before folding over the Or data constructor.

> boolOpP :: Parser BoolOp
> boolOpP = And <$ tok "AND" <*> boolFactP
>       <|> Or <$ tok "OR" <*> boolFactP

Finally the compP function is a parser for the comparison operators. It has a case for each operator and works by folding the parser for the operator symbol as a string over the data constructor for that value.

> compP :: Parser Comp
> compP = (:<>:)  <$ tok "<>"
>   <|> (:<>:)  <$ tok "><"
>   <|> (:==:)   <$ tok "=="
>   <|> (:<=:)  <$ tok "<="
>   <|> (:<:)   <$ tok "<"
>   <|> (:>=:)  <$ tok ">="
>   <|> (:>:)   <$ tok ">"

Conclusion
------------------------------

Compiling a program is done in stages, the first being lexical analysis where the source code is split into tokens and usually stripped of whitespace. Here this is done through the use of the parseFile function (figure 1 in appendix) which reads in an input file, as well as the use of the whitespace, tok and cr parsers. The next stage is the parsing itself which consumes the tokens and produces values for them stored in the AST. This produces a structure for the program created that is structurally correct. Next the AST must be semantically analysed and meaning must be given to the values stored in the tree. Semantic analysis also checks for errors not picked up by the parser such as scope errors, type checking and array bound checking. Finally the machine code for the program must be generated. Each value in the AST much be converted into equivalent machine code, furthermore optimisation of the source code is done here to ensure there are no redundant commands and to reduce the run time of the execution of the program where possible.

Examples of while programs can be seen in figures 2,3,4 and their resultant ASTs in figures 5,6,7.

In conclusion, the function statementP is a parser for the while language. As such when it is applied to an input stream of a structurally correct while program it will produce a AST for that program. If the program is not structurally correct, it will not be parsed and will throw errors. To extend the parser, the acceptable variables could encompass all strings starting with a letter that are not already pre-assigned key-words. Furthermore additional operations such as modulo could be added to the parser.
