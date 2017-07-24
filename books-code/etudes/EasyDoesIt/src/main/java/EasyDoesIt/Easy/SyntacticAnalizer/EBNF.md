## EBNF of the Easy language

To the specification are applied:

- left factorization: ```XY | XZ``` become ```X(Y|Z)```

- elimination of left recursion: ```N ::= X | NY``` become ```N ::= X|(Y)*

- substitution of nonterminal symbols: 
  In ```N ::= X``` we may substitute X for any occurrence of N on the right-hand side of 
  another production rule.

- a little bit simplification

### KEYWORDS

```
ARRAY, BEGIN, BY, CALL, CASE, DECLARE, ELSE, END, EXIT, FI, FIELD, FOR, FUNCTION, IF, IS, OF,
OTHERWISE, PROCEDURE, PROGRAM, REPEAT, REPENT, RETURN, SELECT, SET, STRUCTURE, THEN, TO, TYPE,
WHILE
```

### LIBRARY FUNCTIONS

```
XOR, MOD, FLOOR, LENGTH, SUBSTR, CHARACTER, NUMBER, FLOAT, FIX, INPUT, OUTPUT,
INTEGER, REAL, BOOLEAN, STRING, TRUE, FALSE
```

### TOKEN_TABLE

Build scanner based on this _lexical grammar_:

```
<Program>           ::=  (<Token> | <Comment> | <Blank>)*
<Token>             ::=  <Integer-Literal> | <String-Literal> | <Identifier> | <Operator> |
                         ARRAY | BEGIN | BY | CALL | CASE | DECLARE | ELSE | END | EXIT | FI |
                         FIELD | FOR | FUNCTION | IF | IS | OF | OTHERWISE | PROCEDURE | PROGRAM |
                         REPEAT | REPENT | RETURN | SELECT | STRUCTURE | THEN | TO | TYPE | WHILE |
                         . | : | ; | , | :(empty|=) | ( | ) | [ | ]
                        
<Integer-Literal>   ::=  <Digit>(<Digit>)*
<String-Literal>    ::=  " <Graphic>* "
<Identifier>        ::=  <Letter> (<Letter> | <Digit>)*
<Operator>          ::=  <Op-character>(<Op-character>)*
<Comment>           ::=  /* <Graphic> */
<Blank>             ::=  space | tab | end-of-line

<Graphic>           ::=  <Letter> | <Digit> | <Op-character> | space | tab | . | : | ; | , |
                         ~ | ( | ) | [ | ] | { | } | _ | ! | ' | ` | " | # | $ | % | ? | ^ | \
                         
<Letter>            ::=  A | B | C | D | E | F | G | H | I | J | K | L | M |
                         N | O | P | Q | R | S | T | U | V | W | X | Y | Z |
                         a | b | c | d | e | f | g | h | i | j | k | l | m |
                         n | o | p | q | r | s | t | u | v | w | x | y | z
                          
<Digit>             ::=  0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<Op-character>      ::=  + | - | * | / | < | = | > | & | | |
```


### PROGRAM

```                    
<program>      ::=  <program head> <segment body> <program end>
<program head> ::=  PROGRAM <identifier> :
<program end>  ::=  END PROGRAM <indentifier> ;
```


### EXTERNAL_PROCEDURES (not implemented)


### SEGMENTS

```
<segment body>  ::=  {<type definition>}*
                     {<variable declaration>}*
                     {<procedure definition>}*
                     <executable statement> | {<executable statement>}*
```


### TYPES

```
<type definition>   ::=  TYPE <identifier> IS <type> ;

<type>              ::=  <basic type>
                     |   <arrayed type>
                     |   <structured type>
                     |   <type identifier>
                   
<basic type>        ::=  INTEGER
                     |   REAL
                     |   BOOLEAN
                     |   STRING
                    
<arrayed type>      ::=  ARRAY <bounds> OF <type>

<bounds>            ::=  [ <expression> ]
                     |   [ <expression> : <expression> ]

<structured type>   ::=  STRUCTURE <field list> END STRUCTURE
<field list>        ::=  <field> | {, <field>}*
<field>             ::=  FIELD <identifier> IS <type>
<type identifier>   ::=  <identifier>

```


### DECLARATIONS

```
<variable declaration> ::=  DECLARE <declared names> <type> ;
<declared names>       ::=  <identifier>
                        |   ( <identifier> | {, <identifier>}* )
```                        


### INTERNAL_PROCEDURES

```
<procedure definition>           ::=  <subprogram definition>
                                  |   <function definition>
                                  
<subprogram definition>          ::=  <subprogram head> : <segment body> <subprogram end>
<subprogram head>                ::=  PROCEDURE <procedure name>
<subprogram end>                 ::=  END PROCEDURE <identifier> ;

<function definition>            ::=  <function head> : <segment body> <function end>
<function head>                  ::=  FUNCTION <procedure name> <type>
<function end>                   ::=  END FUNCTION <identifier> ;

<procedure name>                 ::=  <identifier> {<internal parameter list>}*
<internal parameter list>        ::=  ( <internal parameter> | {, <internal parameter>}* )
<internal parameter>             ::=  <identifier> <type>
```


### EXECUTABLE_STATEMENTS

```
<executable statement> ::=  <assignment statement>
                        |   <call statement>
                        |   <return statement>
                        |   <exit statement>
                        |   <conditional statement>
                        |   <compound statement>
                        |   <iteration statement>
                        |   <selection statement>
                        |   <repeat statement>
                        |   <repent statement>
                        |   <input statement>
                        |   <output statement>
                        |   <null statement>

```


### ASSIGNMENTS

```
<assignment statement> ::=  SET <target list> <expression> ;
<target list>          ::=  <target> | {<target>}*
<target>               ::=  <variable> :=
```

Example: ```SET a := b := 42;

### PROCEDURE_CALLS

```
<call statement>       ::=  CALL <procedure reference> ;
<procedure reference>  ::=  <identifier> | {<actual argument list>}*
<actual argument list> ::=  ( <expression>  | {, <expression>}* )

```


### FUNCTION_CALLS
        
```
<function reference>  ::=  <identifier> ( )
                       |   <identifier> <actual argument list>
```


### RETURN

```
<return statement> ::=  RETURN ;
                    |   RETURN <expression> ;

```

### EXITS

```
<exit statement>  ::=  EXIT ;
```


### CONDITIONALS

```
<conditional statement>        ::=  <simple conditional statement>
                                |   <label> <simple conditional statement>

<simple conditional statement> ::=  <conditional clause> <true branch> FI ;
                                |   <conditional clause> <true branch> <false branch> FI ;

<conditional clause>           ::=  IF <expression>
<true branch>                  ::=  THEN <segment body>
<false branch>                 ::=  ELSE <segment body>

```


### COMPOUNDS

```
<compound statement> ::=  <simple compound>
                      |   <label> <simple compound>
                      
<simple compound>    ::=  BEGIN <compound body> <compound end>

<compound body>      ::=  <segment body>

<compound end>       ::=  END ;
                      |   END <identifier> ;
```


### ITERATIONS
          
```
<iteration statement>        ::=  <simple iteration statement>
                              |   <label> <simple iteration statement>
                              
<simple iteration statement> ::=  <iteration head> <segment body> <iteration end> 

<iteration head>             ::=  FOR <iteration target> <control> DO

<iteration end>              ::=  END FOR ;
                              |   END FOR <identifier> ;

<iteration target>           ::=  <variable> :=

<control>                    ::=  <step control>
                              |   <step control> <while control>
                              
<step control>               ::=  <expression> <step>
                              |   <expression> <limit>
                              |   <expression> <step> <limit>
                              
<step>                       ::=  BY <expression>
<limit>                      ::=  TO <expression>
<while control>              ::=  WHILE <expression>          
```


### SELECTION

```
<selection statement>  ::=  <simple selection>
                        |   <label> <simple selection>
                       
<simple selection>     ::=  <selection head> <selection body> <selection end>

<selection head>       ::=  SELECT <expression> OF
<selection body>       ::=  <case list>
                        |   <case list> <escape case>
<selection end>        ::=  END SELECT ;
                        |   END SELECT <identifier> ;

<case list>            ::=  <case> | {<case>}*
<case>                 ::=  <case head> <segment body>
<case head>            ::=  CASE <selector> :

<selector>             ::=  ( <expression> | {, <expression>}* )
                        
<escape case>          ::=  <escape head> <segment body>

<escape head>          ::=  OTHERWISE :
```


### REPEAT_AND_REPENT
        
```
<repeat statement> ::= REPEAT <identifier> ;
<repent statement> ::= REPENT <identifier> ;
```


### INPUT_AND_OUTPUT
        
```
<input statement>  ::=  INPUT <input list> ;
<input list>       ::=  <variable> | {, <variable>}*
                  
<output statement> ::=  OUTPUT <output list> ;
<output list>      ::=  <expression> | {, <expression>}*
```   


### NULLS_AND_LABELS

```
<null statement> ::=  ;
<label>          ::=  <identifier> :
```         


### EXPRESSIONS

```
<expression>       ::=  <expression one>
                    |   <expression> | <expression one>
                    |   <expression> XOR <expression one>
                    
<expression one>   ::=  <expression two>
                    |   <expression one> & <expression two>
                    
<expression two>   ::=  <expression three>
                    |   NOT <expression three>

<expression three> ::=  <expression four>
                    |   <expression three> <relation> <expression four>

<expression four>  ::=  <expression five>
                    |   <expression four> || <expression five>
                    
<expression five>  ::=  <expression six>
                    |   <expression five> <adding operator> <expression six>
                    |   <adding operator> <expression six>
                    
<expression six>   ::=  <expression seven>
                    |   <expression six> <multiplying operator> <expression seven>
                    
<expression seven> ::=  FLOOR  ( <expression> )
                    |   LENGTH ( <expression> )
                    |   SUBSTR ( <expression> , <expression> , <expression> )
                    |   CHARACTER ( <expression> )
                    |   NUMBER ( <expression> )
                    |   FLOAT  ( <expression> )
                    |   FIX    ( <expression> )
                    |   <expression eight> 

<expression eight> ::=  <variable>
                    |   <constant>
                    |   <function reference>
                    |   ( <expression> )
```                    

### VARIABLES

```
<variable> ::=  <identifier>
            |   <variable> . <identifier>
            |   <variable> [ <expression> ]
```    

### CONSTANTS
        
```
<constant>         ::=  <integer constant>
                    |   <real constant>
                    |   TRUE
                    |   FALSE
                    |   <string constant>
```


### LEXICAL_ITEMS
        
```
<relation>             ::=  <
                        |   >
                        |   <=
                        |   >=
                        |   <>
                        
<adding operator>      ::=  +
                        |   -
                        
<multiplying operator> ::=  *
                        |   /
                        |   MOD
```          

The syntax category ```<identifier>``` consists of strings that must start with a letter followed by any number
of letters and digits. Also, <identifier> includes none of the keywords.
 

### Comments

Comments can't be nested