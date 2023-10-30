# Parser Toolkit Grammar

## Syntax

```rb

@Identifier # references Identifier from the user context. can be used for types, functions, values
<Rule>      # references another rule named Rule
!Node       # references another ast node called Node


```

## Types

```rb 
literal `text`  # pastes text into the code
optional ...    # makes ... an optional type

struct          # constructs a structure type, having two fields:
    field: !type,
    field: !type

union           # constructs a type for alternatives, here with two variants:
    Foo: !type, # alternative called Foo
    Bar: !type  # alternative called Bar

```

## Strings

- `\x00 ... \xFF` => Hexadecimal escape
- `\000 ... \377` => Octal escape
- `\n` => LF (0x0A)
- `\r` => CR (0x0D)
- `\'` => single quote (0x27)
- `\"` => double quote (0x22)
- `\\` => back slash (0x5C)
- `\u????` => UTF-16
- `\U????????` => UTF-32
