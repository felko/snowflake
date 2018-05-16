```
File:
    Header:
        magic                            "snowflake" encoded in ASCII ( 9 bytes)
        Version:
            major                                                     ( 1 byte )
            minor                                                     ( 1 byte )
            patch                                                     ( 1 byte )
            fix                                                       ( 1 byte )
        timestamp                        POSIX timestamp              ( 4 bytes)
        hash                             MD5-hashed body              (16 bytes)
    Body:
        Segment                          Top level segment
        Segment*                         Other segments

Segment:
    ConstantTable                        
    SymbolTable
    Code                 

ConstantTable:
    size                                                              ( 4 bytes)
    Constant*

Constant:
    type                                                              ( 4 bytes)
    value

SymbolTable:
    size                                                              ( 4 bytes)
    Symbol*

Symbol:
    size                                                              ( 4 bytes)
    value

Code:
    size                                                              ( 4 bytes)
    Instr*

Instr:
    code                                                              ( 4 bytes)
    arg?                                                              ( 4 bytes)
```
