-   [X] Arguments from C++. Lua style, ie stack based.
    
    (+ 1 2 3) becomes
    
        pushValue(vm, value(1));
        pushValue(vm, value(2));
        pushValue(vm, value(3));
        getGlobal(vm, symbol(vm, "+"));
        call(vm, 3);

-   [X] Convert parse tree into lists

-   [X] Pretty printing for lists

-   [X] Globals

-   [X] Move lambda from lexer to ast transformation

-   [X] Printing objects

-   [X] GC

-   [X] Make the compiler compile function by function
    
    That is, dont parse the file then compile but parse a function,
    then compile it.
    This helps macros a LOT!
    Would also make mismatched parens crash, which is good
    bc right now we just get weird behavior.
    Need a generic parseExpr() that dispatch to different parse\*()
    instead of just parseList(). parseExpr should fail on EOF

-   [ ] REMOVE NEW

-   [ ] Objects
    -   [X] Replace with one built on top of DynamicVector
        
        map sucks

-   [ ] Parent

-   [ ] Array part

-   [ ] Replace DynamicVector
    
    std::vector sucks

-   [ ] multi-line printing of lists

-   [ ] (better) string interning

-   [ ] quote.
    -   [X] primitives

-   [ ] lists
    
    prob. just a call to a C function.

-   [ ] objects

-   [ ] quote et. al. abreviations

-   [ ] make-object sugar

-   [ ] get-object-slot

-   [ ] get-object-slot sugar

-   [ ] lambda sugar
    
    [args] -> lambda (args)

-   [ ] Closures
    
    Lua style upvalues will be used.

-   [ ] Globals

-   [ ] Calling C++

-   [ ] Lists, cons et. al.

-   [ ] Macros

-   [ ] let

-   [ ] set!

-   [ ] if

-   [ ] for, maybe as iterators?

-   [ ] Abstract char \* into a stream for the parser.
    
    Might be a good place for "OOP"

-   [ ] Tune GC