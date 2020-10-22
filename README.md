# fcall
A call graph generator for haskell,displaying how functions reference and call each other.

# implementation
Basically it parse source to AST,and make function definations as nodes,and if a defination is reference in other defination,a edge is added to the call graph.

