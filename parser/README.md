# Parser
This is the stage of the pipeline that takes in a sequence of tokens and turns
them into the abstract syntax tree.
This particular parser is a recursive descent parser where each production rule
of the languages grammar is implemented as a function.
Although the code is a bit complex as it is essentially five mutually recursive
functions all working together, bearing in mind that each function is really
only trying to look for one specific pattern helps to understand it.
