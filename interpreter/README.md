# Interpreter/Reducer
This is the stage in the pipline that takes in the parsed abstract syntax tree
and tries to simplify (beta-reduce) it.
The rules for beta reduction are quite simple and are as follows.

+ Applications where the beta-reduced left expression is a known lambda become
the beta-reduction of the environment in which the left lambdas varible is
assigned to the right expression.
`Application ((Lambda (var, body), right) -> Environment (var, right, beta_reduce body)`
+ Applications where the beta-reduced left expression isn't a known lambda
become an application where the beta-reduced right expression is applied to the
beta-reduced left expression.
`Application (left, right) -> Application (beta_reduce left, beta_reduce right)`
+ Environments become the beta-reduction of the expression where the
environments variable is replaced with the substitute expression in the body
expression.
`Environment (var, sub, body) -> beta_reduce (substitute sub var body)`
