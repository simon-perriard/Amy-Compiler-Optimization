# Amy-Compiler-Optimization
## Compiler for Amy language written in Scala
This compiler has been developed during the Computer Language Processing course 2018 at EPFL.

Current optimizations :
  - AmyDoc (in development)
  
### AmyDoc
The delimitters for AmyDoc are '~', it supports @param and @return.
It will generate .md files that will be processed by default by Pandoc.

Usage example:  

```
~  
This function builds a new nice String  
@param name the name of the user  
@return a nice message  
~  
```
```scala
def foo(name: String): String = {
  "Have a nice day " ++ name
}
```


Developped by Hédi Sassi and Simon Perriard
