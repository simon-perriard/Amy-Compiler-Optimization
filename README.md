# Amy-Compiler-Optimization
## Compiler for Amy language written in Scala
This compiler has been developed during the [Computer Language Processing course 2018](http://lara.epfl.ch/w/cc18:top) at EPFL.

Current optimizations :

  - AmyDoc (in development)
  
### AmyDoc
The delimitters for AmyDoc are '~', it supports @param, @return and @see.
It will generate .md files that will be processed by default by Pandoc.

Usage example:  

```
~  
This function builds a new nice String that will be used in @see Module.hello
@param name the name of the user  
@return a nice message  
~  
```
```scala
def foo(name: String): String = {
  "Have a nice day " ++ name
}
```


Developed by Hédi Sassi and Simon Perriard
