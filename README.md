# Amy-Compiler-Optimization
## Compiler for [Amy language](https://lara.epfl.ch/w/_media/cc18/amy-spec.pdf) written in Scala
This compiler has been developed during the [Computer Language Processing course 2018](http://lara.epfl.ch/w/cc18:top) at EPFL.

Current optimizations :

  - AmyDoc
  
### AmyDoc
The delimitters for AmyDoc are '~', it supports @param, @return and @see.
It will generate .md files that will be processed by default by Pandoc.
In order to generate the HTML files, run ```./mdTohtml.sh```, the out put will be in the ```amydoc``` folder.

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
