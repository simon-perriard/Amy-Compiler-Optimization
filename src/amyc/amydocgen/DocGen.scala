package amyc
package amydocgen

import java.io.{File, PrintWriter}

import amyc.analyzer.{FunSig, SymbolTable}
import utils._
import ast.{Identifier, TreeModule, NominalTreeModule => N, SymbolicTreeModule => S}


object DocGen extends Pipeline[(S.Program, SymbolTable, N.Program), (S.Program, SymbolTable)]{

  def run(ctx: Context)(v: (S.Program, SymbolTable, N.Program)): (S.Program, SymbolTable) = {
    val (sProgram, table, nProgram) = v

    val modules = nProgram.modules

    modules.foreach{  //generate doc for each module in the program
      m =>
        val writer = new PrintWriter(new File(m.name))   //create writer for the module's file

        val funDefs = m.defs.map{case funDef: N.FunDef =>funDef}  //only keep function definition

       // val functions = m.defs.map(d => table.getFunction(m.name,d.name).get) //get all functions from the current module

        val docHeader = "#Module "+m.name+"\n\n"      //header for the file


       // val markdownText = functions.map(f => funDocGen(f._1.name,f._2)).fold(docHeader)(_+_) //generate markdown for each function

        val markdownText = funDefs.map(d => funDocGen(d,m.name,modules.map(_.name))).fold("")(_+_)

        writer.write(markdownText)

        writer.close()
    }

    def parseDoc(doc: String, modules: List[String], paramNames: List[String])(implicit currModule: String, currFun: String): String = {

      val (generalDoc, spec) = doc.span(p => p!= '@')

      if(spec.isEmpty){ //no special instruction
        return generalDoc
      }


      if(spec.startsWith("@param")){      //parameter description

        val (parameter, follow) =  spec.drop(6).span(c => !c.isWhitespace)          //following string until whitespace is the name of the parameter (need to check coherence with function definition)

        if(!paramNames.contains(parameter)){  //check if there is a matching parameter
          ctx.reporter.error("AmyDoc doesn't match function parameter in function "+currFun+" in module "+currModule)
        }
        generalDoc+"\n\n"+"#####Parameter : **"+parameter+"** \n"+parseDoc(follow, modules, paramNames.filter(s => !s.equals(parameter)))   //recursion while consuming parameter

      }


      else if(spec.startsWith("@return")){  //description of what is returned
        val follow = spec.drop(7)

        generalDoc+"\n\n"+"#####Return : "+parseDoc(follow,modules,paramNames)

      }


      else if(spec.startsWith("@see")){  //ling to a class name (need to check)

        val (link, follow) = spec.drop(4).span(c => !c.isWhitespace)

        if(!modules.contains(link)){
          ctx.reporter.error("AmyDoc link doesn't match any existing module in function "+currFun+" in module "+currModule)
        }

        generalDoc+"\n\n"+"see ["+link+".scala]("+link+".html)"+parseDoc(follow,modules,paramNames)

      }

        
      else {    //not a valid instruction => not one of those (param,return,see)
        generalDoc
      }
    }

    def funDocGen(funDef: N.FunDef, moduleName: String, modules: List[String]): String = {

      //does funDef.params.map(p => (name,p.tt)) work ???? instead of using paramNames....
      val args = funDef.paramNames.zip(funDef.params.map(p => p.tt)).map(e => e._1+" : "+e._2).fold("")(_+_)  //string for args with name and type

      val functionSig = "### "+funDef.name+"("+args+")"

      val doc = funDef.doc

      if(doc.isEmpty){  //no doc found

        functionSig

      }
      else{ //doc found. Parse it and insert it

        val documentation = parseDoc(doc.get,modules,funDef.paramNames)(moduleName,funDef.name)

        functionSig+"\n\n"+documentation+"\n\n"

      }
    }

    (sProgram, table)
  }





}
