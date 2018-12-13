package amyc
package amydocgen

import java.io.{File, PrintWriter}

import amyc.analyzer.{FunSig, SymbolTable}
import utils._
import ast.{Identifier, TreeModule, NominalTreeModule => N, SymbolicTreeModule => S}


object DocGen extends Pipeline[(S.Program, SymbolTable, N.Program), (S.Program, SymbolTable)]{

  def run(ctx: Context)(v: (S.Program, SymbolTable, N.Program)): (S.Program, SymbolTable) = {
    val (sProgram, table, nProgram) = v

   nProgram.modules.foreach{  //generate doc for each module in the program
      m =>
        val writer = new PrintWriter(new File(m.name))   //create writer for the module's file

        val funDefs = m.defs.map{case funDef: N.FunDef =>funDef}


       // val functions = m.defs.map(d => table.getFunction(m.name,d.name).get) //get all functions from the current module

        val docHeader = "#Module "+m.name+"\n\n"      //header for the file


       // val markdownText = functions.map(f => funDocGen(f._1.name,f._2)).fold(docHeader)(_+_) //generate markdown for each function

        val markdownText = funDefs.map(d => funDocGen(d)).fold("")(_+_)

        writer.write(markdownText)

        writer.close()
    }


    def funDocGen(funDef: N.FunDef): String = {

      //does funDef.params.map(p => (name,p.tt)) work ???? instead of using paramNames....
      val args = funDef.paramNames.zip(funDef.params.map(p => p.tt)).map(e => e._1+" : "+e._2).fold("")(_+_)  //string for args with name and type

      val functionSig = "### "+funDef.name+"("+args+")"

      val doc = funDef.doc

      if(doc.isEmpty){  //no doc found

        functionSig

      }
      else{ //doc found. Parse it and insert it

        val documentation = "TODO"

        functionSig+"\n\n"+documentation

      }
    }

    (sProgram, table)
  }





}
