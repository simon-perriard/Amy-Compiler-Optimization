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

        val functions = m.defs.map(d => table.getFunction(m.name,d.name).get) //get all functions from the current module

        val docHeader = "#Module "+m.name+"\n\n"      //header for the file


        val markdownText = functions.map(f => funDocGen(f._1.name,f._2)).fold(docHeader)(_+_) //generate markdown for each function


        writer.write(markdownText)

        writer.close()
    }


    def funDocGen(funName: String, sig: FunSig): String = {

    

    }

    (sProgram, table)
  }





}
