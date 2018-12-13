package amyc
package amydocgen

import amyc.analyzer.SymbolTable
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}
import utils._
import java.io._


object DocGen extends Pipeline[(S.Program, SymbolTable, N.Program), (S.Program, SymbolTable)] {

  def run(ctx: Context)(v: (S.Program, SymbolTable, N.Program)): (S.Program, SymbolTable) = {
    import ctx.reporter._

    val (sProgram, table, nProgram) = v

    var folderExists = false
    val mdFolderName = "markdown"

    //Create the dir containing the doc if needed
    val mdFolder = new File(mdFolderName)
    if (!mdFolder.exists()) {
      try {
        mdFolder.mkdir()
        folderExists = true
      } catch {
        case e: SecurityException => println("Unable to create a folder for the doc, please create a folder named 'markdown'")
          e.printStackTrace()
      }
    } else {
      folderExists = true
    }

    if (folderExists) {
      nProgram.modules.foreach {
        //Generate one file per module
        mod =>

          val sb = new StringBuilder()
          mod.defs.foreach {
            {
              case f@N.FunDef(_, _, _, _, _) => sb.append(System.lineSeparator()).append(funDocGen(f)).append(System.lineSeparator())
              case _ =>
            }
          }
          if (sb.nonEmpty) {
            val file = new File(mdFolderName + File.separator + mod.name + ".md")
            val bw = new BufferedWriter(new FileWriter(file))
            bw.write(mdGenModuleHeader(mod.name))
            bw.write(sb.toString())
            bw.close()
          }
      }
    }

    (sProgram, table)
  }

  private val doubleJump = System.lineSeparator() + System.lineSeparator()

  def mdGenModuleHeader(name: String): String = {
    "#Module " + name + doubleJump
  }

  def parseDoc(doc: String): String = {

    val (generalDoc, spec) = doc.span(p => p != '@')

    if (spec.isEmpty) { //no special instruction
      return generalDoc
    }

    if (spec.startsWith("@param")) { //parameter description
      val parameter = "" //TODO following string until whitespace is the name of the parameter (need to check coherence with function definition)
      generalDoc + doubleJump + "**" + parameter + "**"
    } else if (spec.startsWith("@return")) { //description of what is returned
      ""
    } else if (spec.startsWith("@see")) { //ling to a class name (need to check)
      ""
    }
    else { //not a valid instruction => not one of those (param,return,see)
      generalDoc
    }
  }

  def funDocGen(funDef: N.FunDef): String = {
    //does funDef.params.map(p => (name,p.tt)) work ???? instead of using paramNames....
    val args = funDef.paramNames.zip(funDef.params.map(p => p.tt)).map(e => e._1 + " : " + e._2).fold("")(_ + _) //string for args with name and type
    val functionSig = "### " + funDef.name + "(" + args + ")"
    val doc = funDef.doc

    if (doc.isEmpty) { //no doc found
      functionSig
    }
    else { //doc found. Parse it and insert it
      val documentation = parseDoc(doc.get)
      functionSig + doubleJump + documentation
    }
  }
}
