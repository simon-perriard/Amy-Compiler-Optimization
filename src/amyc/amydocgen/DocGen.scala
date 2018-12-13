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
              case f@N.FunDef(_, _, _, _, doc) => if (doc.isDefined) sb.append(System.lineSeparator()).append(mdGenFunDef(f)).append(System.lineSeparator())
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

  def mdGenFunDef(funDef: N.FunDef): String = {
    println(funDef.doc.getOrElse(""))
    "bite"
  }

  def mdGenModuleHeader(moduleName: String): String = {
    println(moduleName)
    moduleName
  }

}
