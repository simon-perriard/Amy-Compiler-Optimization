package amyc
package amydocgen

import amyc.analyzer.SymbolTable
import ast.{SymbolicTreeModule => S}
import utils._
import java.io._


object DocGen extends Pipeline[(S.Program, SymbolTable), (S.Program, SymbolTable)] {


  def run(ctx: Context)(v: (S.Program, SymbolTable)): (S.Program, SymbolTable) = {
    import ctx.reporter._

    val (sProgram, table) = v

    val moduleNames = sProgram.modules.map(_.name.toString)

    val jmp = System.lineSeparator()
    val doubleJump = jmp + jmp

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
      sProgram.modules.foreach {
        //Generate one file per module
        mod =>

          val sb = new StringBuilder()
          mod.defs.foreach {
            {
              case f@S.FunDef(_, _, _, _, _) => sb.append(funDocGen(f, mod.name)).append("<br><br>").append(doubleJump)
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

    def mdGenModuleHeader(name: S.Name): String = {
      "%Module " + name + doubleJump
    }

    def parseDoc(doc: String, paramNames: List[String], currModule: String, currFun: S.Name): String = {

      val (generalDoc, spec) = doc.span(p => p != '@')

      if (spec.isEmpty) { //no special instruction
        return generalDoc
      }


      if (spec.startsWith("@param")) { //parameter description

        val (parameter, follow) = spec.drop(7).span(c => !c.isWhitespace) //following string until whitespace is the name of the parameter (need to check coherence with function definition)

        if (!paramNames.contains(parameter)) { //check if there is a matching parameter
          error("AmyDoc doesn't match function parameter in function " + currFun + " in module " + currModule)
        }
        generalDoc + "<br>" + "**Parameter : " + parameter + "**" + parseDoc(follow, paramNames.filter(s => !s.equals(parameter)), currModule, currFun) //recursion while consuming parameter

      }


      else if (spec.startsWith("@return")) { //description of what is returned
        val follow = spec.drop(8)

        generalDoc + "<br>" + "**Return : **" + parseDoc(follow, paramNames, currModule, currFun)

      }


      else if (spec.startsWith("@see")) { //link to a class name (need to check)

        val (link, follow) = spec.drop(5).span(c => !c.isWhitespace)

        if (link.contains('.')) { //case @see Module.function

          val (mod, fun) = link.span(c => c != '.')
          lazy val funNamesInModule = sProgram.modules.filter(m => m.name.toString.equals(mod)).head.defs.map(d => d.name.toString)

          if (!moduleNames.contains(mod) || !funNamesInModule.contains(fun.tail)) { //check if module and function exist
            ctx.reporter.error("AmyDoc link doesn't match any existing module in function " + currFun + " in module " + currModule)
          }

          //try to link to the module AND the function
          generalDoc + "see [" + link + ".scala](" + mod + ".html5#" + fun.tail + ")" + parseDoc(follow, paramNames, currModule, currFun)

        }

        else { //case @see Module

          if (!moduleNames.contains(link)) {
            ctx.reporter.error("AmyDoc link doesn't match any existing module in function " + currFun + " in module " + currModule)
          }

          generalDoc + "see [" + link + ".scala](" + link + ".html5)" + parseDoc(follow, paramNames, currModule, currFun)
        }
      }

      else { //not a valid instruction => not one of those (param,return,see)
        generalDoc
      }
    }

    def funDocGen(funDef: S.FunDef, moduleName: S.Name): String = {

      val args = funDef.params.toString().substring(4)
      val functionSig = "<h4 id=\""+funDef.name+"\">" + funDef.name + args + ": " + funDef.retType + "</h4>"
      val doc = funDef.doc

      if (doc.isEmpty) { //no doc found

        functionSig

      } else { //doc found. Parse it and insert it

        val documentation = parseDoc(doc.get, funDef.paramNames.map(_.toString), moduleName.toString, funDef.name)

        doubleJump + functionSig + documentation
      }
    }

    (sProgram, table)
  }
}
