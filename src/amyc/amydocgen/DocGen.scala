package amyc
package amydocgen

import amyc.analyzer.SymbolTable
import utils._
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}


object DocGen extends Pipeline[(S.Program, SymbolTable, N.Program), (S.Program, SymbolTable)]{

  def run(ctx: Context)(v: (S.Program, SymbolTable, N.Program)): (S.Program, SymbolTable) = {
    val (sProgram, table, nProgram) = v



    (sProgram, table)
  }
}
