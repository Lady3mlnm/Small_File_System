/**
  * My additional simple method to see inner information about state of the system.
 */

package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.DirEntry
import com.rtjvm.scala.oop.filesystem.State

class Info extends Command {

  override def apply(state: State): State = {
    def createNiceOutput(contents: List[DirEntry]): String = {
      if (contents.isEmpty) ""
      else {
        val entry = contents.head
        "  " + entry.name + "[" + entry.getType + "]\n" + createNiceOutput(contents.tail)
      }
    }

    val str: String = "state.root.parentPath: >" + state.root.parentPath + "<\n" +
        "state.root.name: >" + state.root.name + "<\n" +
        "state.wd.parentPath: >" + state.wd.parentPath + "<\n" +
        "state.wd.name: >" + state.wd.name + "<\n" +
        "state.root.contents:\n" +
        createNiceOutput(state.root.contents)

    state.setMessage(str)
  }
}
