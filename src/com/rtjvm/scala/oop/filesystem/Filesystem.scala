package com.rtjvm.scala.oop.filesystem

import com.rtjvm.scala.oop.commands.Command
import com.rtjvm.scala.oop.files.Directory

object Filesystem extends App {
  val root: Directory = Directory.ROOT
  var state: State = State(root, root)

  /*
    0 (op) 1 => 1
    1 (op) 2 => 3
    3 (op) 3 => 6
    6 (op) 4 => your last value, 10

    List(1,2,3,4).foldLeft(0)((x, y) => x + y)
   */
  state.show
  io.Source.stdin.getLines().foldLeft(state)((currentState, newLine) => {
        state = Command.from(newLine).apply(state)
        state.show
        state
  })

  //var state: State = State(root, root)
  //val scanner: Scanner = new Scanner(System.in)
  //
  //while(true) {
  //  state.show
  //  val input = scanner.nextLine()
  //  state = Command.from(input).apply(state)
  //}
}
