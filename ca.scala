#!/bin/bash
exec scala -savecompiled "$0" "$@"
!#

object CA {
  val twos = Array(1, 2, 4, 8, 16, 32, 64, 128)
  var width = 175

  def getRow(row: Vector[Int], cell: Int, rule: Int):Vector[Int] = {
    if (cell == 0)  //first cell in a row 
      Vector[Int](0) ++ getRow(row, 1, rule)
    else if (cell < row.length -1) //middle cells
      Vector(getCell(row, cell, rule)) ++ getRow(row, cell + 1, rule)
    else  //last cell
      Vector(0)
  }

  def getCell(row: Vector[Int], cell: Int, rule: Int): Int = {
    val neighborhood = 
      twos(
        row(cell - 1)         //left cell 
        + (row(cell) * 2)     //middle
        + (row(cell + 1) * 4) //right
      )
    if ((neighborhood & rule) > 0 ) 1
    else 0
  }

  def showRow(row: Vector[Int]):Unit = {
    println(row.foldLeft("")(_ + _.toString))
  }

  def initAutomaton(count: Int):Vector[Int] = {
    if (count > 0) 
      Vector(if (count == width/2) 1 else 0) ++ initAutomaton(count - 1)
    else 
      Vector(0)
  }

  def run(rule:Int = 30, width: Int = 175, rows: Int = 100):Unit = {
    this.width = width
    var row = initAutomaton(width)
    for(n <- 1 to rows) {
      showRow(row)
      row = getRow(row, 0, rule)
    }
  }
}

val argList = argv.toList.map(_.toInt)
if (argList.length >= 3)
  CA.run(argList(0),argList(1), argList(2))
else
  CA.run()




