#!/bin/bash
exec scala -savecompiled "$0" "$@"
!#
import scala.Math.pow

object CA {
  val twos = Array(1, 2, 4, 8, 16, 32, 64, 128)

  def iterate(row: Vector[Int], cell: Int, rule: Int):Vector[Int] = {
    if (cell == 0) 
      Vector[Int](0) ++ iterate(row, 1, rule)
    else if (cell < row.length -1) 
      Vector(getCell(row, cell, rule)) ++ iterate(row, cell + 1, rule)
    else 
      Vector(0)
  }

  def getCell(row: Vector[Int], cell: Int, rule: Int): Int = {
    if (((twos(row(cell - 1) + (row(cell) * 2) + (row(cell + 1) * 4))) & rule) > 0 ) 1
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
}


val width = 175

var row = CA.initAutomaton(width)

for(n <- 1 to 100) {
  row = CA.iterate(row,0,30 )
  CA.showRow(row)
}

