#!/bin/bash
exec scala -savecompiled "$0" "$@"
!#
import scala.Math.pow

object CA {
  def iterate(row: Vector[Int], cell: Int, rule: Int):Vector[Int] = {
    if (cell == 0) 
      Vector[Int](0) ++ iterate(row, 1, rule)
    else if (cell < row.length -1) 
      Vector(getCell(row, cell, rule)) ++ iterate(row, cell + 1, rule)
    else 
      Vector(0)
  }

  def getCell(row: Vector[Int], cell: Int, rule: Int): Int = {
    if (((pow(2,(row(cell - 1) + (row(cell) * 2) + (row(cell + 1) * 4))).toInt) & rule) > 0 ) 1
    else 0
  }

  def showRow(row: Vector[Int]):Unit = {
    println(row.foldLeft("")(_ + _.toString))
  }
}


val width = 175

def thing(count: Int):Vector[Int] = {
  if (count > 0) 
    Vector(if (count == width/2) 1 else 0) ++ thing(count - 1)
  else 
    Vector(0)
}
var row = thing(width)

for(n <- 1 to 100) {
  row = CA.iterate(row,0,30 )
  CA.showRow(row)
}

