package com.example.scala.ch3datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString() = {
//    @annotation.tailrec
//    def loop(s: String, l: List[A], first: Boolean): String = l match {
//      case Nil => s + "]"
//      case Cons(h, t) => loop(s + (if (first) "" else ", ") + h, t, false /* use tail match? */)
//    }
//    loop("[", this, true)
    head + (tail match {
      case Nil => ""
      case _ => ", " + tail
    })
  }
}

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  
}