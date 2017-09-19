package com.feelangaFree

import cats.{Id, ~>}
import cats.free.Free
import cats.free.Free.liftF

object MyApp extends App{

  sealed trait DoMath[A]
  case class Add[T](a:T,b:T) extends DoMath[T]
  case class Multiply[T](a:T,b:T) extends DoMath[T]
  case class Square[T](a:T) extends DoMath[T]


  def add[T](a:T,b:T):Free[DoMath, T] = liftF[DoMath,T](Add[T](a,b))
  def mult[T](a:T,b:T):Free[DoMath,T] = liftF[DoMath,T](Multiply(a,b))
  def square[T](a:T):Free[DoMath,T] = liftF(Square(a))

  val theProg = for{
    a<-add(1,2)
    b<-mult(a,3)
    c<-square(b)
  } yield c

  def theInterp:DoMath~>Id= new (DoMath ~> Id){
    override def apply[A](fa: DoMath[A]): Id[A] = {
      fa match {
        case Add(a:Int,b:Int) => (a+b).asInstanceOf[A]
        case Multiply(a:Int,b:Int) => (a*b).asInstanceOf[A]
        case Square(a:Int) => (a*a).asInstanceOf[A]
      }
    }
  }



  println(theProg.foldMap(theInterp))
}
