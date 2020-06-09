/*
 * Copyright 2019 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package fs2
package data
package msgpack

sealed trait Token {
  def kind: String
}

abstract class FixedLength(val kind: String, val length: Int) extends Token

abstract class UnfixedLength(val kind: String) extends Token {
  def getLength(bytes: List[String]): Int
}

object Token {

  case object NilByte extends FixedLength("nil", 1)

  case object TrueValue extends FixedLength("boolean", 1)

  case object FalseValue extends FixedLength("boolean", 1)

  case object PositiveFixnum extends FixedLength("fixnum", 1)

  case object NegativeFixnum extends FixedLength("fixnum", 1)

  case object UInt8Bit extends FixedLength("uint", 2)

  case object UInt16Bit extends FixedLength("uint", 3)

  case object UInt32Bit extends FixedLength("uint", 5)

  case object UInt64Bit extends FixedLength("uint", 9)

  case object Int8Bit extends FixedLength("int", 2)

  case object Int16Bit extends FixedLength("int", 3)

  case object Int32Bit extends FixedLength("int", 5)

  case object Int64Bit extends FixedLength("int", 9)

  case object Float32Bit extends FixedLength("float", 5)

  case object Float64Bit extends FixedLength("float", 9)

  case object FixStr extends UnfixedLength("string") {
    def getLength(bytes: List[String]): Int =
      bytes match {
        case byte :: Nil => Integer.parseInt(byte.drop(3), 2)
        case _ => -1
    }
  }

  case object Str8 extends UnfixedLength("string") {
    def getLength(bytes: List[String]): Int =
      bytes match {
        case byte :: Nil => Integer.parseInt(byte, 2)
        case _ => -1
      }
  }

  case object Str16 extends UnfixedLength("string") {
    def getLength(bytes: List[String]): Int =
      bytes match {
        case b :: bb :: Nil => Integer.parseInt(b.concat(bb), 2)
        case _ => -1
      }
  }

  case object Str32 extends UnfixedLength("string") {
    def getLength(bytes: List[String]): Int =
      bytes match {
        case b :: bb :: bbb :: bbbb :: Nil => Integer.parseInt(b.concat(bb).concat(bbb).concat(bbbb), 2)
        case _ => -1
      }
  }
}
