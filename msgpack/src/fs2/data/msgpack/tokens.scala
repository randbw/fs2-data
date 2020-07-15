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

abstract class DynamicLength(val kind: String) extends Token {
  def getLength(bytes: List[String]): Int
}

object DynamicLength {

  def getLengthFromPortion(bytes: List[String], toDrop: Int): Int =
    bytes.headOption
      .fold(-1)(b => parseLengthFromByte(b.drop(toDrop), minus1 = false))

  def getLengthFromOne(bytes: List[String]): Int =
    bytes.headOption
      .fold(-1)(b => parseLengthFromByte(b, minus1 = true))

  def getLengthFromTwo(bytes: List[String]): Int =
    bytes match {
      case b :: bb :: Nil => parseLengthFromByte(b, bb)
      case _              => -1
    }

  def getLengthFromFour(bytes: List[String]): Int =
    bytes match {
      case b :: bb :: bbb :: bbbb :: Nil => parseLengthFromByte(b, bb, bbb, bbb)
      case _                             => -1
    }

  private def parseLengthFromByte(b: String, minus1: Boolean): Int =
    Integer.parseInt(b, 2) - { if (minus1) 1 else 0 }

  private def parseLengthFromByte(b: String, bb: String): Int =
    Integer.parseInt(b.concat(bb), 2) - 1

  private def parseLengthFromByte(b: String, bb: String, bbb: String, bbbb: String): Int =
    Integer.parseInt(b.concat(bb).concat(bbb).concat(bbbb), 2) - 1
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

  case object FixStr extends DynamicLength("string") {
    def getLength(bytes: List[String]): Int =
      DynamicLength.getLengthFromPortion(bytes, toDrop = 3)
  }

  case object Str8 extends DynamicLength("string") {
    def getLength(bytes: List[String]): Int =
      DynamicLength.getLengthFromOne(bytes)
  }

  case object Str16 extends DynamicLength("string") {
    def getLength(bytes: List[String]): Int =
      DynamicLength.getLengthFromTwo(bytes)
  }

  case object Str32 extends DynamicLength("string") {
    def getLength(bytes: List[String]): Int =
      DynamicLength.getLengthFromFour(bytes)
  }

  case object Bin8 extends DynamicLength("bin") {
    def getLength(bytes: List[String]): Int =
      DynamicLength.getLengthFromOne(bytes)
  }

  case object Bin16 extends DynamicLength("bin") {
    def getLength(bytes: List[String]): Int =
      DynamicLength.getLengthFromTwo(bytes)
  }

  case object Bin32 extends DynamicLength("bin") {
    def getLength(bytes: List[String]): Int =
      DynamicLength.getLengthFromFour(bytes)
  }

  case object FixArray extends DynamicLength("array") {
    def getLength(bytes: List[String]): Int =
      DynamicLength.getLengthFromPortion(bytes, toDrop = 4)
  }

  case object Array16 extends DynamicLength("array") {
    def getLength(bytes: List[String]): Int =
      DynamicLength.getLengthFromTwo(bytes)
  }

  case object Array32 extends DynamicLength("array") {
    def getLength(bytes: List[String]): Int =
      DynamicLength.getLengthFromFour(bytes)
  }
}
