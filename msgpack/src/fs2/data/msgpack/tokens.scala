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

sealed abstract class Token(val kind: String) {
  def length: Int
}

object Token {

  case object Nil extends Token("nil") {
    override def length: Int = 1
  }

  case object TrueValue extends Token("boolean") {
    override def length: Int = 1
  }

  case object FalseValue extends Token("boolean") {
    override def length: Int = 1
  }

  case object PositiveFixnum extends Token("fixnum") {
    override def length: Int = 1
  }

  case object NegativeFixnum extends Token("fixnum") {
    override def length: Int = 1
  }

  case object UInt8Bit extends Token("uint") {
    override def length: Int = 2
  }

  case object UInt16Bit extends Token("uint") {
    override def length: Int = 3
  }

  case object UInt32Bit extends Token("uint") {
    override def length: Int = 5
  }

  case object UInt64Bit extends Token("uint") {
    override def length: Int = 9
  }

  case object Int8Bit extends Token("int") {
    override def length: Int = 2
  }

  case object Int16Bit extends Token("int") {
    override def length: Int = 3
  }

  case object Int32Bit extends Token("int") {
    override def length: Int = 5
  }

  case object Int64Bit extends Token("int") {
    override def length: Int = 9
  }
}
