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
package fs2.data.csv
package generic

import semiauto._

import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CsvRowEncoderTest extends AnyFlatSpec with Matchers {

  val csvRow = new CsvRow(NonEmptyList.of("1", "test", "42"), NonEmptyList.of("i", "s", "j"))
  val csvRowK = new CsvRow(NonEmptyList.of("1", "test", "42"), NonEmptyList.of("i", "s", "k"))
  val csvRowDefaultI = new CsvRow(NonEmptyList.of("", "test", "42"), NonEmptyList.of("i", "s", "j"))
  val csvRowEmptyJ =
    new CsvRow(NonEmptyList.of("1", "test", ""), NonEmptyList.of("i", "s", "j"))

  case class Test(i: Int = 0, s: String, j: Option[Int])
  case class TestRename(i: Int, s: String, @CsvName("j") k: Int)
  case class TestOptionRename(i: Int, s: String, @CsvName("j") k: Option[Int])

  val testEncoder = deriveCsvRowEncoder[Test]
  val testRenameEncoder = deriveCsvRowEncoder[TestRename]
  val testOptionRenameEncoder = deriveCsvRowEncoder[TestOptionRename]

  "case classes" should "be encoded properly" in {
    testEncoder(Test(1, "test", Some(42))) shouldBe csvRow
  }

  it should "be handled properly with optional value and empty cell" in {
    testEncoder(Test(1, "test", None)) shouldBe csvRowEmptyJ
  }

  it should "be encoded according to their field renames" in {
    testRenameEncoder(TestRename(1, "test", 42)) shouldBe csvRow
  }

  it should "be encoded according to their field renames if value is optional" in {
    testOptionRenameEncoder(TestOptionRename(1, "test", Some(42))) shouldBe csvRow
  }

}
