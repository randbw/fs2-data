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
package json
package internals

import ast._

private[json] object TokenSelector {

  private def transformValue[F[_], Json](chunk: Chunk[Token],
                                         idx: Int,
                                         rest: Stream[F, Token],
                                         f: Json => Option[Json],
                                         context: JsonContext,
                                         chunkAcc: List[Token],
                                         key: Option[String])(implicit
      F: RaiseThrowable[F],
      builder: Builder[Json],
      tokenizer: Tokenizer[Json]): Pull[F, Token, Result[F, Token, List[Token]]] =
    ValueParser.pullValue(chunk, idx, rest).flatMap {
      case Some((chunk, idx, rest, json)) =>
        val chunkAcc1 = f(json) match {
          case Some(json) => tokenizer.tokenize(json).toList reverse_::: key.map(Token.Key(_)).toList ::: chunkAcc
          case None       => chunkAcc
        }
        Pull.pure(Some((chunk, idx, rest, chunkAcc1)))
      case None => Pull.pure(None)
    }

  private def transformValueF[F[_], Json](chunk: Chunk[Token],
                                          idx: Int,
                                          rest: Stream[F, Token],
                                          f: Json => F[Json],
                                          context: JsonContext,
                                          chunkAcc: List[Token],
                                          key: Option[String])(implicit
      F: RaiseThrowable[F],
      builder: Builder[Json],
      tokenizer: Tokenizer[Json]): Pull[F, Token, Result[F, Token, List[Token]]] =
    ValueParser.pullValue(chunk, idx, rest).flatMap {
      case Some((chunk, idx, rest, json)) =>
        Pull
          .eval(f(json))
          .handleErrorWith(t =>
            emitChunk(chunkAcc) >> Pull.raiseError(
              new JsonException("An error occurred while transforming Json data", Some(context), t)))
          .map(transformed => Some((chunk, idx, rest, tokenizer.tokenize(transformed).toList reverse_::: chunkAcc)))
      case None => Pull.pure(None)
    }

  private def selectName[F[_]](chunk: Chunk[Token],
                               idx: Int,
                               rest: Stream[F, Token],
                               emitNonSelected: Boolean,
                               wrap: Boolean,
                               emitEarly: Boolean,
                               toSelect: String => Boolean,
                               mandatories: Set[String],
                               context: JsonContext,
                               onSelect: (Chunk[Token],
                                          Int,
                                          Stream[F, Token],
                                          JsonContext,
                                          List[Token],
                                          Option[String]) => Pull[F, Token, Result[F, Token, List[Token]]],
                               chunkAcc: List[Token])(implicit
      F: RaiseThrowable[F]): Pull[F, Token, Result[F, Token, List[Token]]] =
    if (idx >= chunk.size) {
      emitChunk(chunkAcc) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) =>
          selectName(hd, 0, tl, emitNonSelected, wrap, emitEarly, toSelect, mandatories, context, onSelect, Nil)
        case None => Pull.raiseError[F](new JsonException("unexpected end of input", Some(context)))
      }
    } else
      chunk(idx) match {
        case key @ Token.Key(name) =>
          val action =
            if (toSelect(name)) {
              // name is to be selected, then continue
              val chunkAcc1 = if (wrap && emitEarly) key :: chunkAcc else chunkAcc
              onSelect(chunk, idx + 1, rest, JsonContext.Key(name, context), chunkAcc1, Some(name))
            } else if (emitNonSelected) {
              val chunkAcc1 = if (wrap) key :: chunkAcc else chunkAcc
              emitValue(chunk, idx + 1, rest, 0, chunkAcc1)
            } else {
              // skip the value and continue
              skipValue(chunk, idx + 1, rest, 0, chunkAcc)
            }
          action.flatMap {
            case Some((chunk, idx, rest, chunkAcc)) =>
              selectName(chunk,
                         idx,
                         rest,
                         emitNonSelected,
                         wrap,
                         emitEarly,
                         toSelect,
                         mandatories - name,
                         JsonContext.Key(name, context),
                         onSelect,
                         chunkAcc)
            case None =>
              Pull.raiseError[F](new JsonException("unexpected end of input", Some(context)))
          }

        case Token.EndObject =>
          // object is done, go up
          // but first check if all mandatory fields have been seen
          if (mandatories.isEmpty) {
            val chunkAcc1 = if (wrap) Token.EndObject :: chunkAcc else chunkAcc
            Pull.pure(Some((chunk, idx + 1, rest, chunkAcc1)))
          } else {
            emitChunk(chunkAcc) >> Pull.raiseError[F](
              new JsonMissingFieldException(s"missing mandatory fields: ${mandatories.mkString(", ")}", mandatories))
          }
        case token =>
          emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException("malformed json", Some(context)))
      }

  private def selectIndex[F[_]](chunk: Chunk[Token],
                                idx: Int,
                                rest: Stream[F, Token],
                                emitNonSelected: Boolean,
                                wrap: Boolean,
                                arrIdx: Int,
                                toSelect: Int => Boolean,
                                context: JsonContext,
                                onSelect: (Chunk[Token],
                                           Int,
                                           Stream[F, Token],
                                           JsonContext,
                                           List[Token],
                                           Option[String]) => Pull[F, Token, Result[F, Token, List[Token]]],
                                chunkAcc: List[Token])(implicit
      F: RaiseThrowable[F]): Pull[F, Token, Result[F, Token, List[Token]]] =
    if (idx >= chunk.size) {
      emitChunk(chunkAcc) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) => selectIndex(hd, 0, tl, emitNonSelected, wrap, arrIdx, toSelect, context, onSelect, Nil)
        case None           => Pull.raiseError[F](new JsonException("unexpected end of input", Some(context)))
      }
    } else
      chunk(idx) match {
        case Token.EndArray =>
          // array is done, go up
          val chunkAcc1 = if (wrap) Token.EndArray :: chunkAcc else chunkAcc
          Pull.pure(Some((chunk, idx + 1, rest, chunkAcc1)))
        case token =>
          val action =
            if (toSelect(arrIdx))
              // index is to be selected, then continue
              onSelect(chunk, idx, rest, JsonContext.Index(arrIdx, context), chunkAcc, None)
            else if (emitNonSelected)
              emitValue(chunk, idx, rest, 0, chunkAcc)
            else
              // skip the value and continue
              skipValue(chunk, idx, rest, 0, chunkAcc)
          action.flatMap {
            case Some((chunk, idx, rest, chunkAcc)) =>
              selectIndex(chunk, idx, rest, emitNonSelected, wrap, arrIdx + 1, toSelect, context, onSelect, chunkAcc)
            case None =>
              Pull.raiseError[F](new JsonException("unexpected end of input", Some(context)))
          }
      }

  private def filterChunk[F[_]](chunk: Chunk[Token],
                                idx: Int,
                                rest: Stream[F, Token],
                                selector: Selector,
                                emitNonSelected: Boolean,
                                wrap: Boolean,
                                emitEarly: Boolean,
                                context: JsonContext,
                                onSelect: (Chunk[Token],
                                           Int,
                                           Stream[F, Token],
                                           JsonContext,
                                           List[Token],
                                           Option[String]) => Pull[F, Token, Result[F, Token, List[Token]]],
                                chunkAcc: List[Token],
                                key: Option[String])(implicit
      F: RaiseThrowable[F]): Pull[F, Token, Result[F, Token, List[Token]]] =
    if (idx >= chunk.size) {
      emitChunk(chunkAcc) >> rest.pull.uncons.flatMap {
        case Some((hd, tl)) =>
          filterChunk(hd, 0, tl, selector, emitNonSelected, wrap, emitEarly, context, onSelect, Nil, key)
        case None => Pull.pure(None)
      }
    } else
      selector match {
        case Selector.ThisSelector =>
          onSelect(chunk, idx, rest, context, chunkAcc, key)
        case Selector.NameSelector(pred, strict, mandatory) =>
          chunk(idx) match {
            case Token.StartObject =>
              // enter the object context and go down to the name
              val chunkAcc1 = if (wrap) Token.StartObject :: chunkAcc else chunkAcc
              selectName(chunk,
                         idx + 1,
                         rest,
                         emitNonSelected,
                         wrap,
                         emitEarly,
                         pred,
                         if (mandatory) pred.values else Set.empty,
                         context,
                         onSelect,
                         chunkAcc1)
            case token =>
              if (strict)
                emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"cannot index ${token.kind} with string", Some(context)))
              else if (emitNonSelected)
                emitValue(chunk, idx, rest, 0, chunkAcc)
              else
                // skip the value and go up
                skipValue(chunk, idx, rest, 0, chunkAcc)
          }
        case Selector.IndexSelector(pred, strict) =>
          chunk(idx) match {
            case Token.StartArray =>
              // enter the array context and go down to the indices
              val chunkAcc1 = if (wrap) Token.StartArray :: chunkAcc else chunkAcc
              selectIndex(chunk, idx + 1, rest, emitNonSelected, wrap, 0, pred, context, onSelect, chunkAcc1)
            case token =>
              if (strict)
                emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"cannot index ${token.kind} with number", Some(context)))
              else if (emitNonSelected)
                emitValue(chunk, idx, rest, 0, chunkAcc)
              else
                // skip the value and go up
                skipValue(chunk, idx, rest, 0, chunkAcc)
          }
        case Selector.IteratorSelector(strict) =>
          chunk(idx) match {
            case Token.StartArray =>
              // enter the array context and go down to the indices
              val chunkAcc1 = if (wrap) Token.StartArray :: chunkAcc else chunkAcc
              selectIndex(chunk,
                          idx + 1,
                          rest,
                          emitNonSelected,
                          wrap,
                          0,
                          IndexPredicate.All,
                          context,
                          onSelect,
                          chunkAcc1)
            case Token.StartObject =>
              // enter the object context and go down to the name
              val chunkAcc1 = if (wrap) Token.StartObject :: chunkAcc else chunkAcc
              selectName(chunk,
                         idx + 1,
                         rest,
                         emitNonSelected,
                         wrap,
                         emitEarly,
                         NamePredicate.All,
                         Set.empty,
                         context,
                         onSelect,
                         chunkAcc1)
            case token =>
              if (strict)
                emitChunk(chunkAcc) >> Pull.raiseError[F](new JsonException(s"cannot iterate over ${token.kind}", Some(context)))
              else if (emitNonSelected)
                emitValue(chunk, idx, rest, 0, chunkAcc)
              else
                // skip the value and go up
                skipValue(chunk, idx, rest, 0, chunkAcc)
          }
        case Selector.PipeSelector(left, right) =>
          filterChunk(
            chunk,
            idx,
            rest,
            left,
            emitNonSelected,
            wrap,
            true,
            context,
            filterChunk(_, _, _, right, emitNonSelected, wrap, emitEarly, _, onSelect, _, _),
            chunkAcc,
            key
          )
      }

  private def go[F[_]](chunk: Chunk[Token],
                       idx: Int,
                       rest: Stream[F, Token],
                       selector: Selector,
                       emitNonSelected: Boolean,
                       wrap: Boolean,
                       emitEarly: Boolean,
                       context: JsonContext,
                       onSelect: (Chunk[Token],
                                  Int,
                                  Stream[F, Token],
                                  JsonContext,
                                  List[Token],
                                  Option[String]) => Pull[F, Token, Result[F, Token, List[Token]]],
                       chunkAcc: List[Token])(implicit F: RaiseThrowable[F]): Pull[F, Token, Unit] =
    filterChunk(chunk, idx, rest, selector, emitNonSelected, wrap, emitEarly, context, onSelect, chunkAcc, None)
      .flatMap {
        case Some((chunk, idx, rest, chunkAcc)) =>
          go(chunk, idx, rest, selector, emitNonSelected, wrap, emitEarly, context, onSelect, chunkAcc)
        case None =>
          Pull.done
      }

  def pipe[F[_]](selector: Selector, wrap: Boolean)(implicit F: RaiseThrowable[F]): Pipe[F, Token, Token] =
    s => go(Chunk.empty, 0, s, selector, false, wrap, true, JsonContext.Root, emit[F], Nil).stream

  def transformPipe[F[_], Json: Builder: Tokenizer](selector: Selector, f: Json => Option[Json])(implicit
      F: RaiseThrowable[F]): Pipe[F, Token, Token] =
    s => go(Chunk.empty, 0, s, selector, true, true, false, JsonContext.Root, transform[F, Json](f), Nil).stream

  def transformPipeF[F[_], Json: Builder: Tokenizer](selector: Selector, f: Json => F[Json])(implicit
      F: RaiseThrowable[F]): Pipe[F, Token, Token] =
    s => go(Chunk.empty, 0, s, selector, true, true, true, JsonContext.Root, transformF[F, Json](f), Nil).stream

  private def emit[F[_]](chunk: Chunk[Token],
                         idx: Int,
                         rest: Stream[F, Token],
                         context: JsonContext,
                         chunkAcc: List[Token],
                         key: Option[String])(implicit
      F: RaiseThrowable[F]): Pull[F, Token, Result[F, Token, List[Token]]] =
    emitValue[F](chunk, idx, rest, 0, chunkAcc)

  private def transform[F[_], Json: Builder: Tokenizer](f: Json => Option[Json])(chunk: Chunk[Token],
                                                                                 idx: Int,
                                                                                 rest: Stream[F, Token],
                                                                                 context: JsonContext,
                                                                                 chunkAcc: List[Token],
                                                                                 key: Option[String])(implicit
      F: RaiseThrowable[F]): Pull[F, Token, Result[F, Token, List[Token]]] =
    transformValue(chunk, idx, rest, f, context, chunkAcc, key)

  private def transformF[F[_], Json: Builder: Tokenizer](f: Json => F[Json])(chunk: Chunk[Token],
                                                                             idx: Int,
                                                                             rest: Stream[F, Token],
                                                                             context: JsonContext,
                                                                             chunkAcc: List[Token],
                                                                             key: Option[String])(implicit
      F: RaiseThrowable[F]): Pull[F, Token, Result[F, Token, List[Token]]] =
    transformValueF(chunk, idx, rest, f, context, chunkAcc, key)


}
