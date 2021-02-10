import * as I from 'io-ts'
import * as TE from 'fp-ts/TaskEither'
import * as NEA from 'fp-ts/NonEmptyArray'
import { pipe } from 'fp-ts/function'
import { Env } from '../../infrastructure/env'
import { DBError } from '../../infrastructure/db'
import { ApplicationError, InvalidRequest, ValidationFailed } from '../../infrastructure/error'
import { Snippet } from './snippet'
import { mapLeft } from 'fp-ts/lib/Either'
import { insertSnippet } from './snippetRepo'
import { CustomError } from 'ts-custom-error'
import { allSnippets as getAllSnippets } from './snippetRepo'

class NoSnippetsError extends CustomError implements ApplicationError {
   status = 400
   code = 'NoSnippetsError'
   log = false
}

const NewSnippetBody = I.interface({
   title: I.string,
   description: I.string,
   creator: I.number,
   content: I.string,
})

export type NewSnippetBodyT = I.TypeOf<typeof NewSnippetBody>

export const newSnippet = (
   env: Env,
   rawBody: unknown
): TE.TaskEither<DBError | ValidationFailed, Snippet> =>
   pipe(
      TE.fromEither(
         pipe(
            NewSnippetBody.decode(rawBody),
            mapLeft(() => new InvalidRequest())
         )
      ),
      TE.chain(newPost => insertSnippet(newPost, env.pool)),
      TE.chain(TE.fromOption(() => new DBError()))
   )

export const allSnippets = (
   env: Env
): TE.TaskEither<NoSnippetsError | DBError, NEA.NonEmptyArray<Snippet>> =>
   pipe(
      getAllSnippets(env.pool),
      TE.chain(maybeSnippets =>
         pipe(
            maybeSnippets,
            TE.fromOption(() => new DBError())
         )
      ),
      TE.chain(snippets =>
         pipe(
            snippets,
            NEA.fromArray,
            TE.fromOption(() => new NoSnippetsError())
         )
      )
   )
