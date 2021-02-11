import * as I from 'io-ts'
import * as TE from 'fp-ts/TaskEither'
import * as NEA from 'fp-ts/NonEmptyArray'
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'
import { Env } from '../../infrastructure/env'
import { DBError } from '../../infrastructure/db'
import { ApplicationError, InvalidRequest, ValidationFailed } from '../../infrastructure/error'
import { Snippet } from './snippet'
import { CustomError } from 'ts-custom-error'
import { allSnippets as getAllSnippets, findSnippetsByCreator, insertSnippet } from './snippetRepo'
import { Pool } from 'pg'
import { UserNotFound } from '../auth/loginService'
import { findUserById } from '../auth/userRepo'

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
            E.mapLeft(() => new InvalidRequest())
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

const SnippetsByCreatorBody = I.interface({
   creator: I.number,
})

export type SnippetsByCreatorBody = I.TypeOf<typeof SnippetsByCreatorBody>

export const snippetsByCreator = (
   env: Env,
   rawBody: unknown
): TE.TaskEither<
   NoSnippetsError | DBError | UserNotFound | InvalidRequest,
   NEA.NonEmptyArray<Snippet>
> =>
   pipe(
      TE.fromEither(
         pipe(
            SnippetsByCreatorBody.decode(rawBody),
            E.mapLeft(() => new InvalidRequest())
         )
      ),
      TE.chain(({ creator }) =>
         pipe(
            findUserById(creator, env.pool),
            TE.chain(maybeUser =>
               pipe(
                  maybeUser,
                  TE.fromOption(() => new UserNotFound())
               )
            )
         )
      ),
      TE.chain(user =>
         pipe(
            findSnippetsByCreator(user.id, env.pool),
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
      )
   )
