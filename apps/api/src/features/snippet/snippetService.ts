import * as I from 'io-ts'
import * as TE from 'fp-ts/TaskEither'
import * as NEA from 'fp-ts/NonEmptyArray'
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'

import { Env } from '../../infrastructure/env'
import { DBError } from '../../infrastructure/db'
import {
   ApplicationError,
   InvalidRequest,
   ValidationFailed,
} from '../../infrastructure/error'
import { Snippet } from '@snipts/types'
import { CustomError } from 'ts-custom-error'
import {
   allSnippets as getAllSnippets,
   findSnippetsByAuthor,
   insertSnippet,
} from './snippetRepo'
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
   author: I.number,
   content: I.string,
})

export type NewSnippetBodyT = I.TypeOf<typeof NewSnippetBody>

export const newSnippet = (
   env: Env,
   rawBody: unknown
): TE.TaskEither<DBError | ValidationFailed, Snippet.Snippet> =>
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
): TE.TaskEither<
   NoSnippetsError | DBError,
   NEA.NonEmptyArray<Snippet.Snippet>
> =>
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

const SnippetsByAuthorBody = I.interface({
   author: I.number,
})

export type SnippetsByAuthorBody = I.TypeOf<typeof SnippetsByAuthorBody>

export const snippetsByAuthor = (
   env: Env,
   rawBody: unknown
): TE.TaskEither<
   NoSnippetsError | DBError | UserNotFound | InvalidRequest,
   NEA.NonEmptyArray<Snippet.Snippet>
> =>
   pipe(
      TE.fromEither(
         pipe(
            SnippetsByAuthorBody.decode(rawBody),
            E.mapLeft(() => new InvalidRequest())
         )
      ),
      TE.chain(({ author }) =>
         pipe(
            findUserById(author, env.pool),
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
            findSnippetsByAuthor(user.id, env.pool),
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
