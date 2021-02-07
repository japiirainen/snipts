import * as I from 'io-ts'
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'
import { Env } from '../../infrastructure/env'
import { DBError } from '../../infrastructure/db'
import { InvalidRequest, ValidationFailed } from '../../infrastructure/error'
import { Snippet } from './snippet'
import { mapLeft } from 'fp-ts/lib/Either'
import { insertSnippet } from './snippetRepo'

const NewSnippetBody = I.interface({
   title: I.string,
   description: I.string,
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
      TE.chain(newPost => insertSnippet({ ...newPost, creator: env.user.id }, env.pool)),
      TE.chain(TE.fromOption(() => new DBError()))
   )
