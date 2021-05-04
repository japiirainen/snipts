import { User } from '@snipts/types'
import * as NA from 'fp-ts/NonEmptyArray'
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'
import { CustomError } from 'ts-custom-error'

import { DBError } from '../../infrastructure/db'
import { Env } from '../../infrastructure/env'
import { ApplicationError } from '../../infrastructure/error'
import { allUsers as findAllUsers } from './userRepo'

class NoUsersFound extends CustomError implements ApplicationError {
   status = 400
   code = 'NoUsersFound'
   log = true
}

export const allUsers = (
   env: Env
): TE.TaskEither<
   DBError | NoUsersFound,
   { users: NA.NonEmptyArray<User.User> }
> =>
   pipe(
      findAllUsers(env.pool),
      TE.chain(maybeUser =>
         pipe(
            maybeUser,
            TE.fromOption(() => new DBError())
         )
      ),
      TE.chain(users =>
         pipe(
            users,
            NA.fromArray,
            TE.fromOption(() => new NoUsersFound())
         )
      ),
      TE.map(users => ({ users }))
   )
