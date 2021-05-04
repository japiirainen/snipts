import { User } from '@snipts/types'
import * as E from 'fp-ts/Either'
import * as TE from 'fp-ts/TaskEither'
import { pipe } from 'fp-ts/function'
import { CustomError } from 'ts-custom-error'

import { BcryptError, comparePasswords } from '../../infrastructure/bcrypt'
import { DBError } from '../../infrastructure/db'
import { Env } from '../../infrastructure/env'
import { ApplicationError, InvalidRequest } from '../../infrastructure/error'
import {
   generateAccessToken,
   generateRefreshToken,
} from '../../infrastructure/jwt'
import { findUserByUsername } from './userRepo'

export class UserNotFound extends CustomError implements ApplicationError {
   status = 400
   code = 'UserNotFound'
   log = true
}

export const login = (
   env: Env,
   rawBody: unknown
): TE.TaskEither<
   InvalidRequest | UserNotFound | DBError | BcryptError,
   {
      accessToken: string
      refreshToken: string
      user: User.User
   }
> =>
   pipe(
      TE.fromEither(
         pipe(
            User.LoginBody.decode(rawBody),
            E.mapLeft(() => new InvalidRequest())
         )
      ),
      TE.chain(body =>
         pipe(
            findUserByUsername(body.username, env.pool),
            TE.chain(maybeUser =>
               pipe(
                  maybeUser,
                  TE.fromOption(() => new UserNotFound()),
                  TE.chain(user =>
                     pipe(
                        comparePasswords(user.password, body.password),
                        TE.chain(isSamePassword =>
                           isSamePassword
                              ? TE.right(user)
                              : TE.left(new UserNotFound())
                        )
                     )
                  )
               )
            )
         )
      ),
      TE.map(user => ({
         accessToken: generateAccessToken(user.username),
         refreshToken: generateRefreshToken(user.username),
         user,
      }))
   )
