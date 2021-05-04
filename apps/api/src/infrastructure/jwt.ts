import * as E from 'fp-ts/Either'
import * as O from 'fp-ts/Option'
import { pipe } from 'fp-ts/function'
import jwt from 'jsonwebtoken'
import { v4 as uuidv4 } from 'uuid'
import { Request } from 'express'
import { CustomError } from 'ts-custom-error'

import { ApplicationError } from './error'
import { config } from './config'

export class JwtError extends CustomError implements ApplicationError {
   status = 401
   code = uuidv4()
   log = true
}

export const generateAccessToken = (username: string): string =>
   jwt.sign({ username }, config.jwt.at!, {
      algorithm: 'HS256',
      issuer: config.application.name,
      expiresIn: '15m',
   })

export const generateRefreshToken = (username: string): string =>
   jwt.sign({ username }, config.jwt.rt!, {
      algorithm: 'HS256',
      issuer: config.application.name,
      expiresIn: '30d',
   })

export const verifyAccessToken = (
   token: string
): E.Either<JwtError, { username: string }> =>
   E.tryCatch(
      () => jwt.verify(token, config.jwt.at!) as never,
      e =>
         e instanceof Error
            ? new JwtError(e.message)
            : new JwtError('unknown error')
   )

export const verifyRefreshToken = (
   token: string
): E.Either<JwtError, { username: string }> =>
   E.tryCatch(
      () => jwt.verify(token, config.jwt.rt!) as never,
      e =>
         e instanceof Error
            ? new JwtError(e.message)
            : new JwtError('unknown error')
   )

export const getAccessTokenFromRequest = (req: Request): O.Option<string> =>
   pipe(
      O.fromNullable(req.header('Authorization')),
      O.chainNullableK(maybeToken => maybeToken.split(' ').pop())
   )
