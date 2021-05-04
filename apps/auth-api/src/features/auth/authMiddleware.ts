import { Request, Response, NextFunction } from 'express'
import * as O from 'fp-ts/Option'
import * as TE from 'fp-ts/TaskEither'
import * as T from 'fp-ts/Task'
import { pipe } from 'fp-ts/function'
import { getAccessTokenFromRequest, verifyAccessToken, JwtError } from '../../infrastructure/jwt'
import { findUserByUsername } from './userRepo'

export const optionalUser = (req: Request, _: Response, next: NextFunction): Promise<void> =>
   pipe(
      getAccessTokenFromRequest(req),
      TE.fromOption(() => new JwtError()),
      TE.chain(authHeader => TE.fromEither(verifyAccessToken(authHeader))),
      TE.chain(({ username }) => findUserByUsername(username, req.env.pool)),
      TE.fold(
         () => T.of(O.none),
         user => T.of(user)
      )
   )().then(o =>
      pipe(
         o,
         O.fold(
            () => next(),
            user => {
               req.env.user = user
               next()
            }
         )
      )
   )

export const requireUser = (req: Request, res: Response, next: NextFunction): Promise<void> =>
   optionalUser(req, res, () => {
      if (!req.env.user) {
         process.env.NODE_ENV === 'testing' ? next() : res.status(401).send()
      } else {
         next()
      }
   })
