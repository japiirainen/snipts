/* eslint-disable @typescript-eslint/ban-ts-comment */
import { Request, Response } from 'express'
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'
import { processError } from '../../infrastructure/error'
import { register as processRegister } from './registerService'
import { toPublicUser } from './user'
import ms from 'ms'
import {
   generateAccessToken,
   generateRefreshToken,
   verifyRefreshToken,
} from '../../infrastructure/jwt'

const setRefreshToken = (res: Response, token: string) => {
   res.cookie('rtid', token, {
      httpOnly: true,
      path: '/refresh-token',
      /** Must be same as the refresh token exp! */
      expires: new Date(Date.now() + ms('30d')),
   })
}

export const authRoutes = {
   me(req: Request, res: Response): void {
      res.json(toPublicUser(req.env.user))
   },

   register(req: Request, res: Response): void {
      processRegister(req.env, req.body)().then(e =>
         pipe(
            e,
            E.fold(processError(res), () => res.status(200).send())
         )
      )
   },

   refreshToken(req: Request, res: Response): void {
      pipe(
         verifyRefreshToken(req.cookies.rtid),
         E.fold(
            () => res.status(401).send(),
            ({ username }) => {
               setRefreshToken(res, generateRefreshToken(username))
               return res.json({ accesstoken: generateAccessToken(username) })
            }
         )
      )
   },
}
