import { Request, Response } from 'express'
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'
import { processError } from '../../infrastructure/error'
import { register as processRegister } from './registerService'
import { toPublicUser } from './user'

export const authRoutes = {
   me(req: Request, res: Response): void {
      res.json(toPublicUser(req.env.user))
   },

   register(req: Request, res: Response): void {
      processRegister(req.env, req.body)().then(e =>
         pipe(
            e,
            E.fold(processError(res), () => res.status(200).json({ foo: 'bar' }))
         )
      )
   },
}
