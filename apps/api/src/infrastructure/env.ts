import { User } from '@snipts/types'
import { NextFunction, Request, Response } from 'express'
import { Pool } from 'pg'

export interface Env {
   user: User.User
   pool: Pool
}

export const initializeEnv = (pool: Pool | null) => (
   req: Request,
   _: Response,
   next: NextFunction
): void => {
   req.env = { pool } as never
   next()
}
