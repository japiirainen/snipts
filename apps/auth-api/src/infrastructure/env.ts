import { NextFunction, Request, Response } from 'express'
import { Pool } from 'pg'
import { User } from '../features/auth/user'

export interface Env {
   user: User
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
