import cookieParser from 'cookie-parser'
import cors from 'cors'
import express, { Express } from 'express'
import morgan from 'morgan'
import { requireUser } from './features/auth/authMiddleware'
import { authRoutes } from './features/auth/authRoutes'
import { snippetRoutes } from './features/snippet/snippetRouter'
import { createDbPool } from './infrastructure/db'
import { initializeEnv } from './infrastructure/env'

export const createApp = async (): Promise<Express> => {
   const prerequisites = [createDbPool(), Promise.resolve()] as const
   const [pool] = await Promise.all(prerequisites)
   const app = express()

   app.use(morgan('dev'))
      .use(
         cors({
            credentials: true,
            origin: 'http://localhost:4000',
         })
      )
      .use(express.json())
      .use(cookieParser())
      .get('/health', (_, res) =>
         pool
            ? res.status(200).json({ status: 'healthy' })
            : res.status(503).json({ status: 'unavailable' })
      )
      .use(initializeEnv(pool))
      // ? Auth
      .get('/me', requireUser, authRoutes.me)
      .post('/login', authRoutes.login)
      .post('/logout', requireUser, authRoutes.logout)
      .post('/register', authRoutes.register)
      .post('/refresh-token', authRoutes.refreshToken)
      // ? Snippets
      .get('/snippet', snippetRoutes.all)
      .post('/snippet', requireUser, snippetRoutes.new)
      .get('/creator/snippets', snippetRoutes.allByCreator)

   return app
}