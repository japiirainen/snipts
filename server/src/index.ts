import cookieParser from 'cookie-parser'
import cors from 'cors'
import express, { Express } from 'express'
import morgan from 'morgan'
import { requireUser } from './features/auth/authMiddleware'
import { authRoutes } from './features/auth/authRoutes'
import { config } from './infrastructure/config'
import { createDbPool } from './infrastructure/db'
import { initializeEnv } from './infrastructure/env'
import { logger } from './infrastructure/logger'

const createApp = async (): Promise<Express> => {
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
      .get('/health', (_, res) => {
         if (pool) {
            res.status(200).json({ status: 'healthy' })
         } else {
            res.status(503).json({ status: 'unavailable' })
         }
      })
      .use(initializeEnv(pool))
      .get('/me', requireUser, authRoutes.me)
      .post('login', authRoutes.login)
      .post('/logout', authRoutes.logout)
      .post('/register', authRoutes.register)
      .post('/refresh-token', authRoutes.refreshToken)

   return app
}

createApp().then(app =>
   app.listen(config.application.port, () =>
      logger.info(`${config.application.name} is listening on ${config.application.port}`)
   )
)
