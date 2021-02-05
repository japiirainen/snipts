import cookieParser from 'cookie-parser'
import cors from 'cors'
import express, { Express } from 'express'
import morgan from 'morgan'
import { authRoutes } from './features/auth/AuthRouter'
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
      .get('/me', authRoutes.me)
      .post('/register', authRoutes.register)
   return app
}

createApp().then(app =>
   app.listen(config.port, () => logger.info(`App listening on ${config.port}`))
)
