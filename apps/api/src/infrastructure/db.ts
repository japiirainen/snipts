import * as TE from 'fp-ts/TaskEither'
import { Pool, PoolClient } from 'pg'
import { migrate } from 'postgres-migrations'
import { CustomError } from 'ts-custom-error'
import { v4 as uuidv4 } from 'uuid'
import { config } from './config'
import { ApplicationError } from './error'
import { logger } from './logger'

export class DBError extends CustomError implements ApplicationError {
   status = 500
   code = uuidv4()
   log = true
}

export const createDbPool = async (): Promise<Pool | null> => {
   const pool = new Pool({
      database: config.db.database,
      user: config.db.user,
      password: config.db.password,
      host: config.db.host,
      port: Number(config.db.port),
   })

   try {
      const client = await pool.connect()

      try {
         await migrate({ client }, 'sql')
      } finally {
         await client.release()
      }
   } catch (e) {
      logger.error('Failed to connect to db')
      return null
   }
   return pool
}

export const withConn = <T>(
   pool: Pool,
   f: (conn: PoolClient) => Promise<T>
): TE.TaskEither<DBError, T> =>
   TE.tryCatch(
      async () => {
         const client = await pool.connect()
         try {
            return await f(client)
         } catch (e) {
            throw new DBError(e.message)
         } finally {
            await client.release()
         }
      },
      () => new DBError('db error')
   )
