import * as A from 'fp-ts/Array'
import * as O from 'fp-ts/Option'
import * as TE from 'fp-ts/TaskEither'
import { Pool } from 'pg'
import { DBError, witConn } from '../../infrastructure/db'
import { User } from './user'

export interface InsertUserDTO {
   username: string
   email: string
   password: string
}

export const insertUser = (dto: InsertUserDTO, pool: Pool): TE.TaskEither<DBError, void> =>
   witConn(pool, async conn => {
      await conn.query('INSERT INTO users (username, email, password) VALUES ($1, $2, $3)', [
         dto.username,
         dto.email,
         dto.password,
      ])
   })

export const findUserByUsername = (
   username: string,
   pool: Pool
): TE.TaskEither<DBError, O.Option<User>> =>
   witConn(pool, conn =>
      conn
         .query('SELECT * FROM users WHERE username = $1 LIMIT 1', [username])
         .then(res => A.head(res.rows))
   )

export const findUserByEmail = (
   email: string,
   pool: Pool
): TE.TaskEither<DBError, O.Option<User>> =>
   witConn(pool, conn =>
      conn
         .query('SELECT * FROM users where email = $1 LIMIT 1', [email])
         .then(res => A.head(res.rows))
   )
