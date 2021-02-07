import * as TE from 'fp-ts/TaskEither'
import * as O from 'fp-ts/Option'
import * as A from 'fp-ts/Array'
import { Pool } from 'pg'
import { DBError } from '../../infrastructure/db'
import { withConn } from '../../infrastructure/db'
import { Snippet } from './snippet'
import { Id } from '../../infrastructure/id'
import { User } from '../auth/user'

export interface InsertSnippetDTO {
   title: string
   description?: string
   creator: Id<User>
   content: string
}

export const insertSnippet = (
   dto: InsertSnippetDTO,
   pool: Pool
): TE.TaskEither<DBError, O.Option<{ id: Id<Snippet> }>> =>
   withConn(pool, conn =>
      conn
         .query(
            'INSERT INTO snippets (title, creator, description, content) VALUES ($1, $2, $3, $4) RETURNING id',
            [dto.title, dto.creator, dto.description, dto.content]
         )
         .then(res => A.head(res.rows))
   )

export const allSnippets = (pool: Pool): TE.TaskEither<DBError, O.Option<Array<Snippet>>> =>
   withConn(pool, conn =>
      conn.query('SELECT * FROM snippets;').then(res => O.fromNullable(res.rows))
   )

export const findSnippetById = (
   id: Id<Snippet>,
   pool: Pool
): TE.TaskEither<DBError, O.Option<Snippet>> =>
   withConn(pool, conn =>
      conn.query('SELECT * from snippets WHERE id = $1 LIMIT 1', [id]).then(res => A.head(res.rows))
   )

export const findSnippetsByCreator = (
   userId: Id<User>,
   pool: Pool
): TE.TaskEither<DBError, O.Option<Array<Snippet>>> =>
   withConn(pool, conn =>
      conn
         .query('SELECT * FROM snippets WHERE creator = $1', [userId])
         .then(res => O.fromNullable(res.rows))
   )
