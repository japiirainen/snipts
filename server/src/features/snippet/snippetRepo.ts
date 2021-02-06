import * as TE from 'fp-ts/TaskEither'
import * as O from 'fp-ts/Option'
import * as A from 'fp-ts/Array'
import { pipe } from 'fp-ts/function'
import { Pool } from 'pg'
import { DBError } from '../../infrastructure/db'
import { withConn } from '../../infrastructure/db'
import { Snippet } from './snippet'

export interface InsertSnippetDTO {
   title: string
   description?: string
   content: string
}

export const insertSnippet = (
   dto: InsertSnippetDTO,
   pool: Pool
): TE.TaskEither<DBError, O.Option<{ id: string }>> =>
   withConn(pool, conn =>
      conn
         .query(
            'INSERT INTO snippets (title, description, content) VALUES ($1, $2, $3) RETURNING id',
            [dto.title, dto.description, dto.content]
         )
         .then(res => A.head(res.rows))
   )

export const allSnippets = (pool: Pool): TE.TaskEither<DBError, O.Option<Array<Snippet>>> =>
   withConn(pool, conn =>
      conn.query('SELECT * FROM snippets;').then(res => O.fromNullable(res.rows))
   )

export const findSnippetById = (
   id: string,
   pool: Pool
): TE.TaskEither<DBError, O.Option<Snippet>> =>
   withConn(pool, conn =>
      conn.query('SELECT * from snippets WHERE id = $1 LIMIT 1').then(res => A.head(res.rows))
   )
