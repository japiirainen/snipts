import { Snippet, User, Id } from '@snipts/types'
import * as TE from 'fp-ts/TaskEither'
import * as O from 'fp-ts/Option'
import * as A from 'fp-ts/Array'
import { Pool } from 'pg'

import { DBError } from '../../infrastructure/db'
import { withConn } from '../../infrastructure/db'

export const insertSnippet = (
   dto: Snippet.InsertSnippetDTO,
   pool: Pool
): TE.TaskEither<DBError, O.Option<Snippet.Snippet>> =>
   withConn(pool, conn =>
      conn
         .query(
            'INSERT INTO snippets (title, author, description, content) VALUES ($1, $2, $3, $4) RETURNING *',
            [dto.title, dto.author, dto.description, dto.content]
         )
         .then(res => A.head(res.rows))
   )

export const allSnippets = (
   pool: Pool
): TE.TaskEither<DBError, O.Option<Array<Snippet.Snippet>>> =>
   withConn(pool, conn =>
      conn
         .query('SELECT * FROM snippets;')
         .then(res => O.fromNullable(res.rows))
   )

export const findSnippetById = (
   id: Id.Id<Snippet.Snippet>,
   pool: Pool
): TE.TaskEither<DBError, O.Option<Snippet.Snippet>> =>
   withConn(pool, conn =>
      conn
         .query('SELECT * from snippets WHERE id = $1 LIMIT 1', [id])
         .then(res => A.head(res.rows))
   )

export const findSnippetsByAuthor = (
   userId: Id.Id<User.User>,
   pool: Pool
): TE.TaskEither<DBError, O.Option<Array<Snippet.Snippet>>> =>
   withConn(pool, conn =>
      conn
         .query('SELECT * FROM snippets WHERE author = $1', [userId])
         .then(res => O.fromNullable(res.rows))
   )
